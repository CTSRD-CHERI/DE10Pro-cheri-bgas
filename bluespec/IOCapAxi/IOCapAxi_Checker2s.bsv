import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;
import MapFIFO :: *;
import ConfigReg :: *;
import SamUtil :: *;
import Assert :: *;

import IOCapAxi_Types :: *;
import IOCapAxi_Flits :: *;
import IOCapAxi_Konata :: *;

import Cap2024 :: *;
import Cap2024_11 :: *;
import Cap2024_11_Decode_FastFSM :: *;
import Cap2024_SigCheck_Aes_1RoundPerCycle :: *; // Get CapSigCheckIn
import Cap2024_SigCheck_Aes_2RoundPerCycleFast :: *;

interface IOCapAxiChecker2#(type iocap_flit, type no_iocap_flit);
    interface Sink#(Tuple2#(iocap_flit, KFlitId)) in;
    interface Source#(Tuple4#(no_iocap_flit, KFlitId, KeyId, Bool)) checkResponse;
    interface ReadOnly#(UInt#(64)) insertKThreadId;
endinterface

typedef union tagged {
    void DecodeIdle;
    void AwaitingFlitBounds;
    struct {
        Bit#(64) flitMin;
        Bit#(65) flitMax;
        Bool boundsFailed;
    } AwaitingIOCapDecode;
    struct {
        // If either the Flit Bounds decode or the IOCap decode failed.
        Bool failed;
    } Decoded;
} DecodeState deriving (Bits, FShow);

typedef union tagged {
    void SigCheckIdle;
    // Waiting for the keyRequest pipe to unblock
    KeyId AwaitingKeyAvailable;
    // Issued a keyRequest, waiting for a key response
    KeyId AwaitingKey;
    struct {
        KeyId keyId;
        Key keyData;
    } AwaitingSigCheckStart;
    KeyId SigCheckFailedEarly;
    struct {
        KeyId keyId;
        Bool keyInvalidatedDuringSigCheck;
    } AwaitingSigCheck;
    // Once the signature check completes, we assume that everything else has completed.
    struct {
        KeyId keyId;
        // If the signature check failed or the key was invalid
        Bool failed;
    } AwaitingRespAvailable;
} SigCheckState deriving (Bits, FShow, Eq);

typedef union tagged {
    void NoFlit;
    struct {
        no_iocap_flit flit;
        // Bit#(86) capBits1;
        // Bit#(86) capBits2;
        // Bit#(84) capBits3; 
    } Building0;
    struct {
        no_iocap_flit flit;
        Bit#(86) capBits1;
        // Bit#(86) capBits2;
        // Bit#(84) capBits3; 
    } Building1;
    struct {
        no_iocap_flit flit;
        Bit#(86) capBits1;
        Bit#(86) capBits2;
        // Bit#(84) capBits3; 
    } Building2;
    // struct {
    //     no_iocap_flit start;
    //     Bit#(86) capBits1;
    //     Bit#(86) capBits2;
    //     Bit#(84) capBits3; 
    // } Building3;
    // AuthenticatedFlit#(no_iocap_flit, Cap2024_11) Ready;
    AuthenticatedFlit#(no_iocap_flit, Cap2024_11) DecodingAndSigChecking;
} FlitState#(type no_iocap_flit) deriving (Bits, FShow);

// typedef  CurrentFlitState#(type no_iocap_flit) deriving (Bits, FShow);

// One-at-a-time IOCap flit checker merged with a AddressChannelCapUnwrapper - takes in raw IOCapAXI, sends requests to the keymanager,
// decodes+checks the iocap against the transasction, and outputs valid back.
// This interface improves over the v1 Checker by avoiding FIFO stages between a separate AddressChannelCapUnwrapper and the checker,
// and provides the opportunity to issue key ID requests as soon as the relevant data comes in in the first cap-data flit.
//
// Takes a function which returns a module connecting (inputs to a iocap decoder) to (outputs to a iocap decoder),
// a Sink to push key requests into,
// a ReadOnly#() that is constantly monitored for key data responses,
// a ReadOnly#() that is constantly monitored for key revoking requests (which cause the current job to immediately be revoked and called invalid if it uses that key),
// and a function that maps the capability to the actual keyId (sometimes not all the bits are used).
//
// TODO If I used a ReadOnly#(flit) and a ReadOnly#(yourenext) would that allow me to ditch a cycle in the pooling case? Right now I have to 
// have a mux in front of the pool that directs to one of N modules...
//
// Uses the 2-round-per-cycle signature checker, which should have the following latencies:
// | n_cavs | sigchk | 2024_11_fsm |
// | ------ | ------ | ----------- |
// |   0    |    6   |      3      |
// |   1    |   12   |      6      |
// |   2    |   18   |      8      |
//
// The Exposer testbenches measure AW->AW latency where Cycle 0 is the cycle where the last(!) flit is put into the unit
// Cycle #-3 - testbench puts first flit into Exposer FIFO
// Cycle #-2 - pull first flit, no IOCap data
//  (-> Building0, SigCheckIdle, DecodeIdle)
// Cycle #-1 - accumulate CapData1 flit, issue keyRequest into keyReqFF
//  (-> Building1, -> AwaitingKey, DecodeIdle)
// Cycle #0 - testbench inserts last flit, accumulate CapData2 flit, keyStore pulls out request
// TODO could start decoding here, but it wouldn't help. AES dominates
//  (-> Building2, AwaitingKey, DecodeIdle)
// Cycle #1 - accumulate CapData3 flit, keyStore puts resp into keyRespFF, start decoding
//  (-> DecodingAndSigChecking, AwaitingKey, -> AwaitingFlitBounds)
// Cycle #2 - keyRequest arrives from keyRespFF, decoding continues, begin AES directly using the key
//  (DecodingAndSigChecking, -> AwaitingSigCheck, -> AwaitingIOCapDecode)
// Cycle #8(?) - signature completes, decode has since completed, enqueue result into FIFO
// Cycle #9(?) - exposer sees result, pushes flit into output FIFO
// Cycle #10(?) - testbench sees result

//
// TODO FIGURE OUT THE EXPECTED LATENCIES, THE BELOW IS OLD
//
// Capabilities are decoded and signature-checked in parallel, and we can assume the decoder latency is always less than the signature check.
// We add ~3 cycles of latency on top of the signature check with the various FIFO stages, so the maximum latency should be ~21 cycles.
module mkSimpleIOCapAxiChecker2V1#(
    function module#(Empty) makeDecoder(Get#(Cap2024_11) ins, Put#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) outs),
    Sink#(KeyId) keyRequest,
    ReadOnly#(Maybe#(Tuple2#(KeyId, Maybe#(Key)))) keyResponse,
    ReadOnly#(Maybe#(KeyId)) keyToKill, 
    function KeyId keyIdOf(Cap2024_11 cap)
)(IOCapAxiChecker2#(iocap_flit, no_iocap_flit)) provisos (
    Bits#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11), a__),
    Bits#(FlitState#(no_iocap_flit), b__),
    Bits#(iocap_flit, c__),
    AxiCtrlFlit64#(no_iocap_flit),
    FShow#(no_iocap_flit),
    IOCapPackableFlit#(iocap_flit, no_iocap_flit)
);
    function Tuple4#(no_iocap_flit, KFlitId, KeyId, Bool) checkKeyNotKilled(Tuple4#(no_iocap_flit, KFlitId, KeyId, Bool) tup);
        if (keyToKill == tagged Valid tpl_3(tup)) begin
            return tuple4(tpl_1(tup), tpl_2(tup), tpl_3(tup), False);
        end else begin
            return tup;
        end
    endfunction

    NumProxy#(3) respsMapFIFOSize = ?;
    MapFIFO#(Tuple4#(no_iocap_flit, KFlitId, KeyId, Bool)) respsMapFIFO <- mkSizedMapFIFO(respsMapFIFOSize, checkKeyNotKilled);
    let resps = respsMapFIFO.enq;

    ConfigReg#(KFlitId) flitId <- mkConfigReg(?);

    FIFOF#(Tuple2#(iocap_flit, KFlitId)) reqFlits <- mkFIFOF;
    let incomingFlits = toSource(reqFlits);

    ConfigReg#(FlitState#(no_iocap_flit)) currentFlit <- mkConfigReg(tagged NoFlit);
    // Reg#(Maybe#(CurrentFlitState#(no_iocap_flit))) currentFlit <- mkReg(tagged Invalid);
    ConfigReg#(DecodeState) decodeState <- mkConfigReg(tagged DecodeIdle);
    ConfigReg#(SigCheckState) sigCheckState <- mkConfigReg(tagged SigCheckIdle);

    // The keyId we just extracted from the flit in the Building0 -> Building1 transition.
    // Used for ticking the sigCheck machine as early as possible.
    RWire#(KeyId) keyIdForConstructingFlit <- mkRWire;
    // The full AuthenticatedFlit we just extracted in the Building2 -> DecodingAndSigChecking transition.
    // Will NEVER be Valid at the same time as the keyIdForConstructingFlit
    RWire#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11)) completedFlit <- mkRWire;
    // Pulses when the signature check finishes, triggering everything else to reset.
    PulseWire flitCompleted <- mkPulseWire;

    // TODO FIFOs are terrible for latency
    FIFOF#(Cap2024_11) decodeInFIFO <- mkFIFOF; 
    FIFOF#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) decodeOutFIFO <- mkFIFOF;
    makeDecoder(toGet(decodeInFIFO), toPut(decodeOutFIFO));
    let decodeIn <- toUnguardedSink(decodeInFIFO);
    let decodeOut <- toUnguardedSource(decodeOutFIFO, ?);

    // FIFOs are terrible for latency, this adds 2-cycles
    // mk2RoundPerCycleCapSigCheck(toGet(sigCheckInFIFO), toPut(sigCheckOutFIFO));
    // let sigCheckIn <- toUnguardedSink(sigCheckInFIFO);
    // let sigCheckOut <- toUnguardedSource(sigCheckOutFIFO, ?);

    RWire#(CapSigCheckIn#(Cap2024_11)) sigCheckInRWire <- mkRWire;
    RWire#(CapCheckResult#(Bit#(0))) sigCheckOutRWire <- mkRWire;
    mk2RoundPerCycleCapSigCheckFast(rwireToReadOnly(sigCheckInRWire), rwireToWriteOnly(sigCheckOutRWire));
    let sigCheckIn = interface Sink;
        method Bool canPut() = True;
        method Action put(x) = sigCheckInRWire.wset(x);
    endinterface;
    let sigCheckOut = interface Source;
        method Bool canPeek() = isValid(sigCheckOutRWire.wget());
        method peek() = fromMaybe(?, sigCheckOutRWire.wget());
        method Action drop() = noAction;
    endinterface;

    function Bool canAccumFlit();
        if (currentFlit matches tagged DecodingAndSigChecking .*)
            return False;
        else 
            return True;
    endfunction

    // (* no_implicit_conditions *)
    rule accumulate_flit(canAccumFlit() && incomingFlits.canPeek());
        case (currentFlit) matches
            tagged NoFlit : if (incomingFlits.canPeek()) begin
                match { .iocapFlit, .iocapFlitId } = incomingFlits.peek();
                IOCapFlitSpec#(no_iocap_flit) flitSpec = unpackSpec(iocapFlit);
                if (flitSpec matches tagged Start .flit) begin
                    currentFlit <= tagged Building0 {
                        flit: flit
                    };
                    $display("S\t", fshow(iocapFlitId), "\t10\tBuilding0");
                end else begin
                    $display("TODO BIG ERROR");
                end
                flitId <= iocapFlitId;
                incomingFlits.drop();
            end
            tagged Building0 { flit: .flit } : if (incomingFlits.canPeek()) begin
                match { .iocapFlit, .iocapFlitId } = incomingFlits.peek();
                dynamicAssert(flitId == iocapFlitId, "Got new mismatching flit while building");
                IOCapFlitSpec#(no_iocap_flit) flitSpec = unpackSpec(iocapFlit);
                if (flitSpec matches tagged CapBits1 .capBits1) begin
                    // This is possible because capBits1 is the MIDDLE.
                    // capBits1 covers a bottom part of the MAC and the top bits of the text,
                    // so unpack the pair. TODO CHECK PACKING ORDER
                    Tuple2#(Bit#(128), Cap2024_11) partialCap = unpack({ 84'b0, capBits1, 86'b0 });
                    let keyId = keyIdOf(tpl_2(partialCap));
                    keyIdForConstructingFlit.wset(keyId);
                    $display("L\t", fshow(flitId), "\t1\tkeyId ", fshow(keyId));
                    currentFlit <= tagged Building1 {
                        flit: flit,
                        capBits1: capBits1
                    };
                    $display("S\t", fshow(flitId), "\t10\tBuilding1");
                end else begin
                    $error("TODO BIG ERROR");
                    $finish();
                end
                incomingFlits.drop();
            end
            tagged Building1 { flit: .flit, capBits1: .capBits1 } : if (incomingFlits.canPeek()) begin
                match { .iocapFlit, .iocapFlitId } = incomingFlits.peek();
                dynamicAssert(flitId == iocapFlitId, "Got new mismatching flit while building");
                IOCapFlitSpec#(no_iocap_flit) flitSpec = unpackSpec(iocapFlit);
                if (flitSpec matches tagged CapBits2 .capBits2) begin
                    currentFlit <= tagged Building2 {
                        flit: flit,
                        capBits1: capBits1,
                        capBits2: capBits2
                    };
                    $display("S\t", fshow(flitId), "\t10\tBuilding2");
                end else begin
                    $error("TODO BIG ERROR");
                    $finish();
                end
                incomingFlits.drop();
            end
            tagged Building2 { flit: .flit, capBits1: .capBits1, capBits2: .capBits2 } : if (incomingFlits.canPeek()) begin
                match { .iocapFlit, .iocapFlitId } = incomingFlits.peek();
                dynamicAssert(flitId == iocapFlitId, "Got new mismatching flit while building");
                IOCapFlitSpec#(no_iocap_flit) flitSpec = unpackSpec(iocapFlit);
                if (flitSpec matches tagged CapBits3 .capBits3) begin
                    // let combinedBits = { capBits3, capBits2, capBits1 };
                    // See IOCapAxi_Flits.bsv, capBits1 and capBits2 are swapped for an amazing reason
                    let combinedBits = { capBits3, capBits1, capBits2 };
                    AuthenticatedFlit#(no_iocap_flit, Cap2024_11) authFlit = AuthenticatedFlit {
                        flit: flit,
                        cap: unpack(combinedBits[127:0]),
                        sig: combinedBits[255:128]
                    };
                    currentFlit <= tagged DecodingAndSigChecking authFlit;
                    $display("S\t", fshow(flitId), "\t10\tDecodingAndSigChecking");
                end else begin
                    $error("TODO BIG ERROR");
                    $finish();
                end
                incomingFlits.drop();
            end
            default : noAction; // DecodingAndSigChecking handled in a later rule
        endcase
    endrule

    function Maybe#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11)) getAuthFlit();
        if (completedFlit.wget() matches tagged Valid .authFlit) begin
            return tagged Valid authFlit;
        end else if (currentFlit matches tagged DecodingAndSigChecking .authFlit) begin 
            return tagged Valid authFlit;
        end else begin
            return tagged Invalid;
        end

        // case (tuple2(completedFlit.wget(), currentFlit._read())) matches
        //     { tagged Valid .authFlit, .* } : return tagged Valid authFlit;
        //     { tagged Invalid, tagged DecodingAndSigChecking .authFlit2 } : return tagged Valid authFlit2;
        //     default: return tagged Invalid;
        // endcase
    endfunction

    // rule find_valid_keyIdForConstructing;
    //     if (isValid(keyIdForConstructingFlit.wget())) begin
    //         $display("Found valid keyIdForConstructing ", fshow(keyIdForConstructingFlit.wget()));
    //     end
    // endrule

    // (* no_implicit_conditions *)
    rule tick_sigcheck;
        let newSigCheckState = sigCheckState;
        case (sigCheckState) matches
            tagged SigCheckIdle : begin
                if (keyIdForConstructingFlit.wget() matches tagged Valid .keyId) begin
                    // $display("SigCheckIdle triggering on ", fshow(keyIdForConstructingFlit.wget()));
                    if (keyToKill == tagged Valid keyId) begin
                        newSigCheckState = tagged SigCheckFailedEarly keyId;
                    end else if (keyResponse matches tagged Valid { .keyRespId, .maybeKeyData } &&& keyRespId == keyId) begin
                        // if (completedFlit.wget() matches tagged Valid .authFlit)
                        // The above can never be true!
                        if (isValid(maybeKeyData)) begin
                            newSigCheckState = tagged AwaitingSigCheckStart {
                                keyId: keyId,
                                keyData: fromMaybe(?, maybeKeyData)
                            };
                        end else begin
                            newSigCheckState = tagged SigCheckFailedEarly keyId;
                        end
                    end else if (keyRequest.canPut()) begin
                        keyRequest.put(keyId);
                        newSigCheckState = tagged AwaitingKey keyId;
                    end else begin
                        newSigCheckState = tagged AwaitingKeyAvailable keyId;
                    end
                end
            end
            tagged AwaitingKeyAvailable .keyId : begin
                if (keyToKill == tagged Valid keyId) begin
                    newSigCheckState = tagged SigCheckFailedEarly keyId;
                end else if (keyResponse matches tagged Valid { .keyRespId, .maybeKeyData } &&& keyRespId == keyId) begin
                    if (getAuthFlit() matches tagged Valid .authFlit &&& isValid(maybeKeyData) && sigCheckIn.canPut()) begin
                        sigCheckIn.put(CapSigCheckIn {
                            cap: authFlit.cap,
                            expectedSig: authFlit.sig,
                            secret: fromMaybe(?, maybeKeyData)
                        });
                        newSigCheckState = tagged AwaitingSigCheck {
                            keyId: keyId,
                            keyInvalidatedDuringSigCheck: False
                        };
                    end else begin
                        if (isValid(maybeKeyData)) begin
                            newSigCheckState = tagged AwaitingSigCheckStart {
                                keyId: keyId,
                                keyData: fromMaybe(?, maybeKeyData)
                            };
                        end else begin
                            newSigCheckState = tagged SigCheckFailedEarly keyId;
                        end
                    end
                end else if (keyRequest.canPut()) begin
                    keyRequest.put(keyId);
                    newSigCheckState = tagged AwaitingKey keyId;
                end
            end
            tagged AwaitingKey .keyId : begin
                if (keyToKill == tagged Valid keyId) begin
                    newSigCheckState = tagged SigCheckFailedEarly keyId;
                end else if (keyResponse matches tagged Valid { .keyRespId, .maybeKeyData } &&& keyRespId == keyId) begin
                    if (getAuthFlit() matches tagged Valid .authFlit &&& isValid(maybeKeyData) && sigCheckIn.canPut()) begin
                        sigCheckIn.put(CapSigCheckIn {
                            cap: authFlit.cap,
                            expectedSig: authFlit.sig,
                            secret: fromMaybe(?, maybeKeyData)
                        });
                        newSigCheckState = tagged AwaitingSigCheck {
                            keyId: keyId,
                            keyInvalidatedDuringSigCheck: False
                        };
                    end else begin
                        if (isValid(maybeKeyData)) begin
                            newSigCheckState = tagged AwaitingSigCheckStart {
                                keyId: keyId,
                                keyData: fromMaybe(?, maybeKeyData)
                            };
                        end else begin
                            newSigCheckState = tagged SigCheckFailedEarly keyId;
                        end
                    end
                end
            end
            tagged AwaitingSigCheckStart { keyId: .keyId, keyData: .keyData } : begin
                // $display(fshow(getAuthFlit()), fshow(sigCheckIn.canPut()));
                if (keyToKill == tagged Valid keyId) begin
                    newSigCheckState = tagged SigCheckFailedEarly keyId;
                end else if (getAuthFlit() matches tagged Valid .authFlit &&& sigCheckIn.canPut()) begin
                    sigCheckIn.put(CapSigCheckIn {
                        cap: authFlit.cap,
                        expectedSig: authFlit.sig,
                        secret: keyData
                    });
                    newSigCheckState = tagged AwaitingSigCheck {
                        keyId: keyId,
                        keyInvalidatedDuringSigCheck: False
                    };
                end
            end
            tagged SigCheckFailedEarly .keyId : begin
                // We already know we failed, don't need to keep checking
                // We don't need to forward if we transition to Decoded *this cycle*, because
                // we don't care about the latency of failed transactions as much.
                // if (decodeState matches tagged Decoded .decodeFailed &&&
                //     currentFlit matches tagged DecodingAndSigChecking .authFlit)
                //     $display("Waiting to SigCheckFailedEarly");
                if (decodeState matches tagged Decoded .decodeFailed &&&
                    currentFlit matches tagged DecodingAndSigChecking .authFlit &&& 
                    resps.canPut()) begin

                    let failed = True;

                    // $display("flitCompleted on early fail");
                    resps.put(tuple4(authFlit.flit, flitId, keyId, !failed));
                    flitCompleted.send();
                    newSigCheckState = tagged SigCheckIdle;
                end
            end
            tagged AwaitingSigCheck { keyId: .keyId, keyInvalidatedDuringSigCheck: .keyInvalidatedDuringSigCheck } : begin
                let failed = keyInvalidatedDuringSigCheck;
                if (keyToKill == tagged Valid keyId) begin
                    // For now, don't skip ahead to SigChecked
                    // - don't want to leave a spare entry in the output FIFO
                    failed = True;
                end

                // Assume decodeState == Decoded by this point, and currentFlit == DecodingAndSigChecking.
                // We don't need to forward if we transition to Decoded *this cycle*, because we just won't.
                // AES is that much slower.
                if (sigCheckOut.canPeek()) begin
                    let sigCheckRes = sigCheckOut.peek();
                    sigCheckOut.drop();

                    if (sigCheckRes matches tagged Fail .*) begin
                        failed = True;
                        $display("// IOCap - flit failed sigcheck");
                    end else if (decodeState.Decoded.failed) begin
                        failed = True;
                        $display("// IOCap - flit failed decode earlier");
                    end

                    if (resps.canPut()) begin
                        resps.put(tuple4(currentFlit.DecodingAndSigChecking.flit, flitId, keyId, !failed));
                        $display("// flitCompleted from AwaitingSigCheck");
                        flitCompleted.send();
                        newSigCheckState = tagged SigCheckIdle;
                    end else begin
                        $display("// AwaitingSigCheck waiting on resps");
                        newSigCheckState = tagged AwaitingRespAvailable {
                            keyId: keyId,
                            failed: failed
                        };
                    end
                end else begin
                    newSigCheckState = tagged AwaitingSigCheck {
                        keyId: keyId,
                        keyInvalidatedDuringSigCheck: keyInvalidatedDuringSigCheck
                    };
                end
            end
            tagged AwaitingRespAvailable { keyId: .keyId, failed: .failed1 } : begin
                let failed = failed1;
                if (keyToKill == tagged Valid keyId) begin
                    // For now, don't skip ahead to SigChecked
                    // - don't want to leave a spare entry in the output FIFO
                    failed = True;
                end

                if (resps.canPut()) begin
                    resps.put(tuple4(currentFlit.DecodingAndSigChecking.flit, flitId, keyId, !failed));
                    $display("// flitCompleted from AwaitingRespAvailable");
                    flitCompleted.send();
                    newSigCheckState = tagged SigCheckIdle;
                end else begin
                    // $display("AwaitingRespAvailable waiting on resps");
                    newSigCheckState = tagged AwaitingRespAvailable {
                        keyId: keyId,
                        failed: failed
                    };
                end
            end
        endcase

        if (sigCheckState != newSigCheckState) begin
            case (newSigCheckState) matches
                // Start and end SigCheckIdle to show that this flit is being finished
                tagged SigCheckIdle : $display("S\t", fshow(flitId), "\t12\tSigCheckIdle\nE\t", fshow(flitId), "\t12\tSigCheckIdle");
                tagged AwaitingKeyAvailable .* : $display("S\t", fshow(flitId), "\t12\tAwaitingKeyAvailable");
                tagged AwaitingKey .* :  $display("S\t", fshow(flitId), "\t12\tAwaitingKey");
                tagged AwaitingSigCheckStart .* : $display("S\t", fshow(flitId), "\t12\tAwaitingSigCheckStart");
                tagged SigCheckFailedEarly .* : $display("S\t", fshow(flitId), "\t12\tSigCheckFailedEarly");
                tagged AwaitingSigCheck {} : $display("S\t", fshow(flitId), "\t12\tAwaitingSigCheck");
                tagged AwaitingRespAvailable {} : $display("S\t", fshow(flitId), "\t12\tAwaitingRespAvailable");
            endcase
        end
        sigCheckState <= newSigCheckState;
    endrule

    rule tick_decode;
        case (decodeState) matches
            tagged DecodeIdle : begin
                if (getAuthFlit() matches tagged Valid .authFlit &&& decodeIn.canPut()) begin
                    // $display("-> AwaitingFlitBounds");
                    decodeIn.put(authFlit.cap);
                    decodeState <= tagged AwaitingFlitBounds;
                    $display("S\t", fshow(flitId), "\t13\tAwaitingFlitBounds");
                end
            end
            tagged AwaitingFlitBounds : begin
                let flit = fromMaybe(?, getAuthFlit()).flit;
                let bounds_failed = False;
                Bit#(64) min_addr = 0;
                Bit#(65) max_addr = 0;
                case (burstKind(flit)) matches
                    FIXED: begin
                        // Each beat of a burst starts at the same address
                        // The max address = min address + the number of bytes per beat
                        // number of bytes per beat = 1 << burstSize, up to 128 => length = 7
                        min_addr = burstAddr(flit);
                        Bit#(7) beatSize = 7'b1 << burstSize(flit).val;
                        max_addr = zeroExtend(min_addr) + zeroExtend(beatSize);
                    end
                    INCR: begin
                        // Each beat of a burst starts at (last beat address + beat length)
                        // min address = starting address
                        // max address = min address + (beats/burst) * (bytes/beat)
                        // beats/burst = burstLen [0..=255] + 1, [1..=256]
                        // bytes/beat  = 1 << burstSize, up to 128
                        // multiplied together the max is 32640, up to 15 bits
                        min_addr = burstAddr(flit);
                        Bit#(9) beatsPerBurst = zeroExtend(burstLen(flit)) + 1;
                        Bit#(15) totalBurstBytes = zeroExtend(beatsPerBurst) << burstSize(flit).val;
                        max_addr = zeroExtend(min_addr) + zeroExtend(totalBurstBytes);
                    end
                    WRAP: begin
                        // TODO support WRAP
                        bounds_failed = True;
                    end
                    default: bounds_failed = True;
                endcase
                // Make sure it doesn't extend over the 64-bit boundary
                // Up to the boundary? fine.
                // Over the boundary? no.
                // Note: the capability may extend over the 64-bit boundary and that's fine - the bounds are explicitly 65-bit.
                // We only care about the bounds of the AXI flit overstepping because that would wrap around to 0 when the addresses are compressed to 64-bit.

                if (max_addr > (1 << 64)) begin
                    bounds_failed = True;
                end

                if (bounds_failed) begin
                    $display("IOCap - flit failed Bounds ", fshow(flit));
                end
                decodeState <= tagged AwaitingIOCapDecode {
                    flitMin: min_addr,
                    flitMax: max_addr,
                    boundsFailed: bounds_failed
                };
                $display("S\t", fshow(flitId), "\t13\tAwaitingIOCapDecode");
            end
            tagged AwaitingIOCapDecode { flitMin: .flitMin, flitMax: .flitMax, boundsFailed: .boundsFailed } : begin
                if (decodeOut.canPeek()) begin
                    decodeOut.drop();
                    
                    let decodeRes = decodeOut.peek();

                    let flit = currentFlit.DecodingAndSigChecking.flit;
                    let failed = boundsFailed;
                    case (decodeRes) matches
                        tagged Succ ({ .perms, .range }) : begin
                            // Check permissions
                            if (isBurstRead(flit) && perms == Write) begin
                                failed = True;
                            end else if (!isBurstRead(flit) && perms == Read) begin
                                failed = True;
                            end

                            // Check range
                            if ((flitMin < range.base) || (flitMax > range.top)) begin
                                failed = True;
                            end
                        end
                        tagged Fail .* : failed = True;
                    endcase

                    if (failed) begin
                        $display("// IOCap - flit failed Decode ", fshow(flit), " - ", fshow(decodeRes));
                    end
                    $display("S\t", fshow(flitId), "\t13\tDecoded");
                    decodeState <= tagged Decoded {
                        failed: failed
                    };
                end
            end
            tagged Decoded .failed : begin
                if (flitCompleted) begin
                    $display("E\t", fshow(flitId), "\t13\tDecoded");
                    decodeState <= tagged DecodeIdle;
                end
            end
        endcase
    endrule

    rule complete_flit(currentFlit matches tagged DecodingAndSigChecking .*);
        if (flitCompleted) begin
            $display("E\t", fshow(flitId), "\t10\tDecodingAndSigChecking");
            currentFlit <= tagged NoFlit;
        end
    endrule

    interface in = toSink(reqFlits);
    interface checkResponse = respsMapFIFO.deq;
    interface insertKThreadId = interface ReadOnly;
        method UInt#(64) _read = 0;
    endinterface;
endmodule


// mkIOCapAxiCheckerPool#(n, flit) to make a Vector#(n, someChecker) and take the first available one.
// Max input/output rate are still 1/cycle, n should be tuned such that n = ceil((x cycles for one check)/(y cycles to receive an authenticated IOCapAxiFlit))
// i.e. that whenever a new authed flit arrives, which can at most be once every y cycles, a checker in the pool will be ready.
// Note that order needs to be preserved here - a 1-caveat write that arrives after a 3-caveat write must be blocked until the 3-caveat write has been checked - otherwise the w-flits will get mixed up.
    // Should be able to enforce this by using a round-robin pool. If you insert into #1, then #2, then #3, and take out responses from #1, then #2, then #3, you're fine.
    // Technically this doesn't apply to reads - could take a shortcut there?
    // TODO this is worth thinking about in the write-up! In PCIe land where data+address arrive at once, do we also have this latency dependency? Likely worse because writes and reads are ordered together?

// Can't use Integer for n because "Integer" != "numeric type"

module mkInOrderIOCapAxiChecker2V1Pool#(
    NumProxy#(n) n_proxy,
    function module#(Empty) makeDecoder(Get#(Cap2024_11) ins, Put#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) outs),
    Sink#(KeyId) keyRequest,
    ReadOnly#(Maybe#(Tuple2#(KeyId, Maybe#(Key)))) keyResponse,
    ReadOnly#(Maybe#(KeyId)) keyToKill, 
    function KeyId keyIdOf(Cap2024_11 cap)
)(IOCapAxiChecker2#(iocap_flit, no_iocap_flit)) provisos (
    Bits#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11), a__),
    Bits#(FlitState#(no_iocap_flit), b__),
    Bits#(iocap_flit, c__),
    AxiCtrlFlit64#(no_iocap_flit),
    FShow#(no_iocap_flit),
    IOCapPackableFlit#(iocap_flit, no_iocap_flit),
    Add#(n__, TLog#(n), 64)
);

    // Separately track the insert, key request and retrieve pointers.
    // insertPointer is allowed to wrap around past retrievePointer multiple times
    // - although that likely isn't possible in normal cases -
    // because the baseChecker is expected to spit out checkResponses in the same order as checkRequests.
    // This could be done differently, TODO construct a mkOutOfOrderIOCapAxiCheckerPool?
    Reg#(Bit#(TLog#(n))) insertPointer <- mkReg(0);
    PulseWire incrementInsert <- mkPulseWire;
    Reg#(Bit#(TLog#(n))) keyRequestPointer <- mkReg(0);
    PulseWire incrementKeyRequest <- mkPulseWire;
    Reg#(Bit#(TLog#(n))) retrievePointer <- mkReg(0);
    PulseWire incrementRetrieve <- mkPulseWire;

    rule increment_counters;
        if (incrementInsert) begin
            $display("// tick incrementInsert ", fshow(insertPointer));
            let newInsertPointer = insertPointer + 1;
            if (inLiteralRange(insertPointer, valueOf(n)) && newInsertPointer >= fromInteger(valueOf(n)))
                insertPointer <= 0;
            else
                insertPointer <= newInsertPointer;
        end
        if (incrementKeyRequest) begin
            $display("V\tIncKeyReq\t", fshow(keyRequestPointer));
            let newKeyRequestPointer = keyRequestPointer + 1;
            if (inLiteralRange(keyRequestPointer, valueOf(n)) && newKeyRequestPointer >= fromInteger(valueOf(n)))
                keyRequestPointer <= 0;
            else
                keyRequestPointer <= newKeyRequestPointer;
        end
        if (incrementRetrieve) begin
            $display("// tick incrementRetrieve ", fshow(retrievePointer));
            let newRetrievePointer = retrievePointer + 1;
            if (inLiteralRange(retrievePointer, valueOf(n)) && newRetrievePointer >= fromInteger(valueOf(n)))
                retrievePointer <= 0;
            else
                retrievePointer <= newRetrievePointer;
        end
    endrule

    Vector#(n, RWire#(KeyId)) keyRequests <- replicateM(mkRWire);

    function UInt#(64) sumValidRequests(UInt#(64) i, RWire#(KeyId) w);
        if (isValid(w.wget())) begin
            return i + 1;
        end else begin
            return i;
        end
    endfunction

    rule passthru_key_request(keyRequest.canPut());
        if (foldl(sumValidRequests, 64'b0, keyRequests) > 1) begin
            $error("TODO BIG ERROR TOO MANY KEY REQUESTS");
            $finish();
        end
        if (keyRequests[keyRequestPointer].wget() matches tagged Valid .req) begin
            keyRequest.put(req);
            // Each pool member should send exactly one key request per flit group
            incrementKeyRequest.send();
        end
    endrule

    rule check_not_dropping_key_request(!keyRequest.canPut());
        let nRequests = foldl(sumValidRequests, 64'b0, keyRequests);
        if (nRequests > 1) begin
            $error("TODO BIG ERROR TOO MANY KEY REQUESTS AND DROPPING");
            $finish();
        end else if (nRequests != 0) begin
            $error("TODO BIG ERROR DROPPING KEY REQUEST");
            $finish();
        end
    endrule

    function module#(IOCapAxiChecker2#(iocap_flit, no_iocap_flit)) genChecker(Integer i);
        let keyRequestIfc = interface Sink;
            method Bool canPut = (keyRequestPointer == fromInteger(i) && keyRequest.canPut());
            method Action put(x);
                $display("V\tPutKeyReq\t", fshow(keyRequestPointer));
                keyRequests[i].wset(x);
            endmethod
        endinterface;
        return mkSimpleIOCapAxiChecker2V1(
            makeDecoder,
            keyRequestIfc,
            keyResponse,
            keyToKill,
            keyIdOf
        );
    endfunction

    Vector#(n, IOCapAxiChecker2#(iocap_flit, no_iocap_flit)) checkers <- genWithM(genChecker);

    interface in = interface Sink;
        method Bool canPut;
            return checkers[insertPointer].in.canPut();
        endmethod
        method Action put (Tuple2#(iocap_flit, KFlitId) val);
            checkers[insertPointer].in.put(val);
            // On the final flit, switch the pointer
            IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(tpl_1(val));
            if (spec matches tagged CapBits3 .*) begin
                incrementInsert.send();
            end
        endmethod
    endinterface;
    interface checkResponse = interface Source;
        method Bool canPeek;
            return checkers[retrievePointer].checkResponse.canPeek();
        endmethod
        method Tuple4#(no_iocap_flit, KFlitId, KeyId, Bool) peek;
            return checkers[retrievePointer].checkResponse.peek();
        endmethod
        method Action drop;
            checkers[retrievePointer].checkResponse.drop();
            incrementRetrieve.send();
        endmethod
    endinterface;
    interface insertKThreadId = interface ReadOnly;
        method UInt#(64) _read = zeroExtend(unpack(insertPointer));
    endinterface;
endmodule

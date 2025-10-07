import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;
import MapFIFO :: *;
import ConfigReg :: *;

import IOCapAxi_Types :: *;
import IOCapAxi_Flits :: *;

import Cap2024 :: *;
import Cap2024_02 :: *;
import Cap2024_02_Decode_FastFSM :: *;
import Cap2024_SigCheck_Aes_1RoundPerCycle :: *; // Get CapSigCheckIn
import Cap2024_SigCheck_Aes_2RoundPerCycle :: *;

interface IOCapAxiChecker2#(type iocap_flit, type no_iocap_flit);
    interface Sink#(iocap_flit) in;
    interface Source#(Tuple3#(no_iocap_flit, KeyId, Bool)) checkResponse;
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
    // struct {
    //     KeyId keyId;
    //     // If the signature check failed or the key was invalid
    //     Bool failed;
    // } SigChecked;
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
    // AuthenticatedFlit#(no_iocap_flit, tcap) Ready;
    AuthenticatedFlit#(no_iocap_flit, tcap) DecodingAndSigChecking;
} FlitState#(type no_iocap_flit, type tcap) deriving (Bits, FShow);

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
// TODO FIGURE OUT THE EXPECTED LATENCIES, THE BELOW IS OLD
//
// Capabilities are decoded and signature-checked in parallel, and we can assume the decoder latency is always less than the signature check.
// We add ~3 cycles of latency on top of the signature check with the various FIFO stages, so the maximum latency should be ~21 cycles.
module mkSimpleIOCapAxiChecker2#(
    function module#(Empty) makeDecoder(Get#(tcap) ins, Put#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) outs),
    Sink#(KeyId) keyRequest,
    ReadOnly#(Maybe#(Tuple2#(KeyId, Maybe#(Key)))) keyResponse,
    ReadOnly#(Maybe#(KeyId)) keyToKill, 
    function KeyId keyIdOf(tcap cap)
)(IOCapAxiChecker2#(iocap_flit, no_iocap_flit)) provisos (
    Bits#(AuthenticatedFlit#(no_iocap_flit, tcap), a__),
    Bits#(FlitState#(no_iocap_flit, tcap), b__),
    Bits#(iocap_flit, c__),
    Bits#(tcap, 128),
    AxiCtrlFlit64#(no_iocap_flit),
    FShow#(no_iocap_flit),
    IOCapPackableFlit#(iocap_flit, no_iocap_flit),
    Cap#(tcap)
);
    function Tuple3#(no_iocap_flit, KeyId, Bool) checkKeyNotKilled(Tuple3#(no_iocap_flit, KeyId, Bool) tup);
        if (keyToKill == tagged Valid tpl_2(tup)) begin
            return tuple3(tpl_1(tup), tpl_2(tup), False);
        end else begin
            return tup;
        end
    endfunction

    NumProxy#(3) respsMapFIFOSize = ?;
    MapFIFO#(Tuple3#(no_iocap_flit, KeyId, Bool)) respsMapFIFO <- mkSizedMapFIFO(respsMapFIFOSize, checkKeyNotKilled);
    let resps = respsMapFIFO.enq;

    FIFOF#(iocap_flit) reqFlits <- mkFIFOF;
    let incomingFlits = toSource(reqFlits);

    ConfigReg#(FlitState#(no_iocap_flit, tcap)) currentFlit <- mkConfigReg(tagged NoFlit);
    // Reg#(Maybe#(CurrentFlitState#(no_iocap_flit))) currentFlit <- mkReg(tagged Invalid);
    ConfigReg#(DecodeState) decodeState <- mkConfigReg(tagged DecodeIdle);
    ConfigReg#(SigCheckState) sigCheckState <- mkConfigReg(tagged SigCheckIdle);

    // The keyId we just extracted from the flit in the Building0 -> Building1 transition.
    // Used for ticking the sigCheck machine as early as possible.
    RWire#(KeyId) keyIdForConstructingFlit <- mkRWire;
    // The full AuthenticatedFlit we just extracted in the Building2 -> DecodingAndSigChecking transition.
    // Will NEVER be Valid at the same time as the keyIdForConstructingFlit
    RWire#(AuthenticatedFlit#(no_iocap_flit, tcap)) completedFlit <- mkRWire;
    // Pulses when the signature check finishes, triggering everything else to reset.
    PulseWire flitCompleted <- mkPulseWire;

    // TODO dual-end FIFOs are terrible for latency
    FIFOF#(tcap) decodeInFIFO <- mkFIFOF; 
    FIFOF#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) decodeOutFIFO <- mkFIFOF;
    makeDecoder(toGet(decodeInFIFO), toPut(decodeOutFIFO));
    let decodeIn <- toUnguardedSink(decodeInFIFO);
    let decodeOut <- toUnguardedSource(decodeOutFIFO, ?);

    // TODO dual-end FIFOs are terrible for latency
    FIFOF#(CapSigCheckIn#(tcap)) sigCheckInFIFO <- mkFIFOF;
    FIFOF#(CapCheckResult#(Bit#(0))) sigCheckOutFIFO <- mkFIFOF;
    mk2RoundPerCycleCapSigCheck(toGet(sigCheckInFIFO), toPut(sigCheckOutFIFO));
    let sigCheckIn <- toUnguardedSink(sigCheckInFIFO);
    let sigCheckOut <- toUnguardedSource(sigCheckOutFIFO, ?);

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
                // $display("-> Building0");
                IOCapFlitSpec#(no_iocap_flit) capFlit = unpackSpec(incomingFlits.peek());
                if (capFlit matches tagged Start .flit) begin
                    currentFlit <= tagged Building0 {
                        flit: flit
                    };
                end else begin
                    $display("TODO BIG ERROR");
                end
                incomingFlits.drop();
            end
            tagged Building0 { flit: .flit } : if (incomingFlits.canPeek()) begin
                // $display("-> Building1");
                IOCapFlitSpec#(no_iocap_flit) capBits = unpackSpec(incomingFlits.peek());
                if (capBits matches tagged CapBits1 .capBits1) begin
                    // // TODO THIS REALLY NEEDS TO BE POSSIBLE. IF IT ISN'T, REARRANGE IOCAPAXI SO THAT IT IS
                    // tcap partialCap = unpack({ ?, capBits1 });
                    // let keyId = keyIdOf(partialCap);
                    // keyIdForConstructingFlit.wset(keyId);
                    currentFlit <= tagged Building1 {
                        flit: flit,
                        capBits1: capBits1
                    };
                end else begin
                    $display("TODO BIG ERROR");
                end
                incomingFlits.drop();
            end
            tagged Building1 { flit: .flit, capBits1: .capBits1 } : if (incomingFlits.canPeek()) begin
                // $display("-> Building2");
                IOCapFlitSpec#(no_iocap_flit) capBits = unpackSpec(incomingFlits.peek());
                if (capBits matches tagged CapBits2 .capBits2) begin
                    currentFlit <= tagged Building2 {
                        flit: flit,
                        capBits1: capBits1,
                        capBits2: capBits2
                    };
                end else begin
                    $display("TODO BIG ERROR");
                end
                incomingFlits.drop();
            end
            tagged Building2 { flit: .flit, capBits1: .capBits1, capBits2: .capBits2 } : if (incomingFlits.canPeek()) begin
                // $display("-> DecodingAndSigChecking");
                IOCapFlitSpec#(no_iocap_flit) capBits = unpackSpec(incomingFlits.peek());
                if (capBits matches tagged CapBits3 .capBits3) begin
                    let combinedBits = { capBits3, capBits2, capBits1 };
                    AuthenticatedFlit#(no_iocap_flit, tcap) authFlit = AuthenticatedFlit {
                        flit: flit,
                        cap: unpack(combinedBits[127:0]),
                        sig: combinedBits[255:128]
                    };
                    // TODO THIS REALLY NEEDS TO BE IN BIT 1. IF IT ISN'T, REARRANGE IOCAPAXI SO THAT IT IS
                    let keyId = keyIdOf(authFlit.cap);
                    keyIdForConstructingFlit.wset(keyId);
                    completedFlit.wset(authFlit);
                    currentFlit <= tagged DecodingAndSigChecking authFlit;
                end else begin
                    $display("TODO BIG ERROR");
                end
                incomingFlits.drop();
            end
            default : noAction; // DecodingAndSigChecking handled in a later rule
        endcase
    endrule

    function Maybe#(AuthenticatedFlit#(no_iocap_flit, tcap)) getAuthFlit();
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

                    $display("flitCompleted on early fail");
                    resps.put(tuple3(authFlit.flit, keyId, !failed));
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

                if (sigCheckOut.canPeek() && !resps.canPut()) begin
                    $display("AwaitingSigCheck waiting on resps");
                end

                // Assume decodeState == Decoded by this point, and currentFlit == DecodingAndSigChecking.
                // We don't need to forward if we transition to Decoded *this cycle*, because we just won't.
                // AES is that much slower.
                if (resps.canPut() &&
                    sigCheckOut.canPeek()) begin
                    let sigCheckRes = sigCheckOut.peek();
                    sigCheckOut.drop();

                    if (sigCheckRes matches tagged Fail .*) begin
                        failed = True;
                        $display("IOCap - flit failed sigcheck");
                    end else if (decodeState.Decoded.failed) begin
                        failed = True;
                        $display("IOCap - flit failed decode earlier");
                    end

                    resps.put(tuple3(currentFlit.DecodingAndSigChecking.flit, keyId, !failed));
                    $display("flitCompleted from AwaitingSigCheck");
                    flitCompleted.send();
                    newSigCheckState = tagged SigCheckIdle;
                end else begin
                    newSigCheckState = tagged AwaitingSigCheck {
                        keyId: keyId,
                        keyInvalidatedDuringSigCheck: keyInvalidatedDuringSigCheck
                    };
                end
            end
            default: noAction;
        endcase

        if (sigCheckState != newSigCheckState)
            $display("-> ", fshow(newSigCheckState));
        sigCheckState <= newSigCheckState;
    endrule

    rule tick_decode;
        case (decodeState) matches
            tagged DecodeIdle : begin
                if (getAuthFlit() matches tagged Valid .authFlit &&& decodeIn.canPut()) begin
                    // $display("-> AwaitingFlitBounds");
                    decodeIn.put(authFlit.cap);
                    decodeState <= tagged AwaitingFlitBounds;
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
                // $display("-> AwaitingIOCapDecode");
                decodeState <= tagged AwaitingIOCapDecode {
                    flitMin: min_addr,
                    flitMax: max_addr,
                    boundsFailed: bounds_failed
                };
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
                        $display("IOCap - flit failed Decode ", fshow(flit), " - ", fshow(decodeRes));
                    end
                    // $display("-> Decoded");
                    decodeState <= tagged Decoded {
                        failed: failed
                    };
                end
            end
            tagged Decoded .failed : begin
                if (flitCompleted) begin
                    // $display("-> DecodeIdle");
                    decodeState <= tagged DecodeIdle;
                end
            end
        endcase
    endrule

    rule complete_flit(currentFlit matches tagged DecodingAndSigChecking .*);
        if (flitCompleted) begin
            currentFlit <= tagged NoFlit;
        end
    endrule

    interface in = toSink(reqFlits);
    interface checkResponse = respsMapFIFO.deq;
endmodule

/*

// mkIOCapAxiCheckerPool#(n, flit) to make a Vector#(n, someChecker) and take the first available one.
// Max input/output rate are still 1/cycle, n should be tuned such that n = ceil((x cycles for one check)/(y cycles to receive an authenticated IOCapAxiFlit))
// i.e. that whenever a new authed flit arrives, which can at most be once every y cycles, a checker in the pool will be ready.
// Note that order needs to be preserved here - a 1-caveat write that arrives after a 3-caveat write must be blocked until the 3-caveat write has been checked - otherwise the w-flits will get mixed up.
    // Should be able to enforce this by using a round-robin pool. If you insert into #1, then #2, then #3, and take out responses from #1, then #2, then #3, you're fine.
    // Technically this doesn't apply to reads - could take a shortcut there?
    // TODO this is worth thinking about in the write-up! In PCIe land where data+address arrive at once, do we also have this latency dependency? Likely worse because writes and reads are ordered together?

// Can't use Integer for n because "Integer" != "numeric type"

// TODO MAKE THIS WORK FOR CHECKER2
module mkInOrderIOCapAxiChecker2Pool#(NumProxy#(n) n_proxy, module#(IOCapAxiChecker2#(no_iocap_flit, tcap)) toPool)(IOCapAxiChecker#(no_iocap_flit, tcap)) provisos (Bits#(AuthenticatedFlit#(no_iocap_flit, tcap), a__), AxiCtrlFlit64#(no_iocap_flit), FShow#(no_iocap_flit));    
    Vector#(n, IOCapAxiChecker#(no_iocap_flit, tcap)) checkers <- replicateM(toPool);
    // Separately track the insert and retrieve pointers.
    // insertPointer is allowed to wrap around past retrievePointer multiple times
    // - although that likely isn't possible in normal cases -
    // because the baseChecker is expected to spit out checkResponses in the same order as checkRequests.
    // This could be done differently, TODO construct a mkOutOfOrderIOCapAxiCheckerPool?
    Reg#(Bit#(TLog#(n))) insertPointer <- mkReg(0);
    PulseWire incrementInsert <- mkPulseWire;
    Reg#(Bit#(TLog#(n))) retrievePointer <- mkReg(0);
    PulseWire incrementRetrieve <- mkPulseWire;

    rule increment_counters;
        if (incrementInsert) begin
            let newInsertPointer = insertPointer + 1;
            if (inLiteralRange(insertPointer, valueOf(n)) && newInsertPointer >= fromInteger(valueOf(n)))
                insertPointer <= 0;
            else
                insertPointer <= newInsertPointer;
        end
        if (incrementRetrieve) begin
            let newRetrievePointer = retrievePointer + 1;
            if (inLiteralRange(insertPointer, valueOf(n)) && newRetrievePointer >= fromInteger(valueOf(n)))
                retrievePointer <= 0;
            else
                retrievePointer <= newRetrievePointer;
        end
    endrule

    interface checkRequest = interface Sink;
        method Bool canPut;
            return checkers[insertPointer].checkRequest.canPut();
        endmethod
        method Action put (Tuple3#(AuthenticatedFlit#(no_iocap_flit, tcap), KeyId, Maybe#(Key)) val);
            checkers[insertPointer].checkRequest.put(val);
            incrementInsert.send();
        endmethod
    endinterface;
    interface checkResponse = interface Source;
        method Bool canPeek;
            return checkers[retrievePointer].checkResponse.canPeek();
        endmethod
        method Tuple3#(no_iocap_flit, KeyId, Bool) peek;
            return checkers[retrievePointer].checkResponse.peek();
        endmethod
        method Action drop;
            checkers[retrievePointer].checkResponse.drop();
            incrementRetrieve.send();
        endmethod
    endinterface;
endmodule

*/
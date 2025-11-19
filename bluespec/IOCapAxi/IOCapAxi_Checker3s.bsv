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
import Cap2024_SigCheck_Aes_1RoundPerCycleFast :: *;
import Cap2024_SigCheck_Aes_2RoundPerCycleFast :: *;

export IOCapAxiChecker3(..);
export IOCapAxiChecker3_Read(..);
export IOCapAxiChecker3_Write(..);
export mkSimpleIOCapAxiChecker3V1;
export mkSimpleIOCapAxiChecker3V1_FastDecode_1CycleAES_Read;
export mkSimpleIOCapAxiChecker3V1_FastDecode_1CycleAES_Write;
export mkSimpleIOCapAxiChecker3V1_FastDecode_2CycleAES_Read;
export mkSimpleIOCapAxiChecker3V1_FastDecode_2CycleAES_Write;
export mkNullIOCapAxiChecker3V1_Read;
export mkNullIOCapAxiChecker3V1_Write;
export mkInOrderIOCapAxiChecker3V1Pool;
export mkInOrderIOCapAxiChecker3V1Pool_Read;
export mkInOrderIOCapAxiChecker3V1Pool_Write;
export mkChecker3CombinedFrontend;
export mkChecker3CombinedPipelinedFrontend;

interface IOCapAxiChecker3#(type no_iocap_flit);
    interface Sink#(Tuple4#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11), KFlitId, KeyId, Maybe#(Key))) in;
    interface Source#(Tuple4#(no_iocap_flit, KFlitId, KeyId, Bool)) checkResponse;

    interface WriteOnly#(Maybe#(KeyId)) keyToKill;
endinterface

interface IOCapAxiChecker3_Read;
    interface IOCapAxiChecker3#(AXI4_ARFlit#(4, 64, 0)) checker;
endinterface

interface IOCapAxiChecker3_Write;
    interface IOCapAxiChecker3#(AXI4_AWFlit#(4, 64, 0)) checker;
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
} DecodeState deriving (Bits, FShow, Eq);

typedef union tagged {
    void SigCheckIdle;
    KeyId SigCheckFailedEarly;
    struct {
        KeyId keyId;
        Bool failed;
    } AwaitingSigCheck;
    // Once the signature check completes, we assume that everything else has completed.
    struct {
        KeyId keyId;
        // If the signature check failed or the key was invalid
        Bool failed;
    } AwaitingRespAvailable;
} SigCheckState deriving (Bits, FShow, Eq);

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
module mkSimpleIOCapAxiChecker3V1#(
    KonataMode kMode,
    function module#(Empty) makeDecoder(Get#(Cap2024_11) ins, Put#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) outs),
    function module#(Empty) makeSigChecker(ReadOnly#(Maybe#(CapSigCheckIn#(Cap2024_11))) in, WriteOnly#(CapCheckResult#(Bit#(0))) out)
)(IOCapAxiChecker3#(no_iocap_flit)) provisos (
    Bits#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11), a__),
    Bits#(FlitState#(no_iocap_flit), b__),
    AxiCtrlFlit64#(no_iocap_flit),
    FShow#(no_iocap_flit)
);
    RWire#(KeyId) keyToKillRWire <- mkRWire;
    function Tuple4#(t, KFlitId, KeyId, Bool) checkKeyNotKilled(Tuple4#(t, KFlitId, KeyId, Bool) tup);
        if (keyToKillRWire.wget() == tagged Valid tpl_3(tup)) begin
            return tuple4(tpl_1(tup), tpl_2(tup), tpl_3(tup), False);
        end else begin
            return tup;
        end
    endfunction

    NumProxy#(3) respsMapFIFOSize = ?;
    MapFIFO#(Tuple4#(no_iocap_flit, KFlitId, KeyId, Bool)) respsMapFIFO <- mkSizedMapFIFO(respsMapFIFOSize, checkKeyNotKilled);
    let resps = respsMapFIFO.enq;

    ConfigReg#(DecodeState) decodeState <- mkConfigReg(tagged DecodeIdle);
    ConfigReg#(SigCheckState) sigCheckState <- mkConfigReg(tagged SigCheckIdle);

    RWire#(Tuple4#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11), KFlitId, KeyId, Maybe#(Key))) incomingFlit <- mkRWire;
    let reqIn = interface Sink;
        method Bool canPut() = (sigCheckState == tagged SigCheckIdle && decodeState == tagged DecodeIdle);
        method Action put(x) = incomingFlit.wset(x);
    endinterface;
    let reqOut = interface Source;
        method Bool canPeek() = isValid(incomingFlit.wget());
        method peek() = fromMaybe(?, incomingFlit.wget());
        method Action drop() = noAction;
    endinterface;

    ConfigReg#(KFlitId) savedFlitId <- mkConfigReg(?);
    ConfigReg#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11)) savedAuthFlit <- mkConfigReg(?);
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
    // Using an RWire on output creates a long path from the AES input to the output FIFO.
    FIFOF#(CapCheckResult#(Bit#(0))) sigCheckOutFIFO <- mkFIFOF;
    makeSigChecker(rwireToReadOnly(sigCheckInRWire), fifofToWriteOnly(sigCheckOutFIFO));
    // makeSigChecker(rwireToReadOnly(sigCheckInRWire), rwireToWriteOnly(sigCheckOutRWire));
    let sigCheckIn = interface Sink;
        method Bool canPut() = True;
        method Action put(x) = sigCheckInRWire.wset(x);
    endinterface;
    let sigCheckOut = toSource(sigCheckOutFIFO);
    // let sigCheckOut = interface Source;
    //     method Bool canPeek() = isValid(sigCheckOutRWire.wget());
    //     method peek() = fromMaybe(?, sigCheckOutRWire.wget());
    //     method Action drop() = noAction;
    // endinterface;

    // (* no_implicit_conditions *)
    rule tick_sigcheck;
        let flitId = savedFlitId;
        let newSigCheckState = sigCheckState;
        case (sigCheckState) matches
            tagged SigCheckIdle : begin
                if (reqOut.canPeek()) begin
                    match { .authFlit, .incomingFlitId, .keyId, .key } = reqOut.peek();
                    flitId = incomingFlitId;
                    savedFlitId <= incomingFlitId;
                    // $display("SigCheckIdle triggering on ", fshow(keyIdForConstructingFlit.wget()));
                    if (keyToKillRWire.wget() == tagged Valid keyId || !isValid(key)) begin
                        newSigCheckState = tagged SigCheckFailedEarly keyId;
                    end else if (sigCheckIn.canPut() && decodeIn.canPut()) begin
                        sigCheckIn.put(CapSigCheckIn {
                            cap: authFlit.cap,
                            expectedSig: authFlit.sig,
                            secret: fromMaybe(?, key)
                        });
                        newSigCheckState = tagged AwaitingSigCheck {
                            keyId: keyId,
                            failed: False
                        };
                    end else begin
                        $error("ERROR PUT INTO REQ WHERE SIG OR DECODE NOT READY");
                        $finish();
                    end
                    reqOut.drop();
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
                    resps.canPut()) begin

                    let failed = True;

                    // $display("flitCompleted on early fail");
                    resps.put(tuple4(savedAuthFlit.flit, flitId, keyId, !failed));
                    flitCompleted.send();
                    newSigCheckState = tagged SigCheckIdle;
                end
            end
            tagged AwaitingSigCheck { keyId: .keyId, failed: .failed1 } : begin
                let failed = failed1;
                if (keyToKillRWire.wget() == tagged Valid keyId) begin
                    // For now, don't skip ahead to SigChecked
                    // - don't want to leave a spare entry in the output FIFO
                    failed = True;
                end

                // Assume decodeState == Decoded by this point.
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
                        resps.put(tuple4(savedAuthFlit.flit, flitId, keyId, !failed));
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
                        failed: failed
                    };
                end
            end
            tagged AwaitingRespAvailable { keyId: .keyId, failed: .failed1 } : begin
                let failed = failed1;
                if (keyToKillRWire.wget() == tagged Valid keyId) begin
                    // For now, don't skip ahead to SigChecked
                    // - don't want to leave a spare entry in the output FIFO
                    failed = True;
                end

                if (resps.canPut()) begin
                    resps.put(tuple4(savedAuthFlit.flit, flitId, keyId, !failed));
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
                tagged SigCheckIdle : konataFlit(kMode, 
                    $format("S\t") + fshow(flitId) + $format("\t12\tSigCheckIdle\nE\t") + fshow(flitId) + $format("\t12\tSigCheckIdle")
                );
                tagged SigCheckFailedEarly .* : konataFlit(kMode,
                    $format("S\t") + fshow(flitId) + $format("\t12\tSigCheckFailedEarly")
                );
                tagged AwaitingSigCheck {} : konataFlit(kMode,
                    $format("S\t") + fshow(flitId) + $format("\t12\tAwaitingSigCheck")
                );
                tagged AwaitingRespAvailable {} : konataFlit(kMode,
                    $format("S\t") + fshow(flitId) + $format("\t12\tAwaitingRespAvailable")
                );
            endcase
        end
        sigCheckState <= newSigCheckState;
    endrule

    rule tick_decode;
        case (decodeState) matches
            tagged DecodeIdle : begin
                if (reqOut.canPeek()) begin
                    match { .authFlit, .incomingFlitId, .keyId, .key } = reqOut.peek();
                    dynamicAssert(decodeIn.canPut(), "NEED TO BE ABLE TO DECODE WHEN reqIn");
                    // $display("-> AwaitingFlitBounds");
                    decodeIn.put(authFlit.cap);
                    decodeState <= tagged AwaitingFlitBounds;
                    savedAuthFlit <= authFlit;
                    konataFlit(kMode, $format("S\t") + fshow(incomingFlitId) + $format("\t13\tAwaitingFlitBounds"));
                end
            end
            tagged AwaitingFlitBounds : begin
                let flit = savedAuthFlit.flit;
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
                    $display("// IOCap - flit failed Bounds ", fshow(flit));
                end
                decodeState <= tagged AwaitingIOCapDecode {
                    flitMin: min_addr,
                    flitMax: max_addr,
                    boundsFailed: bounds_failed
                };
                konataFlit(kMode, $format("S\t") + fshow(savedFlitId) + $format("\t13\tAwaitingIOCapDecode"));
            end
            tagged AwaitingIOCapDecode { flitMin: .flitMin, flitMax: .flitMax, boundsFailed: .boundsFailed } : begin
                if (decodeOut.canPeek()) begin
                    decodeOut.drop();
                    
                    let decodeRes = decodeOut.peek();

                    let flit = savedAuthFlit.flit;
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
                    konataFlit(kMode, $format("S\t") + fshow(savedFlitId) + $format("\t13\tDecoded"));
                    decodeState <= tagged Decoded {
                        failed: failed
                    };
                end
            end
            tagged Decoded .failed : begin
                if (flitCompleted) begin
                    konataFlit(kMode, $format("E\t") + fshow(savedFlitId) + $format("\t13\tDecoded"));
                    decodeState <= tagged DecodeIdle;
                end
            end
        endcase
    endrule

    interface in = reqIn;
    interface checkResponse = respsMapFIFO.deq;
    interface keyToKill = interface WriteOnly;
        method Action _write(Maybe#(KeyId) val);
            if (val matches tagged Valid .keyId) begin
                keyToKillRWire.wset(keyId);
            end
        endmethod
    endinterface;
endmodule

// Synthesizable versions of mkSimpleIOCapAxiChecker3V1
(* synthesize *)
module mkSimpleIOCapAxiChecker3V1_FastDecode_1CycleAES_Write#(KonataMode kMode)(IOCapAxiChecker3_Write);
    let m <- mkSimpleIOCapAxiChecker3V1(
        kMode,
        connectFastFSMCapDecode_2024_11,
        mk1RoundPerCycleCapSigCheckFast
    );
    interface checker = m;
endmodule

(* synthesize *)
module mkSimpleIOCapAxiChecker3V1_FastDecode_1CycleAES_Read#(KonataMode kMode)(IOCapAxiChecker3_Read);
    let m <- mkSimpleIOCapAxiChecker3V1(
        kMode,
        connectFastFSMCapDecode_2024_11,
        mk1RoundPerCycleCapSigCheckFast
    );
    interface checker = m;
endmodule

(* synthesize *)
module mkSimpleIOCapAxiChecker3V1_FastDecode_2CycleAES_Write#(KonataMode kMode)(IOCapAxiChecker3_Write);
    let m <- mkSimpleIOCapAxiChecker3V1(
        kMode,
        connectFastFSMCapDecode_2024_11,
        mk2RoundPerCycleCapSigCheckFast
    );
    interface checker = m;
endmodule

(* synthesize *)
module mkSimpleIOCapAxiChecker3V1_FastDecode_2CycleAES_Read#(KonataMode kMode)(IOCapAxiChecker3_Read);
    let m <- mkSimpleIOCapAxiChecker3V1(
        kMode,
        connectFastFSMCapDecode_2024_11,
        mk2RoundPerCycleCapSigCheckFast
    );
    interface checker = m;
endmodule

module mkNullIOCapAxiChecker3V1#(KonataMode kMode)(IOCapAxiChecker3#(no_iocap_flit)) provisos (
    Bits#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11), a__),
    Bits#(FlitState#(no_iocap_flit), b__),
    AxiCtrlFlit64#(no_iocap_flit),
    FShow#(no_iocap_flit)
);
    FIFOF#(Tuple4#(no_iocap_flit, KFlitId, KeyId, Bool)) fifo <- mkFIFOF;
    Sink#(Tuple4#(no_iocap_flit, KFlitId, KeyId, Bool)) fifoSink = toSink(fifo);

    interface in = interface Sink;
        method Bool canPut = fifoSink.canPut;
        method Action put(x);
            match { .authFlit, .flitId, .keyId, .key } = x;
            fifoSink.put(tuple4(authFlit.flit, flitId, keyId, True));
        endmethod
    endinterface;
    interface checkResponse = toSource(fifo);
    interface keyToKill = interface WriteOnly;
        method Action _write(Maybe#(KeyId) val) = noAction;
    endinterface;
endmodule

(* synthesize *)
module mkNullIOCapAxiChecker3V1_Write#(KonataMode kMode)(IOCapAxiChecker3_Write);
    let m <- mkNullIOCapAxiChecker3V1(kMode);
    interface checker = m;
endmodule

(* synthesize *)
module mkNullIOCapAxiChecker3V1_Read#(KonataMode kMode)(IOCapAxiChecker3_Read);
    let m <- mkNullIOCapAxiChecker3V1(kMode);
    interface checker = m;
endmodule

// mkIOCapAxiCheckerPool#(n, flit) to make a Vector#(n, someChecker) and take the first available one.
// Max input/output rate are still 1/cycle, n should be tuned such that n = ceil((x cycles for one check)/(y cycles to receive an authenticated IOCapAxiFlit))
// i.e. that whenever a new authed flit arrives, which can at most be once every y cycles, a checker in the pool will be ready.
// Note that order needs to be preserved here - a 1-caveat write that arrives after a 3-caveat write must be blocked until the 3-caveat write has been checked - otherwise the w-flits will get mixed up.
    // Should be able to enforce this by using a round-robin pool. If you insert into #1, then #2, then #3, and take out responses from #1, then #2, then #3, you're fine.
    // Technically this doesn't apply to reads - could take a shortcut there?
    // TODO this is worth thinking about in the write-up! In PCIe land where data+address arrive at once, do we also have this latency dependency? Likely worse because writes and reads are ordered together?

// Can't use Integer for n because "Integer" != "numeric type"

module mkInOrderIOCapAxiChecker3V1Pool#(
    KonataMode kMode,
    NumProxy#(n) n_proxy,
    Vector#(n, IOCapAxiChecker3#(no_iocap_flit)) checkers
)(IOCapAxiChecker3#(no_iocap_flit)) provisos (
    Bits#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11), a__),
    Bits#(FlitState#(no_iocap_flit), b__),
    AxiCtrlFlit64#(no_iocap_flit),
    FShow#(no_iocap_flit),
    Add#(n__, TLog#(n), 64)
);

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
            $display("// tick incrementInsert ", fshow(insertPointer));
            let newInsertPointer = insertPointer + 1;
            if (inLiteralRange(insertPointer, valueOf(n)) && newInsertPointer >= fromInteger(valueOf(n)))
                insertPointer <= 0;
            else
                insertPointer <= newInsertPointer;
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

    interface in = interface Sink;
        method Bool canPut;
            return checkers[insertPointer].in.canPut();
        endmethod
        method Action put (Tuple4#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11), KFlitId, KeyId, Maybe#(Key)) val);
            let flitId = tpl_2(val);
            konataFlit(kMode, $format("L\t") + fshow(flitId) + $format("\t1\tChecker ") + fshow(insertPointer));
            checkers[insertPointer].in.put(val);
            incrementInsert.send();
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
    interface keyToKill = interface WriteOnly;
        method Action _write(Maybe#(KeyId) val);
            for (Integer i = 0; i < valueOf(n); i = i + 1) begin
                checkers[i].keyToKill <= val;
            end
        endmethod
    endinterface;
endmodule

module mkInOrderIOCapAxiChecker3V1Pool_Read#(
    KonataMode kMode,
    NumProxy#(n) n_proxy,
    function module#(IOCapAxiChecker3_Read) makeChecker(KonataMode kMode)
)(IOCapAxiChecker3_Read) provisos (Add#(n__, TLog#(n), 64));
   
    function IOCapAxiChecker3#(AXI4_ARFlit#(4, 64, 0)) baseCheckerOf(IOCapAxiChecker3_Read r) = r.checker;

    Vector#(n, IOCapAxiChecker3_Read) checkers <- replicateM(makeChecker(
        kMode
    ));

    let m <- mkInOrderIOCapAxiChecker3V1Pool(
        kMode,
        n_proxy,
        map(baseCheckerOf, checkers)
    );
    interface checker = m;
endmodule

module mkInOrderIOCapAxiChecker3V1Pool_Write#(
    KonataMode kMode,
    NumProxy#(n) n_proxy,
    function module#(IOCapAxiChecker3_Write) makeChecker(KonataMode kMode)
)(IOCapAxiChecker3_Write) provisos (Add#(n__, TLog#(n), 64));
   
    function IOCapAxiChecker3#(AXI4_AWFlit#(4, 64, 0)) baseCheckerOf(IOCapAxiChecker3_Write r) = r.checker;

    Vector#(n, IOCapAxiChecker3_Write) checkers <- replicateM(makeChecker(
        kMode
    ));

    let m <- mkInOrderIOCapAxiChecker3V1Pool(
        kMode,
        n_proxy,
        map(baseCheckerOf, checkers)
    );
    interface checker = m;
endmodule

typedef union tagged {
    void NoFlit;
    void Building0;
    void Building1;
    void Building2;
    struct {
        KFlitId flitId;
        AuthenticatedFlit#(no_iocap_flit, Cap2024_11) authFlit;
    } AuthFlitReady;
} FlitState#(type no_iocap_flit) deriving (Bits, FShow, Eq);

typedef union tagged {
    void NoKeyId;
    // Waiting for the keyRequest pipe to unblock
    struct {
        KeyId keyId;
        KFlitId flitId;
    } AwaitingKeyRequest;
    // Issued a keyRequest, waiting for a key response
    struct {
        KeyId keyId;
        KFlitId flitId;
    } AwaitingKey;
    struct {
        KeyId keyId;
        KFlitId flitId;
        Maybe#(Key) key;
    } HasKey;
} KeyState deriving (Bits, FShow, Eq);

module mkChecker3CombinedFrontend#(
    KonataMode kMode,
    Sink#(Tuple4#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11), KFlitId, KeyId, Maybe#(Key))) insertCompletedCaps,
    Sink#(KeyId) keyRequest,
    ReadOnly#(Maybe#(Tuple2#(KeyId, Maybe#(Key)))) keyResponse,
    ReadOnly#(Maybe#(KeyId)) keyToKill,
    function KeyId keyIdOf(Cap2024_11 cap)
)(Sink#(Tuple2#(iocap_flit, KFlitId))) provisos (
    IOCapPackableFlit#(iocap_flit, no_iocap_flit),
    Bits#(Tuple2#(no_iocap_flit, Bit#(256)), a__),
    Bits#(Tuple2#(iocap_flit, IOCapAxi_Konata::KFlitId), b__),
    Bits#(IOCapAxi_Checker3s::FlitState#(no_iocap_flit), c__),
    FShow#(iocap_flit),
    FShow#(no_iocap_flit)
);
    FIFOF#(Tuple2#(iocap_flit, KFlitId)) inFlitsFIFO <- mkFIFOF();
    let inFlits = toSource(inFlitsFIFO);

    ConfigReg#(KFlitId) savedFlitId <- mkConfigReg(?);
    ConfigReg#(FlitState#(no_iocap_flit)) flitState <- mkConfigReg(tagged NoFlit);
    ConfigReg#(KeyState) keyState <- mkConfigReg(tagged NoKeyId);
    Reg#(Tuple2#(no_iocap_flit, Bit#(256))) flitInProgress <- mkReg(unpack(0));

    // The keyId we just extracted from the flit in the Building0 -> Building1 transition.
    // Used for ticking the keyState machine as early as possible.
    RWire#(Tuple2#(KFlitId, KeyId)) keyIdToStartSearchingFor <- mkRWire;
    PulseWire finishingFlit <- mkPulseWire;

    function Bool canBuildFlit();
        if (flitState matches tagged AuthFlitReady {})
            return False;
        else
            return True;
    endfunction

    rule buildFlit (inFlits.canPeek() && canBuildFlit());
        match { .iocapFlit, .flitId } = inFlits.peek();
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(iocapFlit);

        case (tuple2(flitState, spec)) matches
            { tagged NoFlit, tagged Start .flit } : begin
                inFlits.drop();
                savedFlitId <= flitId;
                flitInProgress <= tuple2(flit, 0);
                flitState <= tagged Building0;
                konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t10\tB0"));
            end
            // Only take CapBits1 if we can start a new key search.
            { tagged Building0, tagged CapBits1 .bits } : begin
                inFlits.drop();
                let newBits = // { 0, bits }
                    { 84'b0, bits, 86'b0 };
                
                Tuple2#(Bit#(128), Cap2024_11) partialCap = unpack(newBits);
                let keyId = keyIdOf(tpl_2(partialCap));
                keyIdToStartSearchingFor.wset(tuple2(flitId, keyId));

                dynamicAssert((keyState == tagged NoKeyId), "Shouldn't be able to get into this situation");

                let flit = tpl_1(flitInProgress);
                flitInProgress <= tuple2(
                    flit,
                    newBits
                );
                flitState <= tagged Building1;
                konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t10\tB1"));
            end
            { tagged Building1, tagged CapBits2 .bits } : begin
                inFlits.drop();
                match { .flit, .bitsInProgress } = flitInProgress;
                flitInProgress <= tuple2(
                    flit,
                    // { 0, bits, bitsInProgress[85:0] }
                    { 0, bitsInProgress[171:86], bits }
                );
                flitState <= tagged Building2;
                konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t10\tB2"));
            end
            { tagged Building2, tagged CapBits3 .bits } : begin
                inFlits.drop();
                match { .flit, .bitsInProgress } = flitInProgress;
                let combinedBits = { bits, bitsInProgress[171:0] };
                // EXPERIMENT FROM COMPLETETLY FLIPPING, DIDNT HELP let combinedBits = { bitsInProgress[255:84], bits };
                AuthenticatedFlit#(no_iocap_flit, Cap2024_11) authFlit = AuthenticatedFlit {
                    flit: flit,
                    cap: unpack(combinedBits[127:0]),
                    sig: combinedBits[255:128]
                };
                // $display("// IOCap - Recevied bits ", fshow(combinedBits));
                // $display("// IOCap - Received auth flitpack ", fshow(authFlit));

                flitState <= tagged AuthFlitReady {
                    flitId: flitId,
                    authFlit: authFlit
                };

                konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t10\tR"));
            end
            default : $error("IOCap protocol error ", fshow(flitState), " ", fshow(iocapFlit));
        endcase
    endrule

    rule tickKey;
        let newKeyState = keyState;
        case (keyState) matches
            tagged NoKeyId : if (keyIdToStartSearchingFor.wget() matches tagged Valid { .flitId, .keyId }) begin
                if (keyToKill == tagged Valid keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: tagged Invalid
                    };
                end else if (keyResponse matches tagged Valid { .keyRespId, .maybeKeyData } &&& keyRespId == keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: maybeKeyData
                    };
                end else if (keyRequest.canPut()) begin
                    keyRequest.put(keyId);
                    newKeyState = tagged AwaitingKey {
                        keyId: keyId,
                        flitId: flitId
                    };
                end else begin
                    newKeyState = tagged AwaitingKeyRequest {
                        keyId: keyId,
                        flitId: flitId
                    };
                end
            end
            tagged AwaitingKeyRequest { keyId: .keyId, flitId: .flitId } : begin
                if (keyToKill == tagged Valid keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: tagged Invalid
                    };
                end else if (keyResponse matches tagged Valid { .keyRespId, .maybeKeyData } &&& keyRespId == keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: maybeKeyData
                    };
                end else if (keyRequest.canPut()) begin
                    keyRequest.put(keyId);
                    newKeyState = tagged AwaitingKey {
                        keyId: keyId,
                        flitId: flitId
                    };
                end else begin
                    newKeyState = tagged AwaitingKeyRequest {
                        keyId: keyId,
                        flitId: flitId
                    };
                end
            end
            tagged AwaitingKey { keyId: .keyId, flitId: .flitId } : begin
                if (keyToKill == tagged Valid keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: tagged Invalid
                    };
                end else if (keyResponse matches tagged Valid { .keyRespId, .maybeKeyData } &&& keyRespId == keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: maybeKeyData
                    };
                end
            end
            tagged HasKey { keyId: .keyId, flitId: .flitId } : begin
                if (keyToKill == tagged Valid keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: tagged Invalid
                    };
                end
            end
        endcase

        if (newKeyState matches tagged HasKey {
            keyId: .keyId,
            key: .key
        } &&& insertCompletedCaps.canPut() &&& flitState matches tagged AuthFlitReady {
            flitId: .flitId,
            authFlit: .authFlit
        }) begin
            newKeyState = tagged NoKeyId;
            insertCompletedCaps.put(tuple4(
                authFlit,
                flitId,
                keyId,
                key
            ));
            finishingFlit.send();
        end

        if (newKeyState != keyState) begin
            case (newKeyState) matches
                // Start and end SigCheckIdle to show that this flit is being finished
                tagged NoKeyId : konataFlit(kMode,
                    $format("S\t") + fshow(savedFlitId) + $format("\t11\tIdle\nE\t") + fshow(savedFlitId) + $format("\t11\tIdle")
                );
                tagged AwaitingKeyRequest .* : konataFlit(kMode,
                    $format("S\t") + fshow(savedFlitId) + $format("\t11\tAwaitingKeyRequest")
                );
                tagged AwaitingKey .* : konataFlit(kMode,
                    $format("S\t") + fshow(savedFlitId) + $format("\t11\tAwaitingKey")
                );
                tagged HasKey .* : konataFlit(kMode,
                    $format("S\t") + fshow(savedFlitId) + $format("\t11\tHasKey")
                );
            endcase
        end
        keyState <= newKeyState;
    endrule

    rule finishFlit(finishingFlit && !canBuildFlit());
        konataFlit(kMode, $format("E\t") + fshow(savedFlitId) + $format("\t10\tR"));
        flitState <= tagged NoFlit;
    endrule

    return toSink(inFlitsFIFO);
endmodule


module mkChecker3CombinedPipelinedFrontend#(
    KonataMode kMode,
    Sink#(Tuple4#(AuthenticatedFlit#(no_iocap_flit, Cap2024_11), KFlitId, KeyId, Maybe#(Key))) insertCompletedCaps,
    Sink#(KeyId) keyRequest,
    ReadOnly#(Maybe#(Tuple2#(KeyId, Maybe#(Key)))) keyResponse,
    ReadOnly#(Maybe#(KeyId)) keyToKill,
    function KeyId keyIdOf(Cap2024_11 cap)
)(Sink#(Tuple2#(iocap_flit, KFlitId))) provisos (
    IOCapPackableFlit#(iocap_flit, no_iocap_flit),
    Bits#(Tuple2#(no_iocap_flit, Bit#(256)), a__),
    Bits#(Tuple2#(iocap_flit, IOCapAxi_Konata::KFlitId), b__),
    Bits#(IOCapAxi_Checker3s::FlitState#(no_iocap_flit), c__),
    FShow#(iocap_flit),
    FShow#(no_iocap_flit)
);
    FIFOF#(Tuple2#(iocap_flit, KFlitId)) inFlitsFIFO <- mkFIFOF();
    let inFlits = toSource(inFlitsFIFO);

    ConfigReg#(KFlitId) savedFlitId <- mkConfigReg(?);
    ConfigReg#(FlitState#(no_iocap_flit)) flitState <- mkConfigReg(tagged NoFlit);
    // Use a separate FIFO for authflit to ensure that flitState can pipeline - keep picking up new flits while doing old ones
    FIFOF#(Tuple2#(KFlitId, AuthenticatedFlit#(no_iocap_flit, Cap2024_11))) authFlitFIFO <- mkUGFIFOF;
    ConfigReg#(KeyState) keyState <- mkConfigReg(tagged NoKeyId);
    Reg#(Tuple2#(no_iocap_flit, Bit#(256))) flitInProgress <- mkReg(unpack(0));

    // The keyId we just extracted from the flit in the Building0 -> Building1 transition.
    // Used for ticking the keyState machine as early as possible.
    RWire#(Tuple2#(KFlitId, KeyId)) keyIdToStartSearchingFor <- mkRWire;

    rule buildFlit (inFlits.canPeek());
        match { .iocapFlit, .flitId } = inFlits.peek();
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(iocapFlit);

        case (tuple2(flitState, spec)) matches
            { tagged NoFlit, tagged Start .flit } : begin
                inFlits.drop();
                savedFlitId <= flitId;
                flitInProgress <= tuple2(flit, 0);
                flitState <= tagged Building0;
                konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t10\tB0"));
            end
            // Only take CapBits1 if we can start a new key search.
            { tagged Building0, tagged CapBits1 .bits } : if (keyState == tagged NoKeyId) begin
                inFlits.drop();
                let newBits = // { 0, bits }
                    { 84'b0, bits, 86'b0 };
                
                Tuple2#(Bit#(128), Cap2024_11) partialCap = unpack(newBits);
                let keyId = keyIdOf(tpl_2(partialCap));
                keyIdToStartSearchingFor.wset(tuple2(flitId, keyId));

                let flit = tpl_1(flitInProgress);
                flitInProgress <= tuple2(
                    flit,
                    newBits
                );
                flitState <= tagged Building1;
                konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t10\tB1"));
            end
            { tagged Building1, tagged CapBits2 .bits } : begin
                inFlits.drop();
                match { .flit, .bitsInProgress } = flitInProgress;
                flitInProgress <= tuple2(
                    flit,
                    // { 0, bits, bitsInProgress[85:0] }
                    { 0, bitsInProgress[171:86], bits }
                );
                flitState <= tagged Building2;
                konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t10\tB2"));
            end
            { tagged Building2, tagged CapBits3 .bits } : if (authFlitFIFO.notFull) begin
                inFlits.drop();
                match { .flit, .bitsInProgress } = flitInProgress;
                let combinedBits = { bits, bitsInProgress[171:0] };
                // EXPERIMENT FROM COMPLETETLY FLIPPING, DIDNT HELP let combinedBits = { bitsInProgress[255:84], bits };
                AuthenticatedFlit#(no_iocap_flit, Cap2024_11) authFlit = AuthenticatedFlit {
                    flit: flit,
                    cap: unpack(combinedBits[127:0]),
                    sig: combinedBits[255:128]
                };
                // $display("// IOCap - Recevied bits ", fshow(combinedBits));
                // $display("// IOCap - Received auth flitpack ", fshow(authFlit));

                flitState <= tagged NoFlit;
                authFlitFIFO.enq(tuple2(flitId, authFlit));

                konataFlit(kMode, $format("E\t") + fshow(flitId) + $format("\t10\tB2"));
            end
            default : begin
                $error("IOCap protocol error ", fshow(flitState), " ", fshow(spec));
                $finish();
            end
        endcase
    endrule

    rule tickKey;
        let keyFlitId = ?;
        let newKeyState = keyState;
        case (keyState) matches
            tagged NoKeyId : if (keyIdToStartSearchingFor.wget() matches tagged Valid { .flitId, .keyId }) begin
                keyFlitId = flitId;
                if (keyToKill == tagged Valid keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: tagged Invalid
                    };
                end else if (keyResponse matches tagged Valid { .keyRespId, .maybeKeyData } &&& keyRespId == keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: maybeKeyData
                    };
                end else if (keyRequest.canPut()) begin
                    keyRequest.put(keyId);
                    newKeyState = tagged AwaitingKey {
                        keyId: keyId,
                        flitId: flitId
                    };
                end else begin
                    newKeyState = tagged AwaitingKeyRequest {
                        keyId: keyId,
                        flitId: flitId
                    };
                end
            end
            tagged AwaitingKeyRequest { keyId: .keyId, flitId: .flitId } : begin
                keyFlitId = flitId;
                if (keyToKill == tagged Valid keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: tagged Invalid
                    };
                end else if (keyResponse matches tagged Valid { .keyRespId, .maybeKeyData } &&& keyRespId == keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: maybeKeyData
                    };
                end else if (keyRequest.canPut()) begin
                    keyRequest.put(keyId);
                    newKeyState = tagged AwaitingKey {
                        keyId: keyId,
                        flitId: flitId
                    };
                end else begin
                    newKeyState = tagged AwaitingKeyRequest {
                        keyId: keyId,
                        flitId: flitId
                    };
                end
            end
            tagged AwaitingKey { keyId: .keyId, flitId: .flitId } : begin
                keyFlitId = flitId;
                if (keyToKill == tagged Valid keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: tagged Invalid
                    };
                end else if (keyResponse matches tagged Valid { .keyRespId, .maybeKeyData } &&& keyRespId == keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: maybeKeyData
                    };
                end
            end
            tagged HasKey { keyId: .keyId, flitId: .flitId } : begin
                keyFlitId = flitId;
                if (keyToKill == tagged Valid keyId) begin
                    newKeyState = tagged HasKey {
                        keyId: keyId,
                        flitId: flitId,
                        key: tagged Invalid
                    };
                end
            end
        endcase

        if (newKeyState matches tagged HasKey {
            keyId: .keyId,
            key: .key
        } &&& insertCompletedCaps.canPut() &&& authFlitFIFO.notEmpty) begin
            match { .flitId, .authFlit } = authFlitFIFO.first;
            authFlitFIFO.deq();
            newKeyState = tagged NoKeyId;
            insertCompletedCaps.put(tuple4(
                authFlit,
                flitId,
                keyId,
                key
            ));
        end

        if (newKeyState != keyState) begin
            case (newKeyState) matches
                // Start and end SigCheckIdle to show that this flit is being finished
                tagged NoKeyId : konataFlit(kMode,
                    $format("S\t") + fshow(keyFlitId) + $format("\t11\tIdle\nE\t") + fshow(keyFlitId) + $format("\t11\tIdle")
                );
                tagged AwaitingKeyRequest .* : konataFlit(kMode,
                    $format("S\t") + fshow(keyFlitId) + $format("\t11\tAwaitingKeyRequest")
                );
                tagged AwaitingKey .* : konataFlit(kMode,
                    $format("S\t") + fshow(keyFlitId) + $format("\t11\tAwaitingKey")
                );
                tagged HasKey .* : konataFlit(kMode,
                    $format("S\t") + fshow(keyFlitId) + $format("\t11\tHasKey")
                );
            endcase
        end
        keyState <= newKeyState;
    endrule

    return toSink(inFlitsFIFO);
endmodule

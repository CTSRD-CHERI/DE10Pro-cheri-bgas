import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;

import IOCapAxi_Types :: *;
import IOCapAxi_Flits :: *;
import IOCapAxi_KeyManagers :: *;

import Cap2024 :: *;
import Cap2024_02 :: *;
import Cap2024_02_Decode_FastFSM :: *;
import Cap2024_SigCheck_Aes_1RoundPerCycle :: *; // Get CapSigCheckIn
import Cap2024_SigCheck_Aes_2RoundPerCycle :: *;

typeclass AxiCtrlFlit64#(type flit);
    function Bit#(64) burstAddr(flit f);
    function AXI4_Len burstLen(flit f);
    function AXI4_Size burstSize(flit f);
    function AXI4_Burst burstKind(flit f);
    function Bool isBurstRead(flit f);
endtypeclass

instance AxiCtrlFlit64#(AXI4_AWFlit#(t_id, 64, t_data));
    function Bit#(64) burstAddr(AXI4_AWFlit#(t_id, 64, t_data) f) = f.awaddr;
    function AXI4_Len burstLen(AXI4_AWFlit#(t_id, 64, t_data) f) = f.awlen;
    function AXI4_Size burstSize(AXI4_AWFlit#(t_id, 64, t_data) f) = f.awsize;
    function AXI4_Burst burstKind(AXI4_AWFlit#(t_id, 64, t_data) f) = f.awburst;
    function Bool isBurstRead(AXI4_AWFlit#(t_id, 64, t_data) f) = False;
endinstance

instance AxiCtrlFlit64#(AXI4_ARFlit#(t_id, 64, t_data));
    function Bit#(64) burstAddr(AXI4_ARFlit#(t_id, 64, t_data) f) = f.araddr;
    function AXI4_Len burstLen(AXI4_ARFlit#(t_id, 64, t_data) f) = f.arlen;
    function AXI4_Size burstSize(AXI4_ARFlit#(t_id, 64, t_data) f) = f.arsize;
    function AXI4_Burst burstKind(AXI4_ARFlit#(t_id, 64, t_data) f) = f.arburst;
    function Bool isBurstRead(AXI4_ARFlit#(t_id, 64, t_data) f) = True;
endinstance

interface IOCapAxiChecker#(type no_iocap_flit, type tcap);
    interface Sink#(Tuple3#(AuthenticatedFlit#(no_iocap_flit, tcap), KeyId, Maybe#(Key))) checkRequest;
    interface Source#(Tuple3#(no_iocap_flit, KeyId, Bool)) checkResponse;
endinterface

typedef union tagged {
    struct {
        no_iocap_flit flit;
        KeyId keyId;
    } WaitingForBoundsAndDecodeAndSig;
    struct {
        no_iocap_flit flit;
        KeyId keyId;
        Bit#(64) min_addr;
        Bit#(65) max_addr;
        Bool bounds_failed;
    } WaitingForDecodeAndSig;
    struct {
        no_iocap_flit flit;
        KeyId keyId;
        Bool bounds_or_decode_failed;
    } WaitingForSig;
} IOCapFlitInProgress#(type no_iocap_flit) deriving (Bits, FShow);

// One-at-a-time IOCap flit checker
//
// Takes a function which returns a module connecting (inputs to a iocap decoder) to (outputs to a iocap decoder).
//
// Uses the 2-round-per-cycle signature checker, which should have the following latencies:
// | n_cavs | sigchk | 2024_11_fsm |
// | ------ | ------ | ----------- |
// |   0    |    6   |      3      |
// |   1    |   12   |      6      |
// |   2    |   18   |      8      |
//
// Capabilities are decoded and signature-checked in parallel, and we can assume the decoder latency is always less than the signature check.
// We add ~3 cycles of latency on top of the signature check with the various FIFO stages, so the maximum latency should be ~21 cycles.
module mkSimpleIOCapAxiChecker#(function module#(Empty) makeDecoder(Get#(tcap) ins, Put#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) outs))(IOCapAxiChecker#(no_iocap_flit, tcap)) provisos (Bits#(AuthenticatedFlit#(no_iocap_flit, tcap), a__), AxiCtrlFlit64#(no_iocap_flit), FShow#(no_iocap_flit), Cap#(tcap));
    FIFOF#(Tuple3#(AuthenticatedFlit#(no_iocap_flit, tcap), KeyId, Maybe#(Key))) reqs <- mkFIFOF;
    // TODO this could be a bypass fifof...
    FIFOF#(Tuple3#(no_iocap_flit, KeyId, Bool)) resps <- mkFIFOF;

    Reg#(Maybe#(IOCapFlitInProgress#(no_iocap_flit))) flitInProgress <- mkReg(tagged Invalid);

    FIFOF#(tcap) decodeIn <- mkFIFOF; 
    FIFOF#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) decodeOut <- mkFIFOF;
    makeDecoder(toGet(decodeIn), toPut(decodeOut));

    FIFOF#(CapSigCheckIn#(tcap)) sigCheckIn <- mkFIFOF;
    FIFOF#(CapCheckResult#(Bit#(0))) sigCheckOut <- mkFIFOF;
    mk2RoundPerCycleCapSigCheck(toGet(sigCheckIn), toPut(sigCheckOut));

    rule start(flitInProgress matches tagged Invalid);
        reqs.deq();

        case (reqs.first) matches
            { .authFlit, .keyId, tagged Invalid } : begin
                // Got a request to use an invalid key
                // Just pass it through to the output, 1 cycle latency
                resps.enq(tuple3(authFlit.flit, keyId, False));

                // // TEST - actually put everything through the checking system
                // flitInProgress <= tagged Valid (tagged WaitingForBoundsAndDecodeAndSig (authFlit.flit));
                // // Will take ~18 cycles
                // sigCheckIn.enq(CapSigCheckIn {
                //     cap: authFlit.cap,
                //     expectedSig: authFlit.sig,
                //     secret: 128'hdeadbeef
                // });
                // // Will take ~7 cycles at most - always shorter than sigCheckIn
                // decodeIn.enq(authFlit.cap);
            end
            { .authFlit, .keyId, tagged Valid .key } : begin                
                flitInProgress <= tagged Valid (tagged WaitingForBoundsAndDecodeAndSig {
                    flit: authFlit.flit,
                    keyId: keyId
                });
                $display("IOCap - starting check on flit with secret ", fshow(key), " - cap ", fshow(pack(authFlit.cap)), " - expectedSig ", fshow(authFlit.sig));
                // Will take ~18 cycles at most
                sigCheckIn.enq(CapSigCheckIn {
                    cap: authFlit.cap,
                    expectedSig: authFlit.sig,
                    secret: key
                });
                // Will take ~7 cycles at most - always shorter than sigCheckIn
                decodeIn.enq(authFlit.cap);
            end
        endcase
    endrule

    rule bounds(flitInProgress matches tagged Valid (tagged WaitingForBoundsAndDecodeAndSig .flitAndKeyId));
        let bounds_failed = False;
        Bit#(64) min_addr = 0;
        Bit#(65) max_addr = 0;
        case (burstKind(flitAndKeyId.flit)) matches
            FIXED: begin
                // Each beat of a burst starts at the same address
                // The max address = min address + the number of bytes per beat
                // number of bytes per beat = 1 << burstSize, up to 128 => length = 7
                min_addr = burstAddr(flitAndKeyId.flit);
                Bit#(7) beatSize = 7'b1 << burstSize(flitAndKeyId.flit).val;
                max_addr = zeroExtend(min_addr) + zeroExtend(beatSize);
            end
            INCR: begin
                // Each beat of a burst starts at (last beat address + beat length)
                // min address = starting address
                // max address = min address + (beats/burst) * (bytes/beat)
                // beats/burst = burstLen [0..=255] + 1, [1..=256]
                // bytes/beat  = 1 << burstSize, up to 128
                // multiplied together the max is 32640, up to 15 bits
                min_addr = burstAddr(flitAndKeyId.flit);
                Bit#(9) beatsPerBurst = zeroExtend(burstLen(flitAndKeyId.flit)) + 1;
                Bit#(15) totalBurstBytes = zeroExtend(beatsPerBurst) << burstSize(flitAndKeyId.flit).val;
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
            $display("IOCap - flit failed Bounds ", fshow(flitAndKeyId.flit));
        end

        flitInProgress <= tagged Valid (tagged WaitingForDecodeAndSig {
            flit: flitAndKeyId.flit,
            keyId: flitAndKeyId.keyId,
            min_addr: min_addr,
            max_addr: max_addr,
            bounds_failed: bounds_failed
        });
    endrule

    rule decode(flitInProgress matches tagged Valid (tagged WaitingForDecodeAndSig .flitWithBounds));
        decodeOut.deq();
        let decodeRes = decodeOut.first;

        let flit = flitWithBounds.flit;
        let fail = False;
        if (flitWithBounds.bounds_failed) begin
            fail = True;
        end else begin
            case (decodeRes) matches
                tagged Succ ({ .perms, .range }) : begin
                    // Check permissions
                    if (isBurstRead(flit) && perms == Write) begin
                        fail = True;
                    end else if (!isBurstRead(flit) && perms == Read) begin
                        fail = True;
                    end

                    // Check range
                    if ((flitWithBounds.min_addr < range.base) || (flitWithBounds.max_addr > range.top)) begin
                        fail = True;
                    end
                end
                tagged Fail .* : fail = True;
            endcase

            if (fail) begin
                $display("IOCap - flit failed Decode ", fshow(flit), " - ", fshow(decodeRes));
            end
        end

        flitInProgress <= tagged Valid (tagged WaitingForSig {
            flit: flitWithBounds.flit,
            keyId: flitWithBounds.keyId,
            bounds_or_decode_failed: fail
        });
    endrule

    rule sigcheck(flitInProgress matches tagged Valid (tagged WaitingForSig .decodedFlit));
        sigCheckOut.deq();
        let sigCheckRes = sigCheckOut.first;

        let allow = True;
        if (sigCheckRes matches tagged Fail .*) begin
            allow = False;
            $display("IOCap - flit failed sigcheck");
        end else if (decodedFlit.bounds_or_decode_failed) begin
            allow = False;
        end
        resps.enq(tuple3(decodedFlit.flit, decodedFlit.keyId, allow));
        flitInProgress <= tagged Invalid;
    endrule

    interface checkRequest = toSink(reqs);
    interface checkResponse = toSource(resps);
endmodule

// mkIOCapAxiCheckerPool#(n, flit) to make a Vector#(n, someChecker) and take the first available one.
// Max input/output rate are still 1/cycle, n should be tuned such that n = ceil((x cycles for one check)/(y cycles to receive an authenticated IOCapAxiFlit))
// i.e. that whenever a new authed flit arrives, which can at most be once every y cycles, a checker in the pool will be ready.
// Note that order needs to be preserved here - a 1-caveat write that arrives after a 3-caveat write must be blocked until the 3-caveat write has been checked - otherwise the w-flits will get mixed up.
    // Should be able to enforce this by using a round-robin pool. If you insert into #1, then #2, then #3, and take out responses from #1, then #2, then #3, you're fine.
    // Technically this doesn't apply to reads - could take a shortcut there?
    // TODO this is worth thinking about in the write-up! In PCIe land where data+address arrive at once, do we also have this latency dependency? Likely worse because writes and reads are ordered together?

// Can't use Integer for n because "Integer" != "numeric type"

// TODO FIGURE OUT HOW TO MAKE THIS PARAMETERIZABLE ON DECODER TYPE
module mkInOrderIOCapAxiCheckerPool#(NumProxy#(n) n_proxy, module#(IOCapAxiChecker#(no_iocap_flit, tcap)) toPool)(IOCapAxiChecker#(no_iocap_flit, tcap)) provisos (Bits#(AuthenticatedFlit#(no_iocap_flit, tcap), a__), AxiCtrlFlit64#(no_iocap_flit), FShow#(no_iocap_flit));    
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
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

import Cap2024_02 :: *;
import Cap2024_02_Decode_FastFSM :: *;
import Cap2024_02_SigCheck_Aes_1RoundPerCycle :: *; // Get CapSigCheckIn
import Cap2024_02_SigCheck_Aes_2RoundPerCycle :: *;

interface IOCapSingleExposer#(numeric type t_iocap_id, numeric type t_iocap_data );
    interface IOCapAXI4_Slave#(t_iocap_id, t_iocap_data) iocapsIn;

    interface AXI4_Master#(t_iocap_id, 64, t_iocap_data, 0, 0, 0, 0, 0) sanitizedOut;
endinterface

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

interface IOCapAxiChecker#(type no_iocap_flit);
    interface Sink#(Tuple2#(AuthenticatedFlit#(no_iocap_flit), Maybe#(Key))) checkRequest;
    interface Source#(Tuple2#(no_iocap_flit, Bool)) checkResponse;
endinterface

typedef union tagged {
    no_iocap_flit WaitingForBoundsAndDecodeAndSig;
    struct {
        no_iocap_flit flit;
        Bit#(64) min_addr;
        Bit#(65) max_addr;
        Bool bounds_failed;
    } WaitingForDecodeAndSig;
    struct {
        no_iocap_flit flit;
        Bool bounds_or_decode_failed;
    } WaitingForSig;
} IOCapFlitInProgress#(type no_iocap_flit) deriving (Bits, FShow);

// One-at-a-time IOCap flit checker
module mkSimpleIOCapAxiChecker(IOCapAxiChecker#(no_iocap_flit)) provisos (Bits#(AuthenticatedFlit#(no_iocap_flit), a__), AxiCtrlFlit64#(no_iocap_flit));
    FIFOF#(Tuple2#(AuthenticatedFlit#(no_iocap_flit), Maybe#(Key))) reqs <- mkFIFOF;
    // TODO this could be a bypass fifof...
    FIFOF#(Tuple2#(no_iocap_flit, Bool)) resps <- mkFIFOF;

    Reg#(Maybe#(IOCapFlitInProgress#(no_iocap_flit))) flitInProgress <- mkReg(tagged Invalid);

    FIFOF#(Cap2024_02) decodeIn <- mkFIFOF; 
    FIFOF#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) decodeOut <- mkFIFOF;
    mkFastFSMCapDecode(toGet(decodeIn), toPut(decodeOut));

    FIFOF#(CapSigCheckIn) sigCheckIn <- mkFIFOF;
    FIFOF#(CapCheckResult#(Bit#(0))) sigCheckOut <- mkFIFOF;
    mk2RoundPerCycleCapSigCheck(toGet(sigCheckIn), toPut(sigCheckOut));

    rule start(flitInProgress matches tagged Invalid);
        reqs.deq();

        case (reqs.first) matches
            { .authFlit, tagged Invalid } : begin
                // Got a request to use an invalid key
                // Just pass it through to the output, 1 cycle latency
                resps.enq(tuple2(authFlit.flit, False));
            end
            { .authFlit, tagged Valid .key } : begin                
                flitInProgress <= tagged Valid (tagged WaitingForBoundsAndDecodeAndSig (authFlit.flit));
                // Will take ~18 cycles
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

    rule bounds(flitInProgress matches tagged Valid (tagged WaitingForBoundsAndDecodeAndSig .flit));
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
                // beats/burst = burstLen, up to 255
                // bytes/beat  = 1 << burstSize, up to 128
                // multiplied together the max is 32640, up to 15 bits
                min_addr = burstAddr(flit);
                Bit#(15) totalBurstBytes = zeroExtend(burstLen(flit)) << burstSize(flit).val;
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

        flitInProgress <= tagged Valid (tagged WaitingForDecodeAndSig {
            flit: flit,
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
        end

        flitInProgress <= tagged Valid (tagged WaitingForSig {
            flit: flitWithBounds.flit,
            bounds_or_decode_failed: fail
        });
    endrule

    rule sigcheck(flitInProgress matches tagged Valid (tagged WaitingForSig .decodedFlit));
        sigCheckOut.deq();
        let sigCheckRes = sigCheckOut.first;

        let allow = True;
        if (sigCheckRes matches tagged Fail .*) begin
            allow = False;
        end else if (decodedFlit.bounds_or_decode_failed) begin
            allow = False;
        end
        resps.enq(tuple2(decodedFlit.flit, allow));
        flitInProgress <= tagged Invalid;
    endrule

    interface checkRequest = toSink(reqs);
    interface checkResponse = toSource(resps);
endmodule

// TODO mkIOCapAxiCheckerPool#(n, flit) to make a Vector#(n, mkSimpleIOCapAxiChecker#(flit)) and take the first available one.
// Max input/output rate are still 1/cycle, n should be tuned such that n = ceil((x cycles for one check)/(y cycles to receive an authenticated IOCapAxiFlit))
// i.e. that whenever a new authed flit arrives, which can at most be once every y cycles, a checker in the pool will be ready.
// Note that order needs to be preserved here - a 1-caveat write that arrives after a 3-caveat write must be blocked until the 3-caveat write has been checked - otherwise the w-flits will get mixed up.
    // Should be able to enforce this by using a round-robin pool. If you insert into #1, then #2, then #3, and take out responses from #1, then #2, then #3, you're fine.
    // Technically this doesn't apply to reads - could take a shortcut there?
    // TODO this is worth thinking about in the write-up! In PCIe land where data+address arrive at once, do we also have this latency dependency? Likely worse because writes and reads are ordered together?

module mkSimpleIOCapExposer#(IOCap_KeyManager#(t_keystore_data) keyStore)(IOCapSingleExposer#(t_id, t_data)) provisos (
    Mul#(TDiv#(t_keystore_data, 8), 8, t_keystore_data),
    Add#(t_keystore_data, a__, 128),
    Add#(TDiv#(t_keystore_data, 8), b__, 16)
);
    // Doesn't support WRAP bursts right now

    AddressChannelCapUnwrapper#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0)) awIn <- mkSimpleAddressChannelCapUnwrapper;
    FIFOF#(AXI4_AWFlit#(t_id, 64, 0)) awOut <- mkFIFOF;

    FIFOF#(AXI4_WFlit#(t_data, 0)) wff <- mkFIFOF;

    FIFOF#(AXI4_BFlit#(t_id, 0)) bIn <- mkFIFOF;
    FIFOF#(AXI4_BFlit#(t_id, 0)) bOut <- mkFIFOF;

    AddressChannelCapUnwrapper#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0)) arIn <- mkSimpleAddressChannelCapUnwrapper;
    FIFOF#(AXI4_ARFlit#(t_id, 64, 0)) arOut <- mkFIFOF;

    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) rIn <- mkFIFOF;
    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) rOut <- mkFIFOF;

    // Epoch checking: every time a capability is revoked, the current epoch changes.
    // All transactions initiated in the previous epoch must finish before the revocation in the key exposer completes.
        // For the purposes of simplicity, "transactions initiated" = transactions where the entire 
    // Revocation is immediate from the perspective of the key store, so other accesses *could* go ahead as part of the next epoch, but that requires tracking here.
    // Right now I just want to do the simplest impl possible, so while waiting for a new epoch other requests can't go through.
    Reg#(Bool) wantEpochIncrease <- mkReg(False);
    Reg#(Epoch) currentEpoch <- mkReg(0);
    Reg#(UInt#(64)) outstandingAccessesInCurrentEpoch <- mkReg(0);
    Reg#(Epoch) nextEpoch <- mkReg(0);
    
    RWire#(Epoch) requestedNextEpoch <- mkRWire;
    
    rule start_epoch_change(!wantEpochIncrease);
        let reqNextEpoch <- get(keyStore.newEpochRequests);
        requestedNextEpoch.wset(reqNextEpoch);
    endrule

    // Track the initiated and completed transactions for each cycle
    PulseWire initiatedWrite <- mkPulseWire;
    PulseWire initiatedRead <- mkPulseWire;
    PulseWire completedWrite <- mkPulseWire;
    PulseWire completedRead <- mkPulseWire;

    rule track_epoch;
        // Get PulseWires from recv_aw, recv_ar, recv_b, recv_r and tally them to determine the change in outstanding accesses.
        // Use that to step the epoch forward if needed.
        
        let initiated = (initiatedRead ? 1 : 0) + (initiatedWrite ? 1 : 0);
        let completed = (completedRead ? 1 : 0) + (completedWrite ? 1 : 0);

        let newOutstandingAccesses = outstandingAccessesInCurrentEpoch + initiated - completed;
        // TODO detect overflow? negative or positive?

        // If we're currently trying to transition between epochs, handle that
        if (wantEpochIncrease) begin
            if (newOutstandingAccesses == 0) begin
                wantEpochIncrease <= False;
                currentEpoch <= nextEpoch;
                keyStore.finishedEpochs.put(currentEpoch);
            end
        end else begin
            // Otherwise start_epoch_change may have pulled a new request to transition,
            // handle that
            case (requestedNextEpoch.wget()) matches 
                tagged Invalid : noAction;
                tagged Valid .requestedNextEpoch : begin
                    // If there are no outstanding accesses, finish immediately
                    // TODO may create a too-long path
                    if (newOutstandingAccesses == 0) begin
                        wantEpochIncrease <= False;
                        currentEpoch <= requestedNextEpoch;
                        keyStore.finishedEpochs.put(currentEpoch);
                    end else begin
                        // If there are outstanding accesses, note that a new epoch is imminent
                        wantEpochIncrease <= True;
                        nextEpoch <= requestedNextEpoch;
                    end
                end
            endcase
        end
        
        outstandingAccessesInCurrentEpoch <= newOutstandingAccesses;
    endrule

    function Bool canInitiateTransaction() = (!wantEpochIncrease);

    // // Once a write transaction has been checked, then and only then can we pass through the write flits for that transaction.
    // // This manifests as a credit system: when a write transaction is valid, increment the credit count, and that many w flits will be passed on.
    // // If a write transaction is *invalid*, those flits need to be dropped instead. In that case, wait for "valid credit" to expire, set wDropCredited <= True and increment the credit count.
    // // Same applies for valid transactions. Wait for "drop credit" to expire, set wDropCredited <= False and increment the credit count.
    // // Blocking all transactions on a switch between send/drop sucks, but should be uncommon as invalid transactions are not expected.
    // // After a few invalid transactions, it would be good to block the sender.
    // Reg#(UInt#(64)) wSendCredits <- mkReg(0);
    // Reg#(Bool) wDropCredited <- mkReg(False);

    FIFOF#(AuthenticatedFlit#(AXI4_AWFlit#(t_id, 64, 0))) awPreCheckBuffer <- mkFIFOF;
    FIFOF#(AuthenticatedFlit#(AXI4_ARFlit#(t_id, 64, 0))) arPreCheckBuffer <- mkFIFOF;

    IOCapAxiChecker#(AXI4_AWFlit#(t_id, 64, 0)) awChecker <- mkSimpleIOCapAxiChecker;
    IOCapAxiChecker#(AXI4_ARFlit#(t_id, 64, 0)) arChecker <- mkSimpleIOCapAxiChecker;

    function KeyId keyIdForFlit(AuthenticatedFlit#(t) authFlit);
        return truncate(authFlit.cap.secret_key_id);
    endfunction

    // There are two possible strategies for epoch counting.
    // 1. Count transactions as "initiated" when they move into the preCheck buffer, as we ask the keyStore to retrieve the relevant key.
    // 2. Count transactions as "initiated" when they move into the checker, *out* of the preCheck buffer, after the keyStore responds with the relevant key.
    //
    // What's the purpose of counting "initiated" transactions?
    // It's to count the transactions that might be authenticated based on data from the current epoch, rather than the new one we're trying to move to.
    // In that case 2. is wrong, because the response from keyStore is *buffered*. A keyStore response from a previous epoch may be buffered
    // past an epoch transition (where there are zero "initiated" transactions), and in that case a new transaction could be "initiated" using stale data from the *old* epoch.

    (* descending_urgency = "recv_aw, recv_ar" *)
    // Conflict with recv_ar because they both request keys
    rule recv_aw(canInitiateTransaction());
        // Put the AW flit into a buffer, and ask to retrieve the key from the keystore
        // Retrieve the key from the keystore
        // TODO could optimize key retrieval by caching the key here - for now be simple and re-request it every time.
        let awFlit <- get(awIn.out);
        awPreCheckBuffer.enq(awFlit);
        keyStore.keyRequests.put(keyIdForFlit(awFlit));
        initiatedWrite.send();
        $display("IOCap - recv_aw ", fshow(awFlit));
    endrule

    rule recv_ar(canInitiateTransaction());
        // Put the AR flit into a buffer, and ask to retrieve the key from the keystore
        // NOTE: this will conflict with recv_aw, because there's only one "key request" port right now.
        // TODO could optimize key retrieval by caching the key here - for now be simple and re-request it every time.
        let arFlit <- get(arIn.out);
        arPreCheckBuffer.enq(arFlit);
        keyStore.keyRequests.put(keyIdForFlit(arFlit));
        initiatedRead.send();
        $display("IOCap - recv_ar ", fshow(arFlit));
    endrule

    // TODO this never triggers in practice, even though the key is retrieved in the keystore correctly. Bug!
    rule recv_key((awPreCheckBuffer.notEmpty && awChecker.checkRequest.canPut) || (arPreCheckBuffer.notEmpty && arChecker.checkRequest.canPut));
        // Retrieve the latest key request, check against the buffered AW and AR flits, and if they're good then send them into their respective checkers.
        let resp <- get(keyStore.keyResponses);
        $display("IOCap - recv_key ", fshow(resp));
        let keyId = tpl_1(resp);
        let key = tpl_2(resp); // May be tagged Invalid if the key is wrong

        // TODO prob need to split the rule up. Don't want to delay AW if AR is empty or vice versa.
        // TODO need to think this through further. What happens if a response arrives from the keyStore with the key for AW, but AWchecker isn't ready? shouldn't deq, but then for every cycle that happens you are holding stale key data.


        if (!awPreCheckBuffer.notEmpty && keyIdForFlit(awPreCheckBuffer.first) == keyId) begin
            awPreCheckBuffer.deq();
            awChecker.checkRequest.put(tuple2(awPreCheckBuffer.first, key));
            $display("IOCap - recv_key awChecker.checkRequest.put ", fshow(awPreCheckBuffer.first));
        end

        if (!arPreCheckBuffer.notEmpty && keyIdForFlit(arPreCheckBuffer.first) == keyId) begin
            arPreCheckBuffer.deq();
            arChecker.checkRequest.put(tuple2(arPreCheckBuffer.first, key));
            $display("IOCap - recv_key arChecker.checkRequest.put ", fshow(arPreCheckBuffer.first));
        end
        // If the key ID matches and is invalid, send the request through with a null key anyway.
        // We could try to cancel the request right now instead of sending it through to the checker, but then cancelling could happen here *and* in check_aw, check_ar - it would get more complicated.
    endrule

    rule check_aw;
        // Pull the AW check result out of the awChecker
        let awResp <- get(awChecker.checkResponse);
        $display("IOCap - check_aw ", fshow(awResp));
        // If valid, pass on and increment send credits (if wDropCredited = True, don't dequeue - wait for wSendCredits == 0 so we can set it to False)
        // If invalid, also pass on and increment send credits
        //      TODO flip the switch to block these transactions and send a failure response, and to not actually dequeue but wait for wSendCredits == 0 so we can set wDropCredited
        case (awResp) matches
            { .flit, .allowed } : begin
                awOut.enq(flit);
                if (allowed)
                    keyStore.bumpPerfCounterGoodWrite();
                else
                    keyStore.bumpPerfCounterBadWrite();
            end
        endcase
    endrule
    
    // rule send_w;
    //     // Pass W flits through using the credit system shown above
    // endrule

    rule check_ar;
        // Pull the AR check result out of the arChecker
        let arResp <- get(arChecker.checkResponse);
        $display("IOCap - check_ar ", fshow(arResp));
        // If valid, pass on
        // If invalid, also pass on TODO flip the switch to block these transactions and send a failure response
        case (arResp) matches
            { .flit, .allowed } : begin
                arOut.enq(flit);
                if (allowed)
                    keyStore.bumpPerfCounterGoodRead();
                else
                    keyStore.bumpPerfCounterBadRead();
            end
        endcase
    endrule

    rule recv_b;
        // Pass the responses from the b channel, TODO interleaved with failure responses from check_aw
        bOut.enq(bIn.first);
        bIn.deq();
        // Each B flit signals the end of a write transaction
        completedWrite.send();
    endrule

    rule recv_r;
        // Pass the responses from the r channel, TODO interleaved with failure responses from check_ar
        rOut.enq(rIn.first);
        rIn.deq();
        // The read is only completed once the last flit in the burst has been sent
        if (rIn.first.rlast) begin
            completedRead.send();
        end
    endrule

    interface iocapsIn = interface IOCapAXI4_Slave;
        interface axiSignals = interface AXI4_Slave;
            interface aw = toSink(awIn.in);
            interface  w = toSink(wff);
            interface  b = toSource(bOut);
            interface ar = toSink(arIn.in);
            interface  r = toSource(rOut);
        endinterface;
    endinterface;

    interface sanitizedOut = interface AXI4_Master;
        interface aw = toSource(awOut);
        interface  w = toSource(wff);
        interface  b = toSink(bIn);
        interface ar = toSource(arOut);
        interface  r = toSink(rIn);
    endinterface;

endmodule

// An IOCapSingleExposer that strips off capability metadata instead of using it
module mkStrippingIOCapExposer(IOCapSingleExposer#(t_id, t_data));
    // This doesn't have any key storage or checking logic yet! It just receives IOCapAXI and converts it back to plain AXI.

    AddressChannelCapUnwrapper#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0)) aw <- mkSimpleAddressChannelCapUnwrapper;
    FIFOF#(AXI4_WFlit#(t_data, 0)) wff <- mkFIFOF;
    FIFOF#(AXI4_BFlit#(t_id, 0)) bff <- mkFIFOF;
    AddressChannelCapUnwrapper#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0)) ar <- mkSimpleAddressChannelCapUnwrapper;
    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) rff <- mkFIFOF;

    function t_flit stripCapFromAuthFlit(AuthenticatedFlit#(t_flit) authFlit) = authFlit.flit;

    interface iocapsIn = interface IOCapAXI4_Slave;
        interface axiSignals = interface AXI4_Slave;
            interface aw = toSink(aw.in);
            interface  w = toSink(wff);
            interface  b = toSource(bff);
            interface ar = toSink(ar.in);
            interface  r = toSource(rff);
        endinterface;
    endinterface;

    interface sanitizedOut = interface AXI4_Master;
        interface aw = mapSource(stripCapFromAuthFlit, aw.out);
        interface  w = toSource(wff);
        interface  b = toSink(bff);
        interface ar = mapSource(stripCapFromAuthFlit, ar.out);
        interface  r = toSink(rff);
    endinterface;
endmodule
import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;

import IOCapAxi_Types :: *;
import IOCapAxi_Flits :: *;
import IOCapAxi_KeyManager2s :: *;
import IOCapAxi_CreditValve :: *;
import IOCapAxi_Checkers :: *;

import Cap2024_11 :: *;
import Cap2024_11_Decode_FastFSM :: *;


// NOT AXI COMPLIAMT
// - doesn't support WRAP bursts
// - doesn't correctly handle ordering for same-ID transaction responses if one of those transactions is correctly authenticated and the other isn't.
// Changes from V1
// - correctly blocks invalid transactions
// Changes from V2
// - uses a pool of checkers
// - has a 50-depth FIFO for w flits
// Changes from V3
// - uses Cap2024_11 format
// - make blocking invalid transactions a parameter to the module
// - increased checker pool size to handle 2-cav iocaps with full throughput
// Changes from V4
// - compatability with KeyManagerV2, which requires...
// - TODO swapping out the checkers with versions that support in-situ invalidation by KeyId
// - TODO swapping out the valves with versions that support per-transaction KeyId tracking
module mkSimpleIOCapExposerV5#(IOCapAxi_KeyManager2_ExposerIfc keyStore, Bool blockInvalid)(IOCapSingleExposer#(t_id, t_data)) provisos (
);
    // Doesn't support WRAP bursts right now

    // AW transactions come in encoding an IOCap with a standard AW flit. The IOCap and flit are examined, and if verified they are passed on through awOut.
    AddressChannelCapUnwrapper#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0), Cap2024_11) awIn <- mkSimpleAddressChannelCapUnwrapper(Proxy{});
    FIFOF#(AXI4_AWFlit#(t_id, 64, 0)) awOut <- mkFIFOF;

    // W flits are passed through or dropped depending on the AW transactions they map to - if the AW transaction is valid, its w flits go through.
    // If the AW transaction is invalid, the w flits are dropped.
    // This is managed by a credit system in wValve.
    FIFOF#(AXI4_WFlit#(t_data, 0)) wIn <- mkSizedFIFOF(50); // TODO figure out the correct size
    CreditValve#(AXI4_WFlit#(t_data, 0), 32) wValve <- mkSimpleCreditValve(toSource(wIn));

    // B responses from the subordinate (de facto for *valid* requests) are sent through to the master, interleaved with responses from invalid requests.
    // This interleaving is currently done without considering order.
    // TODO that's bad! - I need to track the outstanding IDs and make sure that "Transaction responses with the same ID are returned in the same order as the requests were issued."
    // as per the AXI Spec Issue K A6.3
    FIFOF#(AXI4_BFlit#(t_id, 0)) bIn <- mkFIFOF;
    FIFOF#(AXI4_BFlit#(t_id, 0)) invalidBToInsert <- mkFIFOF;
    FIFOF#(AXI4_BFlit#(t_id, 0)) bOut <- mkFIFOF;

    // AR transactions come in encoding an IOCap with a standard AR flit. The IOCap and flit are examined, and if verified they are passed on through arOut.
    AddressChannelCapUnwrapper#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0), Cap2024_11) arIn <- mkSimpleAddressChannelCapUnwrapper(Proxy{});
    FIFOF#(AXI4_ARFlit#(t_id, 64, 0)) arOut <- mkFIFOF;

    // R responses from the subordinate (de facto for *valid* requests) are sent through to the master, interleaved with responses from invalid requests.
    // This interleaving is currently done without considering order.
    // TODO that's bad! - I need to track the outstanding IDs and make sure that "Transaction responses with the same ID are returned in the same order as the requests were issued."
    // as per the AXI Spec Issue K A6.3
    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) rIn <- mkFIFOF;
    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) invalidRToInsert <- mkFIFOF;
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
    // These are all initiated/completed *valid* transactions - ones which were correctly authenticated with an IOcap.
    // TODO rename these pulsewires to reflect that!
    PulseWire initiatedWrite <- mkPulseWire;
    PulseWire initiatedRead <- mkPulseWire;
    PulseWire completedWrite <- mkPulseWire;
    PulseWire completedRead <- mkPulseWire;

    rule track_epoch;
        // Get PulseWires from recv_aw, recv_ar, recv_b, recv_r and tally them to determine the change in outstanding accesses.
        // Use that to step the epoch forward if needed.
        
        let initiated = (initiatedRead ? 1 : 0) + (initiatedWrite ? 1 : 0);
        let completed = (completedRead ? 1 : 0) + (completedWrite ? 1 : 0);

        if (initiatedRead || initiatedWrite || completedRead || completedWrite) begin
            $display("IOCap - track_epoch - outstandingAccesses = ", outstandingAccessesInCurrentEpoch, " initiated = ", initiated, " completed = ", completed, " init r/w ", fshow(initiatedRead), fshow(initiatedWrite), " comp r/w ", fshow(completedRead), fshow(completedWrite));
        end
        let newOutstandingAccesses = outstandingAccessesInCurrentEpoch + initiated - completed;
        // TODO detect overflow? negative or positive?

        // If we're currently trying to transition between epochs, handle that
        if (wantEpochIncrease) begin
            if (newOutstandingAccesses == 0) begin
                wantEpochIncrease <= False;
                currentEpoch <= nextEpoch;
                keyStore.finishedEpochs.put(currentEpoch);
                $display("IOCap - track_epoch - Finishing after newOutstandingAccesses = 0");
            end
        end else begin
            // Otherwise start_epoch_change may have pulled a new request to transition,
            // handle that
            case (requestedNextEpoch.wget()) matches 
                tagged Invalid : noAction;
                tagged Valid .requestedNextEpoch : begin
                    $display("IOCap - track_epoch - wantEpochIncrease=False, requestedNextEpoch=", fshow(requestedNextEpoch));
                    // If there are no outstanding accesses, finish immediately
                    // TODO may create a too-long path
                    if (newOutstandingAccesses == 0) begin
                        wantEpochIncrease <= False;
                        currentEpoch <= requestedNextEpoch;
                        keyStore.finishedEpochs.put(currentEpoch);
                        $display("IOCap - track_epoch - Immediately finishing");
                    end else begin
                        // If there are outstanding accesses, note that a new epoch is imminent
                        wantEpochIncrease <= True;
                        nextEpoch <= requestedNextEpoch;
                        $display("IOCap - track_epoch - Delaying for newOutstandingAccesses = ", fshow(newOutstandingAccesses));
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

    FIFOF#(AuthenticatedFlit#(AXI4_AWFlit#(t_id, 64, 0), Cap2024_11)) awPreCheckBuffer <- mkFIFOF;
    FIFOF#(AuthenticatedFlit#(AXI4_ARFlit#(t_id, 64, 0), Cap2024_11)) arPreCheckBuffer <- mkFIFOF;

    // Each AW and AR AuthenticatedFlit takes 4 cycles to receive
    // => we need the checker pool on each of the {AW, AR} ports to be able to receive a new request every 4 cycles
    // Latencies for 0, 1, 2 caveat checking are ~9, ~15, ~21 cycles respectively
    // In worst case if constantly receiving requests with ~21 cycle latency every 4 cycles, need ceil(~21/4) = 6 checkers per pool
    // => in total, 12 checker units
    // 2 AES rounds per cycle => in total, 12*2 = 24 AES round evaluators (which are the big parts)
    // if you had a naive fully pipelined impl you'd have 30 round evaluators per port or 60 overall, and each would have 1/4 occupancy (or 30 with 1/2 occupancy)
    // we use 2/5 of that :)
    // but interestingly, we prob have too much decoder hardware.
    // decoding is much shorter than sigcheck, so a fully pipelined ver would have at most 8 sets of arithmetic, so either (1/port = 16 total with 1/4 occupancy)
    // or (1 shared = 8 total with 1/2 occupancy) vs the 12 we use.
    NumProxy#(6) poolSize = ?;
    // TODO test throughput of these vs non-pooled
    IOCapAxiChecker#(AXI4_AWFlit#(t_id, 64, 0), Cap2024_11) awChecker <- mkInOrderIOCapAxiCheckerPool(poolSize, mkSimpleIOCapAxiChecker(connectFastFSMCapDecode_2024_11));
    // TODO could do out-of-order for ar
    IOCapAxiChecker#(AXI4_ARFlit#(t_id, 64, 0), Cap2024_11) arChecker <- mkInOrderIOCapAxiCheckerPool(poolSize, mkSimpleIOCapAxiChecker(connectFastFSMCapDecode_2024_11));

    function KeyId keyIdForFlit(AuthenticatedFlit#(t, Cap2024_11) authFlit);
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

    // After requesting a key, it will eventually arrive at the keyStore.keyResponses Source.
    // There is exactly one keyStore.keyResponses item for each AW and AR request, 
    // but if an AW and AR request use the same keyId there's no reason not to use a single response for both.
    // TODO reason about how that works with epochs?
    // If we use a single response for two transactions (a single response is *split* across AW and AR), the second response needs to be discarded.
    // The situation can be modelled with three queues: the AW request queue, the AR request queue, and the key request-response queue.
    // Each of these queues are ordered.
    // AW requests are enqueued into the AW request queue in the same order as their key requests are enqueued into the key queue.
    // Ditto for AR requests.
    // Key responses arrive in the same order as key requests.
    // Thus the key queue is an *interleaving* of the AW and AR request queues *with the relative order between AR and between AW requests maintained*.
    // This means if we *don't* have split key responses, every key response received will either be for the head of the AR queue or the head of the AW queue.
    // If we *do* have split key responses, every key response received will either be for the head of the AR or AW queue *or for a transaction that has been popped off either queue*.
    // Thus we can tell if a key request should be discarded if its key ID does *not* match the key ID for the AR queue head or AW queue head - it must be for a transaction that has been popped off recently, it can't be for a request that's *farther behind in the queue*.
    // However, if a key *does* match but *can't* be used - i.e. if it matches the head of the AW request, but the AW checker is busy - then we should still block.
    // Thus, we always peek the key. If it is used to start checking an AW or AR queue head transaction, dequeue it.
    // If it isn't, but it *does* match either the AW or AR queue head transaction, keep it in the queue - it will be relevant once those checkers become unblocked.
    // If it doesn't match the AW or AR queue head transactions, dequeue it - it must be the remnant of a split response.
    Wire#(Tuple2#(KeyId, Maybe#(Key))) peekedKey <- mkWire;
    PulseWire keyMatchedAw <- mkPulseWire;
    PulseWire usedPeekedKeyForAw <- mkPulseWire;

    PulseWire keyMatchedAr <- mkPulseWire;
    PulseWire usedPeekedKeyForAr <- mkPulseWire;

    rule peek_key(keyStore.keyResponses.canPeek);
        // Retrieve the latest key request, check against the buffered AW and AR flits, and if they're good then send them into their respective checkers.
        let resp = keyStore.keyResponses.peek;
        $display("IOCap - peek_key ", fshow(resp));
        peekedKey <= resp;
    endrule

    rule start_aw_with_key(awPreCheckBuffer.notEmpty);
        // Important - aggressive conditions required to split canPut from cantPut
        if (awChecker.checkRequest.canPut) begin
            // We can start a new check!
            let keyId = tpl_1(peekedKey);
            let key = tpl_2(peekedKey); // May be tagged Invalid if the key is Invalid

            if (keyIdForFlit(awPreCheckBuffer.first) == keyId) begin
                awPreCheckBuffer.deq();
                awChecker.checkRequest.put(tuple3(awPreCheckBuffer.first, keyId, key));
                keyMatchedAw.send();
                usedPeekedKeyForAw.send();
                $display("IOCap - start_aw_with_key awChecker.checkRequest.put ", fshow(awPreCheckBuffer.first));
            end
        end else begin
            // We can't start a new check - still see if the key matches the head of the AW queue, because in that case we may need to stop it from dropping
            let keyId = tpl_1(peekedKey);
            let key = tpl_2(peekedKey); // May be tagged Invalid if the key is Invalid

            if (keyIdForFlit(awPreCheckBuffer.first) == keyId) begin
                keyMatchedAw.send();
                $display("IOCap - start_aw_with_key blocked ", fshow(awPreCheckBuffer.first));
            end
        end
    endrule

    rule start_ar_with_key;
        // Important - aggressive conditions required to split canPut from cantPut
        if (arChecker.checkRequest.canPut) begin
            // We can start a new check!
            let keyId = tpl_1(peekedKey);
            let key = tpl_2(peekedKey); // May be tagged Invalid if the key is Invalid

            if (keyIdForFlit(arPreCheckBuffer.first) == keyId) begin
                arPreCheckBuffer.deq();
                arChecker.checkRequest.put(tuple3(arPreCheckBuffer.first, keyId, key));
                keyMatchedAr.send();
                usedPeekedKeyForAr.send();
                $display("IOCap - start_ar_with_key arChecker.checkRequest.put ", fshow(arPreCheckBuffer.first));
            end
        end else begin
            // We can't start a new check - still see if the key matches the head of the AR queue, because in that case we may need to stop it from dropping
            let keyId = tpl_1(peekedKey);
            let key = tpl_2(peekedKey); // May be tagged Invalid if the key is Invalid

            if (keyIdForFlit(arPreCheckBuffer.first) == keyId) begin
                keyMatchedAr.send();
                $display("IOCap - start_ar_with_key blocked ", fshow(arPreCheckBuffer.first));
            end
        end
    endrule

    rule deq_peeked_key(keyStore.keyResponses.canPeek);
        if ((usedPeekedKeyForAw || usedPeekedKeyForAr)) begin
            keyStore.keyResponses.drop();
            $display("IOCap - deq_peeked_key dequeued ", fshow(peekedKey));
        end else if (!keyMatchedAr && !keyMatchedAw) begin
            keyStore.keyResponses.drop();
            $display("IOCap - deq_peeked_key dequeued ", fshow(peekedKey));
        end else begin
            $display("IOCap - deq_peeked_key wasn't dequeued ", fshow(peekedKey));
        end
    endrule

    rule check_aw if (awChecker.checkResponse.canPeek && (
        // If !blockInvalid, we will always be in Pass mode.
        (tpl_3(awChecker.checkResponse.peek) == True && wValve.canUpdateCredits(Pass)) || (tpl_3(awChecker.checkResponse.peek) == False && wValve.canUpdateCredits(Drop)) || !blockInvalid
    ));
        // Pull the AW check result out of the awChecker
        let awResp <- get(awChecker.checkResponse);
        $display("IOCap - check_aw ", fshow(awResp));
        // If valid, pass on and increment send credits (if wDropCredited = True, don't dequeue - wait for wSendCredits == 0 so we can set it to False)
        // If invalid, drop the AW flit and increment drop credits
        
        case (awResp) matches
            { .flit, .keyId, .allowed } : begin
                Bit#(8) awlen = flit.awlen;
                Bit#(9) nCredits = zeroExtend(awlen) + 1;
                if (allowed) begin
                    keyStore.bumpPerfCounterGoodWrite();
                    // Pass through the valid write
                    awOut.enq(flit);
                    // Tell the W valve to let through the right number of flits
                    wValve.updateCredits(Pass, extend(unpack(nCredits)));
                end else begin
                    keyStore.bumpPerfCounterBadWrite();
                    if (blockInvalid) begin
                        // Drop the AW flit, insert an invalid-write response
                        invalidBToInsert.enq(AXI4_BFlit {
                            bid: flit.awid,
                            bresp: SLVERR,
                            buser: ?
                        });
                        // Tell the W valve to drop the right number of flits
                        wValve.updateCredits(Drop, extend(unpack(nCredits)));
                    end else begin
                        // Pass through the invalid write
                        awOut.enq(flit);
                        // Tell the W valve to let through the right number of flits
                        wValve.updateCredits(Pass, extend(unpack(nCredits)));
                    end
                end
            end
        endcase
    endrule

    rule check_ar;
        // Pull the AR check result out of the arChecker
        let arResp <- get(arChecker.checkResponse);
        $display("IOCap - check_ar ", fshow(arResp));
        // If valid, pass on
        // If invalid, send a failure response
        case (arResp) matches
            { .flit, .keyId, .allowed } : begin
                if (allowed) begin
                    keyStore.bumpPerfCounterGoodRead();
                    // Pass through the valid AR flit
                    arOut.enq(flit);
                end else begin
                    keyStore.bumpPerfCounterBadRead();
                    if (blockInvalid) begin
                        // Drop the AR flit, insert an invalid-read response
                        invalidRToInsert.enq(AXI4_RFlit {
                            rid: flit.arid,
                            rresp: SLVERR,
                            ruser: ?,
                            rdata: ?,
                            rlast: True
                        });
                    end else begin
                        // Pass through the invalid AR flit
                        arOut.enq(flit);
                    end
                end
            end
        endcase
    endrule

    // If there isn't an invalid-b-flit to insert, just pass through valid completions from bIn to bOut
    rule passthru_b if (!invalidBToInsert.notEmpty);
        // Pass the responses from the b channel
        bOut.enq(bIn.first);
        bIn.deq();
        // Each B flit signals the end of a write transaction we received an AW for - valid or not
        completedWrite.send();
    endrule

    rule insert_invalid_b if (invalidBToInsert.notEmpty);
        // Insert the b into the stream
        bOut.enq(invalidBToInsert.first);
        invalidBToInsert.deq();
        completedWrite.send();
    endrule

    // If there isn't an invalid-r-flit to insert, just pass through valid completions from rIn to rOut
    rule passthru_r if (!invalidRToInsert.notEmpty);
        // Pass the responses from the r channel
        rOut.enq(rIn.first);
        rIn.deq();
        // Each R flit signals the end of a read transaction we received an AR for - valid or not
        // The read is only completed once the last flit in the burst has been sent
        if (rIn.first.rlast) begin
            completedRead.send();
        end
    endrule

    rule insert_invalid_r if (invalidRToInsert.notEmpty);
        // Insert the r into the stream
        rOut.enq(invalidRToInsert.first);
        invalidRToInsert.deq();
        completedRead.send();
    endrule

    interface iocapsIn = interface IOCapAXI4_Slave;
        interface axiSignals = interface AXI4_Slave;
            interface aw = toSink(awIn.in);
            interface  w = toSink(wIn);
            interface  b = toSource(bOut);
            interface ar = toSink(arIn.in);
            interface  r = toSource(rOut);
        endinterface;
    endinterface;

    interface sanitizedOut = interface AXI4_Master;
        interface aw = toSource(awOut);
        interface  w = toSource(wValve.out);
        interface  b = toSink(bIn);
        interface ar = toSource(arOut);
        interface  r = toSink(rIn);
    endinterface;

endmodule
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
// TODO the structs from these two should be exported by KeyManager2s
import IOCapAxi_KeyManager2_MMIO :: *;
import IOCapAxi_KeyManager2_RefCountPipe :: *;
import IOCapAxi_CreditValve :: *;
import IOCapAxi_Checker2s :: *;

import Cap2024_11 :: *;
import Cap2024_11_Decode_FastFSM :: *;

// The Scoreboard tracks which TxnIds for outstanding valid transactions are associated with which KeyIds for the purposes of reference counting.
// It also tracks which TxnIds currently have outstanding valid transactions, which means it can help avoid out-of-order completions for a TxnId
// when there's an invalid transaction while there's a currently outstanding valid transaction - the completion for the invalid transaction
// should fire after the completion for the valid one.
//
// 
interface TxnKeyIdScoreboard#(numeric type t_id);
    method Bool canBeginTxn(Bit#(t_id) txnId);
    method Action beginTxn(Bit#(t_id) txnId, KeyId keyId, Bool isValid);
    method Action completeValidTxn(Bit#(t_id) txnId);
    interface Source#(KeyId) completedValidTxnKeys;
    interface Source#(Bit#(t_id)) invalidTxnsToComplete;
endinterface


// This impl of the scoreboard assumes a small t_id, codify this as <8
// This impl of the scoreboard uses a register with one bit per txnid to see if that txnid is outstanding,
// and doesn't allow you to begin new txns with the same id as a currently outstanding one.
// This coarsely adheres to the AXI spec - transactions with the same ID are ordered relative to each other - but 
// doesn't adhere to the intent, that trasnactions with the same ID can be pipelined together.
module mkSimpleTxnKeyIdScoreboard(TxnKeyIdScoreboard#(t_id)) provisos (Add#(8, t_id, __a));
    // Reg#(Bool) hasClearedBram <- mkReg(False);
    // Reg#(Bit#(t_id)) nextTxnIdToClear <- mkReg(0);
    BRAM_Configure bramConfig = BRAM_Configure {
        memorySize: 0, // Number of words is inferred from the KeyId parameter to BRAM2Port below.
        // Size of each word is determined by the other parameter to BRAM2Port below.
        latency: 2, // (address is registered, data is too because this isn't latency sensitive)
        loadFormat: None,
        outFIFODepth: 4, // latency+2
        allowWriteResponseBypass: False // TODO check if this is fine
    };
    // Single bank
    // Addressed by Bit#(t_id)
    // Holds items of type KeyId
    // 2 ports - one read, one write
    BRAM2Port#(Bit#(t_id), KeyId) bram <- mkBRAM2Server(bramConfig);

    // rule clear_bram(!hasClearedBram);
    //     KeyId newNextTxnIdToClear = nextTxnIdToClear + 2;
    //     if (newNextTxnIdToClear == 0) // has overflowed
    //         hasClearedBram <= True;
    //     nextTxnIdToClear <= newNextTxnIdToClear;

    //     keyRefcountBram.portA.request.put(BRAMRequest {
    //         write: True,
    //         responseOnWrite: False,
    //         address: nextTxnIdToClear + 0,
    //         datain: tagged Invalid
    //     });
    //     keyRefcountBram.portB.request.put(BRAMRequest {
    //         write: True,
    //         responseOnWrite: False,
    //         address: nextTxnIdToClear + 1,
    //         datain: tagged Invalid
    //     });
    // endrule

    // Bit for every transaction id indicating if it's currently in progress
    // Because this is here, we don't need to zero out the bram
    Reg#(Bit#(TExp#(t_id))) txnsInProgress <- mkReg(0);

    FIFOF#(KeyId) completedValidTxnKeysImpl <- mkFIFOF;
    FIFOF#(Bit#(t_id)) invalidTxnsToCompleteImpl <- mkFIFOF;

    RWire#(Tuple2#(Bit#(t_id), KeyId)) toWriteToBram <- mkRWire;
    RWire#(Bit#(t_id)) toComplete <- mkRWire;

    rule handle_txnsInProgress;
        let newTxnsInProgress = txnsInProgress;
        case (tuple2(toWriteToBram.wget(), toComplete.wget())) matches
            { tagged Invalid,                  tagged Invalid } : noAction;
            { tagged Valid { .txnId, .keyId }, tagged Invalid } : begin
                newTxnsInProgress[txnId] = 1;
            end
            { tagged Invalid, tagged Valid .txnId } : begin
                newTxnsInProgress[txnId] = 0;
            end
            { tagged Valid { .txnIdEnq, .keyId }, tagged Valid .txnIdDeq } : begin
                if (txnIdEnq == txnIdDeq) begin
                    // TODO assert error
                end else begin
                    newTxnsInProgress[txnIdEnq] = 1;
                    newTxnsInProgress[txnIdDeq] = 0;
                end
            end
        endcase
        txnsInProgress <= newTxnsInProgress;
    endrule

    rule enq_new_transaction;
        case (toWriteToBram.wget()) matches
            tagged Invalid : noAction;
            tagged Valid { .txnId, .keyId } : begin
                bram.portA.request.put(BRAMRequest {
                    write: True,
                    responseOnWrite: False,
                    address: txnId,
                    datain: keyId
                });
            end
        endcase
    endrule

    rule start_complete_transaction;
        case (toComplete.wget()) matches
            tagged Invalid : noAction;
            tagged Valid .txnId : begin
                bram.portB.request.put(BRAMRequest {
                    write: False,
                    responseOnWrite: False,
                    address: txnId,
                    datain: ?
                });
            end
        endcase
    endrule

    rule get_completed_keyid;
        let keyId <- bram.portB.response.get();
        completedValidTxnKeysImpl.enq(keyId);
    endrule
    
    method Bool canBeginTxn(Bit#(t_id) txnId) = txnsInProgress[txnId] == 0;
    // Use the implicit condition to force the caller to block if two trnasactions with the same id exist
    method Action beginTxn(Bit#(t_id) txnId, KeyId keyId, Bool isValid);
        if (txnsInProgress[txnId] == 1) begin
            // TODO some sort of error
            $display("beginTxn called when can't begin");
        end
        if (isValid) begin
            toWriteToBram.wset(tuple2(txnId, keyId));
        end else begin
            invalidTxnsToCompleteImpl.enq(txnId);
        end
    endmethod
    method Action completeValidTxn(Bit#(t_id) txnId); // if (txnsInProgress[txnId] == 1)
        if (txnsInProgress[txnId] == 0) begin
            // TODO some sort of error
            $display("completeValidTxn called when wasn't begun");
        end
        toComplete.wset(txnId);
    endmethod
    interface completedValidTxnKeys = toSource(completedValidTxnKeysImpl);
    interface invalidTxnsToComplete = toSource(invalidTxnsToCompleteImpl);
endmodule

// NOT AXI COMPLIAMT
// - doesn't support WRAP bursts
// - doesn't correctly handle ordering for same-ID transaction responses if one of those transactions is correctly authenticated and the other isn't.
//      TODO Samuel's guess about this is that if the second transaction is bad, it might send a response first.
//      TODO this may be fixed? Need to write a test
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
// - TODO (maybe done?) swapping out the checkers with versions that support in-situ invalidation by KeyId
// - TODO (maybe done?) Support per-transaction KeyId tracking
// - for now, swapping back to a single checker per lane
module mkSimpleIOCapExposerV5#(IOCapAxi_KeyManager2_ExposerIfc keyStore, Bool blockInvalid)(IOCapSingleExposer#(t_id, t_data)) provisos (
);
    // IOCapAxiChecker2 Doesn't support WRAP bursts right now

    function KeyId keyIdForFlit(Cap2024_11 cap);
        return truncate(cap.secret_key_id);
    endfunction

    // Simple arbitration between AW and AR, prioritising AW
    RWire#(KeyId) awKeyRequest <- mkRWire;
    RWire#(KeyId) arKeyRequest <- mkRWire;
    let awKeyReqIfc = interface Sink;
        method Bool canPut = keyStore.checker.keyRequest.canPut;
        method Action put(keyId) = awKeyRequest.wset(keyId);
    endinterface;
    let arKeyReqIfc = interface Sink;
        method Bool canPut = keyStore.checker.keyRequest.canPut() && !isValid(awKeyRequest.wget());
        method Action put(keyId) = arKeyRequest.wset(keyId);
    endinterface;

    rule sendKeyReq;
        case (tuple2(awKeyRequest.wget(), arKeyRequest.wget())) matches
            { tagged Invalid, tagged Invalid } : noAction;
            { tagged Valid .awKeyId, .* } : keyStore.checker.keyRequest.put(awKeyId);    
            { .*, tagged Valid .arKeyId } : keyStore.checker.keyRequest.put(arKeyId);  
            default : $display("SOMEHOW SET AW AND AR REQUEST AT THE SAME TIME");
        endcase  
    endrule

    let keyResponse = interface ReadOnly;
        method Maybe#(Tuple2#(KeyId, Maybe#(Key))) _read();
            return (keyStore.checker.keyResponse.canPeek() ? 
                tagged Valid (keyStore.checker.keyResponse.peek()) : tagged Invalid); 
        endmethod
    endinterface;

    rule pump_keyResponse(keyStore.checker.keyResponse.canPeek());
        $display("dropping from keyResponse");
        keyStore.checker.keyResponse.drop();
    endrule

    NumProxy#(2) perAddrChannelPoolSize = ?;

    // AW transactions come in encoding an IOCap with a standard AW flit. The IOCap and flit are examined, and if verified they are passed on through awOut.
    // AddressChannelCapUnwrapper#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0), Cap2024_11) awIn <- mkSimpleAddressChannelCapUnwrapper(Proxy{});
    // IOCapAxiChecker2#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0)) awIn <- mkSimpleIOCapAxiChecker2(
    //     connectFastFSMCapDecode_2024_11,
    //     awKeyReqIfc,
    //     keyResponse,
    //     keyStore.checker.killKeyMessage,
    //     keyIdForFlit
    // );
    IOCapAxiChecker2#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0)) awIn <- mkInOrderIOCapAxiChecker2V1Pool(
        perAddrChannelPoolSize,
        connectFastFSMCapDecode_2024_11,
        awKeyReqIfc,
        keyResponse,
        keyStore.checker.killKeyMessage,
        keyIdForFlit
    );
    FIFOF#(AXI4_AWFlit#(t_id, 64, 0)) awOut <- mkFIFOF;

    // W flits are passed through or dropped depending on the AW transactions they map to - if the AW transaction is valid, its w flits go through.
    // If the AW transaction is invalid, the w flits are dropped.
    // This is managed by a credit system in wValve.
    FIFOF#(AXI4_WFlit#(t_data, 0)) wIn <- mkSizedFIFOF(50); // TODO figure out the correct size
    CreditValve#(AXI4_WFlit#(t_data, 0), 32) wValve <- mkSimpleCreditValve(toSource(wIn));
    TxnKeyIdScoreboard#(t_id) wScoreboard <- mkSimpleTxnKeyIdScoreboard;

    // B responses from the subordinate (de facto for *valid* requests) are sent through to the master, interleaved with responses from invalid requests.
    // These invalid responses are taken from the wScoreboard, and are prioritized over any pending responses from valid requests to ensure ordering.
    FIFOF#(AXI4_BFlit#(t_id, 0)) bIn <- mkFIFOF;
    FIFOF#(AXI4_BFlit#(t_id, 0)) bOut <- mkFIFOF;

    // AR transactions come in encoding an IOCap with a standard AR flit. The IOCap and flit are examined, and if verified they are passed on through arOut.
    // AddressChannelCapUnwrapper#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0), Cap2024_11) arIn <- mkSimpleAddressChannelCapUnwrapper(Proxy{});
    // IOCapAxiChecker2#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0)) arIn <- mkSimpleIOCapAxiChecker2(
    //     connectFastFSMCapDecode_2024_11,
    //     arKeyReqIfc,
    //     keyResponse,
    //     keyStore.checker.killKeyMessage,
    //     keyIdForFlit
    // );
    IOCapAxiChecker2#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0)) arIn <- mkInOrderIOCapAxiChecker2V1Pool(
        perAddrChannelPoolSize,
        connectFastFSMCapDecode_2024_11,
        arKeyReqIfc,
        keyResponse,
        keyStore.checker.killKeyMessage,
        keyIdForFlit
    );
    FIFOF#(AXI4_ARFlit#(t_id, 64, 0)) arOut <- mkFIFOF;

    // R responses from the subordinate (de facto for *valid* requests) are sent through to the master, interleaved with responses from invalid requests.
    // These invalid responses are taken from the rScoreboard, and are prioritized over any pending responses from valid requests to ensure ordering.
    TxnKeyIdScoreboard#(t_id) rScoreboard <- mkSimpleTxnKeyIdScoreboard;
    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) rIn <- mkFIFOF;
    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) rOut <- mkFIFOF;

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
            $display("IOCap - track_epoch - initiated = ", initiated, " completed = ", completed, " init r/w ", fshow(initiatedRead), fshow(initiatedWrite), " comp r/w ", fshow(completedRead), fshow(completedWrite));
        end
    endrule

    function Bool canInitiateTransaction() = True;

    /*

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
    IOCapAxiChecker#(AXI4_AWFlit#(t_id, 64, 0), Cap2024_11) awIn <- mkInOrderIOCapAxiCheckerPool(poolSize, mkSimpleIOCapAxiChecker(connectFastFSMCapDecode_2024_11));
    // TODO could do out-of-order for ar
    IOCapAxiChecker#(AXI4_ARFlit#(t_id, 64, 0), Cap2024_11) arIn <- mkInOrderIOCapAxiCheckerPool(poolSize, mkSimpleIOCapAxiChecker(connectFastFSMCapDecode_2024_11));

    */

    // There are two possible strategies for epoch counting.
    // 1. Count transactions as "initiated" when they move into the preCheck buffer, as we ask the keyStore to retrieve the relevant key.
    // 2. Count transactions as "initiated" when they move into the checker, *out* of the preCheck buffer, after the keyStore responds with the relevant key.
    //
    // What's the purpose of counting "initiated" transactions?
    // It's to count the transactions that might be authenticated based on data from the current epoch, rather than the new one we're trying to move to.
    // In that case 2. is wrong, because the response from keyStore is *buffered*. A keyStore response from a previous epoch may be buffered
    // past an epoch transition (where there are zero "initiated" transactions), and in that case a new transaction could be "initiated" using stale data from the *old* epoch.

    /*
    (* descending_urgency = "recv_aw, recv_ar" *)
    // Conflict with recv_ar because they both request keys
    rule recv_aw(canInitiateTransaction());
        // Put the AW flit into a buffer, and ask to retrieve the key from the keystore
        // Retrieve the key from the keystore
        // TODO could optimize key retrieval by caching the key here - for now be simple and re-request it every time.
        let awFlit <- get(awIn.out);
        awPreCheckBuffer.enq(awFlit);
        keyStore.checker.keyRequest.put(keyIdForFlit(awFlit));
        initiatedWrite.send();
        $display("IOCap - recv_aw ", fshow(awFlit));
    endrule

    rule recv_ar(canInitiateTransaction());
        // Put the AR flit into a buffer, and ask to retrieve the key from the keystore
        // NOTE: this will conflict with recv_aw, because there's only one "key request" port right now.
        // TODO could optimize key retrieval by caching the key here - for now be simple and re-request it every time.
        let arFlit <- get(arIn.out);
        arPreCheckBuffer.enq(arFlit);
        keyStore.checker.keyRequest.put(keyIdForFlit(arFlit));
        initiatedRead.send();
        $display("IOCap - recv_ar ", fshow(arFlit));
    endrule
    */

    /*
    // After requesting a key, it will eventually arrive at the keyStore.checker.keyResponse Source.
    // There is exactly one keyStore.checker.keyResponse item for each AW and AR request, 
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

    rule peek_key(keyStore.checker.keyResponse.canPeek);
        // Retrieve the latest key request, check against the buffered AW and AR flits, and if they're good then send them into their respective checkers.
        let resp = keyStore.checker.keyResponse.peek;
        $display("IOCap - peek_key ", fshow(resp));
        peekedKey <= resp;
    endrule

    rule start_aw_with_key(awPreCheckBuffer.notEmpty);
        // Important - aggressive conditions required to split canPut from cantPut
        if (awIn.checkRequest.canPut) begin
            // We can start a new check!
            let keyId = tpl_1(peekedKey);
            let key = tpl_2(peekedKey); // May be tagged Invalid if the key is Invalid

            if (keyIdForFlit(awPreCheckBuffer.first) == keyId) begin
                awPreCheckBuffer.deq();
                awIn.checkRequest.put(tuple3(awPreCheckBuffer.first, keyId, key));
                keyMatchedAw.send();
                usedPeekedKeyForAw.send();
                $display("IOCap - start_aw_with_key awIn.checkRequest.put ", fshow(awPreCheckBuffer.first));
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
        if (arIn.checkRequest.canPut) begin
            // We can start a new check!
            let keyId = tpl_1(peekedKey);
            let key = tpl_2(peekedKey); // May be tagged Invalid if the key is Invalid

            if (keyIdForFlit(arPreCheckBuffer.first) == keyId) begin
                arPreCheckBuffer.deq();
                arIn.checkRequest.put(tuple3(arPreCheckBuffer.first, keyId, key));
                keyMatchedAr.send();
                usedPeekedKeyForAr.send();
                $display("IOCap - start_ar_with_key arIn.checkRequest.put ", fshow(arPreCheckBuffer.first));
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

    rule deq_peeked_key(keyStore.checker.keyResponse.canPeek);
        if ((usedPeekedKeyForAw || usedPeekedKeyForAr)) begin
            keyStore.checker.keyResponse.drop();
            $display("IOCap - deq_peeked_key dequeued ", fshow(peekedKey));
        end else if (!keyMatchedAr && !keyMatchedAw) begin
            keyStore.checker.keyResponse.drop();
            $display("IOCap - deq_peeked_key dequeued ", fshow(peekedKey));
        end else begin
            $display("IOCap - deq_peeked_key wasn't dequeued ", fshow(peekedKey));
        end
    endrule
    */

    rule check_aw if (awIn.checkResponse.canPeek && (
        // If !blockInvalid, we will always be in Pass mode.
        ((tpl_3(awIn.checkResponse.peek) == True && wValve.canUpdateCredits(Pass)) || (tpl_3(awIn.checkResponse.peek) == False && wValve.canUpdateCredits(Drop)) || !blockInvalid)
        && wScoreboard.canBeginTxn(tpl_1(awIn.checkResponse.peek).awid)
    ));
        // Pull the AW check result out of the awIn
        let awResp = awIn.checkResponse.peek();
        awIn.checkResponse.drop();
        $display("IOCap - check_aw ", fshow(awResp));
        // If valid, pass on and increment send credits (if wDropCredited = True, don't dequeue - wait for wSendCredits == 0 so we can set it to False)
        // If invalid, drop the AW flit and increment drop credits
        
        case (awResp) matches
            { .flit, .keyId, .allowed } : begin
                Bit#(8) awlen = flit.awlen;
                Bit#(9) nCredits = zeroExtend(awlen) + 1;
                wScoreboard.beginTxn(flit.awid, keyId, allowed);
                if (allowed) begin
                    keyStore.wValve.perf.bumpPerfCounterGood();
                    // Pass through the valid write
                    awOut.enq(flit);
                    // Tell the W valve to let through the right number of flits
                    wValve.updateCredits(Pass, extend(unpack(nCredits)));
                    // Tell the key manager that we're using a keyId
                    keyStore.wValve.refcount.keyIncrementRefcountRequest.put(keyId);
                end else begin
                    keyStore.wValve.perf.bumpPerfCounterBad();
                    if (blockInvalid) begin
                        // We will send the invalid-write-response once it passes through the scoreboard
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

    rule print_ar;
        if (arIn.checkResponse.canPeek()) begin
            $display("AR ", fshow(arIn.checkResponse.peek()), " canBegin ", fshow(rScoreboard.canBeginTxn(tpl_1(arIn.checkResponse.peek).arid)));
        end
    endrule

    rule check_ar (rScoreboard.canBeginTxn(tpl_1(arIn.checkResponse.peek).arid));
        // Pull the AR check result out of the arIn
        let arResp = arIn.checkResponse.peek();
        arIn.checkResponse.drop();
        $display("IOCap - check_ar ", fshow(arResp));
        // If valid, pass on
        // If invalid, send a failure response
        case (arResp) matches
            { .flit, .keyId, .allowed } : begin
                rScoreboard.beginTxn(flit.arid, keyId, allowed);
                $display("ALLOWED ", fshow(allowed));
                if (allowed) begin
                    keyStore.rValve.perf.bumpPerfCounterGood();
                    $display("put out flit", fshow(flit));
                    // Pass through the valid AR flit
                    arOut.enq(flit);
                    $display("incref ", fshow(keyId));
                    keyStore.rValve.refcount.keyIncrementRefcountRequest.put(keyId);
                end else begin
                    keyStore.rValve.perf.bumpPerfCounterBad();
                    if (blockInvalid) begin
                        // We will send the invalid-read-response once it passes through the scoreboard
                    end else begin
                        // Pass through the invalid AR flit
                        arOut.enq(flit);
                    end
                end
            end
        endcase
    endrule

    // If there isn't an invalid-b-flit to insert, just pass through valid completions from bIn to bOut
    rule passthru_b if (!wScoreboard.invalidTxnsToComplete.canPeek());
        // Pass the responses from the b channel
        bOut.enq(bIn.first);
        bIn.deq();
        // Each B flit signals the end of a write transaction we received an AW for - valid or not
        completedWrite.send();
        // Figure out what key that was so we can tell the Valve
        wScoreboard.completeValidTxn(bIn.first.bid);
    endrule

    rule inform_wValve_keyid_completed;
        wScoreboard.completedValidTxnKeys.drop();
        keyStore.wValve.refcount.keyDecrementRefcountRequest.put(wScoreboard.completedValidTxnKeys.peek());
    endrule

    rule insert_invalid_b if (wScoreboard.invalidTxnsToComplete.canPeek());
        // Insert the b into the stream
        bOut.enq(AXI4_BFlit {
            bid: wScoreboard.invalidTxnsToComplete.peek(),
            bresp: SLVERR,
            buser: ?
        });
        wScoreboard.invalidTxnsToComplete.drop();
        completedWrite.send();
    endrule

    // If there isn't an invalid-r-flit to insert, just pass through valid completions from rIn to rOut
    rule passthru_r if (!rScoreboard.invalidTxnsToComplete.canPeek());
        // Pass the responses from the r channel
        rOut.enq(rIn.first);
        rIn.deq();
        // Each R flit signals the end of a read transaction we received an AR for - valid or not
        // The read is only completed once the last flit in the burst has been sent
        if (rIn.first.rlast) begin
            completedRead.send();
            // Figure out what key that was so we can tell the Valve
            rScoreboard.completeValidTxn(rIn.first.rid);
        end
    endrule

    rule inform_rValve_keyid_completed;
        rScoreboard.completedValidTxnKeys.drop();
        keyStore.rValve.refcount.keyDecrementRefcountRequest.put(rScoreboard.completedValidTxnKeys.peek());
    endrule

    rule insert_invalid_r if (rScoreboard.invalidTxnsToComplete.canPeek());
        // Insert the r into the stream
        rOut.enq(AXI4_RFlit {
            rid: rScoreboard.invalidTxnsToComplete.peek(),
            rresp: SLVERR,
            ruser: ?,
            rdata: ?,
            rlast: True
        });
        rScoreboard.invalidTxnsToComplete.drop();
        completedRead.send();
    endrule

    interface iocapsIn = interface IOCapAXI4_Slave;
        interface axiSignals = interface AXI4_Slave;
            interface aw = awIn.in;
            interface  w = toSink(wIn);
            interface  b = toSource(bOut);
            interface ar = arIn.in;
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
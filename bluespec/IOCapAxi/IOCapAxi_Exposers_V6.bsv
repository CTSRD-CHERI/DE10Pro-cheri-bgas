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
import IOCapAxi_Checker3s :: *;
import IOCapAxi_Konata :: *;

import Cap2024_11 :: *;
import Cap2024_11_Decode_FastFSM :: *;

export mkSimpleIOCapExposerV6;

// The Scoreboard tracks which TxnIds for outstanding valid transactions are associated with which KeyIds (generalized to t_meta) for the purposes of reference counting.
// It also tracks which TxnIds currently have outstanding valid transactions, which means it can help avoid out-of-order completions for a TxnId
// when there's an invalid transaction while there's a currently outstanding valid transaction - the completion for the invalid transaction
// should fire after the completion for the valid one.
//
// 
interface TxnKeyIdScoreboard#(numeric type t_id, type t_meta);
    method Bool canBeginTxn(Bit#(t_id) txnId);
    method Action beginTxn(Bit#(t_id) txnId, t_meta meta, Bool isValid);
    method Action completeValidTxn(Bit#(t_id) txnId);
    interface Source#(t_meta) completedValidTxnMeta;
    interface Source#(Tuple2#(Bit#(t_id), t_meta)) invalidTxnsToComplete;
endinterface


// This impl of the scoreboard assumes a small t_id, codify this as <8
// This impl of the scoreboard uses a register with one bit per txnid to see if that txnid is outstanding,
// and doesn't allow you to begin new txns with the same id as a currently outstanding one.
// This coarsely adheres to the AXI spec - transactions with the same ID are ordered relative to each other - but 
// doesn't adhere to the intent, that trasnactions with the same ID can be pipelined together.
//
// TODO how do we handle blockInvalid=False here?
module mkSimpleTxnKeyIdScoreboard#(KonataMode kMode)(TxnKeyIdScoreboard#(t_id, t_meta)) provisos (Add#(8, t_id, __a), Bits#(t_meta, __b), FShow#(t_meta));
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
    // Holds items of type t_meta
    // 2 ports - one read, one write
    BRAM2Port#(Bit#(t_id), t_meta) bram <- mkBRAM2Server(bramConfig);

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

    FIFOF#(t_meta) completedValidTxnMetaImpl <- mkFIFOF;
    FIFOF#(Tuple2#(Bit#(t_id), t_meta)) invalidTxnsToCompleteImpl <- mkFIFOF;

    RWire#(Tuple2#(Bit#(t_id), t_meta)) toWriteToBram <- mkRWire;
    RWire#(Bit#(t_id)) toComplete <- mkRWire;

    rule handle_txnsInProgress;
        let newTxnsInProgress = txnsInProgress;
        case (tuple2(toWriteToBram.wget(), toComplete.wget())) matches
            { tagged Invalid,                 tagged Invalid } : noAction;
            { tagged Valid { .txnId, .meta }, tagged Invalid } : begin
                newTxnsInProgress[txnId] = 1;
            end
            { tagged Invalid, tagged Valid .txnId } : begin
                newTxnsInProgress[txnId] = 0;
            end
            { tagged Valid { .txnIdEnq, .meta }, tagged Valid .txnIdDeq } : begin
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
            tagged Valid { .txnId, .meta } : begin
                konataEvent(kMode, "ScoreboardWrite", fshow(meta));
                bram.portA.request.put(BRAMRequest {
                    write: True,
                    responseOnWrite: False,
                    address: txnId,
                    datain: meta
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
                konataEvent(kMode, "ScoreboardSearch", fshow(" "));
            end
        endcase
    endrule

    rule get_completed_keyid;
        let meta <- bram.portB.response.get();
        konataEvent(kMode, "ScoreboardFindComplete", fshow(meta));
        completedValidTxnMetaImpl.enq(meta);
    endrule
    
    method Bool canBeginTxn(Bit#(t_id) txnId) = txnsInProgress[txnId] == 0;
    // Use the implicit condition to force the caller to block if two trnasactions with the same id exist
    method Action beginTxn(Bit#(t_id) txnId, t_meta meta, Bool isValid);
        if (txnsInProgress[txnId] == 1) begin
            // TODO some sort of error
            $error("beginTxn called when can't begin");
            $finish();
        end
        if (isValid) begin
            toWriteToBram.wset(tuple2(txnId, meta));
        end else begin
            invalidTxnsToCompleteImpl.enq(tuple2(txnId, meta));
        end
    endmethod
    method Action completeValidTxn(Bit#(t_id) txnId); // if (txnsInProgress[txnId] == 1)
        if (txnsInProgress[txnId] == 0) begin
            // TODO some sort of error
            $error("completeValidTxn called when wasn't begun");
            $finish();
        end
        toComplete.wset(txnId);
    endmethod
    interface completedValidTxnMeta = toSource(completedValidTxnMetaImpl);
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
// - TODO (maybe done?) Swapping out the checkers with versions that support in-situ invalidation by KeyId
// - TODO (maybe done?) Support per-transaction KeyId tracking
// Changes from V5
// - Use Checker3, reverting to the style where the Exposer handles finding keys before dispatching to the pool.
// - See Revocation notes 2025-10-10
module mkSimpleIOCapExposerV6#(
    KonataMode kMode,
    IOCapAxi_KeyManager2_ExposerIfc keyStore,
    Bool blockInvalid,
    NumProxy#(n_pool) perAddrChannelPoolSize
)(IOCapSingleExposer#(t_id, t_data)) provisos (
    Add#(t_id, a__, 64),
    Add#(b__, TLog#(n_pool), 64)
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
        // $display(" // dropping from keyResponse");
        keyStore.checker.keyResponse.drop();
    endrule
    

    // AW transactions come in encoding an IOCap with a standard AW flit. The IOCap and flit are examined, and if verified they are passed on through awOut.
    // AddressChannelCapKeyIdUnwrapper#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0), Cap2024_11) awIn <- mkSimpleAddressChannelCapKeyIdUnwrapper(
    //     Proxy{},
    //     keyIdForFlit
    // );
    // IOCapAxiChecker2#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0)) awIn <- mkSimpleIOCapAxiChecker2(
    //     connectFastFSMCapDecode_2024_11,
    //     awKeyReqIfc,
    //     keyResponse,
    //     keyStore.checker.killKeyMessage,
    //     keyIdForFlit
    // );
    // IOCapAxiChecker2#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0)) awIn <- mkInOrderIOCapAxiChecker2V1Pool(
    //     perAddrChannelPoolSize,
    //     connectFastFSMCapDecode_2024_11,
    //     awKeyReqIfc,
    //     keyResponse,
    //     keyStore.checker.killKeyMessage,
    //     keyIdForFlit
    // );
    IOCapAxiChecker3#(AXI4_AWFlit#(t_id, 64, 0)) awChecker <- mkInOrderIOCapAxiChecker3V1Pool(
        kMode,
        perAddrChannelPoolSize,
        connectFastFSMCapDecode_2024_11,
        keyStore.checker.killKeyMessage
    );
    Sink#(Tuple2#(AXI4_AWFlit#(t_id, 64, 3), KFlitId)) awIn <- mkChecker3CombinedPipelinedFrontend(
        kMode,
        awChecker.in,
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
    CreditValve#(AXI4_WFlit#(t_data, 0), 32) wValve <- mkSimpleCreditValve(kMode, toSource(wIn));
    TxnKeyIdScoreboard#(t_id, Tuple2#(KFlitId, KeyId)) wScoreboard <- mkSimpleTxnKeyIdScoreboard(kMode);

    // B responses from the subordinate (de facto for *valid* requests) are sent through to the master, interleaved with responses from invalid requests.
    // These invalid responses are taken from the wScoreboard, and are prioritized over any pending responses from valid requests to ensure ordering.
    FIFOF#(AXI4_BFlit#(t_id, 0)) bIn <- mkFIFOF;
    FIFOF#(AXI4_BFlit#(t_id, 0)) bOut <- mkFIFOF;

    // AR transactions come in encoding an IOCap with a standard AR flit. The IOCap and flit are examined, and if verified they are passed on through arOut.
    // AddressChannelCapKeyIdUnwrapper#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0), Cap2024_11) arIn <- mkSimpleAddressChannelCapKeyIdUnwrapper(
    //     Proxy{},
    //     keyIdForFlit
    // );
    // IOCapAxiChecker2#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0)) arIn <- mkSimpleIOCapAxiChecker2(
    //     connectFastFSMCapDecode_2024_11,
    //     arKeyReqIfc,
    //     keyResponse,
    //     keyStore.checker.killKeyMessage,
    //     keyIdForFlit
    // );
    // IOCapAxiChecker2#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0)) arIn <- mkInOrderIOCapAxiChecker2V1Pool(
    //     perAddrChannelPoolSize,
    //     connectFastFSMCapDecode_2024_11,
    //     arKeyReqIfc,
    //     keyResponse,
    //     keyStore.checker.killKeyMessage,
    //     keyIdForFlit
    // );
    IOCapAxiChecker3#(AXI4_ARFlit#(t_id, 64, 0)) arChecker <- mkInOrderIOCapAxiChecker3V1Pool(
        kMode,
        perAddrChannelPoolSize,
        connectFastFSMCapDecode_2024_11,
        keyStore.checker.killKeyMessage
    );
    Sink#(Tuple2#(AXI4_ARFlit#(t_id, 64, 3), KFlitId)) arIn <- mkChecker3CombinedPipelinedFrontend(
        kMode,
        arChecker.in,
        arKeyReqIfc,
        keyResponse,
        keyStore.checker.killKeyMessage,
        keyIdForFlit
    );
    FIFOF#(AXI4_ARFlit#(t_id, 64, 0)) arOut <- mkFIFOF;

    // R responses from the subordinate (de facto for *valid* requests) are sent through to the master, interleaved with responses from invalid requests.
    // These invalid responses are taken from the rScoreboard, and are prioritized over any pending responses from valid requests to ensure ordering.
    TxnKeyIdScoreboard#(t_id, Tuple2#(KFlitId, KeyId)) rScoreboard <- mkSimpleTxnKeyIdScoreboard(kMode);
    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) rIn <- mkFIFOF;
    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) rOut <- mkFIFOF;

    rule check_aw if (awChecker.checkResponse.canPeek && (
        // If !blockInvalid, we will always be in Pass mode.
        ((tpl_4(awChecker.checkResponse.peek) == True && wValve.canUpdateCredits(Pass)) || (tpl_4(awChecker.checkResponse.peek) == False && wValve.canUpdateCredits(Drop)) || !blockInvalid)
        && wScoreboard.canBeginTxn(tpl_1(awChecker.checkResponse.peek).awid)
    ));
        // Pull the AW check result out of the awIn
        let awResp = awChecker.checkResponse.peek();
        awChecker.checkResponse.drop();
        $display("// IOCap - check_aw ", fshow(awResp));
        // If valid, pass on and increment send credits (if wDropCredited = True, don't dequeue - wait for wSendCredits == 0 so we can set it to False)
        // If invalid, drop the AW flit and increment drop credits
        
        case (awResp) matches
            { .flit, .flitId, .keyId, .allowed } : begin
                Bit#(8) awlen = flit.awlen;
                Bit#(9) nCredits = zeroExtend(awlen) + 1;
                wScoreboard.beginTxn(flit.awid, tuple2(flitId, keyId), allowed);
                if (allowed) begin
                    keyStore.wValve.perf.bumpPerfCounterGood();
                    // Pass through the valid write
                    awOut.enq(flit);
                    // Tell the W valve to let through the right number of flits
                    wValve.updateCredits(Pass, extend(unpack(nCredits)));
                    // Tell the key manager that we're using a keyId
                    keyStore.wValve.refcount.keyIncrementRefcountRequest.put(keyId);
                    konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tSendValid"));
                end else begin
                    keyStore.wValve.perf.bumpPerfCounterBad();
                    if (blockInvalid) begin
                        // We will send the invalid-write-response once it passes through the scoreboard
                        // Tell the W valve to drop the right number of flits
                        wValve.updateCredits(Drop, extend(unpack(nCredits)));
                        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tBlockInvalid"));
                    end else begin
                        // Pass through the invalid write
                        awOut.enq(flit);
                        // Tell the W valve to let through the right number of flits
                        wValve.updateCredits(Pass, extend(unpack(nCredits)));
                        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tSendInvalid"));
                        konataFlit(kMode, $format("R\t") + fshow(flitId) + $format("\t") + fshow(flitId) + $format("\t0"));
                    end
                end
            end
        endcase
    endrule

    Reg#(Maybe#(Tuple3#(KFlitId, Bool, Bool))) lastAwBlocked <- mkReg(tagged Invalid);
    rule check_aw_blocked if (awChecker.checkResponse.canPeek && !(
        // If !blockInvalid, we will always be in Pass mode.
        ((tpl_4(awChecker.checkResponse.peek) == True && wValve.canUpdateCredits(Pass)) || (tpl_4(awChecker.checkResponse.peek) == False && wValve.canUpdateCredits(Drop)) || !blockInvalid)
        && wScoreboard.canBeginTxn(tpl_1(awChecker.checkResponse.peek).awid)
    ));
        let awResp = awChecker.checkResponse.peek();
        case (awResp) matches
            { .flit, .flitId, .keyId, .allowed } : begin
                let valveBlocked = !((tpl_4(awChecker.checkResponse.peek) == True && wValve.canUpdateCredits(Pass)) || (tpl_4(awChecker.checkResponse.peek) == False && wValve.canUpdateCredits(Drop)) || !blockInvalid);
                let scoreboardBlocked = !wScoreboard.canBeginTxn(tpl_1(awChecker.checkResponse.peek).awid);

                let newAwBlocked = tagged Valid tuple3(flitId, valveBlocked, scoreboardBlocked);
                if (lastAwBlocked != newAwBlocked) begin

                    if (valveBlocked && scoreboardBlocked)
                        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tVSBlocked"));
                    else if (valveBlocked)
                        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tVBlocked"));
                    else if (scoreboardBlocked)
                        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tSBlocked"));
                end
                lastAwBlocked <= newAwBlocked;
            end
        endcase
    endrule

    rule print_ar;
        if (arChecker.checkResponse.canPeek()) begin
            $display("// AR ", fshow(arChecker.checkResponse.peek()), " canBegin ", fshow(rScoreboard.canBeginTxn(tpl_1(arChecker.checkResponse.peek).arid)));
        end
    endrule

    rule check_ar (rScoreboard.canBeginTxn(tpl_1(arChecker.checkResponse.peek).arid));
        // Pull the AR check result out of the arIn
        let arResp = arChecker.checkResponse.peek();
        arChecker.checkResponse.drop();
        $display("// IOCap - check_ar ", fshow(arResp));
        // If valid, pass on
        // If invalid, send a failure response
        case (arResp) matches
            { .flit, .flitId, .keyId, .allowed } : begin
                rScoreboard.beginTxn(flit.arid, tuple2(flitId, keyId), allowed);
                $display("// ALLOWED ", fshow(allowed));
                if (allowed) begin
                    keyStore.rValve.perf.bumpPerfCounterGood();
                    // $display("// put out flit", fshow(flit));
                    // Pass through the valid AR flit
                    arOut.enq(flit);
                    // $display("// incref ", fshow(keyId));
                    konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tSendValid"));
                    keyStore.rValve.refcount.keyIncrementRefcountRequest.put(keyId);
                end else begin
                    keyStore.rValve.perf.bumpPerfCounterBad();
                    if (blockInvalid) begin
                        // We will send the invalid-read-response once it passes through the scoreboard
                        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tBlockInvalid"));
                    end else begin
                        // Pass through the invalid AR flit
                        arOut.enq(flit);
                        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tSendInvalid"));
                        konataFlit(kMode, $format("R\t") + fshow(flitId) + $format("\t") + fshow(flitId) + $format("\t0"));
                    end
                end
            end
        endcase
    endrule

    Reg#(Maybe#(KFlitId)) lastArSblocked <- mkReg(tagged Invalid);
    rule check_ar_blocked if (arChecker.checkResponse.canPeek && !rScoreboard.canBeginTxn(tpl_1(arChecker.checkResponse.peek).arid));
        let arResp = arChecker.checkResponse.peek();
        case (arResp) matches
            { .flit, .flitId, .keyId, .allowed } : begin
                if (lastArSblocked != tagged Valid flitId)
                    konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tSBlocked"));
                lastArSblocked <= tagged Valid flitId;
            end
        endcase
    endrule

    // If there isn't an invalid-b-flit to insert, just pass through valid completions from bIn to bOut
    rule passthru_b if (!wScoreboard.invalidTxnsToComplete.canPeek());
        // Pass the responses from the b channel
        bOut.enq(bIn.first);
        bIn.deq();
        // Each B flit signals the end of a write transaction we received an AW for - valid or not
        // Figure out what key that was so we can tell the Valve
        wScoreboard.completeValidTxn(bIn.first.bid);
    endrule

    rule inform_wValve_keyid_completed;
        match { .flitId, .keyId } = wScoreboard.completedValidTxnMeta.peek();
        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tScoreboardCompleted"));
        konataFlit(kMode, $format("R\t") + fshow(flitId) + $format("\t") + fshow(flitId) + $format("\t0"));
        wScoreboard.completedValidTxnMeta.drop();
        keyStore.wValve.refcount.keyDecrementRefcountRequest.put(keyId);
    endrule

    rule insert_invalid_b if (wScoreboard.invalidTxnsToComplete.canPeek());
        match { .txnId, { .flitId, .keyId } } = wScoreboard.invalidTxnsToComplete.peek();
        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tScoreboardCompleted"));
        konataFlit(kMode, $format("R\t") + fshow(flitId) + $format("\t") + fshow(flitId) + $format("\t0"));
        // Insert the b into the stream
        bOut.enq(AXI4_BFlit {
            bid: txnId,
            bresp: SLVERR,
            buser: ?
        });
        wScoreboard.invalidTxnsToComplete.drop();
    endrule

    // If there isn't an invalid-r-flit to insert, just pass through valid completions from rIn to rOut
    rule passthru_r if (!rScoreboard.invalidTxnsToComplete.canPeek());
        // Pass the responses from the r channel
        rOut.enq(rIn.first);
        rIn.deq();
        // Each R flit signals the end of a read transaction we received an AR for - valid or not
        // The read is only completed once the last flit in the burst has been sent
        if (rIn.first.rlast) begin
            konataEvent(kMode, "ValuePassRComplete", $format("R#") + fshow(rIn.first.rid));
            // Figure out what key that was so we can tell the Valve
            rScoreboard.completeValidTxn(rIn.first.rid);
        end else begin
            konataEvent(kMode, "ValuePassR", $format("R#") + fshow(rIn.first.rid));
        end
    endrule

    rule inform_rValve_keyid_completed;
        match { .flitId, .keyId } = rScoreboard.completedValidTxnMeta.peek();
        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tScoreboardCompleted"));
        konataFlit(kMode, $format("R\t") + fshow(flitId) + $format("\t") + fshow(flitId) + $format("\t0"));
        rScoreboard.completedValidTxnMeta.drop();
        keyStore.rValve.refcount.keyDecrementRefcountRequest.put(keyId);
    endrule

    rule insert_invalid_r if (rScoreboard.invalidTxnsToComplete.canPeek());
        match { .txnId, { .flitId, .keyId } } = rScoreboard.invalidTxnsToComplete.peek();
        konataFlit(kMode, $format("S\t") + fshow(flitId) + $format("\t20\tScoreboardCompleted"));
        konataFlit(kMode, $format("R\t") + fshow(flitId) + $format("\t") + fshow(flitId) + $format("\t0"));
        // Insert the r into the stream
        rOut.enq(AXI4_RFlit {
            rid: txnId,
            rresp: SLVERR,
            ruser: ?,
            rdata: ?,
            rlast: True
        });
        rScoreboard.invalidTxnsToComplete.drop();
    endrule

    ReadOnly#(UInt#(64)) fakeThreadId = interface ReadOnly;
        method _read() = 0;
    endinterface;

    AXI4_AWFlit#(t_id, 64, 0) awNoIOCapFlitProxy = ?;
    let awLabeller <- mkIOCapAxiFlitLabeller(kMode, awIn, fakeThreadId, awNoIOCapFlitProxy);

    AXI4_ARFlit#(t_id, 64, 0) arNoIOCapFlitProxy = ?;
    let arLabeller <- mkIOCapAxiFlitLabeller(kMode, arIn, fakeThreadId, arNoIOCapFlitProxy);

    interface iocapsIn = interface IOCapAXI4_Slave;
        interface axiSignals = interface AXI4_Slave;
            interface aw = awLabeller;
            interface  w = toSink(wIn);
            interface  b = toSource(bOut);
            interface ar = arLabeller;
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
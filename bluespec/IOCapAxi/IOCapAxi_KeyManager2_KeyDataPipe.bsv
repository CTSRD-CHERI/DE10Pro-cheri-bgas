import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;
import LeftShift :: *;
import IOCapAxi_ErrorUnit :: *;
import IOCapAxi_Types :: *;
import IOCapAxi_KeyManager2_Types :: *;
import IOCapAxi_KeyManager2_KeyStatePipe :: *;

interface IOCapAxi_KeyManager2_KeyDataPipe_MMIOIfc;
    // Methods for the KeyManager2 to call from MMIO.
    // These methods return True if they have initiated an event that will be successful, otherwise False if the event would not be successful and has not been initiated.
    // They may cause backpressure, but do not return False, if the event would only be unsuccessful because of other backpressure.
    method ActionValue#(Bool) tryWriteKeyWord(KeyId id, Bit#(64) data, Bit#(1) word);
endinterface

interface IOCapAxi_KeyManager2_KeyDataPipe;
    interface IOCapAxi_KeyManager2_KeyDataPipe_MMIOIfc mmio;

    // Used by the checkers to request key data
    interface Vector#(n_checkers, Sink#(KeyId)) checkerKeyRequest;
    // Used by the checkers to receive key data
    interface Vector#(n_checkers, Source#(Tuple2#(KeyId, Maybe#(Key)))) checkerKeyResponse;
endinterface

module mkIOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleChecker#(IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc keyStatus, KeyManager2ErrorUnit error)(IOCapAxi_KeyManager2_KeyDataPipe);
    // ===============================================
    // KEY DATA PIPELINE
    // ===============================================

    BRAM_Configure keyDataMemConfig = BRAM_Configure {
        memorySize: 0, // Number of words is inferred from the KeyId parameter to BRAM1Port below.
        // Size of each word is determined by the Key parameter to BRAM1Port below.
        latency: 2, // (address is registered, data is too because this isn't latency sensitive)
        loadFormat: None,
        outFIFODepth: 4, // latency+2
        allowWriteResponseBypass: False // TODO check if this is fine
    };
    // Used to be single port so that reads are serialized with writes - see above - but now dual port.
    // Addressed by KeyId
    // Holds items of type Key
    // 16 byte-enable wires
    BRAM2PortBE#(KeyId, Key, 16) keyDataPort <- mkBRAM2ServerBE(keyDataMemConfig);
    RWire#(Tuple3#(KeyId, Bits#(64), Bit#(1))) pendingKeyWrite <- mkRWire;

    rule handle_write_key_word;
        let reqWrite <- pendingKeyWrite;
        case (reqWrite) matches
            (.id, .data, 0): begin
                let req = BRAMRequestBE {
                    writeen: 16'h00FF,
                    responseOnWrite: False,
                    address: id,
                    datain: { 0, data }
                };
                keys.portA.request.put(req);
                keyToEnqueueWriteFor <- tuple2(id, 2'b01)
            end
            (.id, .data, 1): begin
                let req = BRAMRequestBE {
                    writeen: 16'hFF00,
                    responseOnWrite: False,
                    address: id,
                    datain: { data, 0 }
                };
                keys.portA.request.put(req);
                keyToEnqueueWriteFor <- tuple2(id, 2'b10)
            end
        endcase
    endrule

    // Queue of incoming requests for keys
    let keyReqFF <- mkFIFOF;
    // TODO under multiple checkers this should carry the checker ID
    // Queue of (KeyId, keyStatus[KeyId] == KeyValid)s requested from BRAM - should match 1:1 with responses from BRAM.
    // Carries the keyValid signal because carrying things through in order is convenient, and we do need to ensure that it isn't initially invalid (see reasoning in comment on IOCap_KeyManager2_KeyCache).
    // Under multiple port BRAM we have to continually check every pending transaction against keyToStartRevoking,
    // to ensure that we don't read data that gets invalidated and replaced with new valid data before we return the stale result.
    // The latency of the BRAM is 4, so the pending-key FIFO should also be 4 - as long as we're constantly pulling out of the key responses we'll have 1/cycle throughput
    function Tuple2#(KeyId, Bool) checkKeyReadAgainstKeyToStartRevoking(Tuple2#(KeyId, Bool) item);
        if (keyToStartRevoking.wget() matches tagged Valid .toRevoke &&& toRevoke == tpl_1(item)) begin
            // The key for this read is being revoked, invalidate the read request
            return tuple2(tpl_1(item), False);
        end
        return item;
    endfunction
    // TODO this depth might need to be 5 to ensure availability
    let pendingKeyIdFF <- mkSizedMapFIFO(4, checkKeyReadAgainstKeyToStartRevoking);
    // Queue of outgoing responses to the Exposer with (KeyId, Key) pairs.
    let keyRespFF <- mkFIFOF;

    (* conflict_free = "handle_write_key_word, start_retrieve_key" *)
    rule start_retrieve_key;
        keyReqFF.deq();
        let keyId = keyReqFF.first(); 
        pendingKeyIdFF.enq.put(tuple2(keyId, keyStatus(keyId) == KeyValid));
        keys.portB.request.put(BRAMRequestBE {
            writeen: 0,
            responseOnWrite: False,
            address: keyId,
            datain: ?
        });
        $display("IOCap - key manager cache - start retrieve key ", fshow(keyId), " - ", fshow(keyValid[keyId]));
    endrule

    // Push reads from the BRAM directly into the keyRespFF (start_retrieve_key is the only rule that starts BRAM reads)
    rule receive_key_from_bram;
        pendingKeyIdFF.deq();

        let keyId_valid_tuple = pendingKeyIdFF.peek();
        let keyId = tpl_1(keyId_valid_tuple);
        // keyValid[k] may have changed between requesting and receiving the key from BRAM.
        let wasValid = tpl_2(keyId_valid_tuple);
        // TODO this isn't necessary anymore if we're using the MapFIFO?
        let isValid = keyStatus(keyId) == KeyValid;
        let valid = wasValid && isValid;
        let key <- keys.portA.response.get();

        if (valid) begin
            keyRespFF.enq(tuple2(keyId, tagged Valid key));
        end else begin
            keyRespFF.enq(tuple2(keyId, tagged Invalid));
        end

        $display("IOCap - key manager cache - receive_key_from_bram ", fshow(keyId), " - ", fshow(key), " - ", fshow(valid));
    endrule
    
    let keyReqSink = toSink(keyReqFF);
    let keyRespSrc = toSource(keyRespFF);

    interface mmio = interface IOCapAxi_KeyManager2_KeyDataPipe_MMIOIfc;
        // This will never cause backpressure, because it's dual-port - there's no reason to be delayed.
        (* always_enabled*)
        method ActionValue#(Bool) tryWriteKeyWord(KeyId id, Bits#(64) data, Bit#(1) word)
            if (keyStatus.keyStatus(id) != KeyInvalidRevoked) begin
                return False;
            end else begin
                keyStatus.tryWriteKeyWord(id, word);
                pendingKeyWrite <- tuple3(id, data, word);
                return True;
            end
        endmethod
    endinterface;

    interface checkerKeyRequest = cons(keyReqSink, nil);
    interface checkerKeyResponse = cons(keyRespSrc, nil);
endmodule;
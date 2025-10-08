import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;
import LeftShift :: *;
import MapFIFO :: *;
import IOCapAxi_ErrorUnit :: *;
import IOCapAxi_Types :: *;
import IOCapAxi_KeyManager2_Types :: *;
import IOCapAxi_KeyManager2_KeyStatePipe :: *;

interface IOCapAxi_KeyManager2_KeyDataPipe_MMIOIfc;
    // Methods for the KeyManager2 to call from MMIO.
    // These methods return True if they have initiated an event that will be successful, otherwise False if the event would not be successful and has not been initiated.
    // They may cause backpressure, but do not return False, if the event would only be unsuccessful because of other backpressure.
    // They are mutually exclusive - you can call exactly one per cycle, and because they're from MMIO this makes sense - need 1 cycle per MMIO req.
    method ActionValue#(Bool) tryWriteKey(KeyId id, Bit#(128) data, Bit#(16) byteEnable);
    method ActionValue#(Bool) tryRevokeAndClearKey(KeyId id);
endinterface

interface IOCapAxi_KeyManager2_KeyDataPipe#(numeric type n_lanes);
    interface IOCapAxi_KeyManager2_KeyDataPipe_MMIOIfc mmio;

    // Used by the lane checkers to request key data
    interface Vector#(n_lanes, Sink#(KeyId)) checkerKeyRequest;
    // Used by the lane checkers to receive key data
    interface Vector#(n_lanes, Source#(Tuple2#(KeyId, Maybe#(Key)))) checkerKeyResponse;
endinterface

module mkIOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleCheckerPort#(IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc keyState, KeyManager2ErrorUnit error)(IOCapAxi_KeyManager2_KeyDataPipe#(1));
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
    // TODO
    /*
    Warning: "BRAMCore.bsv", line 658, column 51: (S0015)
    Bluespec evaluation-time warning: XST will not infer a BRAM with fewer than
    1 write enable or more than 4 write enables.
    During elaboration of `memory' at "BRAM.bsv", line 719, column 40.
    During elaboration of `keyDataPort' at
    "IOCapAxi_KeyManager2_KeyDataPipe.bsv", line 48, column 34.
    During elaboration of `impl' at
    "testbenches/mkIOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleChecker_Tb.bsv",
    line 45, column 9.
    During elaboration of
    `mkIOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleChecker_Tb' at
    "testbenches/mkIOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleChecker_Tb.bsv",
    line 22, column 8.
    */
    BRAM2PortBE#(KeyId, Key, 16) keyDataPort <- mkBRAM2ServerBE(keyDataMemConfig);

    // BRAM clearing is necessary, because KeyStatus doesn't have a way to stop us from using uninitialized/stale data.
    Reg#(Bool) hasClearedBram <- mkReg(False);
    Reg#(KeyId) nextKeyIdToClear <- mkReg(0);
    rule clear_bram(!hasClearedBram);
        KeyId newNextKeyIdToClear = nextKeyIdToClear + 2;
        if (newNextKeyIdToClear == 0) // has overflowed
            hasClearedBram <= True;
        nextKeyIdToClear <= newNextKeyIdToClear;

        keyDataPort.portA.request.put(BRAMRequestBE {
            writeen: 16'hFFFF,
            responseOnWrite: False,
            address: nextKeyIdToClear + 0,
            datain: 0
        });
        keyDataPort.portB.request.put(BRAMRequestBE {
            writeen: 16'hFFFF,
            responseOnWrite: False,
            address: nextKeyIdToClear + 1,
            datain: 0
        });
    endrule

    RWire#(KeyId) pendingKeyRevoke <- mkRWire;
    RWire#(Tuple3#(KeyId, Bit#(128), Bit#(16))) pendingKeyWrite <- mkRWire;

    // Queue of incoming requests for keys
    let keyReqFF <- mkFIFOF;
    // TODO under multiple checker ports this should carry the checker port ID
    // Queue of (KeyId, keyStatus[KeyId] == KeyValid)s requested from BRAM - should match 1:1 with responses from BRAM.
    // Carries the keyValid signal because carrying things through in order is convenient, and we do need to ensure that it isn't initially invalid (see reasoning in comment on IOCap_KeyManager2_KeyCache).
    // Under multiple port BRAM we have to continually check every pending transaction against pendingKeyRevoke,
    // to ensure that we don't read data that gets invalidated and replaced with new valid data before we return the stale result.
    // It's much better to check this instead of looking up the keyStatus, because that would be a mux against all 256 keys instead of inspecting exactly one index.
    // The latency of the BRAM is 4, so the pending-key FIFO should also be 4 - as long as we're constantly pulling out of the key responses we'll have 1/cycle throughput
    function Tuple2#(KeyId, Bool) checkKeyReadAgainstpendingKeyRevoke(Tuple2#(KeyId, Bool) item);
        if (pendingKeyRevoke.wget() matches tagged Valid .toRevoke &&& toRevoke == tpl_1(item)) begin
            // The key for this read is being revoked, invalidate the read request
            return tuple2(toRevoke, False);
        end else begin
            return item;
        end
    endfunction
    // TODO this depth might need to be 5 to ensure availability
    NumProxy#(4) pendingKeyIdDepth = ?;
    let pendingKeyIdFF <- mkSizedMapFIFO(pendingKeyIdDepth, checkKeyReadAgainstpendingKeyRevoke);
    // Queue of outgoing responses to the Exposer with (KeyId, Key) pairs.
    let keyRespFF <- mkFIFOF;

    rule start_retrieve_key(hasClearedBram);
        keyReqFF.deq();
        let keyId = keyReqFF.first(); 
        pendingKeyIdFF.enq.put(tuple2(keyId, keyState.keyStatus(keyId) == KeyValid));
        keyDataPort.portB.request.put(BRAMRequestBE {
            writeen: 0,
            responseOnWrite: False,
            address: keyId,
            datain: ?
        });
        $display("IOCap - key manager cache - start retrieve key ", fshow(keyId), " - ", fshow(keyState.keyStatus(keyId)));
    endrule

    // Push reads from the BRAM directly into the keyRespFF (start_retrieve_key is the only rule that starts BRAM reads)
    rule receive_key_from_bram(hasClearedBram);
        pendingKeyIdFF.deq.drop();

        let keyId_valid_tuple = pendingKeyIdFF.deq.peek();
        let keyId = tpl_1(keyId_valid_tuple);
        // keyValid[k] may have changed between requesting and receiving the key from BRAM.
        let wasValid = tpl_2(keyId_valid_tuple);
        // TODO this isn't necessary anymore if we're using the MapFIFO?
        let isValid = keyState.keyStatus(keyId) == KeyValid;
        let valid = wasValid && isValid;
        let key <- keyDataPort.portB.response.get();

        if (valid) begin
            keyRespFF.enq(tuple2(keyId, tagged Valid key));
        end else begin
            keyRespFF.enq(tuple2(keyId, tagged Invalid));
        end

        $display("IOCap - key manager cache - receive_key_from_bram ", fshow(keyId), " - ", fshow(key), " - ", fshow(valid));
    endrule

    // TODO instead of mutually_exclusive, which just generates a runtime assertion, make my own rule which checks them and ties into ErrorUnit
    (* mutually_exclusive = "write_key, clear_key" *)
    rule write_key(hasClearedBram && isValid(pendingKeyWrite.wget()));
        match { .id, .data, .byteEn } = fromMaybe(?, pendingKeyWrite.wget());
        let req = BRAMRequestBE {
            writeen: byteEn,
            responseOnWrite: False,
            address: id,
            datain: data
        };
        keyDataPort.portA.request.put(req);
    endrule

    rule clear_key(hasClearedBram && isValid(pendingKeyRevoke.wget()));
        let id = fromMaybe(?, pendingKeyRevoke.wget());
        let req = BRAMRequestBE {
            writeen: 16'hFFFF,
            responseOnWrite: False,
            address: id,
            datain: 0
        };
        keyDataPort.portA.request.put(req);
    endrule
    
    let keyReqSink = toSink(keyReqFF);
    let keyRespSrc = toSource(keyRespFF);

    interface mmio = interface IOCapAxi_KeyManager2_KeyDataPipe_MMIOIfc;
        // This will never cause backpressure, because it's dual-port - there's no reason to be delayed.
        method ActionValue#(Bool) tryWriteKey(KeyId id, Bit#(128) data, Bit#(16) byteEn);
            let canWrite <- keyState.tryWriteKey(id);
            if (canWrite) begin
                pendingKeyWrite.wset(tuple3(id, data, byteEn));
                return True;
            end else begin
                return False;
            end
        endmethod

        // Mutually exclusive with tryWriteKey
        method ActionValue#(Bool) tryRevokeAndClearKey(KeyId id);
            let canClear <- keyState.tryRevokeAndClearKey(id);
            if (canClear) begin
                pendingKeyRevoke.wset(id);
                return True;
            end else begin
                return False;
            end
        endmethod
    endinterface;

    interface checkerKeyRequest = cons(keyReqSink, nil);
    interface checkerKeyResponse = cons(keyRespSrc, nil);
endmodule
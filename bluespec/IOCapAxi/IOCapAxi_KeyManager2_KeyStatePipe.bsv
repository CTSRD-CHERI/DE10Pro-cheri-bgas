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

interface IOCapAxi_KeyManager2_KeyStatePipe_MMIOIfc;
    // Methods for the KeyManager2 to call from MMIO.
    // These methods return True if they have initiated an event that will be successful, otherwise False if the event would not be successful and has not been initiated.
    // They never cause backpressure.
    method ActionValue#(Bool) tryEnableKey(KeyId id);
    method ActionValue#(Bool) tryRevokeKey(KeyId id);

    (* always_enabled *)
    method KeyStatus keyStatus(KeyId key);
endinterface

interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc;
    // Methods for the KeyManager2 RefCountPipe to call to update key status.
    // They never cause backpressure.

    // If key `id` is in the PendingRevoke state, move it to Revoked on the next cycle.
    (* always_ready *)
    method Action tryConfirmingRevokeKey(KeyId id);

    // The refcount must read this value on *every* cycle and enqueue a check-ref-count-0 operation if it is set.
    interface RWire#(KeyId) keyToStartRevoking;
endinterface

interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc#(numeric type key_write_coverage_bits);
    // This methon returns True if it has initiated an event that will be successful, otherwise False if the event would not be successful and has not been initiated.
    // It never causes backpressure
    method ActionValue#(Bool) tryWriteKeyWord(KeyId id, Bit#(key_write_coverage_bits) word);

    // The keydata pipeline must read this value on *every* cycle and invalidate any in-progress key reads if it is set.
    interface RWire#(KeyId) keyToStartRevoking;

    (* always_enabled *)
    method KeyStatus keyStatus(KeyId key);
endinterface

interface IOCapAxi_KeyManager2_KeyStatePip#(numeric type key_write_coverage_bits);
    interface IOCapAxi_KeyManager2_KeyStatePipe_MMIOIfc mmio;
    interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc#(key_write_coverage_bits) keydata;
    interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc refcount;
endinterface

module mkIOCapAxi_KeyManager2_KeyStatePipe_SingleReg#(KeyManager2ErrorUnit error)(IOCapAxi_KeyManager2_KeyStatePipe#(key_write_coverage_bits));
    // ===============================================
    // KEY STATE PIPELINE
    // ===============================================

    // Per-key state machine
    Reg#(Vector#(256, Tuple2#(KeyStatus, Bit#(TExp#(key_write_coverage_bits))))) keyStates <- mkReg(replicate(tuple2(KeyInvalidRevoked, 0)));
    // Inputs to the per-key state machine
    RWire#(KeyId) keyToStartRevoking <- mkRWire;                      // Set via MMIO
    RWire#(KeyId) keyToMakeValid <- mkRWire;                          // Set via MMIO
    RWire#(KeyId) keyToTryConfirmingRevoke <- mkRWire;                // Set via RefCountPipe
    RWire#(Tuple2#(KeyId, Bit#(TExp#(key_write_coverage_bits)))) keyToEnqueueWriteFor <- mkRWire; // Set via KeyDataPipe

    (* no_implicit_conditions *)
    rule stepKeyFSMs;
        Vector#(256, Tuple2#(KeyStatus, Bit#(2))) val = keyStates;
        for (Integer i = 0; i < 256; i = i + 1)
            case (keyStates[i]) matches
                { KeyValid, .* } : if (keyToStartRevoking.wget() == (tagged Valid fromInteger(i))) begin
                    val[i] = tuple2(KeyInvalidPendingRevoke, ?);
                end
                { KeyInvalidPendingRevoke, .* } : if (
                    (keyToTryConfirmingRevoke.wget() == (tagged Valid fromInteger(i)))
                ) begin
                    val[i] = tuple2(KeyInvalidRevoked, 0);
                end
                { KeyInvalidRevoked, .writeCov } if ((~writeCov) == 0): if (
                    keyToMakeValid.wget() == (tagged Valid fromInteger(i))
                ) begin
                    val[i] = tuple2(KeyValid, writeCov);
                end
                { KeyInvalidRevoked, .status } : case (keyToEnqueueWriteFor.wget()) matches
                    tagged Valid { .key, .oneHotWordSelect } : if (key == fromInteger(i)) begin
                        val[i] = tuple2(KeyInvalidRevoked, status | oneHotWordSelect);
                    end
                endcase
            endcase
    endrule

    interface mmio = interface IOCapAxi_KeyManager2_KeyStatePipe_MMIOIfc;
        method ActionValue#(Bool) tryEnableKey(KeyId key);
            if (keyStates[key] != tuple2(KeyInvalidRevoked, 'b11)) begin
                return False;
            end else begin
                keyToMakeValid.wset(key);
                return True;
            end
        endmethod

        method ActionValue#(Bool) tryRevokeKey(KeyId key);
            if (tpl_1(keyStates[key]) != KeyValid) begin
                return False;
            end else begin
                keyToStartRevoking.wset(key);
                return True;
            end
        endmethod

        method KeyStatus keyStatus(KeyId key) = tpl_1(keyStates[key]);
    endinterface;

    interface keydata = interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc#(key_write_coverage_bits);
        method ActionValue#(Bool) tryWriteKeyWord(KeyId id, Bit#(key_write_coverage_bits) word);
            if (tpl_1(keyStates[id]) != KeyInvalidRevoked) begin
                return False;
            end else begin
                Bit#(key_write_coverage_bits) oneHot = 1;
                oneHot = oneHot << word;
                keyToEnqueueWriteFor.wset(tuple2(id, oneHot));
                return True;
            end
        endmethod
        
        interface keyToStartRevoking = keyToStartRevoking;
        
        method KeyStatus keyStatus(KeyId key) = tpl_1(keyStates[key]);
    endinterface;

    interface refcount = interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc;
        method Action tryConfirmingRevokeKey(KeyId id);
            keyToTryConfirmingRevoke.wset(id);
        endmethod

        interface keyToStartRevoking = keyToStartRevoking;
    endinterface;

endmodule
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

    (* always_enabled*)
    method KeyStatus keyStatus(KeyId key);
endinterface;

interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc;
    // Methods for the KeyManager2 RefCountPipe to call to update key status.
    // They never cause backpressure.

    // If key `id` is in the PendingRevoke state, move it to Revoked on the next cycle.
    (* always_ready *)
    method Action tryConfirmingRevokeKey(KeyId id);

    // The refcount must read this value on *every* cycle and enqueue a check-ref-count-0 operation if it is set.
    interface RWire#(KeyId) keyToStartRevoking;
endinterface

interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc;
    (* always_enabled*)
    method Tuple2#(KeyState, Bits#(2)) keyState(KeyId key);

    (* always_enabled*)
    method KeyStatus keyStatus(KeyId key);

    // This methon returns True if it has initiated an event that will be successful, otherwise False if the event would not be successful and has not been initiated.
    // It never causes backpressure
    method ActionValue#(Bool) tryWriteKeyWord(KeyId id, Bit#(1) word);
endinterface

interface IOCapAxi_KeyManager2_KeyStatePipe;
    interface IOCapAxi_KeyManager2_KeyStatePipe_MMIOIfc mmio;
    interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc keydata;
    interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc refcount;
endinterface

module mkIOCapAxi_KeyManager2_KeyStatePipe_SingleReg#(KeyManager2ErrorUnit error)(IOCap_KeyManager2_KeyStatePipe);
    // ===============================================
    // KEY STATE PIPELINE
    // ===============================================

    // Per-key state machine
    Reg#(Vector#(256, Tuple2#(KeyState, Bits#(2)))) keyStates;
    // Inputs to the per-key state machine
    RWire#(KeyId) keyToStartRevoking;                      // Set via MMIO
    RWire#(KeyId) keyToMakeValid;                          // Set via MMIO
    RWire#(KeyId) keyToTryConfirmingRevoke;                // Set via RefCountPipe
    RWire#(Tuple2#(KeyId, Bits#(2))) keyToEnqueueWriteFor; // Set via KeyDataPipe

    (* always_enabled*)
    rule stepKeyFSMs;
        Vector#(256, Tuple2#(KeyState, Bits#(2))) val = keyStates;
        for (int i = 0; i < 256; i = i + 1)
            case (keyStates[i]) matches
                (KeyValid, ?) : if (keyToStartRevoking.wget() == (tagged Valid i)) begin
                    val[i] = tuple2(KeyInvalidPendingRevoke, ?);
                end
                (KeyInvalidPendingRevoke, ?) : if (
                    (keyToTryConfirmingRevoke.wget() == (tagged Valid i))
                ) begin
                    val[i] = tuple2(KeyInvalidRevoked, 2'b00);
                end
                (KeyInvalidRevoked, 'b11) : if (
                    keyToMakeValid.wget() == (tagged Valid i)
                ) begin
                    val[i] = tuple2(KeyValid, 2'b11);
                end
                (KeyInvalidRevoked, .status) : case (keyToEnqueueWriteFor.wget()) matches
                    tagged Valid (.key, .oneHotWordSelect) : if (key == i) begin
                        val[i] = tuple2(KeyInvalidRevoked, status | oneHotWordSelect);
                    end
                endcase
            endcase
    endrule

    interface mmio = interface IOCapAxi_KeyManager2_KeyStatePipe_MMIOIfc;
        method ActionValue#(Bool) tryEnableKey(KeyId id);
            if (keyStates[key] != tuple2(KeyInvalidRevoked, 'b11)) begin
                return False;
            end else begin
                keyToMakeValid <- id;
                return True;
            end
        endmethod

        method ActionValue#(Bool) tryRevokeKey(KeyId id);
            if (tpl_1(keyStates[id]) != KeyValid) begin
                return False;
            end else begin
                keyToStartRevoking <- id;
                return True;
            end
        endmethod

        (* always_enabled*)
        method KeyStatus keyStatus(KeyId key) = tpl_1(keyStates[key]);
    endinterface;

    interface refcount = interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc;
        (* always_enabled*)
        method Action tryConfirmingRevokeKey(KeyId id)
            keyToTryConfirmingRevoke <- id;
        endmethod

        interface keyToStartRevoking = keyToStartRevoking;
    endinterface;

    interface keystatus = interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc;
        (* always_enabled*)
        method Tuple2#(KeyStatus, Bit#(2)) keyState(KeyId key) = keyStates[key];

        (* always_enabled*)
        method KeyStatus keyStatus(KeyId key) = tpl_1(keyStates[key]);

        method ActionValue#(Bool) tryWriteKeyWord(KeyId id, Bit#(1) word)
            if (tpl_1(keyStates[id]) != KeyInvalidRevoked) begin
                return False;
            end else begin
                case (word) matches
                    0 : keyToEnqueueWriteFor <- tuple2(id, 2'b01);
                    1 : keyToEnqueueWriteFor <- tuple2(id, 2'b10);
                endcase
                return True;
            end
        endmethod
    endinterface;
endmodule
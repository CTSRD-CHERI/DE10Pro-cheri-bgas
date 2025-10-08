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

interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc;
    // This methon returns True if it has initiated an event that will be successful, otherwise False if the event would not be successful and has not been initiated.
    // It never causes backpressure
    method ActionValue#(Bool) tryWriteKey(KeyId id);
    // If the key is Valid, clear the key data and transition it to Invalid.
    // The keydata pipeline must invalidate any in-progress key reads for this key on the same cycle
    method ActionValue#(Bool) tryRevokeAndClearKey(KeyId id);

    (* always_enabled *)
    method KeyStatus keyStatus(KeyId key);
endinterface

interface IOCapAxi_KeyManager2_KeyStatePipe;
    interface IOCapAxi_KeyManager2_KeyStatePipe_MMIOIfc mmio;
    interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc keydata;
    interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc refcount;
endinterface

module mkIOCapAxi_KeyManager2_KeyStatePipe_SingleReg#(KeyManager2ErrorUnit error)(IOCapAxi_KeyManager2_KeyStatePipe);
    // ===============================================
    // KEY STATE PIPELINE
    // ===============================================

    // Per-key state machine
    Reg#(Vector#(256, KeyStatus)) keyStates <- mkReg(replicate(KeyInvalidRevoked));
    // Inputs to the per-key state machine
    RWire#(KeyId) keyToStartRevoking <- mkRWire;                      // Set via KeyStatus
    RWire#(KeyId) keyToMakeValid <- mkRWire;                          // Set via MMIO
    RWire#(KeyId) keyToTryConfirmingRevoke <- mkRWire;                // Set via RefCountPipe

    (* no_implicit_conditions *)
    rule stepKeyFSMs;
        Vector#(256, KeyStatus) val = keyStates;
        for (Integer i = 0; i < 256; i = i + 1)
            case (keyStates[i]) matches
                KeyValid : if (keyToStartRevoking.wget() == (tagged Valid fromInteger(i))) begin
                    val[i] = KeyInvalidPendingRevoke;
                end
                KeyInvalidPendingRevoke : if (
                    (keyToTryConfirmingRevoke.wget() == (tagged Valid fromInteger(i)))
                ) begin
                    val[i] = KeyInvalidRevoked;
                end
                KeyInvalidRevoked : if (
                    keyToMakeValid.wget() == (tagged Valid fromInteger(i))
                ) begin
                    val[i] = KeyValid;
                end
            endcase
    endrule

    interface mmio = interface IOCapAxi_KeyManager2_KeyStatePipe_MMIOIfc;
        method ActionValue#(Bool) tryEnableKey(KeyId key);
            if (keyStates[key] != KeyInvalidRevoked) begin
                return False;
            end else begin
                keyToMakeValid.wset(key);
                return True;
            end
        endmethod

        method KeyStatus keyStatus(KeyId key) = keyStates[key];
    endinterface;

    interface keydata = interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc;
        method ActionValue#(Bool) tryWriteKey(KeyId id);
            if (keyStates[id] != KeyInvalidRevoked) begin
                return False;
            end else begin
                return True;
            end
        endmethod

        method ActionValue#(Bool) tryRevokeAndClearKey(KeyId id);
            if (keyStates[id] != KeyValid) begin
                return False;
            end else begin
                keyToStartRevoking.wset(id);
                return True;
            end
        endmethod
                
        method KeyStatus keyStatus(KeyId key) = keyStates[key];
    endinterface;

    interface refcount = interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc;
        method Action tryConfirmingRevokeKey(KeyId id);
            keyToTryConfirmingRevoke.wset(id);
        endmethod

        interface keyToStartRevoking = keyToStartRevoking;
    endinterface;

endmodule
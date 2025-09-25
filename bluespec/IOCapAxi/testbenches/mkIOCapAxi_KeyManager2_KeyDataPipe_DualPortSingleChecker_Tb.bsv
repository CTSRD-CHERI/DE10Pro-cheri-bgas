import IOCapAxi_Types :: *;
import IOCapAxi_KeyManager2_Types :: *;
import IOCapAxi_KeyManager2_KeyStatePipe :: *;
import IOCapAxi_KeyManager2_KeyDataPipe :: *;
import IOCapAxi_ErrorUnit :: *;
import SamUtil :: *;
import Vector :: *;

interface IOCapAxi_KeyManager2_KeyDataPipe_SingleChecker_Tb;
    interface IOCapAxi_KeyManager2_KeyDataPipe#(1) dut;

    interface ReadOnly#(Maybe#(Tuple2#(KeyId, Bit#(1)))) triedWriteKeyWord;

    interface WriteOnly#(KeyId) keyToStartRevoking;

    interface WriteOnly#(Vector#(256, KeyStatus)) keyStatus;

    interface KeyManager2ErrorUnit error;
endinterface

(* synthesize *)
module mkIOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleChecker_Tb(IOCapAxi_KeyManager2_KeyDataPipe_SingleChecker_Tb);
    KeyManager2ErrorUnit errorUnit <- mkErrorUnit;
    
    RWire#(Tuple2#(KeyId, Bit#(1))) triedWriteKeyWordRwire <- mkRWire;
    let triedWriteKeyWordReadOnly <- mkRwireToReadOnlyViaReg(triedWriteKeyWordRwire);
    RWire#(KeyId) keyToStartRevokingRwire <- mkRWire;
    Reg#(Vector#(256, KeyStatus)) keyStatusReg <- mkReg(replicate(KeyInvalidRevoked));

    let implInterface = interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc;
        method ActionValue#(Bool) tryWriteKeyWord(KeyId id, Bit#(1) word);
            if (keyStatusReg[id] != KeyInvalidRevoked) begin
                return False;
            end else begin
                triedWriteKeyWordRwire.wset(tuple2(id, word));
                return True;
            end
        endmethod

        interface keyToStartRevoking = keyToStartRevokingRwire;

        method keyStatus(key) = keyStatusReg[key];
    endinterface;

    let impl <- mkIOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleChecker(implInterface, errorUnit);

    interface dut = impl;

    // After cycle #n, this will be tagged Valid if it was written to during cycle #n
    interface triedWriteKeyWord = triedWriteKeyWordReadOnly;
    interface keyToStartRevoking = rwireToWriteOnly(keyToStartRevokingRwire);
    interface keyStatus = regToWriteOnly(keyStatusReg);

    interface error = errorUnit;
endmodule
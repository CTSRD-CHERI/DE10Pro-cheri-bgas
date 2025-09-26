import IOCapAxi_Types :: *;
import IOCapAxi_KeyManager2_Types :: *;
import IOCapAxi_KeyManager2_KeyStatePipe :: *;
import IOCapAxi_KeyManager2_KeyDataPipe :: *;
import IOCapAxi_ErrorUnit :: *;
import SamUtil :: *;
import Vector :: *;
import Tests :: *;

interface IOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleChecker_Tb;
    interface IOCapAxi_KeyManager2_KeyDataPipe#(1) dut;
    interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc_Shim keyStatusShim;
    interface KeyManager2ErrorUnit error;
endinterface

(* synthesize *)
module mkIOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleCheckerPort_Tb(IOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleChecker_Tb);
    let errorImpl <- mkErrorUnit;
    let keyStatusShimImpl <- mkIOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc_Shim;
    let dutImpl <- mkIOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleCheckerPort(keyStatusShimImpl.keyDataFacing, errorImpl);

    interface dut = dutImpl;
    interface keyStatusShim = keyStatusShimImpl;
    interface error = errorImpl;
endmodule
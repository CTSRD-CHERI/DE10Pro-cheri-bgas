import IOCapAxi_Types :: *;
import IOCapAxi_KeyManager2_Types :: *;
import IOCapAxi_KeyManager2_KeyStatePipe :: *;
import IOCapAxi_KeyManager2_RefCountPipe :: *;
import IOCapAxi_ErrorUnit :: *;
import SamUtil :: *;
import Vector :: *;
import Tests :: *;

interface IOCapAxi_KeyManager2_RefCountPipe_TwoValve_Tb;
    interface IOCapAxi_KeyManager2_RefCountPipe#(2) dut;
    interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc_Shim keyStatusShim;
    interface KeyManager2ErrorUnit error;
endinterface

(* synthesize *)
module mkIOCapAxi_KeyManager2_RefCountPipe_TwoValve_Tb(IOCapAxi_KeyManager2_RefCountPipe_TwoValve_Tb);
    let errorImpl <- mkErrorUnit;
    let keyStatusShimImpl <- mkIOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc_Shim;
    let dutImpl <- mkIOCapAxi_KeyManager2_RefCountPipe_TwoValve(keyStatusShimImpl.refCountFacing, errorImpl);

    interface dut = dutImpl;
    interface keyStatusShim = keyStatusShimImpl;
    interface error = errorImpl;
endmodule
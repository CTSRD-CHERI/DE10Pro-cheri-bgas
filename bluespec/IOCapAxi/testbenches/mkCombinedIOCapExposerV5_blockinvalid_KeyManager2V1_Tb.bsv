import IOCapAxi_KeyManager2s :: *;
import IOCapAxi_Exposers_V5 :: *;

(* synthesize *)
module mkCombinedIOCapExposerV5_blockinvalid_KeyManager2V1_Tb(UnifiedSingleExposerKeyMngrV2Tb);
    let keyMgr32Impl <- mkIOCapAxi_KeyManager2_V1;
    let exposerImpl <- mkSimpleIOCapExposerV5(keyMgr32Impl.exposerPorts[0], True);

    interface keyMgr32 = keyMgr32Impl;
    interface exposer4x32 = exposerImpl;
endmodule
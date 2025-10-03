import IOCapAxi_Exposers :: *;
import Tests :: *;

(* synthesize *)
module mkSimpleIOCapExposerV5_blockinvalid_Tb(SimpleIOCapExposerKeyMngrV2Tb);
    let keyStores <- mkKeyStore2Shim;
    let exposer4x32Impl <- mkSimpleIOCapExposerV5(tpl_2(keyStores), True);

    interface keyStoreShim = tpl_1(keyStores);
    interface exposer4x32 = exposer4x32Impl;
endmodule

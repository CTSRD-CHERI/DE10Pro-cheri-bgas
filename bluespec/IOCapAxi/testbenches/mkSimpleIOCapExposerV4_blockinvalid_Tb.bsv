import IOCapAxi_Exposers :: *;
import Tests :: *;

(* synthesize *)
module mkSimpleIOCapExposerV4_blockinvalid_Tb(SimpleIOCapExposerTb);
    let keyStores <- mkKeyStoreShim;
    let exposer4x32Impl <- mkSimpleIOCapExposerV4(tpl_2(keyStores), True);

    interface keyStoreShim = tpl_1(keyStores);
    interface exposer4x32 = exposer4x32Impl;
endmodule

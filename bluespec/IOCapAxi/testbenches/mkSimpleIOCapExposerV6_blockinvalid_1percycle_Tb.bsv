import IOCapAxi_Exposers :: *;
import IOCapAxi_Konata :: *;
import Tests :: *;
import BlueBasics :: *;
import IOCapAxi_Checker3s :: *;

(* synthesize *)
module mkSimpleIOCapExposerV6_blockinvalid_1percycle_Tb(SimpleIOCapExposerKeyMngrV2Tb);
    NumProxy#(9) exposerPoolSize = ?;

    let keyStores <- mkKeyStore2Shim;
    let exposer4x32Impl <- mkSimpleIOCapExposerV6(
        KONATA_FLIT, tpl_2(keyStores), True,
        exposerPoolSize,
        mkSimpleIOCapAxiChecker3V1_FastDecode_1CycleAES_Read,
        mkSimpleIOCapAxiChecker3V1_FastDecode_1CycleAES_Write
    );

    interface keyStoreShim = tpl_1(keyStores);
    interface exposer4x32 = exposer4x32Impl;
endmodule

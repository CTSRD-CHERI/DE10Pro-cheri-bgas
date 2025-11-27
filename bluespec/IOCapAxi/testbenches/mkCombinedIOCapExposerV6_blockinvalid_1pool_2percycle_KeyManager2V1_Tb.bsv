import IOCapAxi_KeyManager2s :: *;
import IOCapAxi_Exposers_V6 :: *;
import IOCapAxi_Checker3s :: *;
import BlueBasics :: *;
import Tests :: *;
import IOCapAxi_Konata :: *;

(* synthesize *)
module mkCombinedIOCapExposerV6_blockinvalid_1pool_2percycle_KeyManager2V1_Tb(UnifiedSingleExposerKeyMngrTb);
    NumProxy#(1) exposerPoolSize = ?;

    let keyMgr32Impl <- mkIOCapAxi_KeyManager2_V1(KONATA_FLIT);
    let exposerImpl <- mkSimpleIOCapExposerV6(
        KONATA_FLIT, keyMgr32Impl.exposerPorts[0], True,
        exposerPoolSize,
        mkSimpleIOCapAxiChecker3V1_FastDecode_2CycleAES_Read,
        mkSimpleIOCapAxiChecker3V1_FastDecode_2CycleAES_Write
    );

    interface keyStore = keyMgr32Impl.hostFacingSlave;
    interface debugEnableKey = keyMgr32Impl.debugEnableKey;
    interface debugKillKey = keyMgr32Impl.debugKillKey;
    interface debugKeyState = keyMgr32Impl.debugKeyState;
    interface debugGoodWrite = keyMgr32Impl.debugGoodWrite;
    interface debugBadWrite = keyMgr32Impl.debugBadWrite;
    interface debugGoodRead = keyMgr32Impl.debugGoodRead;
    interface debugBadRead = keyMgr32Impl.debugBadRead;
    interface exposer4x32 = exposerImpl;
endmodule
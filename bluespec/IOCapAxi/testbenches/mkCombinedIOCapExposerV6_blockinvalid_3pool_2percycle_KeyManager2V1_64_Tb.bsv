import IOCapAxi_KeyManager2s :: *;
import IOCapAxi_Exposers_V6 :: *;
import IOCapAxi_Checker3s :: *;
import BlueBasics :: *;
import Tests :: *;
import IOCapAxi_Konata :: *;

(* synthesize *)
module mkCombinedIOCapExposerV6_blockinvalid_3pool_2percycle_KeyManager2V1_64_Tb(UnifiedSingleExposerKeyMngr64Tb);
    NumProxy#(3) exposerPoolSize = ?;

    let keyMgr64Impl <- mkIOCapAxi_KeyManager2_V1(KONATA_FLIT);
    let exposerImpl <- mkSimpleIOCapExposerV6(
        KONATA_OFF, keyMgr64Impl.exposerPorts[0], True,
        exposerPoolSize,
        mkSimpleIOCapAxiChecker3V1_FastDecode_2CycleAES_Read,
        mkSimpleIOCapAxiChecker3V1_FastDecode_2CycleAES_Write
    );

    interface keyStore = keyMgr64Impl.hostFacingSlave;
    interface debugEnableKey = keyMgr64Impl.debugEnableKey;
    interface debugKillKey = keyMgr64Impl.debugKillKey;
    interface debugKeyState = keyMgr64Impl.debugKeyState;
    interface debugGoodWrite = keyMgr64Impl.debugGoodWrite;
    interface debugBadWrite = keyMgr64Impl.debugBadWrite;
    interface debugGoodRead = keyMgr64Impl.debugGoodRead;
    interface debugBadRead = keyMgr64Impl.debugBadRead;
    interface exposer4x32 = exposerImpl;
endmodule
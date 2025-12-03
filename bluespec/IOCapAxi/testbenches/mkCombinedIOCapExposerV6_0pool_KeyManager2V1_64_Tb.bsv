import IOCapAxi_KeyManager2s :: *;
import IOCapAxi_Exposers_V6 :: *;
import IOCapAxi_Checker3s :: *;
import BlueBasics :: *;
import Tests :: *;
import IOCapAxi_Konata :: *;

// HACK: implement a zero-sized pool (to see the fmax and size for only the surrounding HW) by implementing the Checkers as null variants
(* synthesize *)
module mkCombinedIOCapExposerV6_0pool_KeyManager2V1_64_Tb(UnifiedSingleExposerKeyMngr64Tb);
    NumProxy#(1) exposerPoolSize = ?;

    let keyMgr64Impl <- mkIOCapAxi_KeyManager2_V1(KONATA_FLIT);
    let exposerImpl <- mkSimpleIOCapExposerV6(
        KONATA_FLIT, keyMgr64Impl.exposerPorts[0], True,
        exposerPoolSize,
        mkNullIOCapAxiChecker3V1_Read,
        mkNullIOCapAxiChecker3V1_Write
    );

    interface keyStore = keyMgr64Impl.hostFacingSlave;
    interface keyStoreReady = keyMgr64Impl.ready;
    interface debugEnableKey = keyMgr64Impl.debugEnableKey;
    interface debugKillKey = keyMgr64Impl.debugKillKey;
    interface debugKeyState = keyMgr64Impl.debugKeyState;
    interface debugGoodWrite = keyMgr64Impl.debugGoodWrite;
    interface debugBadWrite = keyMgr64Impl.debugBadWrite;
    interface debugGoodRead = keyMgr64Impl.debugGoodRead;
    interface debugBadRead = keyMgr64Impl.debugBadRead;
    interface exposer4x32 = exposerImpl;
endmodule
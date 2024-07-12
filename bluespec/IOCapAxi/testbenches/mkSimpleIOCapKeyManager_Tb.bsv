import IOCapAxi_KeyManagers :: *;

interface SimpleIOCapKeyManagerTb;
    interface IOCap_KeyManager#(64) keyMgr64;
endinterface

(* synthesize *)
module mkSimpleIOCapKeyManager_Tb(SimpleIOCapKeyManagerTb);
    let keyMgr64Impl <- mkSimpleIOCapKeyManager;
    interface keyMgr64 = keyMgr64Impl;
endmodule
import IOCapAxi_KeyManagers :: *;

interface SimpleIOCapKeyManagerTb;
    interface IOCap_KeyManager#(32) keyMgr32;
endinterface

(* synthesize *)
module mkSimpleIOCapKeyManager_Tb(SimpleIOCapKeyManagerTb);
    let keyMgr32Impl <- mkSimpleIOCapKeyManager;
    interface keyMgr32 = keyMgr32Impl;
endmodule
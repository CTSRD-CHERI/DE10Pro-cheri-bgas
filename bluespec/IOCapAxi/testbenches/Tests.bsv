import Cap2024 :: *;
import BlueAXI4 :: *;
import FIFOF :: *;
import SourceSink :: *;
import SamUtil :: *;
import Vector :: *;

import IOCapAxi_Types :: *;
import IOCapAxi_Exposers :: *;
import IOCapAxi_KeyManagers :: *;
import IOCapAxi_KeyManager2s :: *;
import IOCapAxi_KeyManager2_Types :: *;
import IOCapAxi_KeyManager2_KeyStatePipe :: *;
import IOCapAxi_KeyManager2_KeyDataPipe :: *;
import IOCapAxi_KeyManager2_RefCountPipe :: *;
import IOCapAxi_KeyManager2_MMIO :: *;
import IOCapAxi_ErrorUnit :: *;

// An inversion of the KeyManager interface which the C++ can use to provide stimulus to the Exposer.
interface KeyStoreShim;
    interface ReadOnly#(Bool) bumpedPerfCounterGoodWrite;
    interface ReadOnly#(Bool) bumpedPerfCounterBadWrite;
    interface ReadOnly#(Bool) bumpedPerfCounterGoodRead;
    interface ReadOnly#(Bool) bumpedPerfCounterBadRead;

    // The C++ can receive keyRequests from the exposer
    interface Source#(KeyId) keyRequests;
    // The C++ can send keyResponses to the exposer
    interface Sink#(Tuple2#(KeyId, Maybe#(Key))) keyResponses;

    // The C++ can send newEpochRequests to the exposer
    interface Sink#(Epoch) newEpochRequests;
    // The C++ can receive finishedEpoch notifications from the exposer
    interface Source#(Epoch) finishedEpochs;
endinterface

module mkKeyStoreShim(Tuple2#(KeyStoreShim, IOCap_KeyManager#(32)));
    PulseWire reqGoodWrite <- mkPulseWire;
    PulseWire reqBadWrite <- mkPulseWire;
    PulseWire reqGoodRead <- mkPulseWire;
    PulseWire reqBadRead <- mkPulseWire;

    let keyReqFF <- mkFIFOF;
    let keyRespFF <- mkFIFOF;
    let newEpochRequest <- mkFIFOF;
    let epochCompleteResponse <- mkFIFOF;

    let keyStoreShim = interface KeyStoreShim;
        interface bumpedPerfCounterGoodWrite = pulseWireToReadOnly(reqGoodWrite);
        interface bumpedPerfCounterBadWrite = pulseWireToReadOnly(reqBadWrite);
        interface bumpedPerfCounterGoodRead = pulseWireToReadOnly(reqGoodRead);
        interface bumpedPerfCounterBadRead = pulseWireToReadOnly(reqBadRead);

        interface keyRequests = toSource(keyReqFF);
        interface keyResponses = toSink(keyRespFF);
        interface newEpochRequests = toSink(newEpochRequest);
        interface finishedEpochs = toSource(epochCompleteResponse);
    endinterface;

    let keyManager = interface IOCap_KeyManager#(32);
        method Action bumpPerfCounterGoodWrite() = reqGoodWrite.send();
        method Action bumpPerfCounterBadWrite() = reqBadWrite.send();
        method Action bumpPerfCounterGoodRead() = reqGoodRead.send();
        method Action bumpPerfCounterBadRead() = reqBadRead.send();

        interface keyRequests = toSink(keyReqFF);
        interface keyResponses = toSource(keyRespFF);
        interface newEpochRequests = toSource(newEpochRequest);
        interface finishedEpochs = toSink(epochCompleteResponse);

        interface hostFacingSlave = culDeSac;
        
        // TODO errorUnit
    endinterface;

    return tuple2(keyStoreShim, keyManager);
endmodule

// An inversion of the KeyManager2_ExposerIfc which the C++ can use to provide stimulus to the Exposer.
// TODO finish this
interface KeyStore2Shim;
    // C++ can detect when exposerIfc::{r,w}Valve.perf.bumpPerfCounter{Good,Bad} are called
    interface ReadOnly#(Bool) bumpedPerfCounterGoodWrite;
    interface ReadOnly#(Bool) bumpedPerfCounterBadWrite;
    interface ReadOnly#(Bool) bumpedPerfCounterGoodRead;
    interface ReadOnly#(Bool) bumpedPerfCounterBadRead;

    // The C++ can receive keyRequests from the exposer
    interface Source#(KeyId) keyRequests;
    // The C++ can send keyResponses to the exposer
    interface Sink#(Tuple2#(KeyId, Maybe#(Key))) keyResponses;
    // The C++ can send killKeyMessages to the exposer
    interface WriteOnly#(KeyId) killKeyMessage;

    // The C++ can receive increment/decrement refcount requests from the exposer
    interface Source#(KeyId) rValve_Increment;
    interface Source#(KeyId) rValve_Decrement;
    interface Source#(KeyId) wValve_Increment;
    interface Source#(KeyId) wValve_Decrement;
endinterface

module mkKeyStore2Shim(Tuple2#(KeyStore2Shim, IOCapAxi_KeyManager2_ExposerIfc));
    PulseWire reqGoodWrite <- mkPulseWire;
    PulseWire reqBadWrite <- mkPulseWire;
    PulseWire reqGoodRead <- mkPulseWire;
    PulseWire reqBadRead <- mkPulseWire;

    let keyReqFF <- mkFIFOF;
    let keyRespFF <- mkFIFOF;

    RWire#(KeyId) checker_killKeyMessageRWire <- mkRWire;
    let checker_killKeyMessageReadonly <-mkRwireToReadOnlyDirect(checker_killKeyMessageRWire); 

    let rValve_Increment <- mkFIFOF;
    let rValve_Decrement <- mkFIFOF;
    let wValve_Increment <- mkFIFOF;
    let wValve_Decrement <- mkFIFOF;

    let keyStoreShim = interface KeyStore2Shim;
        interface bumpedPerfCounterGoodWrite = pulseWireToReadOnly(reqGoodWrite);
        interface bumpedPerfCounterBadWrite = pulseWireToReadOnly(reqBadWrite);
        interface bumpedPerfCounterGoodRead = pulseWireToReadOnly(reqGoodRead);
        interface bumpedPerfCounterBadRead = pulseWireToReadOnly(reqBadRead);

        interface keyRequests = toSource(keyReqFF);
        interface keyResponses = toSink(keyRespFF);
        interface killKeyMessage = rwireToWriteOnly(checker_killKeyMessageRWire);

        interface rValve_Increment = toSource(rValve_Increment);
        interface rValve_Decrement = toSource(rValve_Decrement);
        interface wValve_Increment = toSource(wValve_Increment);
        interface wValve_Decrement = toSource(wValve_Decrement);
    endinterface;

    let keyManager = interface IOCapAxi_KeyManager2_ExposerIfc;
        interface checker = interface IOCapAxi_KeyManager2_CheckerIfc;
            interface keyRequest = toSink(keyReqFF);
            interface keyResponse = toSource(keyRespFF);
            interface killKeyMessage = checker_killKeyMessageReadonly;
        endinterface;
        interface rValve = interface IOCapAxi_KeyManager2_ValveIfc;
            interface refcount = interface IOCapAxi_KeyManager2_RefCountPipe_ValveIfc;
                interface keyIncrementRefcountRequest = toSink(rValve_Increment);
                interface keyDecrementRefcountRequest = toSink(rValve_Decrement);
            endinterface;
            interface perf = interface IOCapAxi_KeyManager2_MMIO_PerfCounterIfc;
                method Action bumpPerfCounterGood(); 
                    reqGoodRead.send();
                    $display("// read bump perf good");
                endmethod
                method Action bumpPerfCounterBad(); 
                    reqBadRead.send();
                    $display(" // read bump perf bad");
                endmethod
            endinterface;
        endinterface;
        interface wValve = interface IOCapAxi_KeyManager2_ValveIfc;
            interface refcount = interface IOCapAxi_KeyManager2_RefCountPipe_ValveIfc;
                interface keyIncrementRefcountRequest = toSink(wValve_Increment);
                interface keyDecrementRefcountRequest = toSink(wValve_Decrement);
            endinterface;
            interface perf = interface IOCapAxi_KeyManager2_MMIO_PerfCounterIfc;
                method Action bumpPerfCounterGood(); 
                    reqGoodWrite.send();
                    $display("// write bump perf good");
                endmethod
                method Action bumpPerfCounterBad(); 
                    reqBadWrite.send();
                    $display("// write bump perf bad");
                endmethod
            endinterface;
        endinterface;
    endinterface;

    return tuple2(keyStoreShim, keyManager);
endmodule

interface SimpleIOCapExposerTb;
    interface KeyStoreShim keyStoreShim;

    interface IOCapSingleExposer#(4 /* ID bits */, 32 /* data bits */) exposer4x32;
endinterface

interface SimpleIOCapExposerKeyMngrV2Tb;
    interface KeyStore2Shim keyStoreShim;

    interface IOCapSingleExposer#(4 /* ID bits */, 32 /* data bits */) exposer4x32;
endinterface

interface UnifiedSingleExposerKeyMngrTb;
    interface AXI4Lite_Slave#(TLog#('h2000), 32 /* data bits */, 0, 0, 0, 0, 0) keyStore;
    interface ReadOnly#(Vector#(256, KeyStatus)) debugKeyState;
    interface ReadOnly#(UInt#(64)) debugGoodWrite;
    interface ReadOnly#(UInt#(64)) debugBadWrite;
    interface ReadOnly#(UInt#(64)) debugGoodRead;
    interface ReadOnly#(UInt#(64)) debugBadRead;

    // TODO expose KeyStore2Shim_ReadOnly

    interface IOCapSingleExposer#(4 /* ID bits */, 32 /* data bits */) exposer4x32;
endinterface

interface UnifiedFourExposerKeyMngrV2Tb;
    interface AXI4Lite_Master#(TLog#('h2000), 32 /* data bits */, 0, 0, 0, 0, 0) keyStore;
    interface Vector#(4, IOCapSingleExposer#(4 /* ID bits */, 32 /* data bits */)) exposer4x32;
endinterface

interface CapDecodeTb#(type tcap);
    interface Sink#(tcap) stimulusIn;
    interface Source#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) stimulusOut;
endinterface


// Generic interface for a KeyDataPipe testbench that provides C++ with the ability to control the KeyStatus pipeline interface
interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc_Shim;
    interface ReadOnly#(Maybe#(KeyId)) tryWriteKey;
    interface ReadOnly#(Maybe#(KeyId)) tryRevokeAndClearKey;
    interface WriteOnly#(Vector#(256, KeyStatus)) keyStatus;

    // Internal interface
    interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc keyDataFacing;
endinterface


module mkIOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc_Shim(IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc_Shim);
    RWire#(KeyId) triedWriteKeyRwire <- mkRWire;
    let triedWriteKeyReadOnly <- mkRwireToReadOnlyDirect(triedWriteKeyRwire);
    RWire#(KeyId) tryRevokeAndClearKeyRWire <- mkRWire;
    let tryRevokeAndClearKeyReadOnly <- mkRwireToReadOnlyDirect(tryRevokeAndClearKeyRWire);
    Reg#(Vector#(256, KeyStatus)) keyStatusReg <- mkReg(replicate(KeyInvalidRevoked));

    // After cycle #n, this will be tagged Valid if it was written to during cycle #n
    interface tryWriteKey = triedWriteKeyReadOnly;
    interface tryRevokeAndClearKey = tryRevokeAndClearKeyReadOnly;
    interface keyStatus = regToWriteOnly(keyStatusReg);

    interface keyDataFacing = interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc;
        method ActionValue#(Bool) tryWriteKey(KeyId id);
            if (keyStatusReg[id] != KeyInvalidRevoked) begin
                return False;
            end else begin
                triedWriteKeyRwire.wset(id);
                return True;
            end
        endmethod

        method ActionValue#(Bool) tryRevokeAndClearKey(KeyId id);
            if (keyStatusReg[id] != KeyValid) begin
                return False;
            end else begin
                tryRevokeAndClearKeyRWire.wset(id);
                return True;
            end
        endmethod

        method keyStatus(key) = keyStatusReg[key];
    endinterface;
endmodule

// Generic interface for a RefCountPipe testbench that provides C++ with the ability to control the KeyStatus pipeline interface
interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc_Shim;
    interface ReadOnly#(Maybe#(KeyId)) tryConfirmingRevokeKey;
    interface WriteOnly#(KeyId) keyToStartRevoking;
endinterface

interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc_ShimAndInternal;
    interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc_Shim shim;
    
    // Internal interface
    // This pattern produces a warning because it technically exposes the RWire for tryConfirmingRevokeKey
    // as writable, even if the testbench doesn't try to write to it
    interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc refCountFacing;
endinterface

module mkIOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc_ShimAndInternal(IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc_ShimAndInternal);
    RWire#(KeyId) tryConfirmingRevokeKeyRWire <- mkRWire;
    let tryConfirmingRevokeKeyReadOnly <- mkRwireToReadOnlyDirect(tryConfirmingRevokeKeyRWire);
    RWire#(KeyId) keyToStartRevokingRWire <- mkRWire;

    interface shim = interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc_Shim;
        // After cycle #n, this will be tagged Valid if it was written to during cycle #n
        interface tryConfirmingRevokeKey = tryConfirmingRevokeKeyReadOnly;
        interface keyToStartRevoking = rwireToWriteOnly(keyToStartRevokingRWire);
    endinterface;

    interface refCountFacing = interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc;
        method Action tryConfirmingRevokeKey(KeyId id);
            tryConfirmingRevokeKeyRWire.wset(id);
        endmethod

        interface keyToStartRevoking = keyToStartRevokingRWire;
    endinterface;
endmodule
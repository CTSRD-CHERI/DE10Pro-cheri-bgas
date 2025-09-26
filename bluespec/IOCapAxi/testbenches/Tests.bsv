import Cap2024 :: *;
import BlueAXI4 :: *;
import FIFOF :: *;
import SourceSink :: *;
import SamUtil :: *;
import Vector :: *;

import IOCapAxi_Types :: *;
import IOCapAxi_Exposers :: *;
import IOCapAxi_KeyManagers :: *;
import IOCapAxi_KeyManager2_Types :: *;
import IOCapAxi_KeyManager2_KeyStatePipe :: *;
import IOCapAxi_KeyManager2_KeyDataPipe :: *;
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

interface SimpleIOCapExposerTb;
    interface KeyStoreShim keyStoreShim;

    interface IOCapSingleExposer#(4 /* ID bits */, 32 /* data bits */) exposer4x32;
endinterface

interface CapDecodeTb#(type tcap);
    interface Sink#(tcap) stimulusIn;
    interface Source#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) stimulusOut;
endinterface


// Generic interface for a KeyDataPipe testbench that provides C++ with the ability to control the KeyStatus pipeline interface
interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc_Shim;
    interface ReadOnly#(Maybe#(Tuple2#(KeyId, Bit#(1)))) tryWriteKeyWord;
    interface WriteOnly#(KeyId) keyToStartRevoking;
    interface WriteOnly#(Vector#(256, KeyStatus)) keyStatus;

    // Internal interface
    interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc keyDataFacing;
endinterface


module mkIOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc_Shim(IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc_Shim);
    RWire#(Tuple2#(KeyId, Bit#(1))) triedWriteKeyWordRwire <- mkRWire;
    let triedWriteKeyWordReadOnly <- mkRwireToReadOnlyViaReg(triedWriteKeyWordRwire);
    RWire#(KeyId) keyToStartRevokingRwire <- mkRWire;
    Reg#(Vector#(256, KeyStatus)) keyStatusReg <- mkReg(replicate(KeyInvalidRevoked));

    // After cycle #n, this will be tagged Valid if it was written to during cycle #n
    interface tryWriteKeyWord = triedWriteKeyWordReadOnly;
    interface keyToStartRevoking = rwireToWriteOnly(keyToStartRevokingRwire);
    interface keyStatus = regToWriteOnly(keyStatusReg);

    interface keyDataFacing = interface IOCapAxi_KeyManager2_KeyStatePipe_KeyDataPipeIfc;
        method ActionValue#(Bool) tryWriteKeyWord(KeyId id, Bit#(1) word);
            if (keyStatusReg[id] != KeyInvalidRevoked) begin
                return False;
            end else begin
                triedWriteKeyWordRwire.wset(tuple2(id, word));
                return True;
            end
        endmethod

        interface keyToStartRevoking = keyToStartRevokingRwire;

        method keyStatus(key) = keyStatusReg[key];
    endinterface;
endmodule
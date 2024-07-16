import BlueAXI4 :: *;
import FIFOF :: *;
import SourceSink :: *;

import IOCapAxi_Exposers :: *;
import IOCapAxi_KeyManagers :: *;

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
    endinterface;

    return tuple2(keyStoreShim, keyManager);
endmodule

interface SimpleIOCapExposerTb;
    interface KeyStoreShim keyStoreShim;

    interface IOCapSingleExposer#(4 /* ID bits */, 32 /* data bits */) exposer4x32;
endinterface

(* synthesize *)
module mkSimpleIOCapExposer_Tb(SimpleIOCapExposerTb);
    let keyStores <- mkKeyStoreShim;
    let exposer4x32Impl <- mkSimpleIOCapExposer(tpl_2(keyStores));

    interface keyStoreShim = tpl_1(keyStores);
    interface exposer4x32 = exposer4x32Impl;
endmodule

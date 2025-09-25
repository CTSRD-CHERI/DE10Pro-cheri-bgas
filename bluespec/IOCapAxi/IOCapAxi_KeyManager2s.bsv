import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;
import LeftShift :: *;
import IOCapAxi_ErrorUnit :: *;
import IOCapAxi_Types :: *;
import IOCapAxi_KeyManager2_Types :: *;
import IOCapAxi_KeyManager2_KeyStatePipe :: *;
import IOCapAxi_KeyManager2_KeyDataPipe :: *;
import IOCapAxi_KeyManager2_RefCountPipe :: *;
import IOCapAxi_KeyManager2_MMIO :: *;

interface IOCapAxi_KeyManager2_CheckerIfc;
    // Used by the checker to request keys from the KeyManager
    interface Sink#(KeyId) keyRequest;
    // Used by the checker to receive key data requested by keyRequest
    interface Source#(Tuple2#(KeyId, Maybe#(Key))) keyResponse;
    // Checkers must listen to this wire at all times, and if it ever shows them a key ID they MUST
    // de-authenticate all currently-processing transactions authenticated with this key without passing them to the valve.
    // They MAY continue to start a new transaction authenticated with this key, triggering all related callbacks in the Valve, on that cycle only,
    // but not on any subsequent cycle. 
    interface RWire#(KeyId) killKeyMessage;
endinterface

interface IOCapAxi_KeyManager2_ValveIfc;
    interface IOCapAxi_KeyManager2_RefCountPipe_ValveIfc refcount;
    interface IOCapAxi_KeyManager2_MMIO_PerfCounterIfc perf;
endinterface

interface IOCapAxi_KeyManager2#(numeric type t_data, numeric type n_checkers);
    interface Vector#(n_checkers, IOCapAxi_KeyManager2_CheckerIfc) checkerPorts;
    interface Vector#(n_checkers, IOCapAxi_KeyManager2_ValveIfc) readValvePorts;
    interface Vector#(n_checkers, IOCapAxi_KeyManager2_ValveIfc) writeValvePorts;

    interface AXI4Lite_Slave#(TLog#('h2000), t_data, 0, 0, 0, 0, 0) hostFacingSlave;

    interface KeyManager2ErrorUnit errorUnit;
endinterface


// Demands on KeyStatus storage:
// - needs to be retrievable alongside key data 
// - needs to be retrieved before writing key data
// - needs to be immediately overridable as InvalidPendingRevoke, all other writes can be delayed


// Each KeyManager2 is made of three pipelines
// 1. The key-state pipeline (1 cycle)
// 2. The key-data pipeline (<=4 cycles), reading and writing the data associated with the keys
// 3. The key-refcount pipeline (* cycles), incrementing and decrementing key refcounts and signalling the transition from (PendingRevoke -> Revoke)
//
// 2 and 3 are independent in terms of timing,
// 1 is always *ahead* of 2 and 3 in terms of timing.
//
// What does that mean?
//
// A Key can be in one of three states: KeyValid, KeyInvalidPendingRevoke, KeyInvalidRevoked.
// (KeyInvalidRevoked has substates to indicate which of the two 64-bit words per key have been overwritten)
//
// Each state has exactly one transition to it and one transition from it.
// - KeyValid -> KeyInvalidPendingRevoke on request from the KeyManager MMIO interface
// - KeyInvalidPendingRevoke -> KeyInvalidRevoke when the refcount track confirms the refcount is zero
// - KeyInvalidRevoked (not all words written) -> KeyInvalidRevoked (more words written) when receiving a write-request from the KeyManager MMIO interface
// - KeyInvalidRevoked (all words written) -> KeyValid on request from the KeyManager MMIO interface
//
// The KeyManager MMIO's write responses (i.e. if the write request was valid) depend on the key status, so key status needs to be retrieved quickly.
// The safety of the revocation feature, in particular the feature that once the KeyManager issues a write response to a revoke request NO new transactions will be allowed
// using that key, requires that writing Invalid to the KeyState bypasses whatever reads were in progress in 2.
// In general, writes to 1 bypass operations completing in 2, so I consider 1 "ahead of" 2.
//
// It would be incorrect if a read request in 2 could return old data marked as Valid.
// If you don't check the data is Valid when enqueueing the read request, you could end up in the following scenario
//  - Cycle #1, read starts,            state = KeyInvalidRevoked (not all words written)
//  - Cycle #2, read captures old data, MMIO key data write is enqueued behind the read and state = KeyInvalidRevoked (all words written)
//  - Cycle #3, stall/pipeline delay,   MMIO key status write upgrades state = KeyValid
//  - Cycle #4, old data emitted,       state checked and equal to KeyValid
// This would be impossible if the write couldn't be enqueued at cycle #2, but that isn't feasible because it would require a delay on MMIO writes which we don't want.
// In theory it could be nice to allow this to happen if we KNEW the read data was from the write, but we don't - especially if they're on different ports.
// Checking the key status is Valid ensures that at least one cycle has passed since the final write, which I believe is enough to ensure the BRAM reads out the correct value? TODO TEST THAT
// If you check the data is Valid when enqueueing the read request, the only way to retrieve old data is to then go through the whole lifecycle
//  - Cycle #1, read starts,            state = KeyValid
//  - Cycle #2, read captures old data, MMIO key status write downgrades state = KeyInvalidPendingRevoke
//  - Cycle #3, stall/pipeline delay,   track 3 sets state = KeyInvalidRevoked (no words written)
//  - Cycle #4, stall/pipeline delay,   MMIO key data write is enqueued behind the read and state = KeyInvalidRevoked (some words written)
//  - Cycle #5, stall/pipeline delay,   MMIO key data write is enqueued behind the read and state = KeyInvalidRevoked (all words written)
//  - Cycle #6, stall/pipeline delay,   MMIO key status write upgrades state = keyValid
//  - Cycle #7, old data emitted,       state checked and equal to KeyValid
// This is IMPOSSIBLE as long as the second write can't be enqueued at Cycle #5 or later, which is guaranteed IF the reads and writes are on the same port i.e. serialized and the pipeline is 4 entries or less (I THINK?)
// It would be completely impossible if the state only changed after confirmed key data writes, but that would mean the Cycle #6 key data status write would fail,
// violating the contract implicit from sending valid key data write responses. If we sent valid responses, that implies the writes went through, and our contract
// should be that key status can be upgraded to valid after the writes go through.
// Unfortunately, if they aren't on the same port there isn't really a way to stop the writes going through while the read is stalled.
// In the end I moved them to separate ports.
//
// It's impossible for the key-refcount pipeline to do anything invalid - when a key is transitioned to KeyInvalidPendingRevoke a CheckZero message is enqueued
// in the refcount pipeline. This message can only transition KeyInvalidPendingRevoke -> KeyInvalidRevoke, so it cannot cause problems if that transition happens earlier.
// In fact, all that message can possibly do is transition a key from pending->real revoke earlier than expected!
// It's guaranteed to arrive after the key state was set to KeyInvalidPendingRevoke, so we can guarantee that the key will eventually be checked for revocation even if it never receives a Decrement message again.
// Even if one or more events happen in-between, pushing the key state further through the cycle, CheckZero can only ever correctly transition a key to revoked after revocation was requested.

module mkIOCapAxi_KeyManager2_V1(IOCapAxi_KeyManager2#(64, 1));
    KeyManager2ErrorUnit error <- mkErrorUnit;

    IOCapAxi_KeyManager2_KeyStatePipe     keyState <- mkIOCapAxi_KeyManager2_KeyStatePipe_SingleReg(error);
    IOCapAxi_KeyManager2_KeyDataPipe#(1)   keyData <- mkIOCapAxi_KeyManager2_KeyDataPipe_DualPortSingleChecker(keyState.keydata, error);
    IOCapAxi_KeyManager2_RefCountPipe#(1) refcount <- mkIOCapAxi_KeyManager2_RefCountPipe_SingleChecker(keyState.refcount, error);

    IOCapAxi_KeyManager2_MMIO#(64, 1)         mmio <- mkIOCapAxi_KeyManager2_MMIO(keyState.mmio, keyData.mmio, error);

    function IOCapAxi_KeyManager2_CheckerIfc makeCheckerPort(Integer idx) = interface IOCapAxi_KeyManager2_CheckerIfc;
        interface keyRequest = keyData.checkerKeyRequest[idx];
        interface keyResponse = keyData.checkerKeyResponse[idx];
        interface killKeyMessage = mmio.checkerKillKeyMessages[idx];
    endinterface;

    function IOCapAxi_KeyManager2_ValveIfc makeReadValvePort(Integer idx) = interface IOCapAxi_KeyManager2_ValveIfc;
        Integer valveIdx = idx * 2;
        interface refcount = refcount.valvePorts[valveIdx];
        interface perf = mmio.valvePerfCounters.read[idx];
    endinterface;

    function IOCapAxi_KeyManager2_ValveIfc makeWriteValvePort(Integer idx) = interface IOCapAxi_KeyManager2_ValveIfc;
        Integer valveIdx = (idx * 2) + 1;
        interface refcount = refcount.valvePorts[valveIdx];
        interface perf = mmio.valvePerfCounters.write[idx];
    endinterface;

    interface checkerPorts = genWith(makeCheckerPort);
    interface readValvePorts = genWith(makeReadValvePort);
    interface writeValvePorts = genWith(makeWriteValvePort);

    interface hostFacingSlave = mmio.hostFacingSlave;

    interface errorUnit = error;
endmodule
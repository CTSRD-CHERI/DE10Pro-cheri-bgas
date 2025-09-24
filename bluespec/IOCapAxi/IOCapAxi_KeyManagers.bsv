import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;
import LeftShift :: *;
import IOCapAxi_ErrorUnit :: *;

// TODO maybe move some of these into IOCapAxi_Types?
typedef UInt#(1) Epoch;
typedef Bit#(128) Key;
// 0x1000 bytes => 4096 bytes => 256 keys => 8 bit ID
typedef Bit#(8) KeyId;

typedef union tagged {
    void KeyValidWhileInvalidating;
    void InvalidRead;
    void InvalidStatusWrite;
    void InvalidDataWrite;
    void PrematurelyCompletedEpoch;
} KeyManagerError deriving (Bits, FShow);

interface IOCap_KeyManager#(numeric type t_data);
    method Action bumpPerfCounterGoodWrite();
    method Action bumpPerfCounterBadWrite();
    method Action bumpPerfCounterGoodRead();
    method Action bumpPerfCounterBadRead();

    interface Sink#(KeyId) keyRequests;
    interface Source#(Tuple2#(KeyId, Maybe#(Key))) keyResponses;

    interface Source#(Epoch) newEpochRequests;
    interface Sink#(Epoch) finishedEpochs;

    interface AXI4Lite_Slave#(TLog#('h2000), t_data, 0, 0, 0, 0, 0) hostFacingSlave;

    // There aren't 8 possible values, there are 5 - but KeyManagerError is encoded in 3 bits.
    interface ErrorUnit#(KeyManagerError, 8) errorUnit;
endinterface

module mkSimpleIOCapKeyManager(IOCap_KeyManager#(t_data)) provisos (
    // t_data must be divisible by 8
    // i.e. (t_data/8) * 8 == t_data
    Mul#(TDiv#(t_data, 8), 8, t_data),
    // t_data must be smaller than or equal to 128 - the size of a key
    Add#(t_data, a__, 128),
    // t_data must be smaller than or equal to 64 - the size of a performance counter
    Add#(t_data, b__, 64),
    // Same thing for t_data/8 - ugh, why can't this be proven implicitly
    Add#(TDiv#(t_data, 8), c__, 16)
);
    // Memory map:
    // [0x0, 0x10, 0x20, 0x30, 0x40... 0x1000) = read/write key status
    // [0x1000, 0x1010, 0x1020... 0x2000)      = write key values
    // [0x1000, 0x1008, 0x1010, 0x1018]        = read performance counters
    //                                           - good write
    //                                           - bad write
    //                                           - good read
    //                                           - bad read

    ErrorUnit#(KeyManagerError, 8) error <- mkErrorUnit;

    // Need a BRAM with key data
    // Set up the secret BRAM
    BRAM_Configure keysConfig = BRAM_Configure {
        memorySize: 0, // Inferred from the KeyId parameter to BRAM1Port below
        latency: 2, // (address is registered, data is too because this isn't latency sensitive)
        loadFormat: None,
        outFIFODepth: 3, // latency+2
        allowWriteResponseBypass: False // TODO check if this is fine
    };
    // TODO second BRAM port for clearing keys?
    // Addressed by KeyId
    // Holds items of type Key
    // 16 byte-enable wires
    BRAM1PortBE#(KeyId, Key, 16) keys <- mkBRAM1ServerBE(keysConfig);
    // Make a register with all the key-valid states
    // if keyValid[k] = 0, the key k is not considered valid in the Epoch we're transitioning to.
    Reg#(Bit#(256)) keyValid <- mkReg(0);

    PulseWire reqGoodWrite <- mkPulseWire;
    Reg#(UInt#(64)) goodWrite <- mkReg(0);
    PulseWire reqBadWrite <- mkPulseWire;
    Reg#(UInt#(64)) badWrite <- mkReg(0);
    PulseWire reqGoodRead <- mkPulseWire;
    Reg#(UInt#(64)) goodRead <- mkReg(0);
    PulseWire reqBadRead <- mkPulseWire;
    Reg#(UInt#(64)) badRead <- mkReg(0);

    rule update_perf_counters;
        if (reqGoodWrite) begin
            goodWrite <= goodWrite + 1;
            $display("IOCap stats - good writes %d", (goodWrite + 1));
        end
        if (reqBadWrite) begin
            badWrite <= badWrite + 1;
            $display("IOCap stats - bad writes %d", (badWrite + 1));
        end
        if (reqGoodRead) begin
            goodRead <= goodRead + 1;
            $display("IOCap stats - good reads %d", (goodRead + 1));
        end
        if (reqBadRead) begin
            badRead <= badRead + 1;
            $display("IOCap stats - bad reads %d", (badRead + 1));
        end
    endrule

    let axiShim <- mkAXI4LiteShimFF;

    Reg#(Epoch) currentEpoch <- mkReg(0);
    // Used for reading status to distinguish "has finished invalidation" for a given key from "currently waiting to finish invalidation"
    Reg#(Maybe#(KeyId)) waitingForEpochToInvalidate <- mkReg(tagged Invalid);

    rule sanity_check;
        if (waitingForEpochToInvalidate matches tagged Valid .keyToInvalidate) begin
            if (keyValid[keyToInvalidate] == 1) begin
                error.assertError(tagged KeyValidWhileInvalidating);
            end
        end
    endrule

    let newEpochRequest <- mkFIFOF;
    let epochCompleteResponse <- mkFIFOF;

    // Reads are purely for status, they can return immediately
    rule handle_read;
        let ar <- get (axiShim.master.ar);
        
        let response = tagged Invalid;

        // Can only read the status area - [0x0, 0x1000)
        if ((ar.araddr & 'h1000) == 0) begin
            KeyId k = ar.araddr[11:4]; // Memory map is byte-addressed, each secret key is 16 bytes = 4 address bits

            // The status of a key is 
            // 0 = invalid, revoked
            // 1 = valid
            // 2 = invalid, waiting for revocation to finish

            let keyStatus = 0;
            if (keyValid[k] == 1) begin
                keyStatus = 1;
            end
            if (waitingForEpochToInvalidate matches tagged Valid .keyToInvalidate &&& keyToInvalidate == k) begin
                keyStatus = 2;
            end

            response = tagged Valid keyStatus;
        end else if (ar.araddr < 'h1020) begin
            // We're between [0x1000 and 0x1020)
            // Read a performance counter
            
            // Each perf counter is 64-bits
            Bit#(2) perfId = ar.araddr[4:3];
            Bit#(3) startByteWithinCounter = ar.araddr[2:0];
            Bit#(4) endByteWithinCounter = zeroExtend(startByteWithinCounter) + fromInteger(valueOf(t_data) / 8);

            if (
                // Reads can't overlap two counters
                endByteWithinCounter <= 8
            ) begin
                Bit#(64) counter = ?;
                case (perfId) matches
                    2'b00 : counter = pack(goodWrite);
                    2'b01 : counter = pack(badWrite);
                    2'b10 : counter = pack(goodRead);
                    2'b11 : counter = pack(badRead);
                endcase
                
                // TODO need a comb_right_shift lol
                Bit#(t_data) contents = ?;
                case (startByteWithinCounter) matches
                    0 : contents = truncate(counter >> 0);
                    1 : contents = truncate(counter >> 8);
                    2 : contents = truncate(counter >> 16);
                    3 : contents = truncate(counter >> 24);
                    4 : contents = truncate(counter >> 32);
                    5 : contents = truncate(counter >> 40);
                    6 : contents = truncate(counter >> 48);
                    7 : contents = truncate(counter >> 56);
                endcase

                response = tagged Valid truncate(contents);
            end
        end else begin
            error.assertError(tagged InvalidRead);
        end

        let flit = ?;
        case (response) matches
            tagged Valid .rdata : begin
                flit = AXI4Lite_RFlit {
                      rdata: rdata
                    , rresp: OKAY // Read was valid
                    , ruser: ?
                };
            end
            tagged Invalid : begin
                flit = AXI4Lite_RFlit {
                      rdata: 0
                    , rresp: SLVERR // Read was to invalid, write-only location
                    , ruser: ?
                };
            end
        endcase
        $display("IOCap - key manager - handle_read - ", fshow(ar), " - ", fshow(response));
        axiShim.master.r.put(flit);
    endrule

    // For simplicity, writes can only happen while we aren't transitioning epochs.
    // A write may be handled and return immediately even if it *starts* an epoch transition,
    // but subsequent writes won't be handled until the epoch is over.
    // When we request a new epoch, it's the responsibility of the Exposer to flush keyRespFF (and thus the pendingKeyIdFF and BRAM responses as well?) before telling us the epoch has completed, because those responses *may* be in the old epoch.
    rule handle_write(waitingForEpochToInvalidate matches tagged Invalid);
        let aw <- get (axiShim.master.aw);
        let w <- get (axiShim.master.w);

        let validWrite = False;

        // Writes to [0x0, 0x1000) set status
        if ((aw.awaddr & 'h1000) == 0) begin
            KeyId k = aw.awaddr[11:4]; // Memory map is byte-addressed, each secret key is 16 bytes = 4 address bits
            Bit#(4) startByteWithinKey = aw.awaddr[3:0];

            if (
                // Valid writes must write to the first byte of the key.
                (startByteWithinKey == 0)
                // That first byte must be enabled,
                && (w.wstrb[0] == 1)
                // and the first byte must only ever be 0 or 1,
                && (w.wdata[7:1] == 0)
                // and all the other enabled bytes must be zero.
                && (((w.wdata & beToMask(w.wstrb)) >> 8) == 0)
            ) begin
                let newKeyValid = keyValid;

                // We're either trying to write 0 (invalid) or 1 (valid)
                case (tuple2(keyValid[k], w.wdata[0])) matches
                    { 1, 1 } : noAction;
                    { 0, 0 } : noAction;

                    // Re-enable a key - go from False to True
                    { 0, 1 } : begin
                        newKeyValid[k] = 1;
                    end

                    // Disable a key - go from True to False
                    { 1, 0 } : begin
                        newKeyValid[k] = 0;
                        newEpochRequest.enq(currentEpoch + 1);
                        waitingForEpochToInvalidate <= tagged Valid k;
                    end
                endcase

                keyValid <= newKeyValid;

                validWrite = True;
            end else begin
                error.assertError(tagged InvalidStatusWrite);
            end

        // Writes to [0x1000, 0x2000) write to key (requires the key is revoked)
        end else begin
            KeyId k = aw.awaddr[11:4]; // Memory map is byte-addressed, each secret key is 16 bytes = 4 address bits
            Bit#(4) startByteWithinKey = aw.awaddr[3:0];
            Bit#(5) endByteWithinKey = zeroExtend(startByteWithinKey) + fromInteger(valueOf(t_data) / 8);
    
            if (
                // Writes to key data can't overlap two keys
                endByteWithinKey <= 16
                // The given key must be invalid
                && keyValid[k] == 0
                // and we can't be in the middle of a revocation (already checked above)
            ) begin
                // Move wstrb and wdata into the 128-bit space based on their offset within the key.

                // wstrb = TDiv#(t_data, 8) bits long
                // We've just checked that startByteWithinKey + TDiv#(t_data, 8) <= 16
                // => the top bit of wstrb will only ever go up to the top bit of Bit#(16), and won't be shifted out.
                Bit#(16) bramByteEnable = left_shift_comb(zeroExtend(w.wstrb), unpack(startByteWithinKey));
                // wdata = t_data bits long
                // We've just checked that startByteWithinKey + TDiv#(t_data, 8) <= 16
                // => (startByteWithinKey + t_data/8) * 8 <= 128
                // => (startByteWithinKey * 8) + t_data <= 128
                // => no bits will be shifted out
                Bit#(128) bramWriteData = left_shift_comb(zeroExtend(w.wdata), unpack({startByteWithinKey, 3'b0}));

                keys.portA.request.put(BRAMRequestBE {
                    writeen: bramByteEnable,
                    // Don't send a write-response, we pipe responses 1:1 out into keyResponses
                    responseOnWrite: False,
                    address: k,
                    datain: bramWriteData
                });
                $display("IOCap - BRAM write - writeen ", fshow(bramByteEnable), " - address ", fshow(k), " - datain ", fshow(bramWriteData));
                
                validWrite = True;
            end else begin
                error.assertError(tagged InvalidDataWrite);
            end
        end

        let flit = ?;
        if (validWrite) begin
            flit = AXI4Lite_BFlit {
                  bresp: OKAY
                , buser: ?
            };
        end else begin
            flit = AXI4Lite_BFlit {
                  bresp: SLVERR
                , buser: ?
            };
        end
        $display("IOCap - key manager - handle_write - ", fshow(aw), " - ", fshow(w), " - ", fshow(validWrite));
        axiShim.master.b.put(flit);
    endrule

    rule epoch_complete_sanitycheck(waitingForEpochToInvalidate matches tagged Invalid);
        epochCompleteResponse.deq();
        error.assertError(tagged PrematurelyCompletedEpoch);
    endrule 

    // Doesn't conflict with handle_write, which could *initiate* a new epoch,
    // because that doesn't fire unless waitingForEpochToInvalidate is Invalid.
    rule complete_epoch(waitingForEpochToInvalidate matches tagged Valid .*);
        epochCompleteResponse.deq();
        currentEpoch <= epochCompleteResponse.first;
        waitingForEpochToInvalidate <= tagged Invalid;
        $display("IOCap - key manager - complete epoch");
    endrule

    // Queue of incoming requests for keys
    let keyReqFF <- mkFIFOF;
    // Queue of (KeyId, keyValid[KeyId])s requested from BRAM - should match 1:1 with responses from BRAM.
    // Carries the keyValid signal because we don't want it to "time-travel".
    // If we request a key from BRAM while it's invalid, then AXI requests write to the BRAM *and* set keyValid, we may receive old write-data but use the new keyValid and effectively reanimate stale key data.
    // If we request a key from BRAM while it's valid, then AXI requests *unset* keyValid, it's not technically a problem - the key is now invalid. TODO allow invalidations to time-travel forwards?
    // TODO eventually replace with a fixed-length pipeline - requires keyRespFF to never have backpressure, which requires the Exposer to never give it backpressure....?
    // The latency of the BRAM is 4, so the pending-key FIFO should also be 4 - as long as we're constantly pulling out of the key responses we'll have 1/cycle throughput
    let pendingKeyIdFF <- mkSizedFIFOF(4);
    // Queue of outgoing responses to the Exposer with (KeyId, Key) pairs.
    let keyRespFF <- mkFIFOF;

    (* descending_urgency = "handle_write, start_retrieve_key" *)
    // Access BRAM with lower priority than handle_write
    rule start_retrieve_key;
        keyReqFF.deq();
        let keyId = keyReqFF.first(); 
        pendingKeyIdFF.enq(tuple2(keyId, keyValid[keyId] == 1));
        keys.portA.request.put(BRAMRequestBE {
            writeen: 0,
            responseOnWrite: False,
            address: keyId,
            datain: ?
        });
        $display("IOCap - key manager - start retrieve key ", fshow(keyId), " - ", fshow(keyValid[keyId]));
    endrule

    // Push reads from the BRAM directly into the keyRespFF (start_retrieve_key is the only rule that starts BRAM reads)
    rule receive_key_from_bram;
        pendingKeyIdFF.deq();

        let keyId_valid_tuple = pendingKeyIdFF.first;
        let keyId = tpl_1(keyId_valid_tuple);
        // keyValid[k] may have changed between requesting and receiving the key from BRAM.
        // For simplicity we want to model all changes to key status and data as atomic and ordered together => retain the keyValid state from when the data was requested.
        // *don't* use keyValid directly here.
        let valid = tpl_2(keyId_valid_tuple);
        let key <- keys.portA.response.get();

        if (valid) begin
            keyRespFF.enq(tuple2(keyId, tagged Valid key));
        end else begin
            keyRespFF.enq(tuple2(keyId, tagged Invalid));
        end

        $display("IOCap - key manager - receive_key_from_bram ", fshow(keyId), " - ", fshow(key), " - ", fshow(valid));
    endrule

    method Action bumpPerfCounterGoodWrite() = reqGoodWrite.send();
    method Action bumpPerfCounterBadWrite() = reqBadWrite.send();
    method Action bumpPerfCounterGoodRead() = reqGoodRead.send();
    method Action bumpPerfCounterBadRead() = reqBadRead.send();

    interface keyRequests = toSink(keyReqFF);
    interface keyResponses = toSource(keyRespFF);
    interface newEpochRequests = toSource(newEpochRequest);
    interface finishedEpochs = toSink(epochCompleteResponse);
    interface hostFacingSlave = axiShim.slave;

    interface errorUnit = error;
endmodule

typedef enum {
    KeyInvalidRevoked,       // = 0
    KeyValid,                // = 1
    KeyInvalidPendingRevoke; // = 2
} KeyStatus deriving (Bits, FShow);

interface IOCap_KeyManager2_CheckerIfc;
    // Used by the checker to request keys from the KeyManager
    interface Sink#(KeyId) keyRequest;
    // Used by the checker to receive key data requested by keyRequest
    interface Source#(Tuple2#(KeyId, Maybe#(Key))) keyResponse;
    // When a checker receives a message from this Source,
    // it indicates that transactions with the given key should be invalidated immediately,
    // and not passed on to the valve.
    interface Source#(KeyId) killKeyMessage;
endinterface

interface IOCap_KeyManager2_ValveIfc;
    // Used by the valve to report key ID transaction-starts to the KeyManager
    interface Sink#(KeyId) keyIncrementRefcountRequest;
    // Used by the valve to report key ID transaction-ends to the KeyManager
    interface Sink#(KeyId) keyDecrementRefcountRequest;
endinterface

interface IOCap_KeyManager2#(numeric type t_data, numeric type n_checkers) provisos (
    // n_valves = n_checkers * 2
    Alias#(n_valves, TMul#(2, n_checkers))
);
    method Action bumpPerfCounterGoodWrite();
    method Action bumpPerfCounterBadWrite();
    method Action bumpPerfCounterGoodRead();
    method Action bumpPerfCounterBadRead();

    interface Vector#(n_checkers, IOCap_KeyManager2_CheckerIfc) checkerPorts;
    interface Vector#(n_valves, IOCap_KeyManager2_ValveIfc) valvePorts;

    interface AXI4Lite_Slave#(TLog#('h2000), t_data, 0, 0, 0, 0, 0) hostFacingSlave;

    // There aren't 8 possible values, there are 5 - but KeyManagerError is encoded in 3 bits.
    interface ErrorUnit#(KeyManagerError, 8) errorUnit;
endinterface


// typedef struct {
//     // This data should be quickly retrievable(?), unrelated to refcount
//     KeyStatus status;
//     Bits#(2) writeMask; // Do the two 64-bit data words have writes committed/in the pipeline yet? can't transition to Valid unless that's true.
//     // This data can be retrieved 'slowly' from a BRAM
//     Bits#(128) data;
// } IOCap_KeyManager2_KeyCache_KeyData deriving (Bits, FShow);

// typedef union tagged {
//     // Write a 64-bit word to the key memory, setting the appropriate value in the key mask.
//     // Requires the key to be KeyInvalidRevoked.
//     Tuple3#(KeyId, Bits#(64), Bits#(1)) WriteData;
//     // Move the key to KeyValid if it is KeyInvalidRevoked.
//     // Key data responses sent after this cycle are allowed to show the key is Invalid, it just has to eventually become Valid.
//     KeyId MakeValid;
//     // Move the key to KeyInvalidPendingRevoke if it is KeyValid.
//     // Ensures that any key data responses sent after this cycle will show the key is Invalid.
//     // Those who issue this event should also issue a tryRevoke RefCountEvent (see below). 
//     KeyId MakeInvalid;
// } IOCap_KeyManager2_KeyCache_DirectWriteEvent deriving (Bits, FShow);

typedef struct {
    // Increment the refcount for the given key if it is not KeyInvalidRevoked.
    Vector#(n, Maybe#(KeyId)) increment;
    // Decrement the refcount for the given key if it is not KeyInvalidRevoked.
    // If the refcount is zero, and the state of the key is KeyInvalidPendingRevoke,
    // upgrade to KeyInvalidRevoked and clear the data mask.
    Vector#(n, Maybe#(KeyId)) decrement;
    // Check if the refcount for the given key is zero.
    // If the refcount is zero, and the state of the key is KeyInvalidPendingRevoke,
    // upgrade to KeyInvalidRevoked and clear the data mask.
    Maybe#(KeyId) tryRevoke;
} IOCap_KeyManager2_KeyCache_CombinedRefCountOps#(numeric type n) deriving (Bits, FShow);

// // TODO this doesn't allow merging Increment/Decrement for the same key
// typedef union tagged {
//     KeyId IncrementRC;
//     KeyId DecrementRC;
//     KeyId CheckZeroRC;
// } IOCap_KeyManager2_KeyCache_RefCountOp deriving (Bits, FShow);

typedef struct {
    KeyId key;
    Int#(n_refcount) change; // If this is zero, just check if it's zero and revoke if so.
} IOCap_KeyManager2_KeyCache_RefCountOp#(numeric type n_refcount) deriving (Bits, FShow);

// Assume for now that even if there are multiple Banks of refcounts, each bank can only handle one incoming Op per cycle
// // Vector of Maybe#(IOCap_KeyManager2_KeyCache_RefCountOp) where all Valid RefCountOps will have a unique Key.
// typedef IOCap_KeyManager2_KeyCache_BankRefCountOps#(numeric type n_ops, numeric type n_refcount) Vector#(n_ops, Maybe#(IOCap_KeyManager2_KeyCache_RefCountOp#(n_refcount)));

interface IOCap_KeyManager2_KeyCache_Scheduler#(numeric type in_refcount_clients, numeric type log_n_banks, /*numeric type n_ops_per_bank, */numeric type n_refcount);
    interface Sink#(IOCap_KeyManager2_KeyCache_CombinedRefCountOps#(in_refcount_clients)) enq;
    interface Source#(Vector#(TExp#(log_n_banks), IOCap_KeyManager2_KeyCache_RefCountOp#(n_refcount))) deq;
endinterface

// module mkIOCap_KeyManager2_KeyCache_Scheduler_OneToOne#(IOCap_KeyManager2_KeyCache_Scheduler#(n, TAdd#(TMul#(n, 2), 1)));
//     let fifo <- mkFIFOF;

//     function Maybe#(IOCap_KeyManager2_KeyCache_RefCountOp) makeIncRefCountOp(Maybe#(KeyId) id);
//         if (id matches tagged Valid .key) begin
//             return tagged Valid IOCap_KeyManager2_KeyCache_RefCountOp {
//                 key: key,
//                 change: 1,
//             };
//         end
//         return tagged Invalid;
//     endfunction
//     function Maybe#(IOCap_KeyManager2_KeyCache_RefCountOp) makeDecRefCountOp(Maybe#(KeyId) id);
//         if (id matches tagged Valid .Id) begin
//             return tagged Valid IOCap_KeyManager2_KeyCache_RefCountOp {
//                 key: key,
//                 change: -1,
//             };
//         end
//         return tagged Invalid;
//     endfunction
//     function Maybe#(IOCap_KeyManager2_KeyCache_RefCountOp) makeChkRefCountOp(Maybe#(KeyId) id);
//         if (id matches tagged Valid .Id) begin
//             return tagged Valid IOCap_KeyManager2_KeyCache_RefCountOp {
//                 key: key,
//                 change: 0,
//             };  
//         end
//         return tagged Invalid;
//     endfunction

//     interface enq = interface Sink;
//         (* always_enable *)
//         method canPut = toSink(fifo).canPut;
//         method Action put(IOCap_KeyManager2_KeyCache_CombinedRefCountOps#(n) in);
//             // TODO merge increment/decrements for the same key
//             let ops = cons(makeChkRefCountOp(in.tryRevoke), append(map(in.increment, makeIncRefCountOp), map(in.decrement, makeDecRefCountOp)));
//             fifo.enq(ops);
//         endmethod
//     endinterface
//     interface deq = toSource(fifo);
// endmodule

// module mkIOCap_KeyManager2_KeyCache_Scheduler_MixDownToOne#(IOCap_KeyManager2_KeyCache_Scheduler#(in_refcount_clients, 1, n_refcount));
//     // Two phases:
//     // 1. gather all inputs into a minimized list of 

//     let splitProcessingData <- mkFIFOF;
    
//     let minoConfig = MIMOConfiguration {
//         unguarded: False,
//         bram_based: True,
//     }
//     let mimo <- mkMIMOBRAM(mimoConfig);


//     Reg#(Vector#())

//     method enq = fifo.enq;
//     interface scheduled = toSource(fifo);
// endmodule

// Split an arbitrary number of in_refcount_clients into 2^log_n_banks
// module mkIOCap_KeyManager2_KeyCache_Scheduler_SimpleNBank#(IOCap_KeyManager2_KeyCache_Scheduler#(in_refcount_clients, log_n_banks, n_refcount));

//     let splitProcessingData <- mkFIFOF;
    
//     let minoConfig = MIMOConfiguration {
//         unguarded: False,
//         bram_based: True,
//     }
//     MIMO#(TAdd#(in_refcount_clients, 1), 1, /* TODO depth = */ 16, Vector#(TExp#(log_n_banks), IOCap_KeyManager2_KeyCache_RefCountOp#(n_refcount))); mimo <- mkMIMOBRAM(mimoConfig);


//     Reg#(Vector#())

//     method enq = fifo.enq;
//     interface scheduled = toSource(fifo);
// endmodule

// TODO mkIOCap_KeyManager2_KeyCache_Scheduler_FourToOne#(IOCap_KeyManager2_KeyCache_Scheduler#(4, 1))

// Demands on KeyStatus storage:
// - needs to be retrievable alongside key data 
// - needs to be retrieved before writing key data
// - needs to be immediately overridable as InvalidPendingRevoke, all other writes can be delayed


// This KeyCache is made of three pipelines
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
// This would be impossible if the write couldn't be enqueued at cycle #2, but that isn't feasible because it would require a delay
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
//
// It's impossible for the key-refcount pipeline to do anything invalid - when a key is transitioned to KeyInvalidPendingRevoke a CheckZero message is enqueued
// in the refcount pipeline. This message can only transition KeyInvalidPendingRevoke -> KeyInvalidRevoke, so it cannot cause problems if that transition happens earlier.
// In fact, all that message can possibly do is transition a key from pending->real revoke earlier than expected!
// It's guaranteed to arrive after the key state was set to KeyInvalidPendingRevoke, so we can guarantee that the key will eventually be checked for revocation even if it never receives a Decrement message again.
// Even if one or more events happen in-between, pushing the key state further through the cycle, CheckZero can only ever correctly transition a key to revoked after revocation was requested.
interface IOCap_KeyManager2_KeyCache#(numeric type n_checkers) provisos (
    // n_valves = n_checkers * 2
    Alias#(n_valves, TMul#(2, n_checkers))
);
    // Methods for the KeyManager2 to call from MMIO.
    // These methods return True if they have initiated an event that will be successful, otherwise False if the event would not be successful and has not been initiated.
    // They cause backpressure, but do not return False, if the event would only be unsuccessful because of other backpressure.
    method ActionValue#(Bool) tryWriteKeyWord(KeyId id, Bits#(64) data, Bit#(1) word);
    method ActionValue#(Bool) tryEnableKey(KeyId id);
    method ActionValue#(Bool) tryRevokeKey(KeyId id);

    // Can't use RWire here because they don't block.
    // IOCap_KeyManager2_ValveIfc
    // interface Vector#(n_valves, RWire#(KeyId)) incrementPorts;
    // interface Vector#(n_valves, RWire#(KeyId)) decrementPorts;
    interface Vector#(n_valves, IOCap_KeyManager2_ValveIfc) valvePorts;

    // Used by the checkers to request keys from the KeyManager
    interface Vector#(n_checkers, Sink#(KeyId)) keyRequest;
    // Used by the checkers to receive key data requested by keyRequest
    interface Vector#(n_checkers, Source#(Tuple2#(KeyId, Maybe#(Key)))) keyResponse;
endinterface

module mkSimpleIOCap_KeyManager2_KeyCache(IOCap_KeyManager2_KeyCache#(1));
    // ===============================================
    // KEY STATE PIPELINE
    // ===============================================

    // Per-key state machine
    Reg#(Vector#(256, Tuple2#(KeyState, Bits#(2)))) keyStates;
    // Inputs to the per-key state machine
    RWire#(KeyId) keyToStartRevoking; // Set via MMIO
    RWire#(KeyId) keyToTryConfirmingRevoke; // Set via Revoker
    RWire#(KeyId) keyToMakeValid; // Set via MMIO
    // TODO make sure this isn't set when the write-queue has backpressure...
    // Actually, using a separate write port we guarantee the write-queue has no backpressure. All good.
    // Accepts (KeyId, OneHot(2))
    RWire#(Tuple2#(KeyId, Bits#(2))) keyToEnqueueWriteFor; // Set via MMIO

    (* always_enable *)
    method KeyStatus keyStatus(KeyId key) = keyStates[key].tpl_1;

    (* always_enable *)
    rule stepKeyFSMs;
        Vector#(256, Tuple2#(KeyState, Bits#(2))) val = keyStates;
        for (int i = 0; i < 256; i = i + 1)
            case (keyStates[i]) matches
                (KeyValid, ?) : if (keyToStartRevoking.wget() == (tagged Valid i)) begin
                    val[i] = tuple2(KeyInvalidPendingRevoke, ?);
                end
                (KeyInvalidPendingRevoke, ?) : if (
                    (keyToTryConfirmingRevoke.wget() == (tagged Valid i))
                ) begin
                    val[i] = tuple2(KeyInvalidRevoked, 2'b00);
                end
                (KeyInvalidRevoked, 'b11) : if (
                    keyToMakeValid.wget() == (tagged Valid i)
                ) begin
                    val[i] = tuple2(KeyValid, 2'b11);
                end
                (KeyInvalidRevoked, .status) : case (keyToEnqueueWriteFor.wget()) matches
                    tagged Valid (.key, .oneHotWordSelect) : if (key == i) begin
                        val[i] = tuple2(KeyInvalidRevoked, status | oneHotWordSelect);
                    end
                endcase
            endcase
    endrule

    method ActionValue#(Bool) tryEnableKey(KeyId id);
        if (keyStates[key] != tuple2(KeyInvalidRevoked, 'b11)) begin
            return False;
        end else begin
            keyToMakeValid <- id;
            return True;
        end
    endmethod

    method ActionValue#(Bool) tryRevokeKey(KeyId id);
        if (keyStates[key].tpl_1 != KeyValid) begin
            return False;
        end else begin
            keyToStartRevoking <- id;
            return True;
        end
    endmethod

    // ===============================================
    // KEY DATA PIPELINE
    // ===============================================

    BRAM_Configure keyDataMemConfig = BRAM_Configure {
        memorySize: 0, // Number of words is inferred from the KeyId parameter to BRAM1Port below.
        // Size of each word is determined by the Key parameter to BRAM1Port below.
        latency: 2, // (address is registered, data is too because this isn't latency sensitive)
        loadFormat: None,
        outFIFODepth: 4, // latency+2
        allowWriteResponseBypass: False // TODO check if this is fine
    };
    // Used to be single port so that reads are serialized with writes - see above - but now dual port.
    // Addressed by KeyId
    // Holds items of type Key
    // 16 byte-enable wires
    BRAM2PortBE#(KeyId, Key, 16) keyDataPort <- mkBRAM2ServerBE(keyDataMemConfig);
    RWire#(Tuple3#(KeyId, Bits#(64), Bit#(1))) pendingKeyWrite <- mkRWire;

    method ActionValue#(Bool) tryWriteKeyWord(KeyId id, Bits#(64) data, Bit#(1) word)
        if (keyStatus(id) != KeyInvalidRevoked) begin
            return False;
        end else begin
            pendingKeyWrite <- tuple3(id, data, word);
            return True;
        end
    endmethod

    rule handle_write_key_word;
        let reqWrite <- pendingKeyWrite;
        case (reqWrite) matches
            (.id, .data, 0): begin
                let req = BRAMRequestBE {
                    writeen: 16'h00FF,
                    responseOnWrite: False,
                    address: id,
                    datain: { 0, data }
                };
                keys.portA.request.put(req);
                keyToEnqueueWriteFor <- tuple2(id, 2'b01)
            end
            (.id, .data, 1): begin
                let req = BRAMRequestBE {
                    writeen: 16'hFF00,
                    responseOnWrite: False,
                    address: id,
                    datain: { data, 0 }
                };
                keys.portA.request.put(req);
                keyToEnqueueWriteFor <- tuple2(id, 2'b10)
            end
        endcase
    endrule

    // Queue of incoming requests for keys
    let keyReqFF <- mkFIFOF;
    // TODO under multiple checkers this should carry the checker ID
    // Queue of (KeyId, keyStatus[KeyId] == KeyValid)s requested from BRAM - should match 1:1 with responses from BRAM.
    // Carries the keyValid signal because carrying things through in order is convenient, and we do need to ensure that it isn't initially invalid (see reasoning in comment on IOCap_KeyManager2_KeyCache).
    // Under multiple port BRAM we have to continually check every pending transaction against keyToStartRevoking,
    // to ensure that we don't read data that gets invalidated and replaced with new valid data before we return the stale result.
    // The latency of the BRAM is 4, so the pending-key FIFO should also be 4 - as long as we're constantly pulling out of the key responses we'll have 1/cycle throughput
    function Tuple2#(KeyId, Bool) checkKeyReadAgainstKeyToStartRevoking(Tuple2#(KeyId, Bool) item);
        if (keyToStartRevoking.wget() matches tagged Valid .toRevoke &&& toRevoke == tpl_1(item)) begin
            // The key for this read is being revoked, invalidate the read request
            return tuple2(tpl_1(item), False);
        end
        return item;
    endfunction
    // TODO this depth might need to be 5 to ensure availability
    let pendingKeyIdFF <- mkSizedMapFIFO(4, checkKeyReadAgainstKeyToStartRevoking);
    // Queue of outgoing responses to the Exposer with (KeyId, Key) pairs.
    let keyRespFF <- mkFIFOF;

    (* conflict_free = "handle_write_key_word, start_retrieve_key" *)
    rule start_retrieve_key;
        keyReqFF.deq();
        let keyId = keyReqFF.first(); 
        pendingKeyIdFF.enq.put(tuple2(keyId, keyStatus(keyId) == KeyValid));
        keys.portB.request.put(BRAMRequestBE {
            writeen: 0,
            responseOnWrite: False,
            address: keyId,
            datain: ?
        });
        $display("IOCap - key manager cache - start retrieve key ", fshow(keyId), " - ", fshow(keyValid[keyId]));
    endrule

    // Push reads from the BRAM directly into the keyRespFF (start_retrieve_key is the only rule that starts BRAM reads)
    rule receive_key_from_bram;
        pendingKeyIdFF.deq();

        let keyId_valid_tuple = pendingKeyIdFF.peek();
        let keyId = tpl_1(keyId_valid_tuple);
        // keyValid[k] may have changed between requesting and receiving the key from BRAM.
        let wasValid = tpl_2(keyId_valid_tuple);
        // TODO this isn't necessary anymore if we're using the MapFIFO?
        let isValid = keyStatus(keyId) == KeyValid;
        let valid = wasValid && isValid;
        let key <- keys.portA.response.get();

        if (valid) begin
            keyRespFF.enq(tuple2(keyId, tagged Valid key));
        end else begin
            keyRespFF.enq(tuple2(keyId, tagged Invalid));
        end

        $display("IOCap - key manager cache - receive_key_from_bram ", fshow(keyId), " - ", fshow(key), " - ", fshow(valid));
    endrule
    
    let keyReqSink = toSink(keyReqFF);
    let keyRespSrc = toSource(keyRespFF);

    interface keyRequest = cons(keyReqSink, nil);
    interface keyResponse = cons(keyRespSrc, nil);

    // ===============================================
    // KEY REFCOUNT PIPELINE
    // ===============================================

    BRAM_Configure keyRefcountMemConfig = BRAM_Configure {
        memorySize: 0, // Number of words is inferred from the KeyId parameter to BRAM2Port below.
        // Size of each word is determined by the other parameter to BRAM2Port below.
        latency: 2, // (address is registered, data is too because this isn't latency sensitive)
        loadFormat: None,
        outFIFODepth: 4, // latency+2
        allowWriteResponseBypass: False // TODO check if this is fine
    };
    // Single bank
    // Addressed by KeyId
    // Holds items of type UInt#(XXX)
    // 2 ports - one read, one write
    BRAM2Port#(KeyId, Int#(64)) keyRefcountBram <- mkBRAM2ServerBE(keyRefcountMemConfig);

    Vector#(2, RWire#(KeyId)) incrementWires <- replicateM(mkRWire);
    Vector#(2, RWire#(KeyId)) decrementWires <- replicateM(mkRWire);

    let mimoConfig = MIMOConfiguration {
        unguarded: False,
        bram_based: True,
    }
    MIMO#(/* inputs/cycle = 2inc + 2dec + 1chk = */ 5, /* outputs/cycle = */ 1, /* TODO depth = */ 15, IOCap_KeyManager2_KeyCache_RefCountOp#(2)) mimo <- mkMIMOBRAM(mimoConfig);

    let rcOpInProgress <- mkSizedFIFOF(5);
    // IF YOU CHANGE THE LENGTH OF THIS KEEP mostRecentRefCount UP TO DATE
    // AND refcount_forward_progress_complete_op
    Reg#(Vector#(5, Tuple2#(Maybe#(KeyId), Int#(64)))) rcWriteForwarding <- mkReg(replicate(tuple2(tagged Invalid, ?)));

    function Int#(64) mostRecentRefcount(KeyId key, Int#(64) justRead);
        let forwarded <- rcWriteForwarding;
        for (int i = 0; i < 5; i = i + 1)
            if (tpl_1(forwarded[i]) == tagged Valid key) begin
                return tpl_2(forwarded[i]);
            end
        return justRead;
    endfunction;
        

    function Tuple2#(Maybe#(t), Tuple5#(Maybe#(t),Maybe#(t),Maybe#(t),Maybe#(t),Maybe#(t))) firstValidOf(Tuple5#(Maybe#(t),Maybe#(t),Maybe#(t),Maybe#(t),Maybe#(t)) vals)
        case (vals) matches
            (tagged Valid .val, .b, .c, .d, .e) :
                return tuple2(tagged Valid .val, tuple5(tagged Invalid, b, c, d, e));
            (tagged Invalid, tagged Valid .val, .c, .d, .e) :
                return tuple2(tagged Valid .val, tuple5(tagged Invalid, tagged Invalid, c, d, e));
            (tagged Invalid, tagged Invalid, tagged Valid .val, .d, .e) :
                return tuple2(tagged Valid .val, tuple5(tagged Invalid, tagged Invalid, tagged Invalid, d, e));
            (tagged Invalid, tagged Invalid, tagged Invalid, tagged Valid .val, .e) :
                return tuple2(tagged Valid .val, tuple5(tagged Invalid, tagged Invalid, tagged Invalid, tagged Invalid, e));
            (tagged Invalid, tagged Invalid, tagged Invalid, tagged Invalid, .e) :
                return tuple2(e, vals);
        endcase
    endfunction

    function UInt#(3) indexOfFirstTrue(Vector#(5, Bool) bits);
        case (pack(bits)) matches
            { True, ? } : return 0;
            { False, True, ? } : return 1;
            { False, False, True, ? } : return 2;
            { False, False, False, True, ? } : return 3;
            { False, False, False, False, True } : return 4;
            default : return 7;
        endcase
    endfunction

    function UInt#(3) indexOfSecondTrue(Vector#(5, Bool) bits);
        case (pack(bits)) matches
            { True, True, ? } : return 1;

            { False, True, True, ? } : return 2;
            { True, False, True, ? } : return 2;
            
            { False, False, True, True, ? } : return 3;
            { False, True, False, True, ? } : return 3;
            { True, False, False, True, ? } : return 3;

            { True, False, False, False, True } : return 4;
            { False, True, False, False, True } : return 4;
            { False, False, True, False, True } : return 4;
            { False, False, False, True, True } : return 4;

            default : return 7;
        endcase
    endfunction

    function UInt#(3) indexOfThirdTrue(Vector#(5, Bool) bits);
        case (pack(bits)) matches
            { True, True, True, ? } : return 2;

            { True, True, False, True, ? } : return 3;
            { True, False, True, True, ? } : return 3;
            { False, True, True, True, ? } : return 3;

            { True, True, False, False, True } : return 4;
            { True, False, True, False, True } : return 4;
            { True, False, False, True, True } : return 4;
            { False, True, True, False, True } : return 4;
            { False, True, False, True, True } : return 4;
            { False, False, True, True, True } : return 4;
            
            default : return 7;
        endcase
    endfunction

    function UInt#(3) indexOfFourthTrue(Vector#(5, Bool) bits);
        case (pack(bits)) matches
            { True, True, True, True, ? } : return 3;

            { True, True, True, False, True } : return 4;
            { True, True, False, True, True } : return 4;
            { True, False, True, True, True } : return 4;
            { False, True, True, True, True } : return 4;
            
            default : return 7;
        endcase
    endfunction

    function UInt#(3) indexOfFifthTrue(Vector#(5, Bool) bits);
        case (pack(bits)) matches
            { True, True, True, True, True } : return 4;
            
            default : return 7;
        endcase
    endfunction

    function Maybe#(IOCap_KeyManager2_KeyCache_RefCountOp#(64)) makeOp(Maybe#(KeyId) keyMaybe, Int#(64) change);
        case (keyMaybe) matches
            tagged Valid .key : return tagged Valid IOCap_KeyManager2_KeyCache_RefCountOp {
                key: key,
                change: change,
            };
            tagged Invalid : return tagged Invalid;
        endcase
    endfunction
    
    // Only process things from the valve if we have at least enough entries to enqueue all values
    // This is a bottleneck! so need to ensure depth is big enough to avoid this ever failing.
    // ALSO! we need to make sure there's ALWAYS enough room for the check from keyToStartRevoking.
    // AFAIK we can ensure there's always forward progress, there should always be space to enqueue stuff.
    // There are more than 5 entries in the MIMO queue.
    // The MIMO queue always makes forward progress.
    // That means if we enqueue 5 entries in cycle #n, either
    //  - we will also dequeue 1 entry in cycle #n, and because there were at least 5 entries available in cycle #n, there must be at least 1 entry left in cycle #n+1
    //  - we will not dequeue any entries, which must mean there was nothing in the queue because the queue always makes forward progress,
    //      there will be exactly 5 entries in the queue on cycle #n+1 and we know there are more than 5 entries. 
    // => enqueueing 5 values in one cycle will never prevent something from being enqueued on the next cycle.

    (* always_enabled *)
    rule process_valve_ports;
        Vector#(5, IOCap_KeyManager2_KeyCache_RefCountOp#(64)) packedVector = replicate(?);
        if (mimo.enqReadyN(pack(5))) begin
            // We have enough space to process incrementWires, decrementWires, and we have space for the keyToStartRevoking.
            // TODO does order matter here? I think it does... make sure we don't prematurely count something as revoked if it's incremented on the same cycle that the request arrives...
            UInt#(3) count = 0;
            // TODO this won't compile. Can't use [] indexing later down. Need to fix. Make this a Vector.
            let items = tuple5(
                makeOp(incrementWires[0].wget(), 1),
                makeOp(incrementWires[1].wget(), 1),
                makeOp(decrementWires[0].wget(), -1),
                makeOp(decrementWires[1].wget(), -1),
                makeOp(keyToStartRevoking.wget(), 0),
            );

            /*
            // Single cycle 5->5 ordering scheduler thingy.
            // Take 5 things which may be Valid or Invalid, sort Valid in front and put them in the Vector, then enqueue only those valid ones.
            // Expressed badly with a recursive-y function, that a logic analyser should be able to flatten into the form shown below, but much more annoying for Bluespec to handle.
            let item_tpl = firstValidOf(items);          
            if (tpl_1(item_tpl) matches tagged Valid .op) begin
                packedVector[0] = op;
                count = 1;
                item_tpl = firstValidOf(tpl_2(item_tpl));
                if (tpl_1(item_tpl) matches tagged Valid .op2) begin
                    packedVector[1] = op2;
                    count = 2;
                    item_tpl = firstValidOf(tpl_2(item_tpl));
                    if (tpl_1(item_tpl) matches tagged Valid .op3) begin
                        packedVector[2] = op3;
                        count = 3;
                        item_tpl = firstValidOf(tpl_2(item_tpl));
                        if (tpl_1(item_tpl) matches tagged Valid .op4) begin
                            packedVector[3] = op4;
                            count = 4;
                            item_tpl = firstValidOf(tpl_2(item_tpl));
                            if (tpl_1(item_tpl) matches tagged Valid .op5) begin
                                packedVector[4] = op5;
                                count = 5;
                            end
                        end
                    end
                end
                */

                let itemValidity = map(isValid, items);

                // Single cycle 5->5 ordering scheduler thingy.
                // Take 5 things which may be Valid or Invalid, sort Valid in front and put them in the Vector, then enqueue only those valid ones.
                // Expressed as efficient parallel LUTs and lookups. The indexing should be reduced to onehots.
                // Each indexOfXYZthTrue is a simple LUT.
                let idx0 = indexOfFirstTrue(itemValidity);
                if (idx0 != 7) begin
                    packedVector[0] = fromMaybe(?, items[idx0]);
                    count = 1;
                end
                let idx1 = indexOfSecondTrue(itemValidity);
                if (idx1 != 7) begin
                    packedVector[1] = fromMaybe(?, items[idx1]);
                    count = 2;
                end
                let idx2 = indexOfThirdTrue(itemValidity);
                if (idx2 != 7) begin
                    packedVector[2] = fromMaybe(?, items[idx2]);
                    count = 3;
                end
                let idx3 = indexOfFourthTrue(itemValidity);
                if (idx3 != 7) begin
                    packedVector[3] = fromMaybe(?, items[idx3]);
                    count = 4;
                end
                let idx4 = indexOfFifthTrue(itemValidity);
                if (idx4 != 7) begin
                    packedVector[4] = fromMaybe(?, items[idx4]);
                    count = 5;
                end

                mimo.enq(count, packedVector);
            end
        end else begin
            // assert mimo.enqReadyN(pack(1)) == True
            // We always have enough space to enqueue the keyToStartRevoking
            if (makeOp(keyToStartRevoking.wget(), 0) matches tagged Valid .op) begin
                packedVector[0] = op;
                mimo.enq(1, packedVector);
            end
        end
    endrule

    (* always_enable *)
    rule refcount_forward_progress_start_op;
        let opVec <- mimo.deq(1);
        let op = opVec[0];

        keyRefcountBram.portA.request.put(BRAMRequest {
            write: False,
            responseOnWrite: False,
            address: op.key,
            datain: ?
        });

        rcOpInProgress.enq(op);
    endrule

    (* always_enable *)
    rule refcount_forward_progress_complete_op;
        let readRc <- keyRefcountBram.portA.response.get();
        let op <- rcOpInProgress.deq;

        let actualRc = mostRecentRefCount(op.key, readRc);
        // Update the Rc if needed
        if (op.change != 0) begin
            actualRc = actualRc + signExtend(op.change);

            // BRAM write
            keyRefcountBram.portB.request.put(BRAMRequest {
                write: True,
                responseOnWrite: False,
                address: op.key,
                datain: actualRc
            });

            // Write forwarding
            let forwarded <- rcWriteForwarding;
            // Add a new forwarding record for this ref at the front so it has highest priority.
            // Assume the forwarding vector is big enough that whatever gets shifted out will definitely be inside the BRAM by this point.
            let newForwarding = rotateR(forwarded);
            forwarded[0] = tuple2(tagged Valid (op.key), actualRc);
        end
        
        if (actualRc == 0) begin
            keyToTryConfirmingRevoke <= op.key;
        end
    endrule

    // Remember, the valve ports cannot enqueue anything unless there's at least 5 elements in the FIFO.
    function buildValveInterface(Integer valveIdx) = interface IOCap_KeyManager2_ValveIfc;
        // Used by the valve to report key ID transaction-starts to the KeyManager
        interface keyIncrementRefcountRequest = interface Sink;
            method canPut = mimo.enqReadyN(pack(5));
            method Action put(KeyId id);
                incrementWires[valveIdx] <- id;
            endmethod
        endinterface;
        // Used by the valve to report key ID transaction-ends to the KeyManager
        interface keyDecrementRefcountRequest = interface Sink;
            method canPut = mimo.enqReadyN(pack(5));
            method Action put(KeyId id);
                decrementWires[valveIdx] <- id;
            endmethod
        endinterface;
    endinterface;

    // Generate 0..1 valve interfaces
    interface valvePorts = genWith(buildValveInterface);


endmodule
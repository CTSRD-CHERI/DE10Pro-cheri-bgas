import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import VectorExtra :: *;
import BlueBasics :: *;
import LeftShift :: *;
import MIMO :: *;
import IOCapAxi_ErrorUnit :: *;
import IOCapAxi_Types :: *;
import IOCapAxi_KeyManager2_Types :: *;
import IOCapAxi_KeyManager2_KeyStatePipe :: *;

interface IOCapAxi_KeyManager2_RefCountPipe_ValveIfc;
    // Used by the valve to report key ID transaction-starts to the KeyManager
    interface Sink#(KeyId) keyIncrementRefcountRequest;
    // Used by the valve to report key ID transaction-ends to the KeyManager
    interface Sink#(KeyId) keyDecrementRefcountRequest;
endinterface

interface IOCapAxi_KeyManager2_RefCountPipe#(numeric type n_valves);
    // Can't use RWire here because they don't block.
    // IOCap_KeyManager2_ValveIfc
    // interface Vector#(n_valves, RWire#(KeyId)) incrementPorts;
    // interface Vector#(n_valves, RWire#(KeyId)) decrementPorts;
    interface Vector#(TAdd#(n_valves, n_valves), IOCapAxi_KeyManager2_RefCountPipe_ValveIfc) valvePorts;
endinterface


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
} IOCapAxi_KeyManager2_KeyCache_CombinedRefCountOps#(numeric type n) deriving (Bits, FShow);

// // TODO this doesn't allow merging Increment/Decrement for the same key
// typedef union tagged {
//     KeyId IncrementRC;
//     KeyId DecrementRC;
//     KeyId CheckZeroRC;
// } IOCap_KeyManager2_KeyCache_RefCountOp deriving (Bits, FShow);

typedef struct {
    KeyId key;
    Int#(n_refcount) change; // If this is zero, just check if it's zero and revoke if so.
} IOCapAxi_KeyManager2_KeyCache_RefCountOp#(numeric type n_refcount) deriving (Bits, FShow);

// Assume for now that even if there are multiple Banks of refcounts, each bank can only handle one incoming Op per cycle
// // Vector of Maybe#(IOCap_KeyManager2_KeyCache_RefCountOp) where all Valid RefCountOps will have a unique Key.
// typedef IOCap_KeyManager2_KeyCache_BankRefCountOps#(numeric type n_ops, numeric type n_refcount) Vector#(n_ops, Maybe#(IOCap_KeyManager2_KeyCache_RefCountOp#(n_refcount)));

interface IOCapAxi_KeyManager2_KeyCache_Scheduler#(numeric type in_refcount_clients, numeric type log_n_banks, /*numeric type n_ops_per_bank, */numeric type n_refcount);
    interface Sink#(IOCapAxi_KeyManager2_KeyCache_CombinedRefCountOps#(in_refcount_clients)) enq;
    interface Source#(Vector#(TExp#(log_n_banks), IOCapAxi_KeyManager2_KeyCache_RefCountOp#(n_refcount))) deq;
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
//         (* always_enabled *)
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


module mkIOCapAxi_KeyManager2_RefCountPipe_SingleChecker#(IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc keyState, KeyManager2ErrorUnit error)(IOCapAxi_KeyManager2_RefCountPipe#(1));
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
    // TODO Make this generic on refcount size
    BRAM2Port#(KeyId, Int#(64)) keyRefcountBram <- mkBRAM2Server(keyRefcountMemConfig);

    Vector#(2, RWire#(KeyId)) incrementWires <- replicateM(mkRWire);
    Vector#(2, RWire#(KeyId)) decrementWires <- replicateM(mkRWire);

    let mimoConfig = MIMOConfiguration {
        unguarded: False,
        bram_based: True
    };
    // mkMimoBRAM is advertised in docs but not exported by the package - set bram_based: True to get it
    MIMO#(/* inputs/cycle = 2inc + 2dec + 1chk = */ 5, /* outputs/cycle = */ 1, /* TODO depth = */ 15, IOCapAxi_KeyManager2_KeyCache_RefCountOp#(2)) mimo <- mkMIMO(mimoConfig);

    FIFOF#(IOCapAxi_KeyManager2_KeyCache_RefCountOp#(2)) rcOpInProgress <- mkSizedFIFOF(5);
    // IF YOU CHANGE THE LENGTH OF THIS KEEP mostRecentRefCount UP TO DATE
    // AND refcount_forward_progress_complete_op
    Reg#(Vector#(5, Tuple2#(Maybe#(KeyId), Int#(64)))) rcWriteForwarding <- mkReg(replicate(tuple2(tagged Invalid, ?)));

    function Int#(64) mostRecentRefCount(KeyId key, Int#(64) justRead);
        let allForwarded = rcWriteForwarding;
        Maybe#(Int#(64)) forwardedForKey = tagged Invalid;
        for (Integer i = 0; i < 5; i = i + 1)
            if (tpl_1(allForwarded[i]) == tagged Valid key && forwardedForKey == tagged Invalid) begin
                forwardedForKey = tagged Valid tpl_2(allForwarded[i]);
            end
        case (forwardedForKey) matches
            tagged Valid .refcount : return refcount;
            tagged Invalid         : return justRead;
        endcase
    endfunction

    function Tuple2#(Maybe#(t), Tuple5#(Maybe#(t),Maybe#(t),Maybe#(t),Maybe#(t),Maybe#(t))) firstValidOf(Tuple5#(Maybe#(t),Maybe#(t),Maybe#(t),Maybe#(t),Maybe#(t)) vals);
        case (vals) matches
            { tagged Valid .val, .b, .c, .d, .e } :
                return tuple2(tagged Valid val, tuple5(tagged Invalid, b, c, d, e));
            { tagged Invalid, tagged Valid .val, .c, .d, .e } :
                return tuple2(tagged Valid val, tuple5(tagged Invalid, tagged Invalid, c, d, e));
            { tagged Invalid, tagged Invalid, tagged Valid .val, .d, .e } :
                return tuple2(tagged Valid val, tuple5(tagged Invalid, tagged Invalid, tagged Invalid, d, e));
            { tagged Invalid, tagged Invalid, tagged Invalid, tagged Valid .val, .e } :
                return tuple2(tagged Valid val, tuple5(tagged Invalid, tagged Invalid, tagged Invalid, tagged Invalid, e));
            { tagged Invalid, tagged Invalid, tagged Invalid, tagged Invalid, .e } :
                return tuple2(e, vals);
        endcase
    endfunction

    function UInt#(3) indexOfFirstTrue(Vector#(5, Bool) bits);
        Tuple5#(Bool, Bool, Bool, Bool, Bool) tup = unpack(pack(bits));
        case (tup) matches
            { True,    .*,    .*,    .*,    .* } : return 0;
            { False, True,    .*,    .*,    .* } : return 1;
            { False, False, True,    .*,    .* } : return 2;
            { False, False, False, True,    .* } : return 3;
            { False, False, False, False, True } : return 4;
            default : return 7;
        endcase
    endfunction

    function UInt#(3) indexOfSecondTrue(Vector#(5, Bool) bits);
        Tuple5#(Bool, Bool, Bool, Bool, Bool) tup = unpack(pack(bits));
        case (tup) matches
            { True, True,    .*,    .*,    .* } : return 1;

            { False, True, True,    .*,    .* } : return 2;
            { True, False, True,    .*,    .* } : return 2;
            
            { False, False, True, True,    .* } : return 3;
            { False, True, False, True,    .* } : return 3;
            { True, False, False, True,    .* } : return 3;

            { True, False, False, False, True } : return 4;
            { False, True, False, False, True } : return 4;
            { False, False, True, False, True } : return 4;
            { False, False, False, True, True } : return 4;

            default : return 7;
        endcase
    endfunction

    function UInt#(3) indexOfThirdTrue(Vector#(5, Bool) bits);
        Tuple5#(Bool, Bool, Bool, Bool, Bool) tup = unpack(pack(bits));
        case (tup) matches
            { True, True, True,    .*,    .* } : return 2;

            { True, True, False, True,    .* } : return 3;
            { True, False, True, True,    .* } : return 3;
            { False, True, True, True,    .* } : return 3;

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
        Tuple5#(Bool, Bool, Bool, Bool, Bool) tup = unpack(pack(bits));
        case (tup) matches
            { True, True, True, True,    .* } : return 3;

            { True, True, True, False, True } : return 4;
            { True, True, False, True, True } : return 4;
            { True, False, True, True, True } : return 4;
            { False, True, True, True, True } : return 4;
            
            default : return 7;
        endcase
    endfunction

    function UInt#(3) indexOfFifthTrue(Vector#(5, Bool) bits);
        Tuple5#(Bool, Bool, Bool, Bool, Bool) tup = unpack(pack(bits));
        case (tup) matches
            { True, True, True, True, True } : return 4;
            
            default : return 7;
        endcase
    endfunction

    function Maybe#(IOCapAxi_KeyManager2_KeyCache_RefCountOp#(n)) makeOp(Maybe#(KeyId) keyMaybe, Int#(n) change);
        case (keyMaybe) matches
            tagged Valid .key : return tagged Valid IOCapAxi_KeyManager2_KeyCache_RefCountOp {
                key: key,
                change: change
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

    (* no_implicit_conditions *)
    rule process_valve_ports;
        Vector#(5, IOCapAxi_KeyManager2_KeyCache_RefCountOp#(2)) packedVector = replicate(?);
        if (mimo.enqReadyN(5)) begin
            // We have enough space to process incrementWires, decrementWires, and we have space for the keyToStartRevoking.
            // TODO does order matter here? I think it does... make sure we don't prematurely count something as revoked if it's incremented on the same cycle that the request arrives...
            UInt#(3) count = 0;
            Vector#(5, Maybe#(IOCapAxi_KeyManager2_KeyCache_RefCountOp#(2))) items = vector(
                makeOp(incrementWires[0].wget(), 1),
                makeOp(incrementWires[1].wget(), 1),
                makeOp(decrementWires[0].wget(), -1),
                makeOp(decrementWires[1].wget(), -1),
                makeOp(keyState.keyToStartRevoking.wget(), 0)
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
        end else begin
            // TODO assert mimo.enqReadyN(1) == True
            // We always have enough space to enqueue the keyToStartRevoking
            if (makeOp(keyState.keyToStartRevoking.wget(), 0) matches tagged Valid .op) begin
                packedVector[0] = op;
                mimo.enq(1, packedVector);
            end
        end
    endrule

    (* fire_when_enabled *)
    rule refcount_forward_progress_start_op;
        mimo.deq(1);
        let op = mimo.first[0];

        keyRefcountBram.portA.request.put(BRAMRequest {
            write: False,
            responseOnWrite: False,
            address: op.key,
            datain: ?
        });

        rcOpInProgress.enq(op);
    endrule

    (* fire_when_enabled *)
    rule refcount_forward_progress_complete_op;
        rcOpInProgress.deq();
        let readRc <- keyRefcountBram.portA.response.get();
        let op = rcOpInProgress.first;

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
            let forwarded = rcWriteForwarding;
            // Add a new forwarding record for this ref at the front so it has highest priority.
            // Assume the forwarding vector is big enough that whatever gets shifted out will definitely be inside the BRAM by this point.
            let newForwarding = rotateR(forwarded);
            forwarded[0] = tuple2(tagged Valid (op.key), actualRc);
        end
        
        if (actualRc == 0) begin
            keyState.tryConfirmingRevokeKey(op.key);
        end
    endrule

    // Remember, the valve ports cannot enqueue anything unless there's at least 5 elements in the FIFO.
    function IOCapAxi_KeyManager2_RefCountPipe_ValveIfc buildValveInterface(Integer valveIdx) = interface IOCapAxi_KeyManager2_RefCountPipe_ValveIfc;
        // Used by the valve to report key ID transaction-starts to the KeyManager
        interface keyIncrementRefcountRequest = interface Sink;
            method canPut = mimo.enqReadyN(5);
            method Action put(KeyId id);
                incrementWires[valveIdx].wset(id);
            endmethod
        endinterface;
        // Used by the valve to report key ID transaction-ends to the KeyManager
        interface keyDecrementRefcountRequest = interface Sink;
            method canPut = mimo.enqReadyN(5);
            method Action put(KeyId id);
                decrementWires[valveIdx].wset(id);
            endmethod
        endinterface;
    endinterface;

    // Generate 0..1 valve interfaces
    interface valvePorts = genWith(buildValveInterface);
endmodule
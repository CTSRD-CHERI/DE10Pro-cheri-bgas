import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import IOCapAxi :: *;
import IOCapAxi_Flits :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;

// 4096 keys => 12 bit ID
typedef Bit#(128) Key;
typedef Bit#(12) KeyId;
typedef Bit#(3) KeyRefCount; // TODO figure out what this is based on how many transactions can be in progress concurrently

typedef union tagged {
    void Cleared;
    KeyRefCount InUse;
    KeyRefCount ClearPending;
} KeyState deriving (Bits, FShow);

interface IOCap_KeyManager_KeyState;
    // Request to transition the key from cleared -> valid, assuming the actual key data has been updated.
    // If the key is already valid at the start of the cycle, does nothing.
    // If the key is in the process of clearing at the start of the cycle, TODO raises error.
    // If the key is cleared at the start of the cycle, transitions it to valid at the end.
    method Action requestUnclear();
    // Request to use the given key.
    // If the key is cleared or in the process of clearing, returns False and has no effect.
    // If the key is still valid, returns True and increments the ref count.
    method ActionValue#(Bool) requestInc();
    // Note that the key was dropped and the refCount should be decremented.
    // If the key is cleared, has no effect.
    // If the key is in the process of clearing, decrements the ref count and if it becomes zero, clears the key.
    // If the key is stil valid, decrements the ref count.
    method Action requestDec();
    // Request for the key to be cleared.
    // If the key is cleared or in the process of clearing, has no effect.
    // If the key is still valid and at the end of the cycle (after processing requestInc and requestDec) the refCount is zero, clears the key.
    method Action requestClear();
    // Return True if the key is cleared.
    method ActionValue#(Bool) isClear();
endinterface

module mkIOCap_KeyManager_KeyState(IOCap_KeyManager_KeyState);
    Reg#(Bool) wantClear <- mkReg(True);
    Reg#(KeyRefCount) refCount <- mkReg(0);

    PulseWire requestedInc <- mkPulseWire;
    PulseWire requestedDec <- mkPulseWire;
    PulseWire requestedClear <- mkPulseWire;
    PulseWire requestedUnclear <- mkPulseWire;

    rule handleRequests;
        if (requestedClear && requestedUnclear) begin
            $error("Shouldn't request Clear and Unclear at the same time");
        end

        if (isClear()) begin
            // wantClear = True, refCount == 0
            // The key is cleared

            // Ignore requestedInc, requestedDec - they have no effect on cleared keys
            // Ignore requestedClear - it's already clear!
            if (requestedUnclear) begin
                wantClear <= False;
                // refcount is still 0
            end
        end else if (wantClear) begin
            // wantClear = True, refCount != 0
            // The key is in the process of clearing
            
            // Ignore requestedInc - should never be pulsed in this situation

            if (requestedDec) begin
                // We know the refCount != 0
                let newRefCount = refCount - 1;
                refCount <= newRefCount;
                // But it may now become zero, in which case it becomes cleared!
            end

            // Ignore requestedClear - has no effect, wantClear is already true

            if (requestedUnclear) begin
                $error("Called requestUnclear when the key is in the process of clearing!");
            end
        end else begin
            // wantClear = False, refCount != 0
            // The key is valid

            // Handle requestedInc, requestedDec
            let newRefCount = refCount;
            if (requestedInc && !requestedDec) begin
                if (refCount == maxBound) begin
                    // TODO signal an assert line somewhere
                    $error("Tried to increment refCount beyond max bound!");
                end else begin
                    newRefCount = refCount + 1;
                end
            end else if (!requestedInc && requestedDec) begin
                // refCount != 0 => can always decrement
                newRefCount = recCount - 1;
            end
            refCount <= newRefCount;

            // If requestedClear, set wantClear.
            // If newRefCount == 0, the key will become cleared on the next cycle.
            // If newRefCount != 0, the key will move into the clearing process on the next cycle.
            if (requestedClear) begin
                wantClear <= True;
            end

            // Ignore requestedUnclear - has no effect, the key is already valid

        end
    endrule

    method Action requestUnclear();
        requestedUnclear.send();
    endmethod
    
    method ActionValue#(Bool) requestInc();
        if (wantClear) begin
            return False;
        end else begin
            requestedInc.send();
            return True;
        end
    endmethod

    method Action requestDec();
        requestedDec.send();
    endmethod

    method Action requestClear();
        requestedClear.send();
    endmethod

    method ActionValue#(Bool) isClear();
        return refCount == 0 && wantClear;
    endmethod


endmodule

interface IOCap_KeyManager;
    // Takes a 0x2000 memory range.
    
    // There are 4096 keys.
    // Each key may be valid, cleared, or pending-clear.
    // In the cleared state, the key is not usable for IOCap validity checks.
    // In the valid state, othe Exposer can access the key and use it for IOCap validity checks.
    // In the pending-clear state, the Exposer cannot 

    // The status of those keys is readable at [0x1000, 0x1010, 0x1020... 0x1FF0].
    // If the status is 1, the key is valid, or it is pending-clear and still in use.
    //      Each key has a refCount, because the Exposer may be in the middle of using it when 

    // Writes to [0x0, 0x10, 0x20, 0x30, 0x40, up to 0x1000) set keys [#0, #1, #2, #3, #4... #4096).
    // These are only valid when the status is 0.

    // Reading [0, 0x1000) range doesn't do anything.
    // Reading [0x1000, 0x1010, 0x1020, up to 0x2000) starts a "key reset" for key #0, #1, etc..
    // All new transactions that arrive using a "resetting" key will be immediately rejected.
    // If there were any transactions still in progress using the given key, the read will return 0x1.
    // Once those transactions finish, the key will be marked as clear and reads to the address will return 0x0.
    // To synchronously clear a key, the CPU must poll the given address until it returns 0x0.
    // To asynchronously clear a key, the CPU can read the address once, then later come back and check it returns 0x0.
    // Writes to a key that hasn't been cleared will TODO fail? it may be insecure for the CPU to write a new key and immediately assume the memory previously associated with that key is free. Could delay the write response until the read would return 0x0?

    // method Action incrementKeyUsage(KeyId k);
    // // Called by an Exposer whenever a transaction completes using a given key.
    // // If that key is in the ClearPending state, and the reference count is decremented to zero, it will transition to the Cleared state.
    // // Should only be called once per cycle??
    // method Action decrementKeyUsage(KeyId k);

    // put()-d by an Exposer whenever a transaction begins processing that uses a given key.
    // Increments the reference count if the key exists and is in use.
    interface Sink#(KeyId) keyRequests;
    interface Source#(Tuple2#(KeyId, Maybe#(Key))) keyResponses;

    interface AXI4Lite_Slave#(13, 128, 0, 0, 0, 0, 0) hostFacingSlave;
endinterface

module mkSimpleIOCapKeyManager(IOCap_KeyManager);
    // Need a BRAM with key data
    // Set up the secret BRAM
    BRAM_Configure keysConfig = BRAM_Configure {
        memorySize: 0, // Inferred from the KeyId parameter to BRAM1Port below
        latency: 2, // (address is registered, data is too because this isn't latency sensitive)
        loadFormat: None, // tagged Hex "exhibition_validallcavs_secrets.memh",
        outFIFODepth: 3, // latency+2
        allowWriteResponseBypass: False // TODO check if this is fine
    };
    // TODO second BRAM port for clearing keys?
    BRAM1Port#(KeyId, Key) keys <- mkBRAM1Server(keysConfig);
    // Need a Vector of 4096 KeyStates
    Vector#(4096, IOCap_KeyManager_KeyState) keyStates <- replicateM(mkIOCap_KeyManager_KeyState);

    let axiShim <- mkAXI4LiteShimFF;

    let keyReqFF <- mkFIFOF;
    let keyRespFF <- mkFIFOF;

    // read requests handling (always answer with upper bits)
    rule read_req;
        let ar <- get (axiShim.master.ar);
        
        let rdata = 0;

        // Reads to anything other than [0x1000, 0x2000) are ignored
        if ((ar.araddr & 'h1000) == 1) begin
            // Actually transition capabilities... 
            // TODO fuck! if this rule triggers at the same time as increment/decrement there will be a conflict
            KeyId k = truncate(ar.araddr >> 4); // Memory map is byte-addressed, each secret key is 16 bytes = 4 address bits

            // Request that the keyState be cleared.
            keyStates[k].requestClear();
            // Send a response based on if the key is already cleared.
            // this will not be affected by the requestClear() call on the previous line,
            // so right now a CPU will always have to poll at least one? not good, TODO fix.
            if (keyStates[k].isClear()) begin
                rdata = 0;
            end else begin
                rdata = 1;
            end
        end else begin
            // TODO signal failure
            $error("IOCap - mkSimpleIOCapKeyManager - Read to invalid address")
        end

        axiShim.master.r.put (AXI4Lite_RFlit { rdata: rdata
                                            , rresp: OKAY // TODO should this always be?
                                            , ruser: ? });
    endrule

    // write requests handling (update the appropriate word of addrBits)
    rule write_req;
        let aw <- get (axiShim.master.aw);
        let w <- get (axiShim.master.w);

        // Writes to anything other than [0x0000, 0x1000) are ignored
        if ((aw.awaddr & 'h1000) == 0) begin
            // TODO how to behave if the key is still valid?
            // TODO Tell the BRAM to update the key with w.wdata
            // TODO Tell the keyState to un-clear
        end else begin
            // TODO signal failure
            $error("IOCap - mkSimpleIOCapKeyManager - Write to invalid address")
        end

        axiShim.master.b.put (AXI4Lite_BFlit { bresp: OKAY // TODO should this always be?
                                            , buser: ? });
    endrule

    rule key_req;
        keyReqFF.deq();
        let requestedKeyId = keyReqFF.first;
        // TODO shortcut if the key is already cleared and there isn't contention for the response port?
        keys.portA.request.put(BRAMRequest {
            write: False,
            responseOnWrite: False,
            address: requestedKeyId,
            datain: ?
        });
    endrule

    rule key_resp;
        let keyResp = 

    // TODO increment and decrement should put things on RWires, which are resolved all at once

    // method Action incrementKeyUsage(KeyId k);
    //     case (keyStates[k]) matches
    //         tagged InUse .refCount : begin
    //             if (refCount == maxBound) begin
    //                 // TODO signal failure
    //             end else begin
    //                 keyStates[k] <= tagged InUse (refCount + 1);
    //             end
    //         end

    //         tagged Cleared : keyStates[k] <= tagged Cleared; // TODO signal failure

    //         tagged ClearPending 0 : noAction; // TODO signal failure
    //         tagged ClearPending .refCount : begin
    //             if (refCount == maxBound) begin
    //                 // TODO signal failure
    //             end else begin
    //                 keyStates[k] <= tagged ClearPending (refCount + 1);
    //             end
    //         end
    //     endcase
    // endmethod

    // method Action decrementKeyUsage(KeyId k);
    //     case (keyStates[k]) matches
    //         tagged InUse .refCount : begin
    //             if (refCount > 0) begin
    //                 keyStates[k] <= tagged InUse (refCount - 1);
    //             end else begin
    //                 // TODO signal failure
    //             end
    //         end

    //         tagged Cleared : keyStates[k] <= tagged Cleared; // TODO signal failure

    //         tagged ClearPending 0 : noAction; // TODO signal failure
    //         tagged ClearPending 1 : keyStates[k] <= tagged Cleared;
    //         tagged ClearPending .n : keyStates[k] <= tagged ClearPending (n-1);
    //     endcase
    // endmethod

    interface keyRequests = toSink(keyReqFF);
    interface keyResponses = toSource(keyRespFF);

    interface hostFacingSlave = axiShim.slave;
endmodule

interface IOCapSingleExposer#(numeric type t_iocap_id, numeric type t_iocap_data );

    interface IOCapAXI4_Slave#(t_iocap_id, t_iocap_data) iocapsIn;

    interface AXI4_Master#(t_iocap_id, 64, t_iocap_data, 0, 0, 0, 0, 0) sanitizedOut;

    interface IOCap_KeyManager keyStore;
endinterface

interface AddressChannelCapUnwrapper#(type iocap_flit, type no_iocap_flit);
    interface Sink#(iocap_flit) in;
    interface Source#(AuthenticatedFlit#(no_iocap_flit)) out;
endinterface

module mkSimpleAddressChannelCapUnwrapper(AddressChannelCapUnwrapper#(iocap_flit, no_iocap_flit)) provisos (Bits#(AuthenticatedFlit#(no_iocap_flit), a__), Bits#(iocap_flit, b__), IOCapPackableFlit#(iocap_flit, no_iocap_flit), FShow#(AuthenticatedFlit#(no_iocap_flit)));
    FIFOF#(iocap_flit) inFlits <- mkFIFOF();
    FIFOF#(AuthenticatedFlit#(no_iocap_flit)) outFlits <- mkSizedBypassFIFOF(4); // TODO check FIFOF type

    Reg#(AuthenticatedFlit#(no_iocap_flit)) flitInProgress <- mkReg(unpack(0));

    Reg#(UInt#(2)) state <- mkReg(0);

    rule st0 if (state == 0);
        inFlits.deq();
        let startFlit = inFlits.first;
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(startFlit);
        if (spec matches tagged Start .flit) begin
            flitInProgress <= AuthenticatedFlit {
                flit: flit,
                cap: 0
            };
        end else begin
            $error("IOCap protocol error");
        end
        state <= 1;
    endrule

    rule st1 if (state == 1);
        inFlits.deq();
        let bitsFlit = inFlits.first;
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(bitsFlit);
        if (spec matches tagged CapBits1 .bits) begin
            flitInProgress <= AuthenticatedFlit {
                flit: flitInProgress.flit,
                cap: { 0, bits }
            };
        end else begin
            $error("IOCap protocol error");
        end
        state <= 2;
    endrule

    rule st2 if (state == 2);
        inFlits.deq();
        let bitsFlit = inFlits.first;
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(bitsFlit);
        if (spec matches tagged CapBits2 .bits) begin
            flitInProgress <= AuthenticatedFlit {
                flit: flitInProgress.flit,
                cap: { 0, bits, flitInProgress.cap[85:0] }
            };
        end else begin
            $error("IOCap protocol error");
        end
        state <= 3;
    endrule

    rule st3 if (state == 3);
        inFlits.deq();
        let bitsFlit = inFlits.first;
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(bitsFlit);
        if (spec matches tagged CapBits3 .bits) begin
            let authFlit = AuthenticatedFlit {
                flit: flitInProgress.flit,
                cap: { bits, flitInProgress.cap[171:0] }
            };
            $display("IOCap - Received auth flitpack ", fshow(authFlit));
            outFlits.enq(authFlit);
        end else begin
            $error("IOCap protocol error");
        end
        state <= 0;
    endrule

    interface in = toSink(inFlits);
    interface out = toSource(outFlits);
endmodule


module mkSimpleIOCapExposer(IOCapSingleExposer#(t_id, t_data));
    // This doesn't have any key storage or checking logic yet! It just receives IOCapAXI and converts it back to plain AXI.

    AddressChannelCapUnwrapper#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0)) aw <- mkSimpleAddressChannelCapUnwrapper;
    FIFOF#(AXI4_WFlit#(t_data, 0)) wff <- mkFIFOF;
    FIFOF#(AXI4_BFlit#(t_id, 0)) bff <- mkFIFOF;
    AddressChannelCapUnwrapper#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0)) ar <- mkSimpleAddressChannelCapUnwrapper;
    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) rff <- mkFIFOF;

    function t_flit stripCapFromAuthFlit(AuthenticatedFlit#(t_flit) authFlit) = authFlit.flit;

    IOCap_KeyManager keyStoreImpl <- mkSimpleIOCapKeyManager;

    interface iocapsIn = interface IOCapAXI4_Slave;
        interface axiSignals = interface AXI4_Slave;
            interface aw = toSink(aw.in);
            interface  w = toSink(wff);
            interface  b = toSource(bff);
            interface ar = toSink(ar.in);
            interface  r = toSource(rff);
        endinterface;
    endinterface;

    interface sanitizedOut = interface AXI4_Master;
        interface aw = mapSource(stripCapFromAuthFlit, aw.out);
        interface  w = toSource(wff);
        interface  b = toSink(bff);
        interface ar = mapSource(stripCapFromAuthFlit, ar.out);
        interface  r = toSink(rff);
    endinterface;

    interface keyStore = keyStoreImpl;
endmodule
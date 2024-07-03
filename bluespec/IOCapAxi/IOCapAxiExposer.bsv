import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import IOCapAxi :: *;
import IOCapAxi_Flits :: *;
import SourceSink :: *;


// 4096 keys => 12 bit ID
typedef Bit#(12) KeyId;
typedef Bit#(3) KeyRefCount; // TODO figure out what this is based on how many transactions can be in progress concurrently

typedef union tagged {
    void Cleared;
    KeyRefCount InUse;
    KeyRefCount ClearPending;
} KeyState deriving (Bits, FShow);

interface IOCap_KeyManager#(numeric type t_keystore_id);
    // Takes a 0x2000 range, 128bit (16 or 0x10 bytes) data path
    // Writes to [0x0, 0x10, 0x20, 0x30, 0x40, up to 0x1000) sets key [#0, #1, #2, #3, #4... #4096).
    // Non-aligned writes don't do anything.
    // Reading [0, 0x1000) range doesn't do anything.
    // Reading [0x1000, 0x1010, 0x1020, up to 0x2000) starts a "key reset" for key #0, #1, etc..
    // All new transactions that arrive using a "resetting" key will be immediately rejected.
    // If there were any transactions still in progress using the given key, the read will return 0x1.
    // Once those transactions finish, the key will be marked as clear and reads to the address will return 0x0.
    // To synchronously clear a key, the CPU must poll the given address until it returns 0x0.
    // To asynchronously clear a key, the CPU can read the address once, then later come back and check it returns 0x0.
    // Writes to a key that hasn't been cleared will TODO fail? it may be insecure for the CPU to write a new key and immediately assume the memory previously associated with that key is free. Could delay the write response until the read would return 0x0?
    //


    // Called by an Exposer whenever a transaction begins processing that uses a given key.
    method Action incrementKeyUsage(KeyId k);
    // Called by an Exposer whenever a transaction completes using a given key.
    // If that key is in the ClearPending state, and the reference count is decremented to zero, it will transition to the Cleared state.
    method Action decrementKeyUsage(KeyId k);
    
    interface AXI4_Slave#(t_keystore_id, TLog#('h2000), 128, 0, 0, 0, 0, 0) hostFacingSlave;
endinterface

// module mkSimpleIOCapKeyManager(IOCap_KeyManager#(t_keystore_id));
//     // Need a BRAM with key data
//     // Need a Vector of 4096 KeyStates
// endmodule

interface IOCapSingleExposer#(numeric type t_iocap_id, numeric type t_iocap_data /*, numeric type t_keystore_id */ );

    interface IOCapAXI4_Slave#(t_iocap_id, t_iocap_data) iocapsIn;

    interface AXI4_Master#(t_iocap_id, 64, t_iocap_data, 0, 0, 0, 0, 0) sanitizedOut;

    // TODO
    // interface IOCap_KeyManager#(t_keystore_id) keyStore;
endinterface

interface AddressChannelCapUnwrapper#(type iocap_flit, type no_iocap_flit);
    interface Sink#(iocap_flit) in;
    // TODO pass out AuthenticatedFlit so we can actually check it :)
    interface Source#(no_iocap_flit) out;
endinterface

module mkSimpleAddressChannelCapUnwrapper(AddressChannelCapUnwrapper#(iocap_flit, no_iocap_flit)) provisos (Bits#(AuthenticatedFlit#(no_iocap_flit), a__), Bits#(iocap_flit, b__), IOCapPackableFlit#(iocap_flit, no_iocap_flit), FShow#(AuthenticatedFlit#(no_iocap_flit)));
    FIFOF#(iocap_flit) inFlits <- mkFIFOF();
    FIFOF#(no_iocap_flit) outFlits <- mkSizedBypassFIFOF(4); // TODO check FIFOF type

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
            let auth_flit = AuthenticatedFlit {
                flit: flitInProgress.flit,
                cap: { bits, flitInProgress.cap[171:0] }
            };
            $display("IOCap - Received auth flitpack ", fshow(auth_flit));
            outFlits.enq(flitInProgress.flit);
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
        interface aw = toSource(aw.out);
        interface  w = toSource(wff);
        interface  b = toSink(bff);
        interface ar = toSource(ar.out);
        interface  r = toSink(rff);
    endinterface;

endmodule
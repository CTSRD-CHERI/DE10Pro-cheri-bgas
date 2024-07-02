import FIFOF :: *;
import IOCapAxi :: *;
import IOCapAxiWindow :: *;
import SourceSink :: *;

// interface IOCap_KeyManager#(numeric type n_keys);
//     // Takes a 0x1000 range, 128bit (16 or 0x10 bytes) data path
//     // Reading always returns 0.
//     // Writes to [0x0, 0x10, 0x20, 0x30, 0x40, up to 0x900) sets key [#0, #1, #2, #3, #4... #143).
//     // Non-aligned writes don't do anything.
//     //
//     // TODO overwriting 
//     // TODO how to signal to the CPU that we've reset/forgotten a key? Send an interrupt?

//     interface AXI4Lite_Slave#(...) keyRequests
// endinterface

interface IOCapSingleExposer#(numeric type t_id, numeric type t_data);
    

    interface IOCapAXI4_Slave#(t_id, t_data) iocapsIn;

    interface AXI4_Master#(t_id, 64, t_data, 0, 0, 0, 0, 0) sanitizedOut;

    // TODO this is for later!
    // interface IOCap_KeyManager keys;   

endinterface

interface AddressChannelCapUnwrapper#(type iocap_flit, type no_iocap_flit);
    interface Sink#(iocap_flit) in;
    // TODO pass out AuthenticatedFlit so we can actually check it :)
    interface Source#(no_iocap_flit) out;
endinterface

module mkSimpleAddressChannelCapUnwrapper(AddressChannelCapUnwrapper#(iocap_flit, no_iocap_flit)) provisos (Bits#(AuthenticatedFlit#(no_iocap_flit), a__), Bits#(iocap_flit, b__));
    FIFOF#(iocap_flit) inFlits <- mkFIFOF();
    FIFOF#(AuthenticatedFlit#(no_iocap_flit)) outFlits <- mkSizedBypassFIFOF(4); // TODO check FIFOF type

    Reg#(AuthenticatedFlit#(no_iocap_flit)) flitInProgress <- mkReg(unpack(0));

    rule st0 if (state == 0);
        let startFlit <- inFlits.deq();
        let spec = unpackSpec(startFlit);
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
        let bitsFlit <- inFlits.deq();
        let spec = unpackSpec(bitsFlit);
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
        let bitsFlit <- inFlits.deq();
        let spec = unpackSpec(bitsFlit);
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
        let bitsFlit <- inFlits.deq();
        let spec = unpackSpec(bitsFlit);
        if (spec matches tagged CapBits3 .bits) begin
            let auth_flit = AuthenticatedFlit {
                flit: flitInProgress.flit,
                cap: { bits, flitInProgress.cap[171:0] }
            };
            $display("IOCap - Received auth flit ", fshow(auth_flit));
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

    interface iocapsIn = interface AXI4_Slave;
        interface aw = toSink(aw.in);
        interface  w = toSink(wff);
        interface  b = toSource(bff);
        interface ar = toSink(ar.in);
        interface  r = toSource(rff);
    endinterface;

    interface sanitizedOut = interface AXI4_Master;
        interface aw = toSource(aw.out);
        interface  w = toSource(wff);
        interface  b = toSink(bff);
        interface ar = toSource(ar.out);
        interface  r = toSink(rff);
    endinterface;

endmodule
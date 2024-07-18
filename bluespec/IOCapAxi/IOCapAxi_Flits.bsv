import BlueAXI4 :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import SourceSink :: *;

import IOCapAxi_Types :: *;
import Cap2024_02 :: *;

/*

The IOCapAxi protocol adds support to the AW and AR channels to attach 256-bit metadata
to each read and write transaction without excessively extending the user bits.
Each transaction on these channels may use one flit as normal (for accesses without metadata)
or four flits (with metadata attached).
Four-flit transactions are known upfront to be four-flit, making flow control easy.
The first of a four-flit transaction is the normal AW/AR flit, and the next three communicate
86 metadata-bits each.
Three additional USER bits are attached to each channel: one to indicate if the flit 
uses metadata, and two to indicate the sequence number in a four-flit transaction.

The IOCapAxi convention for attaching 256-bit cryptographically authenticated capabilities
is to treat the 128-bit capability text as the bottom 128-bits of the metadata,
and the 128-bit capability signature as the top 128-bits.

TODO: The metadata bits are packed in the opposite order to the field bits
e.g. we pack awaddr with lower bits than awlen, even though Bluespec packs awaddr into the more-significant-bits.
TODO reverse this order
*/

typedef union tagged {
    // TODO no_iocap_flit Unauthenticated;
    no_iocap_flit Start;
    Bit#(86) CapBits1;
    Bit#(86) CapBits2;
    Bit#(84) CapBits3;
} IOCapFlitSpec#(type no_iocap_flit) deriving (Bits, FShow);

typeclass IOCapPackableFlit#(type iocap_flit, type no_iocap_flit);
    function iocap_flit packSpec(IOCapFlitSpec#(no_iocap_flit) x);
    function IOCapFlitSpec#(no_iocap_flit) unpackSpec(iocap_flit x);
endtypeclass

instance IOCapPackableFlit#(
    // iocap_flit
    AXI4_AWFlit#(t_id, 64 /* t_addr */, 3 /* t_user */),
    // no_iocap_flit
    AXI4_AWFlit#(t_id, 64 /* t_addr */, 0 /* t_user */)
);
    function AXI4_AWFlit#(t_id, 64, 3) packSpec(IOCapFlitSpec#(AXI4_AWFlit#(t_id, 64, 0)) spec);
        case (spec) matches
            { tagged Start .x } : return AXI4_AWFlit {
                awid: x.awid
                , awaddr: x.awaddr
                , awlen: x.awlen
                , awsize: x.awsize
                , awburst: x.awburst
                , awlock: x.awlock
                , awcache: x.awcache
                , awprot: x.awprot
                , awqos: x.awqos
                , awregion: x.awregion
                , awuser: pack(IOCapAXI4_AddrUserBits{
                    start: True
                    , flitnum: 0
                })
            };
            { tagged CapBits1 .bits } : return AXI4_AWFlit {
                  awid: ?
                , awaddr: bits[63:0]
                , awlen: bits[71:64]
                , awsize: unpack(bits[74:72])
                , awburst: unpack(bits[76:75])
                , awlock: unpack(bits[77])
                , awcache: unpack(bits[81:78])
                , awprot: unpack(bits[84:82])
                , awqos: { 3'h0, bits[85] }
                , awregion: ?
                , awuser: pack(IOCapAXI4_AddrUserBits{
                      start: False
                    , flitnum: 1
                })
            };
            { tagged CapBits2 .bits } : return AXI4_AWFlit {
                  awid: ?
                , awaddr: bits[63:0]
                , awlen: bits[71:64]
                , awsize: unpack(bits[74:72])
                , awburst: unpack(bits[76:75])
                , awlock: unpack(bits[77])
                , awcache: unpack(bits[81:78])
                , awprot: unpack(bits[84:82])
                , awqos: { 3'h0, bits[85] }
                , awregion: ?
                , awuser: pack(IOCapAXI4_AddrUserBits{
                      start: False
                    , flitnum: 2
                })
            };
            { tagged CapBits3 .bits } : return AXI4_AWFlit {
                  awid: ?
                , awaddr: bits[63:0]
                , awlen: bits[71:64]
                , awsize: unpack(bits[74:72])
                , awburst: unpack(bits[76:75])
                , awlock: unpack(bits[77])
                , awcache: unpack(bits[81:78])
                , awprot: unpack({ 1'b0, bits[83:82] })
                , awqos: ?
                , awregion: ?
                , awuser: pack(IOCapAXI4_AddrUserBits{
                      start: False
                    , flitnum: 3
                })
            };
        endcase
    endfunction

    function IOCapFlitSpec#(AXI4_AWFlit#(t_id, 64, 0)) unpackSpec(AXI4_AWFlit#(t_id, 64, 3) x);
        case (unpack(x.awuser)) matches 
            IOCapAXI4_AddrUserBits { start: True, flitnum: 0 } : return tagged Start AXI4_AWFlit {
                  awid: x.awid
                , awaddr: x.awaddr
                , awlen: x.awlen
                , awsize: x.awsize
                , awburst: x.awburst
                , awlock: x.awlock
                , awcache: x.awcache
                , awprot: x.awprot
                , awqos: x.awqos
                , awregion: x.awregion
                , awuser: ?
            };
            // TODO get the ordering right...
            IOCapAXI4_AddrUserBits { start: False, flitnum: 1 } : return tagged CapBits1 ({ pack(x.awqos)[1], pack(x.awprot), pack(x.awcache), pack(x.awlock), pack(x.awburst), pack(x.awsize), x.awlen, x.awaddr });
            IOCapAXI4_AddrUserBits { start: False, flitnum: 2 } : return tagged CapBits2 ({ pack(x.awqos)[1], pack(x.awprot), pack(x.awcache), pack(x.awlock), pack(x.awburst), pack(x.awsize), x.awlen, x.awaddr });
            IOCapAXI4_AddrUserBits { start: False, flitnum: 3 } : return tagged CapBits3 ({ pack(x.awprot)[1:0], pack(x.awcache), pack(x.awlock), pack(x.awburst), pack(x.awsize), x.awlen, x.awaddr });
            default: return ?;
        endcase
    endfunction
endinstance

instance IOCapPackableFlit#(
    // iocap_flit
    AXI4_ARFlit#(t_id, 64 /* t_addr */, 3 /* t_user */),
    // no_iocap_flit
    AXI4_ARFlit#(t_id, 64 /* t_addr */, 0 /* t_user */)
);
    function AXI4_ARFlit#(t_id, 64, 3) packSpec(IOCapFlitSpec#(AXI4_ARFlit#(t_id, 64, 0)) spec);
        case (spec) matches
            { tagged Start .x } : return AXI4_ARFlit {
                arid: x.arid
                , araddr: x.araddr
                , arlen: x.arlen
                , arsize: x.arsize
                , arburst: x.arburst
                , arlock: x.arlock
                , arcache: x.arcache
                , arprot: x.arprot
                , arqos: x.arqos
                , arregion: x.arregion
                , aruser: pack(IOCapAXI4_AddrUserBits{
                    start: True
                    , flitnum: 0
                })
            };
            { tagged CapBits1 .bits } : return AXI4_ARFlit {
                  arid: ?
                , araddr: bits[63:0]
                , arlen: bits[71:64]
                , arsize: unpack(bits[74:72])
                , arburst: unpack(bits[76:75])
                , arlock: unpack(bits[77])
                , arcache: unpack(bits[81:78])
                , arprot: unpack(bits[84:82])
                , arqos: { 3'h0, bits[85] }
                , arregion: ?
                , aruser: pack(IOCapAXI4_AddrUserBits{
                      start: False
                    , flitnum: 1
                })
            };
            { tagged CapBits2 .bits } : return AXI4_ARFlit {
                  arid: ?
                , araddr: bits[63:0]
                , arlen: bits[71:64]
                , arsize: unpack(bits[74:72])
                , arburst: unpack(bits[76:75])
                , arlock: unpack(bits[77])
                , arcache: unpack(bits[81:78])
                , arprot: unpack(bits[84:82])
                , arqos: { 3'h0, bits[85] }
                , arregion: ?
                , aruser: pack(IOCapAXI4_AddrUserBits{
                      start: False
                    , flitnum: 2
                })
            };
            { tagged CapBits3 .bits } : return AXI4_ARFlit {
                  arid: ?
                , araddr: bits[63:0]
                , arlen: bits[71:64]
                , arsize: unpack(bits[74:72])
                , arburst: unpack(bits[76:75])
                , arlock: unpack(bits[77])
                , arcache: unpack(bits[81:78])
                , arprot: unpack({ 1'b0, bits[83:82] })
                , arqos: ?
                , arregion: ?
                , aruser: pack(IOCapAXI4_AddrUserBits{
                      start: False
                    , flitnum: 3
                })
            };
        endcase
    endfunction

    function IOCapFlitSpec#(AXI4_ARFlit#(t_id, 64, 0)) unpackSpec(AXI4_ARFlit#(t_id, 64, 3) x);
        case (unpack(x.aruser)) matches 
            IOCapAXI4_AddrUserBits { start: True, flitnum: 0 } : return tagged Start AXI4_ARFlit {
                  arid: x.arid
                , araddr: x.araddr
                , arlen: x.arlen
                , arsize: x.arsize
                , arburst: x.arburst
                , arlock: x.arlock
                , arcache: x.arcache
                , arprot: x.arprot
                , arqos: x.arqos
                , arregion: x.arregion
                , aruser: ?
            };
            // TODO get the ordering right...
            IOCapAXI4_AddrUserBits { start: False, flitnum: 1 } : return tagged CapBits1 ({ pack(x.arqos)[1], pack(x.arprot), pack(x.arcache), pack(x.arlock), pack(x.arburst), pack(x.arsize), x.arlen, x.araddr });
            IOCapAXI4_AddrUserBits { start: False, flitnum: 2 } : return tagged CapBits2 ({ pack(x.arqos)[1], pack(x.arprot), pack(x.arcache), pack(x.arlock), pack(x.arburst), pack(x.arsize), x.arlen, x.araddr });
            IOCapAXI4_AddrUserBits { start: False, flitnum: 3 } : return tagged CapBits3 ({ pack(x.arprot)[1:0], pack(x.arcache), pack(x.arlock), pack(x.arburst), pack(x.arsize), x.arlen, x.araddr });
            default: return ?;
        endcase
    endfunction
endinstance

typedef struct {
    no_iocap_flit flit;
    Cap2024_02 cap;
    Bit#(128) sig;
} AuthenticatedFlit#(type no_iocap_flit) deriving (Bits, FShow);

interface AddressChannelCapWrapper#(type iocap_flit, type no_iocap_flit);
    interface Sink#(AuthenticatedFlit#(no_iocap_flit)) in;
    interface Source#(iocap_flit) out;
endinterface 

interface AddressChannelCapUnwrapper#(type iocap_flit, type no_iocap_flit);
    interface Sink#(iocap_flit) in;
    // todo rename to authflits?
    interface Source#(AuthenticatedFlit#(no_iocap_flit)) out;
endinterface

module mkSimpleAddressChannelCapWrapper(AddressChannelCapWrapper#(iocap_flit, no_iocap_flit)) provisos (Bits#(AuthenticatedFlit#(no_iocap_flit), a__), Bits#(iocap_flit, b__), IOCapPackableFlit#(iocap_flit, no_iocap_flit), FShow#(AuthenticatedFlit#(no_iocap_flit)));
    FIFOF#(AuthenticatedFlit#(no_iocap_flit)) inFlits <- mkFIFOF();
    FIFOF#(iocap_flit) outFlits <- mkSizedBypassFIFOF(4); // TODO check FIFOF type

    Reg#(UInt#(2)) state <- mkReg(0);
    Reg#(Bit#(256)) cap <- mkReg(0);

    rule st0 if (state == 0);
        let startFlitAndCap = inFlits.first;
        inFlits.deq();
        $display("IOCap - Sending auth flitpack ", fshow(startFlitAndCap));
        outFlits.enq(packSpec(tagged Start (startFlitAndCap.flit)));
        state <= 1;
        cap <= { startFlitAndCap.sig, pack(startFlitAndCap.cap) };
    endrule

    rule st1 if (state == 1);
        IOCapFlitSpec#(no_iocap_flit) bits = tagged CapBits1 cap[85:0];
        outFlits.enq(packSpec(bits));
        state <= 2;
    endrule

    rule st2 if (state == 2);
        IOCapFlitSpec#(no_iocap_flit) bits = tagged CapBits2 cap[171:86];
        outFlits.enq(packSpec(bits));
        state <= 3;
    endrule

    rule st3 if (state == 3);
        IOCapFlitSpec#(no_iocap_flit) bits = tagged CapBits3 cap[255:172];
        outFlits.enq(packSpec(bits));
        state <= 0;
    endrule

    interface in = toSink(inFlits);
    interface out = toSource(outFlits);
endmodule

module mkSimpleAddressChannelCapUnwrapper(AddressChannelCapUnwrapper#(iocap_flit, no_iocap_flit)) provisos (
    // Must be able to pack AuthenticatedFlit into a register
    Bits#(AuthenticatedFlit#(no_iocap_flit), a__),
    // Must be able to pack the flit-with-iocap-bits into a register
    Bits#(iocap_flit, b__),
    // Must be able to pack a flit-with-no-iocap-bits into an iocap-flit, with the user bits for signalling extra capability data
    IOCapPackableFlit#(iocap_flit, no_iocap_flit),
    // Must be able to print the IOCap flit
    FShow#(iocap_flit),
    // Must be able to print the authenticated flit
    FShow#(AuthenticatedFlit#(no_iocap_flit))
);
    FIFOF#(iocap_flit) inFlits <- mkFIFOF();
    FIFOF#(AuthenticatedFlit#(no_iocap_flit)) outFlits <- mkSizedBypassFIFOF(4); // TODO check FIFOF type

    Reg#(Tuple2#(no_iocap_flit, Bit#(256))) flitInProgress <- mkReg(unpack(0));

    Reg#(UInt#(2)) state <- mkReg(0);

    rule st0 if (state == 0);
        inFlits.deq();
        let startFlit = inFlits.first;
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(startFlit);
        if (spec matches tagged Start .flit) begin
            flitInProgress <= tuple2(flit, 0);
        end else begin
            $error("IOCap protocol error ", fshow(pack(startFlit)));
        end
        state <= 1;
    endrule

    rule st1 if (state == 1);
        inFlits.deq();
        let bitsFlit = inFlits.first;
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(bitsFlit);
        if (spec matches tagged CapBits1 .bits) begin
            let flit = tpl_1(flitInProgress);
            flitInProgress <= tuple2(
                flit,
                { 0, bits }
            );
        end else begin
            $error("IOCap protocol error ", fshow(bitsFlit));
        end
        state <= 2;
    endrule

    rule st2 if (state == 2);
        inFlits.deq();
        let bitsFlit = inFlits.first;
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(bitsFlit);
        if (spec matches tagged CapBits2 .bits) begin
            let flit = tpl_1(flitInProgress);
            let bitsInProgress = tpl_2(flitInProgress);
            flitInProgress <= tuple2(
                flit,
                { 0, bits, bitsInProgress[85:0] }
            );
        end else begin
            $error("IOCap protocol error ", fshow(bitsFlit));
        end
        state <= 3;
    endrule

    rule st3 if (state == 3);
        inFlits.deq();
        let bitsFlit = inFlits.first;
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(bitsFlit);
        if (spec matches tagged CapBits3 .bits) begin
            let flit = tpl_1(flitInProgress);
            let bitsInProgress = tpl_2(flitInProgress);
            let combinedBits = { bits, bitsInProgress[171:0] };
            let authFlit = AuthenticatedFlit {
                flit: flit,
                cap: unpack(combinedBits[127:0]),
                sig: combinedBits[255:128]
            };
            $display("IOCap - Received auth flitpack ", fshow(authFlit));
            outFlits.enq(authFlit);
        end else begin
            $error("IOCap protocol error ", fshow(bitsFlit));
        end
        state <= 0;
    endrule

    interface in = toSink(inFlits);
    interface out = toSource(outFlits);
endmodule
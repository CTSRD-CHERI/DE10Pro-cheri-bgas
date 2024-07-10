import BlueAXI4 :: *;
import IOCapAxi :: *;
import Cap2024_02 :: *;

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
import BlueAXI4 :: *;
import Connectable :: *;
import IOCapAxi :: *;
import IOCapAxi_Flits :: *;
import IOCapAxiExposer :: *;
import AxiWindow :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import SourceSink :: *;

typedef struct {
    Bit#(64)  windowAddr;
    Bit#(256) capability;
} WindowData deriving (Bits, FShow);

interface AddressChannelCapWrapper#(type iocap_flit, type no_iocap_flit);
    interface Sink#(AuthenticatedFlit#(no_iocap_flit)) in;
    interface Source#(iocap_flit) out;
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
        cap <= startFlitAndCap.cap;
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

module mkSimpleIOCapWindow(AxiWindow#(
    // Window subordinate port (AXI4Lite)
      t_window_ctrl_addr
    , t_window_ctrl_data
    , t_window_ctrl_awuser
    , t_window_ctrl_wuser
    , t_window_ctrl_buser
    , t_window_ctrl_aruser
    , t_window_ctrl_ruser
    // Access subordinate port (AXI4)
    , t_pre_window_id
    , t_pre_window_addr
    , t_pre_window_data
    , 0 //t_pre_window_awuser
    , 0 //t_pre_window_wuser
    , 0 //t_pre_window_buser
    , 0 //t_pre_window_aruser
    , 0 //t_pre_window_ruser
    // Post-window address widening
    , 64 // t_post_window_addr
    , 3 //t_post_window_awuser
    , 0 //t_post_window_wuser
    , 0 //t_post_window_buser
    , 3 //t_post_window_aruser
    , 0 //t_post_window_ruser
)) provisos (
    Alias #( t_window_ctrl
           , AXI4Lite_Slave #(
               t_window_ctrl_addr, t_window_ctrl_data
             , t_window_ctrl_awuser, t_window_ctrl_wuser, t_window_ctrl_buser
             , t_window_ctrl_aruser, t_window_ctrl_ruser ))
    // The capability we authenticate access with is 256 bits (128 text + 128 signature) plus some other data
    , Bits#(WindowData, t_window_ctrl_len)
    // the t_pre_window_addr may be smaller than 64-bits and get zero extended
    , Add#(a__, t_pre_window_addr, 64)
    // Make sure the windowCtrl can evenly represent the capability with the windowCtrl data words
    , Mul#(TDiv#(t_window_ctrl_len, t_window_ctrl_data), t_window_ctrl_data, t_window_ctrl_len) // Evenly divisible
    // Make sure the windowCtrl has enough address bits to address every word of the t_window_ctrl_len
    // i.e. that t_window_ctrl_addr >= log2(number of windowCtrl data words in t_window_ctrl_len)
    , Add#(b__, TLog#(TDiv#(t_window_ctrl_len, t_window_ctrl_data)), t_window_ctrl_addr)
);

    // Expose an AXI4Lite subordinate which read/writes a register we can read
    // That register holds the window ctrl data
    Tuple2 #(t_window_ctrl, ReadOnly #(Bit #(t_window_ctrl_len))) windowCtrlIfcs <- mkAXI4Lite_SubReg (pack(WindowData {
        windowAddr: 0,
        capability: 256'h01234567_89abcdef_01234567_89abcdef_fdecba98_76543210_fdecba98_76543210
    }));
    match {.windowCtrlIfc, .windowCtrlBits} = windowCtrlIfcs;

    // Function to generate the post-window address from the pre-window address
    function mapAddr (preWindowAddr);
        WindowData windowCtrl = unpack(windowCtrlBits);
        return windowCtrl.windowAddr | zeroExtend(preWindowAddr);
    endfunction
    

    FIFOF#(AXI4_AWFlit#(t_pre_window_id, t_pre_window_addr, 0)) awff_preWindow <- mkBypassFIFOF();
    FIFOF#(AXI4_WFlit#(t_pre_window_data, 0)) wff <- mkFIFOF;
    FIFOF#(AXI4_BFlit#(t_pre_window_id, 0)) bff <- mkFIFOF;
    FIFOF#(AXI4_ARFlit#(t_pre_window_id, t_pre_window_addr, 0)) arff_preWindow <- mkBypassFIFOF();
    FIFOF#(AXI4_RFlit#(t_pre_window_id, t_pre_window_data, 0)) rff <- mkFIFOF;
    
    AddressChannelCapWrapper#(AXI4_AWFlit#(t_pre_window_id, 64, 3), AXI4_AWFlit#(t_pre_window_id, 64, 0)) aw <- mkSimpleAddressChannelCapWrapper;
    AddressChannelCapWrapper#(AXI4_ARFlit#(t_pre_window_id, 64, 3), AXI4_ARFlit#(t_pre_window_id, 64, 0)) ar <- mkSimpleAddressChannelCapWrapper;

    rule mapAWAddrOnPreWindowFlit;
        awff_preWindow.deq();
        let x = awff_preWindow.first;
        WindowData window = unpack(windowCtrlBits);
        aw.in.put(AuthenticatedFlit {
            flit: AXI4_AWFlit {
                awid: x.awid
                , awaddr: mapAddr(x.awaddr)
                , awlen: x.awlen
                , awsize: x.awsize
                , awburst: x.awburst
                , awlock: x.awlock
                , awcache: x.awcache
                , awprot: x.awprot
                , awqos: x.awqos
                , awregion: x.awregion
                , awuser: ?
            },
            cap: window.capability
        });
    endrule

    rule mapARAddrOnPreWindowFlit;
        arff_preWindow.deq();
        let x = arff_preWindow.first;
        WindowData window = unpack(windowCtrlBits);
        ar.in.put(AuthenticatedFlit {
            flit: AXI4_ARFlit {
                arid: x.arid
                , araddr: mapAddr(x.araddr)
                , arlen: x.arlen
                , arsize: x.arsize
                , arburst: x.arburst
                , arlock: x.arlock
                , arcache: x.arcache
                , arprot: x.arprot
                , arqos: x.arqos
                , arregion: x.arregion
                , aruser: ?
            },
            cap: window.capability
        });
    endrule

    interface windowCtrl = windowCtrlIfc;

    interface preWindow = interface AXI4_Slave;
        interface aw = toSink(awff_preWindow);
        interface  w = toSink(wff);
        interface  b = toSource(bff);
        interface ar = toSink(arff_preWindow);
        interface  r = toSource(rff);
    endinterface;

    interface postWindow = interface AXI4_Master;
        interface aw = toSource(aw.out);
        interface  w = toSource(wff);
        interface  b = toSink(bff);
        interface ar = toSource(ar.out);
        interface  r = toSink(rff);
    endinterface;

endmodule

module mkSimpleInternalIOCapWindow(AxiWindow#(
    // Window subordinate port (AXI4Lite)
      t_window_ctrl_addr
    , t_window_ctrl_data
    , t_window_ctrl_awuser
    , t_window_ctrl_wuser
    , t_window_ctrl_buser
    , t_window_ctrl_aruser
    , t_window_ctrl_ruser
    // Access subordinate port (AXI4)
    , t_pre_window_id
    , t_pre_window_addr
    , t_pre_window_data
    , 0 //t_pre_window_awuser
    , 0 //t_pre_window_wuser
    , 0 //t_pre_window_buser
    , 0 //t_pre_window_aruser
    , 0 //t_pre_window_ruser
    // Post-window address widening
    , 64 // t_post_window_addr
    , 0 //t_post_window_awuser
    , 0 //t_post_window_wuser
    , 0 //t_post_window_buser
    , 0 //t_post_window_aruser
    , 0 //t_post_window_ruser
)) provisos (
    Alias #( t_window_ctrl
           , AXI4Lite_Slave #(
               t_window_ctrl_addr, t_window_ctrl_data
             , t_window_ctrl_awuser, t_window_ctrl_wuser, t_window_ctrl_buser
             , t_window_ctrl_aruser, t_window_ctrl_ruser ))
    // The capability we authenticate access with is 256 bits (128 text + 128 signature) plus some other data
    , Bits#(WindowData, t_window_ctrl_len)
    // the t_pre_window_addr may be smaller than 64-bits and get zero extended
    , Add#(a__, t_pre_window_addr, 64)
    // Make sure the windowCtrl can evenly represent the capability with the windowCtrl data words
    , Mul#(TDiv#(t_window_ctrl_len, t_window_ctrl_data), t_window_ctrl_data, t_window_ctrl_len) // Evenly divisible
    // Make sure the windowCtrl has enough address bits to address every word of the t_window_ctrl_len
    // i.e. that t_window_ctrl_addr >= log2(number of windowCtrl data words in t_window_ctrl_len)
    , Add#(b__, TLog#(TDiv#(t_window_ctrl_len, t_window_ctrl_data)), t_window_ctrl_addr)
);
    
    AxiWindow#(
            // Window subordinate port (AXI4Lite)
      t_window_ctrl_addr
      , t_window_ctrl_data
      , t_window_ctrl_awuser
      , t_window_ctrl_wuser
      , t_window_ctrl_buser
      , t_window_ctrl_aruser
      , t_window_ctrl_ruser
      // Access subordinate port (AXI4)
      , t_pre_window_id
      , t_pre_window_addr
      , t_pre_window_data
      , 0 //t_pre_window_awuser
      , 0 //t_pre_window_wuser
      , 0 //t_pre_window_buser
      , 0 //t_pre_window_aruser
      , 0 //t_pre_window_ruser
      // Post-window address widening - includes iocap user data
      , 64 // t_post_window_addr
      , 3 //t_post_window_awuser
      , 0 //t_post_window_wuser
      , 0 //t_post_window_buser
      , 3 //t_post_window_aruser
      , 0 //t_post_window_ruser
    ) window <- mkSimpleIOCapWindow;

    IOCapSingleExposer#(t_pre_window_id, t_pre_window_data) exposer <- mkSimpleIOCapExposer;

    mkConnection(window.postWindow, exposer.iocapsIn.axiSignals);

    interface windowCtrl = window.windowCtrl;
    interface preWindow = window.preWindow;
    interface postWindow = exposer.sanitizedOut;
endmodule
package CHERI_BGAS_Bridge;

import AXI4 :: *;
import AXI4Stream :: *;
import Vector :: *;
import Connectable :: *;
import BlueBasics :: *;

interface CHERI_BGAS_Bridge_Ifc #( numeric type mid_X
                                 , numeric type sid_X
                                 , numeric type mid_Y
                                 , numeric type sid_Y
                                 , numeric type addr_
                                 , numeric type data_
                                 , numeric type awuser_
                                 , numeric type wuser_
                                 , numeric type buser_
                                 , numeric type aruser_
                                 , numeric type ruser_
                                 , numeric type sId_
                                 , numeric type sData_
                                 , numeric type sDest_
                                 , numeric type sUser_ );
  interface AXI4_Master #( mid_X, addr_, data_
                         , awuser_, wuser_, buser_, aruser_, ruser_) manager;
  interface AXI4_Slave #( sid_X, addr_, data_
                        , awuser_, wuser_, buser_, aruser_, ruser_) subordinate;
  interface AXI4Stream_Master #(sId_, sData_, sDest_, sUser_) tx;
  interface AXI4Stream_Slave #(sId_, sData_, sDest_, sUser_) rx;
endinterface

// XXX NOTE:
// Currently this module simply forwards all the AXI4 channels in parallel in
// the sData field of the tx and rx AXI4Streams.
// The plan is to later multiplex these channels and limit the size of the sData
// field in the AXI4Streams, and make use of the appropriate fields to enable
// proper routing.
// It receives a function to turn arriving "global" requests into "local"
// requests
module mkCHERI_BGAS_Bridge #(function mngr_port_t toLocal (mngr_port_t m))
  (CHERI_BGAS_Bridge_Ifc #( mid_X, sid_X, mid_Y, sid_Y, addr_, data_
                          , awuser_, wuser_, buser_, aruser_, ruser_
                          , sId_, sData_, sDest_, sUser_))
  provisos ( // Flit types going over tx stream
             Alias #(AXI4_AWFlit #(sid_Y, addr_, awuser_), tx_awflit)
           , Alias #(AXI4_WFlit  #(data_, wuser_)        , tx_wflit)
           , Alias #(AXI4_BFlit  #(mid_Y, buser_)        , tx_bflit)
           , Alias #(AXI4_ARFlit #(sid_Y, addr_, aruser_), tx_arflit)
           , Alias #(AXI4_RFlit  #(mid_Y, data_, ruser_) , tx_rflit)
             // Flit types going over rx stream
           , Alias #(AXI4_AWFlit #(sid_Y, addr_, awuser_), rx_awflit)
           , Alias #(AXI4_WFlit  #(data_, wuser_)        , rx_wflit)
           , Alias #(AXI4_BFlit  #(mid_Y, buser_)        , rx_bflit)
           , Alias #(AXI4_ARFlit #(sid_Y, addr_, aruser_), rx_arflit)
           , Alias #(AXI4_RFlit  #(mid_Y, data_, ruser_) , rx_rflit)
             // Manager port alias
           , Alias #( AXI4_Master #( mid_X, addr_, data_
                                   , awuser_, wuser_, buser_, aruser_, ruser_ )
                    , mngr_port_t )
             // XXX everything in parallel as a first quick and dirty cut
           , Alias #(Tuple5 #( Maybe #(tx_awflit)
                             , Maybe #(tx_wflit)
                             , Maybe #(tx_bflit)
                             , Maybe #(tx_arflit)
                             , Maybe #(tx_rflit) ), tx_merged)
           , Alias #(Tuple5 #( Maybe #(rx_awflit)
                             , Maybe #(rx_wflit)
                             , Maybe #(rx_bflit)
                             , Maybe #(rx_arflit)
                             , Maybe #(rx_rflit) ), rx_merged)
           , Bits #(tx_merged, tx_merged_sz)
           , Bits #(rx_merged, rx_merged_sz)
           , Add #(tx_merged_sz, a_, sData_)
           , Add #(rx_merged_sz, b_, sData_)
           );

  // Interface shims declaration
  //////////////////////////////////////////////////////////////////////////////

  // Manager interface
  AXI4_Shim #(sid_Y, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_)
    mgrShim <- mkAXI4ShimUGFF;

  // Subordinate interface
  AXI4_Shim #(mid_Y, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_)
    subShim <- mkAXI4ShimUGFF;

  // remap AXI4 IDs on manager and subordinate ports
  NumProxy #(16) proxyTableSz = ?;
  NumProxy #(8)  proxyMaxSameId = ?;

  Tuple2 #( AXI4_Slave #( sid_Y, addr_, data_
                        , awuser_, wuser_, buser_, aruser_, ruser_ )
          , AXI4_Master #( mid_X, addr_, data_
                         , awuser_, wuser_, buser_, aruser_, ruser_ ) )
    mXsYIdCrossIfcs <- mkAXI4IDNameSpaceCrossing (proxyTableSz, proxyMaxSameId);
  match {.mXsYIdCrossSub, .mXsYIdCrossMngr} = mXsYIdCrossIfcs;
  mkConnection (mgrShim.master, mXsYIdCrossSub);

  Tuple2 #( AXI4_Slave #( sid_X, addr_, data_
                        , awuser_, wuser_, buser_, aruser_, ruser_ )
          , AXI4_Master #( mid_Y, addr_, data_
                         , awuser_, wuser_, buser_, aruser_, ruser_ ) )
    mYsXIdCrossIfcs <- mkAXI4IDNameSpaceCrossing (proxyTableSz, proxyMaxSameId);
  match {.mYsXIdCrossSub, .mYsXIdCrossMngr} = mYsXIdCrossIfcs;
  mkConnection (subShim.slave, mYsXIdCrossMngr);

  // Transmission interface
  AXI4Stream_Shim #(sId_, sData_, sDest_, sUser_)
    txShim <- mkAXI4StreamShimFF;

  // Reception interface
  AXI4Stream_Shim #(sId_, sData_, sDest_, sUser_)
    rxShim <- mkAXI4StreamShimFF;

  // Propagate local traffic to the tx stream
  //////////////////////////////////////////////////////////////////////////////
  PulseWire awflit_tx <- mkPulseWire;
  PulseWire  wflit_tx <- mkPulseWire;
  PulseWire  bflit_tx <- mkPulseWire;
  PulseWire arflit_tx <- mkPulseWire;
  PulseWire  rflit_tx <- mkPulseWire;
  let canProduce = txShim.slave.canPut
                 && (  subShim.master.aw.canPeek
                    || subShim.master.w.canPeek
                    || mgrShim.slave.b.canPeek
                    || subShim.master.ar.canPeek
                    || mgrShim.slave.r.canPeek );
  (* fire_when_enabled *)
  rule produce_tx (canProduce);
    let mawflit = Invalid;
    let mwflit  = Invalid;
    let mbflit  = Invalid;
    let marflit = Invalid;
    let mrflit  = Invalid;
    if (subShim.master.aw.canPeek) begin
      mawflit = Valid (subShim.master.aw.peek);
      awflit_tx.send;
    end
    if (subShim.master.w.canPeek) begin
      mwflit = Valid (subShim.master.w.peek);
      wflit_tx.send;
    end
    if (mgrShim.slave.b.canPeek) begin
      mbflit = Valid (mgrShim.slave.b.peek);
      bflit_tx.send;
    end
    if (subShim.master.ar.canPeek) begin
      marflit = Valid (subShim.master.ar.peek);
      arflit_tx.send;
    end
    if (mgrShim.slave.r.canPeek) begin
      mrflit = Valid (mgrShim.slave.r.peek);
      rflit_tx.send;
    end
    Bit #(tx_merged_sz) txflit =
      pack (tuple5 (mawflit, mwflit, mbflit, marflit, mrflit));
    txShim.slave.put (AXI4Stream_Flit { tdata: zeroExtend (txflit)
                                      , tstrb: ~0
                                      , tkeep: ~0
                                      , tlast: True
                                      , tid: 0
                                      , tdest: 0
                                      , tuser: 0 });
  endrule
  (* fire_when_enabled *)
  rule produce_aw_to_tx (awflit_tx); subShim.master.aw.drop; endrule
  (* fire_when_enabled *)
  rule produce_w_to_tx  (wflit_tx);  subShim.master.w.drop;  endrule
  (* fire_when_enabled *)
  rule produce_b_to_tx  (bflit_tx);  mgrShim.slave.b.drop;   endrule
  (* fire_when_enabled *)
  rule produce_ar_to_tx (arflit_tx); subShim.master.ar.drop; endrule
  (* fire_when_enabled *)
  rule produce_r_to_tx  (rflit_tx);  mgrShim.slave.r.drop;   endrule

  // Gather global trafic from the rx stream
  //////////////////////////////////////////////////////////////////////////////
  rx_merged tmp = unpack (truncate (rxShim.master.peek.tdata));
  match {.rx_awflit, .rx_wflit, .rx_bflit ,.rx_arflit, .rx_rflit} = tmp;
  let canConsume = rxShim.master.canPeek
                 && (!isValid (rx_awflit) || mgrShim.slave.aw.canPut)
                 && (!isValid (rx_wflit)  || mgrShim.slave.w.canPut)
                 && (!isValid (rx_bflit)  || subShim.master.b.canPut)
                 && (!isValid (rx_arflit) || mgrShim.slave.ar.canPut)
                 && (!isValid (rx_rflit)  || subShim.master.r.canPut);
  (* fire_when_enabled *)
  rule consume_rx (canConsume); rxShim.master.drop; endrule
  (* fire_when_enabled *)
  rule consume_aw_from_rx (isValid (rx_awflit) && canConsume);
    mgrShim.slave.aw.put (rx_awflit.Valid);
  endrule
  (* fire_when_enabled *)
  rule consume_w_from_rx (isValid (rx_wflit) && canConsume);
    mgrShim.slave.w.put (rx_wflit.Valid);
  endrule
  (* fire_when_enabled *)
  rule consume_b_from_rx (isValid (rx_bflit) && canConsume);
    subShim.master.b.put (rx_bflit.Valid);
  endrule
  (* fire_when_enabled *)
  rule consume_ar_from_rx (isValid (rx_arflit) && canConsume);
    mgrShim.slave.ar.put (rx_arflit.Valid);
  endrule
  (* fire_when_enabled *)
  rule consume_r_from_rx (isValid (rx_rflit) && canConsume);
    subShim.master.r.put (rx_rflit.Valid);
  endrule

  // Wire-up shims to interface
  //////////////////////////////////////////////////////////////////////////////
  return interface CHERI_BGAS_Bridge_Ifc;
    interface manager = toLocal (mXsYIdCrossMngr);
    interface subordinate = mYsXIdCrossSub;
    interface tx = txShim.master;
    interface rx = rxShim.slave;
  endinterface;

endmodule

endpackage

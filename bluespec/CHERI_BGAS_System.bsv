/*-
 * Copyright (c) 2022 Alexandre Joannou
 * All rights reserved.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package CHERI_BGAS_System;

import Vector :: *;
import Clocks :: *;
import Connectable :: *;
import BlueAXI4 :: *;
import BlueBasics :: *;
import AXI4_Fake_16550 :: *;
import Routable :: *;
import SourceSink :: *;
import Fabric_Defs :: *;
import CoreW :: *;
import WindCoreInterface :: *;
import DE10Pro_bsv_shell :: *;
import SoC_Map :: *;
import VirtualDevice :: *;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// A straightforward axi lite subordinate to provide a banking mechanism for
// the h2f window into the core's memory map.
//
// Exposes a readable/writable Wd_Addr at relative address 0x0
// such that the LSB is bit 0 of address 0
// and the MSB is bit 7 of address 0x3 or 0x7, if Wd_Addr is 32 or 64 respectively.
// WSTRB is *ignored*.
// This mapping is repeated over the remaining address space, so 0x8 maps to the same thing as 0x0.
module mkH2FAddrCtrl #(Bit #(Wd_Addr) dfltAddrBits)
  (Tuple2 #( AXI4Lite_Slave #( t_h2f_lw_addr, t_h2f_lw_data
                             , t_h2f_lw_awuser, t_h2f_lw_wuser, t_h2f_lw_buser
                             , t_h2f_lw_aruser, t_h2f_lw_ruser)
           , ReadOnly #(Bit #(Wd_Addr)) ))
  provisos (
    NumAlias #( t_dats_per_addr, TDiv#(Wd_Addr, t_h2f_lw_data))
  , NumAlias #( t_dat_select, TLog#(t_dats_per_addr))
  , NumAlias #( t_sub_lw_word_addr_bits, TLog#(TDiv#(t_h2f_lw_data, 8)))
  , Add#(a__, t_dat_select, t_h2f_lw_addr)
  , Mul#(TDiv#(Wd_Addr, t_h2f_lw_data), t_h2f_lw_data, Wd_Addr) // Evenly divisible
  );

  // internal state and signals
  Reg#(Vector#(t_dats_per_addr,Bit#(t_h2f_lw_data))) addrBits <- mkReg (unpack(dfltAddrBits));
  let axiShim <- mkAXI4LiteShimFF;

  // read requests handling (always answer with upper bits)
  rule read_req;
    let ar <- get (axiShim.master.ar);
    // e.g. if lw_data is 32-bits = 4 bytes then shift down by log2(4) = 2
    // so address 0x4 becomes 0b100 >> 2 = 0b1 = word 1 of addrBits
    Bit#(t_dat_select) i = truncate(ar.araddr >> valueOf(t_sub_lw_word_addr_bits));
    axiShim.master.r.put (AXI4Lite_RFlit { rdata: addrBits[i]
                                         , rresp: OKAY
                                         , ruser: ? });
  endrule

  // write requests handling (update the appropriate word of addrBits)
  rule write_req;
    let aw <- get (axiShim.master.aw);
    // see read_req for explanation of shift
    Bit#(t_dat_select) i = truncate(aw.awaddr >> valueOf(t_sub_lw_word_addr_bits));
    let w <- get (axiShim.master.w);
    addrBits[i] <= w.wdata;
    axiShim.master.b.put (AXI4Lite_BFlit { bresp: OKAY
                                         , buser: ? });
  endrule

  let readAddrBits = interface ReadOnly;
    method _read = pack(addrBits);
  endinterface;

  // return the subordinate port and a ReadOnly interface to addrBits
  return tuple2 (axiShim.slave, readAddrBits);// regToReadOnly (pack(dataBits));//addrBits));

endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

interface CHERI_BGAS_System_Ifc #(
// AXI4Lite subordinate port
  numeric type t_axil_sub_addr
, numeric type t_axil_sub_data
, numeric type t_axil_sub_awuser
, numeric type t_axil_sub_wuser
, numeric type t_axil_sub_buser
, numeric type t_axil_sub_aruser
, numeric type t_axil_sub_ruser
// AXI4 subordinate 0 port
, numeric type t_axi_sub_0_id
, numeric type t_axi_sub_0_addr
, numeric type t_axi_sub_0_data
, numeric type t_axi_sub_0_awuser
, numeric type t_axi_sub_0_wuser
, numeric type t_axi_sub_0_buser
, numeric type t_axi_sub_0_aruser
, numeric type t_axi_sub_0_ruser
// AXI4 subordinate 1 port
, numeric type t_axi_sub_1_id
, numeric type t_axi_sub_1_addr
, numeric type t_axi_sub_1_data
, numeric type t_axi_sub_1_awuser
, numeric type t_axi_sub_1_wuser
, numeric type t_axi_sub_1_buser
, numeric type t_axi_sub_1_aruser
, numeric type t_axi_sub_1_ruser
// AXI4 manager 0 port
, numeric type t_axi_mngr0_id
, numeric type t_axi_mngr0_addr
, numeric type t_axi_mngr0_data
, numeric type t_axi_mngr0_awuser
, numeric type t_axi_mngr0_wuser
, numeric type t_axi_mngr0_buser
, numeric type t_axi_mngr0_aruser
, numeric type t_axi_mngr0_ruser
// AXI4 manager 1 port
, numeric type t_axi_mngr1_id
, numeric type t_axi_mngr1_addr
, numeric type t_axi_mngr1_data
, numeric type t_axi_mngr1_awuser
, numeric type t_axi_mngr1_wuser
, numeric type t_axi_mngr1_buser
, numeric type t_axi_mngr1_aruser
, numeric type t_axi_mngr1_ruser
// AXI4 manager 2 port
, numeric type t_axi_mngr2_id
, numeric type t_axi_mngr2_addr
, numeric type t_axi_mngr2_data
, numeric type t_axi_mngr2_awuser
, numeric type t_axi_mngr2_wuser
, numeric type t_axi_mngr2_buser
, numeric type t_axi_mngr2_aruser
, numeric type t_axi_mngr2_ruser
);
  // AXI4Lite subordinate port
  // -------------------------
  interface AXI4Lite_Slave #( t_axil_sub_addr
                            , t_axil_sub_data
                            , t_axil_sub_awuser
                            , t_axil_sub_wuser
                            , t_axil_sub_buser
                            , t_axil_sub_aruser
                            , t_axil_sub_ruser ) axil_sub;
  // AXI4 subordinate ports
  // ----------------------
  interface AXI4_Slave #( t_axi_sub_0_id
                        , t_axi_sub_0_addr
                        , t_axi_sub_0_data
                        , t_axi_sub_0_awuser
                        , t_axi_sub_0_wuser
                        , t_axi_sub_0_buser
                        , t_axi_sub_0_aruser
                        , t_axi_sub_0_ruser ) axi_sub_0;
  interface AXI4_Slave #( t_axi_sub_1_id
                        , t_axi_sub_1_addr
                        , t_axi_sub_1_data
                        , t_axi_sub_1_awuser
                        , t_axi_sub_1_wuser
                        , t_axi_sub_1_buser
                        , t_axi_sub_1_aruser
                        , t_axi_sub_1_ruser ) axi_sub_1;
  // AXI4 manager ports
  // ------------------
  interface AXI4_Master #( t_axi_mngr0_id
                         , t_axi_mngr0_addr
                         , t_axi_mngr0_data
                         , t_axi_mngr0_awuser
                         , t_axi_mngr0_wuser
                         , t_axi_mngr0_buser
                         , t_axi_mngr0_aruser
                         , t_axi_mngr0_ruser ) axi_mngr_0;
  interface AXI4_Master #( t_axi_mngr1_id
                         , t_axi_mngr1_addr
                         , t_axi_mngr1_data
                         , t_axi_mngr1_awuser
                         , t_axi_mngr1_wuser
                         , t_axi_mngr1_buser
                         , t_axi_mngr1_aruser
                         , t_axi_mngr1_ruser ) axi_mngr_1;
  interface AXI4_Master #( t_axi_mngr2_id
                         , t_axi_mngr2_addr
                         , t_axi_mngr2_data
                         , t_axi_mngr2_awuser
                         , t_axi_mngr2_wuser
                         , t_axi_mngr2_buser
                         , t_axi_mngr2_aruser
                         , t_axi_mngr2_ruser ) axi_mngr_2;
  // Interrupt sender interface
  // --------------------------
  interface Vector #(32, Irq) irqs;
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Wrapper around a single CHERI BGAS system
module mkCHERI_BGAS_System ( CHERI_BGAS_System_Ifc #(
  // AXI4Lite subordinate port
    t_axil_sub_addr
  , t_axil_sub_data
  , t_axil_sub_awuser
  , t_axil_sub_wuser
  , t_axil_sub_buser
  , t_axil_sub_aruser
  , t_axil_sub_ruser
  // AXI4 subordinate 0 port
  , t_axi_sub_0_id
  , t_axi_sub_0_addr
  , t_axi_sub_0_data
  , t_axi_sub_0_awuser
  , t_axi_sub_0_wuser
  , t_axi_sub_0_buser
  , t_axi_sub_0_aruser
  , t_axi_sub_0_ruser
  // AXI4 subordinate 1 port
  , t_axi_sub_1_id
  , t_axi_sub_1_addr
  , t_axi_sub_1_data
  , t_axi_sub_1_awuser
  , t_axi_sub_1_wuser
  , t_axi_sub_1_buser
  , t_axi_sub_1_aruser
  , t_axi_sub_1_ruser
  // AXI4 manager 0 port
  , t_axi_mngr0_id
  , t_axi_mngr0_addr
  , t_axi_mngr0_data
  , t_axi_mngr0_awuser
  , t_axi_mngr0_wuser
  , t_axi_mngr0_buser
  , t_axi_mngr0_aruser
  , t_axi_mngr0_ruser
  // AXI4 manager 1 port
  , t_axi_mngr1_id
  , t_axi_mngr1_addr
  , t_axi_mngr1_data
  , t_axi_mngr1_awuser
  , t_axi_mngr1_wuser
  , t_axi_mngr1_buser
  , t_axi_mngr1_aruser
  , t_axi_mngr1_ruser
  // AXI4 manager 2 port
  , t_axi_mngr2_id
  , t_axi_mngr2_addr
  , t_axi_mngr2_data
  , t_axi_mngr2_awuser
  , t_axi_mngr2_wuser
  , t_axi_mngr2_buser
  , t_axi_mngr2_aruser
  , t_axi_mngr2_ruser
  )) provisos (
    // type aliases
    ////////////////////////////////////////////////////////////////////////////
    // AXI4 Lite control port
    Alias #( t_axil_sub
           , AXI4Lite_Slave #(
               t_axil_sub_addr, t_axil_sub_data
             , t_axil_sub_awuser, t_axil_sub_wuser, t_axil_sub_buser
             , t_axil_sub_aruser, t_axil_sub_ruser ))
    // outgoing traffic
  , NumAlias #(t_core_mid, TAdd #(Wd_MId, 1)) // id width out of the core
  , NumAlias #(t_bus0_sid, TAdd #(t_core_mid, 1)) // cope with 2 masters only
  , Alias #(t_bus0_mngr, AXI4_Master #( t_core_mid, Wd_Addr, Wd_Data
                                      , 0, 0, 0, 0, 0))
  , Alias #(t_bus0_sub, AXI4_Slave #( t_bus0_sid, Wd_Addr, Wd_Data
                                    , 0, 0, 0, 0, 0))
  , Alias #(t_bus0_subshim, AXI4_Shim #( t_bus0_sid, Wd_Addr, Wd_Data
                                       , 0, 0, 0, 0, 0))
  , NumAlias #(t_bus1_sid, t_core_mid) // cope with 1 master only
  , Alias #(t_bus1_mngr, AXI4_Master #( t_core_mid, Wd_Addr, Wd_Data_Periph
                                      , 0, 0, 0, 0, 0))
  , Alias #(t_bus1_sub, AXI4_Slave #( t_bus1_sid, Wd_Addr, Wd_Data_Periph
                                    , 0, 0, 0, 0, 0))
  , Alias #(t_bus1_subshim, AXI4_Shim #( t_bus1_sid, Wd_Addr, Wd_Data_Periph
                                       , 0, 0, 0, 0, 0))
    // incoming traffic
  , Add #(1, t_incoming_id, Wd_CoreW_Bus_MId)
  , Alias #( t_core_sub_shim
           , AXI4_Shim #( t_incoming_id, Wd_Addr, Wd_Data_Periph
                        , Wd_AW_User_Periph, Wd_W_User_Periph, Wd_B_User_Periph
                        , Wd_AR_User_Periph, Wd_R_User_Periph))
  , Alias #( t_h2f_sub
           , AXI4_Slave #(
               t_incoming_id, t_axi_sub_0_addr, t_axi_sub_0_data
             , t_axi_sub_0_awuser, t_axi_sub_0_wuser, t_axi_sub_0_buser
             , t_axi_sub_0_aruser, t_axi_sub_0_ruser ))
    // type constraints
    ////////////////////////////////////////////////////////////////////////////
    // AXI4Lite subordinate port
  , Add #(0, 21, t_axil_sub_addr) // XXX hard-coded in CoreW_IFC
  , Add #(0, 32, t_axil_sub_data) // XXX hard-coded in CoreW_IFC
  , Add #(0, 0, t_axil_sub_awuser)
  , Add #(0, 0, t_axil_sub_wuser)
  , Add #(0, 0, t_axil_sub_buser)
  , Add #(0, 0, t_axil_sub_aruser)
  , Add #(0, 0, t_axil_sub_ruser)
    // AXI4 subordinate 0 port
  , Add #(0, t_incoming_id, t_axi_sub_0_id)
  , Add #(0, 32, t_axi_sub_0_addr)
  , Add #(0, 128, t_axi_sub_0_data)
    // AXI4 subordinate 1 port
  , Add #(0, t_incoming_id, t_axi_sub_1_id)
  , Add #(0, Wd_Addr, t_axi_sub_1_addr)
  , Add #(0, Wd_Data_Periph, t_axi_sub_1_data)
  , Add #(0, Wd_AW_User_Periph, t_axi_sub_1_awuser)
  , Add #(0, Wd_W_User_Periph, t_axi_sub_1_wuser)
  , Add #(0, Wd_B_User_Periph, t_axi_sub_1_buser)
  , Add #(0, Wd_AR_User_Periph, t_axi_sub_1_aruser)
  , Add #(0, Wd_R_User_Periph, t_axi_sub_1_ruser)
    // AXI4 manager 0 port -- used for F2H traffic
  , Add #(0, t_bus1_sid, t_axi_mngr0_id)
  , Add #(0, Wd_Addr, t_axi_mngr0_addr)
  , Add #(0, Wd_Data_Periph, t_axi_mngr0_data)
  , Add #(0, 0, t_axi_mngr0_awuser)
  , Add #(0, 0, t_axi_mngr0_wuser)
  , Add #(0, 0, t_axi_mngr0_buser)
  , Add #(0, 0, t_axi_mngr0_aruser)
  , Add #(0, 0, t_axi_mngr0_ruser)
    // AXI4 manager 1 port -- used for DDR traffic
  , Add #(0, t_bus0_sid, t_axi_mngr1_id)
  , Add #(0, Wd_Addr, t_axi_mngr1_addr)
  , Add #(0, Wd_Data, t_axi_mngr1_data)
  , Add #(0, 0, t_axi_mngr1_awuser)
  , Add #(0, 0, t_axi_mngr1_wuser)
  , Add #(0, 0, t_axi_mngr1_buser)
  , Add #(0, 0, t_axi_mngr1_aruser)
  , Add #(0, 0, t_axi_mngr1_ruser)
    // AXI4 manager 2 port -- used for global traffic
  , Add #(0, t_bus1_sid, t_axi_mngr2_id)
  , Add #(0, Wd_Addr, t_axi_mngr2_addr)
  , Add #(0, Wd_Data_Periph, t_axi_mngr2_data)
  , Add #(0, 0, t_axi_mngr2_awuser)
  , Add #(0, 0, t_axi_mngr2_wuser)
  , Add #(0, 0, t_axi_mngr2_buser)
  , Add #(0, 0, t_axi_mngr2_aruser)
  , Add #(0, 0, t_axi_mngr2_ruser)
  );

  // declare cpu core with WindCoreMid interface
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  Clock clk <- exposeCurrentClock;
  Reset rst <- exposeCurrentReset;
  let newRst <- mkReset (0, True, clk, reset_by rst);
  Tuple2 #( PulseWire
          , CoreW_IFC #(N_External_Interrupt_Sources)) both
    <- mkCoreW_reset (rst, reset_by newRst.new_rst);
  match {.otherRst, .midCore} = both;
  rule rl_forward_debug_reset (otherRst);
    newRst.assertReset;
  endrule

  // instanciate the SoC_Map
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  SoC_Map_IFC soc_map <- mkSoC_Map (reset_by newRst.new_rst);

  // declare extra AXI4 lite ctrl subordinates
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  // uart0 - fake 16550
  Tuple2 #( Tuple2 #(t_axil_sub, ReadOnly #(Bool) )
          , Tuple2 #(t_axil_sub, ReadOnly #(Bool) ))
    uart0ifcs <- mkAXI4_Fake_16550_Pair ( 50_000_000
                                        , 16
                                        , 16
                                        , reset_by newRst.new_rst);
  match { {.uart0s0, .uart0irq0}
        , {.uart0s1, .uart0irq1} } = uart0ifcs;
  // ctrl sub entry
  let ctrSubUART0 =
        tuple2 (uart0s0, Range { base: 'h0000_3000, size: 'h0000_1000 });

  // uart1 - fake 16550
  Tuple2 #( Tuple2 #(t_axil_sub, ReadOnly #(Bool) )
          , Tuple2 #(t_axil_sub, ReadOnly #(Bool) ))
    uart1ifcs <- mkAXI4_Fake_16550_Pair ( 50_000_000
                                        , 2048
                                        , 2048
                                        , reset_by newRst.new_rst);
  match { {.uart1s0, .uart1irq0}
        , {.uart1s1, .uart1irq1} } = uart1ifcs;
  // ctrl sub entry
  let ctrSubUART1 =
        tuple2 (uart1s0, Range { base: 'h0000_4000, size: 'h0000_1000 });

  // h2f address upper 32-bits banking register
  // (h2f port only has 32-bit addresses, this mechanism is intended to enable
  // control over a full 64-bit address)
  Tuple2 #(t_axil_sub, ReadOnly #(Bit #(Wd_Addr)))
    h2fCtrlIfcs <- mkH2FAddrCtrl (0, reset_by newRst.new_rst);
  match {.h2fAddrCtrlSub, .h2fAddrCtrlRO} = h2fCtrlIfcs;
  // ctrl sub entry
  let ctrSubH2FAddrCtrl =
    tuple2 (h2fAddrCtrlSub, Range { base: 'h0000_5000, size: 'h0000_1000 });

  // Virtual device for emulating control registers, e.g. for virtio.
  // (Has both a control interface and a virtualised interface;
  // The control interface for AXI4 lite, and virtualised for Toooba MMIO.
  // The virtual device does not support bursts.)
  VirtualDeviceIfc #( t_bus1_sid, t_axil_sub_addr, t_axil_sub_data
                    , t_bus1_sid, Wd_Addr, Wd_Data_Periph )
    virtDev <- mkVirtualDevice (reset_by newRst.new_rst);
  let ctrSubVirtDevCtrl =
    tuple2 ( fromAXI4ToAXI4Lite_Slave (virtDev.mngt)
           , Range { base: 'h0000_8000, size: 'h0000_4000 } );

  // Prepare Wind Core
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  // re-wrap wind core:
  // - convert mid core to hi core
  // - add outside-world-facing AXI4 Lite subordinates to expose throug the
  //   core's AXI4 Lite subordinate port (with their mappping)
  // - add IRQs into the wind core
  let core <- windCoreMid2Hi_Core (
                // the mid-level interface core to convert
                midCore
                // the vector of additional AXI4 Lite subordinates to expose
              , cons (        ctrSubUART0
                     , cons ( ctrSubUART1
                     , cons ( ctrSubH2FAddrCtrl
                     , cons ( ctrSubVirtDevCtrl
                            , nil ))))
                // the vector of IRQs going in the wind core
              , cons (        uart0irq1
                     , cons ( uart1irq1
                            , nil ))
                // explicit reset_by
              , reset_by newRst.new_rst );

  // Bus 0 - core cached traffic / forwarded uncached
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  // gather all managers
  Vector #(2, t_bus0_mngr) bus0_ms;
  bus0_ms[0] = core.manager_0;
  bus0_ms[1] = ?; // later assigned out of a bridge out of bus1

  // prepare AXI4 subordinates exposed to the cached interface
  ////////////////////////////////////////////////////////////

  // prepare AXI4 manager ports traffic
  t_bus0_subshim ddrShim <- mkAXI4ShimFF (reset_by newRst.new_rst);

  // gather all subordinates
  Vector #(1, t_bus0_sub) bus0_ss;
  bus0_ss[0] = ddrShim.slave;

  // build route
  function Vector #(1, Bool) bus0_route (Bit #(Wd_Addr) addr);
    Vector #(1, Bool) x = replicate (False);
    if (   inRange (soc_map.m_ddr4_0_cached_addr_range, addr)
        || inRange (soc_map.m_ddr4_0_uncached_addr_range, addr) )
      x[0] = True;
    return x;
  endfunction

  // Bus 1 - core uncached traffic
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  // gather all managers
  Vector #(1, t_bus1_mngr) bus1_ms;
  bus1_ms[0] = core.manager_1;

  // prepare AXI4 subordinates exposed to the bus1 interface
  //////////////////////////////////////////////////////////

  // prepare AXI4 manager ports traffic
  Vector #(2, t_bus1_subshim)
    mngrShim <- replicateM (mkAXI4ShimFF (reset_by newRst.new_rst));

  // prepare uart0
  AXI4_Shim #( t_bus1_sid, t_axil_sub_addr, t_axil_sub_data
             , t_axil_sub_awuser, t_axil_sub_wuser, t_axil_sub_buser
             , t_axil_sub_aruser, t_axil_sub_ruser)
    uart0DeBurst <- mkBurstToNoBurst (reset_by newRst.new_rst);
  mkConnection (uart0DeBurst.master, uart0s1, reset_by newRst.new_rst);
  t_bus1_sub uart0_s <-
    toWider_AXI4_Slave ( truncate_AXI4_Slave_addr (uart0DeBurst.slave)
                       , reset_by newRst.new_rst );

  // prepare uart1
  AXI4_Shim #( t_bus1_sid, t_axil_sub_addr, t_axil_sub_data
             , t_axil_sub_awuser, t_axil_sub_wuser, t_axil_sub_buser
             , t_axil_sub_aruser, t_axil_sub_ruser)
    uart1DeBurst <- mkBurstToNoBurst (reset_by newRst.new_rst);
  mkConnection (uart1DeBurst.master, uart1s1, reset_by newRst.new_rst);
  t_bus1_sub uart1_s <-
    toWider_AXI4_Slave ( truncate_AXI4_Slave_addr (uart1DeBurst.slave)
                       , reset_by newRst.new_rst );

  // prepare bootrom
  t_bus1_subshim fakeBootRomDeBurst <-
    mkBurstToNoBurst (reset_by newRst.new_rst);
  t_bus1_sub fakeBootRom <- mkPerpetualZeroAXI4Slave (reset_by newRst.new_rst);
  mkConnection ( fakeBootRomDeBurst.master, fakeBootRom
               , reset_by newRst.new_rst );

  // prepare bridge to bus0
  NumProxy #(2)  proxyBuffInDepth = ?;
  NumProxy #(4) proxyBuffOutDepth = ?;
  match {.bus0BridgeSub, .bus0BridgeMngr} <-
    mkAXI4DataWidthShim_NarrowToWide ( proxyBuffInDepth
                                     , proxyBuffOutDepth
                                     , reset_by newRst.new_rst );
 /*
  t_bus1_subshim bus0BridgeShim <- mkAXI4ShimFF (reset_by newRst.new_rst);
  AXI4_Master #(t_core_mid, Wd_Addr, TMul #(Wd_Data_Periph, 2), 0, 0, 0, 0, 0)
    m_wide_a <- toWider_AXI4_Master ( bus0BridgeShim.master
                                    , reset_by newRst.new_rst );
  AXI4_Master #(t_core_mid, Wd_Addr, TMul #(Wd_Data_Periph, 4), 0, 0, 0, 0, 0)
    m_wide_b <- toWider_AXI4_Master (m_wide_a, reset_by newRst.new_rst);
  t_bus0_mngr bus0BridgeMngr <- toWider_AXI4_Master ( m_wide_b
                                                    , reset_by newRst.new_rst );
  t_bus1_sub bus0BridgeSub = bus0BridgeShim.slave;
  */

  //bus0_ms[1] = debugAXI4_Master (bus0BridgeMngr, $format ("bus0BridgeMngr"));
  bus0_ms[1] = bus0BridgeMngr;

  // gather all subordinates
  Vector #(7, t_bus1_sub) bus1_ss;
  bus1_ss[0] = mngrShim[0].slave; // f2h accesses
  bus1_ss[1] = mngrShim[1].slave; // global accesses
  bus1_ss[2] = uart0_s;
  bus1_ss[3] = uart1_s;
  bus1_ss[4] = fakeBootRomDeBurst.slave;
  bus1_ss[5] = virtDev.virt;
  //bus1_ss[6] = debugAXI4_Slave (bus0BridgeSub, $format ("bus0BridgeSub"));
  bus1_ss[6] = bus0BridgeSub;

  // build route
  function Vector #(7, Bool) bus1_route (Bit #(Wd_Addr) addr);
    Vector #(7, Bool) x = replicate (False);
    if (inRange (soc_map.m_ddr4_0_uncached_addr_range, addr))
      x[6] = True;
    else if (inRange (soc_map.m_virt_dev_addr_range, addr))
      x[5] = True;
    else if (inRange (soc_map.m_boot_rom_addr_range, addr))
      x[4] = True;
    else if (inRange (soc_map.m_uart_1_addr_range, addr))
      x[3] = True;
    else if (inRange (soc_map.m_uart_0_addr_range, addr))
      x[2] = True;
    else if (  inRange (soc_map.m_global_bgas_addr_range, addr)
            || inRange (soc_map.m_bgas_router_conf_addr_range, addr) )
      x[1] = True;
    else if (inRange (soc_map.m_f2h_addr_range, addr))
      x[0] = True;
    return x;
  endfunction

  // wire it all up
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  mkAXI4Bus (bus0_route, bus0_ms, bus0_ss, reset_by newRst.new_rst);
  mkAXI4Bus (bus1_route, bus1_ms, bus1_ss, reset_by newRst.new_rst);

  // Incoming interconnect
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  // Shims for
  //   - incoming H2F traffic
  //   - incoming global traffic
  Vector #(2, t_core_sub_shim)
    subShim <- replicateM (mkAXI4ShimFF (reset_by newRst.new_rst));

  // H2F interface wrapping (extra address bits & data size shim)
  // h2fAddrCtrlRO is Bits#(Wd_Addr) in size, i.e. 32-bit unless FABRIC64 is defined in which case 64-bit.
  // FABRIC64 is usually defined.
  // toWider_AXI4_Slave produces a new AXI slave with double the data width of the argument.
  // prepend_AXI4_Slave_addr produces a new AXI slave from an argument,
  // where transactions to the new AXI slave (of *fewer* address bits) are passed to the argument with 
  // *extra* address bits prepended.
  // This address of {32'b0, h2f_addr'32} is then OR-d with `h2fAddrCtrlRO` by `or_AXI4_Slave_addr`,
  // before arriving at the shim.
  t_h2f_sub h2fSub <- toWider_AXI4_Slave (
                        zero_AXI4_Slave_user (
                          prepend_AXI4_Slave_addr ( 32'b0
                                                  , or_AXI4_Slave_addr ( h2fAddrCtrlRO
                                                                       , subShim[0].slave)))
                      , reset_by newRst.new_rst );

  mkAXI4Bus ( constFn (cons (True, nil))
            , cons (subShim[0].master, cons (subShim[1].master, nil))
            , cons (core.subordinate_0, nil)
            , reset_by newRst.new_rst );

  // prepare outside-world-facing IRQs
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  Vector #(32, Irq) allIrqs = replicate (noIrq);
  // uart0 irq
  allIrqs[0] = interface Irq; method _read = uart0irq0._read; endinterface;
  // uart1 irq
  allIrqs[1] = interface Irq; method _read = uart1irq0._read; endinterface;

  // interface
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  interface axil_sub = core.control_subordinate; // incoming control traffic
  interface axi_sub_0 = h2fSub;                  // incoming H2F traffic
  interface axi_sub_1 = subShim[1].slave;        // incoming global traffic
  interface axi_mngr_0 = mngrShim[0].master;     // outgoing F2H traffic
  interface axi_mngr_1 = ddrShim.master;         // outgoing ddr traffic
  interface axi_mngr_2 = mngrShim[1].master;     // outgoing global traffic
  interface irqs = allIrqs;                      // outgoing interrupts

endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

endpackage

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
import AxiWindow :: *;
import Cap2024_11 :: *;
import IOCapAxi :: *;

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

  // Expose the banking register (the "axi window") for h2f accesses
  // (h2f port only has 32-bit addresses, this mechanism is intended to enable
  // control over a full 64-bit address)
  // Create an AxiWindow which exposes a
  // - H2F_LW AXI4Lite subordinate `h2fWindow.windowCtrl`, exposing a 64-bit "window" register
  // - H2F AXI4 subordinate `h2fWindow.preWindow` which converts 32-bit h2f accesses to 64-bit accesses offset by the window
  // - H2F AXI4 manager `h2fWindow.postWindow` which puts out the converted accesses + an IOCap authenticating each one
  // let h2fWindow <- mkAddrOffsetAxiWindow(reset_by newRst.new_rst);
  Proxy#(Cap2024_11) h2fWindowCapType = ?;
  let h2fWindow <- mkSimpleIOCapWindow(h2fWindowCapType, reset_by newRst.new_rst);
  // Expose the windowCtrl on the AXI4 lite bus
  let ctrSubH2FAddrCtrl =
    tuple2 (h2fWindow.windowCtrl, Range { base: 'h0000_5000, size: 'h0000_1000 });
  // Connect the h2fWindow to an IOCap Exposer, which checks the IOCap against the keys written in by the host.
  IOCap_KeyManager#(Wd_Data_Periph) iocapKeyStore <- mkSimpleIOCapKeyManager(reset_by newRst.new_rst);
  // Use a V4 exposer which *doesn't* block invalid transactions
  // - invalid txns are necessary for e.g. uploading code via the debug infrastructure which isn't IOCap-compatible
  // - invalid txns are still counted by performance counters, so OSs can verify no invalid txns pass through
  let iocapExposer <- mkSimpleIOCapExposerV4(iocapKeyStore, False /* Don't block invalid */, reset_by newRst.new_rst);
  mkConnection(iocapExposer.iocapsIn.axiSignals, h2fWindow.postWindow, reset_by newRst.new_rst);

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
                      // the IOCap exposer keystore is NOT included here - the outside world should never be able to see that!
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

  // gather all subordinates which Toooba can access
  Vector #(8, t_bus1_sub) bus1_ss;
  bus1_ss[0] = mngrShim[0].slave; // f2h accesses
  bus1_ss[1] = mngrShim[1].slave; // global accesses
  bus1_ss[2] = uart0_s;
  bus1_ss[3] = uart1_s;
  bus1_ss[4] = fakeBootRomDeBurst.slave;
  bus1_ss[5] = virtDev.virt;
  bus1_ss[6] = truncate_AXI4_Slave_addr (
    fromAXI4LiteToAXI4_Slave (
      iocapKeyStore.hostFacingSlave
    )
  );
  //bus1_ss[6] = debugAXI4_Slave (bus0BridgeSub, $format ("bus0BridgeSub"));
  bus1_ss[7] = bus0BridgeSub;

  // build route
  function Vector #(8, Bool) bus1_route (Bit #(Wd_Addr) addr);
    Vector #(8, Bool) x = replicate (False);
    if (inRange (soc_map.m_ddr4_0_uncached_addr_range, addr))
      x[7] = True;
    else if (inRange (soc_map.m_iocap_exposer_addr_range, addr))
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

  // Shim for incoming global traffic, converts the slave at mkCHERI_BGAS_System's
  // input boundary to a master
  t_core_sub_shim globalShim <- mkAXI4ShimFF (reset_by newRst.new_rst);

  // incoming H2F traffic is passed through four layers...
  // - toWider_AXI4_Slave   = split double-data-width transactions into halves
  // - zero_AXI4_Slave_user = ignore any user data supplied
  // - h2fWindow.preWindow  = offset the access by the MMIO-writable "window" register, attach an IOCap
  // - iocapExposer         = decode and check the IOCap
  // ...before arriving at the core
  let h2fExternalSub <- toWider_AXI4_Slave (
    zero_AXI4_Slave_user (
      h2fWindow.preWindow
    ),
    reset_by newRst.new_rst
  );

  // Route all incoming requests from the h2f and global AXI4 interfaces
  // to the core's subordinate port
  // iocapExposer.sanitizedOut initiates the requests recieved from h2f
  // after they've passed through the three layers shown above.
  mkAXI4Bus ( constFn (cons (True, nil))
            , cons (iocapExposer.sanitizedOut, cons (globalShim.master, nil))
            // , cons (h2fWindow.postWindow, cons (globalShim.master, nil))
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
  interface axi_sub_0 = h2fExternalSub;          // incoming H2F traffic
  interface axi_sub_1 = globalShim.slave;        // incoming global traffic
  interface axi_mngr_0 = mngrShim[0].master;     // outgoing F2H traffic
  interface axi_mngr_1 = ddrShim.master;         // outgoing ddr traffic
  interface axi_mngr_2 = mngrShim[1].master;     // outgoing global traffic
  interface irqs = allIrqs;                      // outgoing interrupts

endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

endpackage

/*-
 * Copyright (c) 2022-2024 Alexandre Joannou
 * Copyright (c) 2024 Samuel W Stark
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

import Vector :: *;
import Clocks :: *;
import Connectable :: *;
import BlueAXI4 :: *;
import BlueBasics :: *;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Exposes a readable/writable Bit#(t_data) at relative address 0x0
// such that the LSB is bit 0 of address 0
// and the MSB is bit 7 of address 0x3 or 0x7, if t_data is 32 or 64 respectively.
// WSTRB is *ignored*, TODO fix this.
// This mapping is repeated over the remaining address space, so 0x8 maps to the same thing as 0x0.
module mkAXI4Lite_SubReg #(Bit #(t_data) def)
  (Tuple2 #( AXI4Lite_Slave #( t_axil_addr, t_axil_data
                             , t_axil_awuser, t_axil_wuser, t_axil_buser
                             , t_axil_aruser, t_axil_ruser)
           , ReadOnly #(Bit #(t_data)) ))
  provisos (
    // We store t_data as a vector of t_words_per_data axil words
    NumAlias #( t_words_per_data, TDiv#(t_data, t_axil_data))
    // Need log2(n) bits to select one of n words to write into
  , NumAlias #( t_word_select, TLog#(t_words_per_data))
    // We expose a byte-addressable interface, although we can expect the write data to be aligned to axil words.
    // Figure out how much we need to shift a byte-address to make it a word-address
  , NumAlias #( t_sub_axil_word_addr_bits, TLog#(TDiv#(t_axil_data, 8)))
    // Make sure there are enough window_addr bits to address all the selectable words TODO this should be bytes
  , Add#(a__, t_word_select, t_axil_addr)
    // Make sure the t_data is evenly divisible by axil words
  , Mul#(TDiv#(t_data, t_axil_data), t_axil_data, t_data) // Evenly divisible
  );

  // internal state and signals
  Reg#(Vector#(t_words_per_data, Bit#(t_axil_data))) addrBits <- mkReg (unpack(def));
  let axiShim <- mkAXI4LiteShimFF;

  // read requests handling (always answer with upper bits)
  rule read_req;
    let ar <- get (axiShim.master.ar);
    // e.g. if lw_data is 32-bits = 4 bytes then shift down by log2(4) = 2
    // so address 0x4 becomes 0b100 >> 2 = 0b1 = word 1 of addrBits
    Bit#(t_word_select) i = truncate(ar.araddr >> valueOf(t_sub_axil_word_addr_bits));
    axiShim.master.r.put (AXI4Lite_RFlit { rdata: addrBits[i]
                                         , rresp: OKAY
                                         , ruser: ? });
  endrule

  // write requests handling (update the appropriate word of addrBits)
  rule write_req;
    let aw <- get (axiShim.master.aw);
    // see read_req for explanation of shift
    Bit#(t_word_select) i = truncate(aw.awaddr >> valueOf(t_sub_axil_word_addr_bits));
    let w <- get (axiShim.master.w);
    addrBits[i] <= w.wdata;
    axiShim.master.b.put (AXI4Lite_BFlit { bresp: OKAY
                                         , buser: ? });
  endrule

  let readAddrBits = interface ReadOnly;
    method _read = pack(addrBits);
  endinterface;

  // return the subordinate port and a ReadOnly interface to addrBits
  return tuple2 (axiShim.slave, readAddrBits);

endmodule

interface AxiWindow #(
    // Window subordinate port (AXI4Lite)
      numeric type t_window_ctrl_addr
    , numeric type t_window_ctrl_data
    , numeric type t_window_ctrl_awuser
    , numeric type t_window_ctrl_wuser
    , numeric type t_window_ctrl_buser
    , numeric type t_window_ctrl_aruser
    , numeric type t_window_ctrl_ruser
    // Access subordinate port (AXI4)
    , numeric type t_pre_window_id
    , numeric type t_pre_window_addr
    , numeric type t_pre_window_data
    , numeric type t_pre_window_awuser
    , numeric type t_pre_window_wuser
    , numeric type t_pre_window_buser
    , numeric type t_pre_window_aruser
    , numeric type t_pre_window_ruser
);
    interface AXI4Lite_Slave #(
          t_window_ctrl_addr, t_window_ctrl_data
        , t_window_ctrl_awuser, t_window_ctrl_wuser, t_window_ctrl_buser
        , t_window_ctrl_aruser, t_window_ctrl_ruser
    ) windowCtrl;
    
    interface AXI4_Slave #(
          t_pre_window_id, t_pre_window_addr, t_pre_window_data
        , t_pre_window_awuser, t_pre_window_wuser, t_pre_window_buser
        , t_pre_window_aruser, t_pre_window_ruser
    ) preWindow;
endinterface

// TODO prevent the window from being changed during an access
module mkAddrOffsetAxiWindow#(
    // Take the post-window slave as an argument,
    // we pump requests through from the pre-window
    // and the parameters are the same except the address size
    AXI4_Slave#(
          t_pre_window_id
        , t_post_window_addr
        , t_pre_window_data
        , t_pre_window_awuser
        , t_pre_window_wuser
        , t_pre_window_buser
        , t_pre_window_aruser
        , t_pre_window_ruser
    ) postWindow
)(AxiWindow#(
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
    , t_pre_window_awuser
    , t_pre_window_wuser
    , t_pre_window_buser
    , t_pre_window_aruser
    , t_pre_window_ruser
)) provisos (
    // type aliases
    ////////////////////////////////////////////////////////////////////////////
    // AXI4 Lite control port
    Alias #( t_window_ctrl
           , AXI4Lite_Slave #(
               t_window_ctrl_addr, t_window_ctrl_data
             , t_window_ctrl_awuser, t_window_ctrl_wuser, t_window_ctrl_buser
             , t_window_ctrl_aruser, t_window_ctrl_ruser ))
    , Alias #( t_pre_window
             , AXI4_Slave #(
                t_pre_window_id, t_pre_window_addr, t_pre_window_data
                , t_pre_window_awuser, t_pre_window_wuser, t_pre_window_buser
                , t_pre_window_aruser, t_pre_window_ruser ))
    // numeric relations
    ////////////////////////////////////////////////////////////////////////////
    // t_post_window_addr may be larger than t_pre_window_addr (which gets zero extended)
    , Add#(a__, t_pre_window_addr, t_post_window_addr)
    // Make sure the windowCtrl can evenly represent the post_window address with the windowCtrl data words
    , Mul#(TDiv#(t_post_window_addr, t_window_ctrl_data), t_window_ctrl_data, t_post_window_addr) // Evenly divisible
    // Make sure the windowCtrl has enough address bits to address every word of the t_post_window_addr
    // i.e. that t_window_ctrl_addr >= log2(number of windowCtrl data words in post_window_addr)
    , Add#(b__, TLog#(TDiv#(t_post_window_addr, t_window_ctrl_data)), t_window_ctrl_addr)
);
    // Expose an AXI4Lite subordinate which read/writes a register we can read
    // That register holds the window offset
    Tuple2 #(t_window_ctrl, ReadOnly #(Bit #(t_post_window_addr))) windowCtrlIfcs <- mkAXI4Lite_SubReg (0);
    match {.windowCtrlIfc, .windowAddr} = windowCtrlIfcs;

    // Function to generate the post-window address from the pre-window address
    function mapAddr (preWindowAddr) = windowAddr | zeroExtend(preWindowAddr);

    interface windowCtrl = windowCtrlIfc;
    // The preWindow interface takes requests, applies the mapAddr function, then passes them to postWindow
    interface preWindow  = mapAXI4_Slave_addr (mapAddr, postWindow);
endmodule
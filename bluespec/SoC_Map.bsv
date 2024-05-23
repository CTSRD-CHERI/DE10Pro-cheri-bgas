// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved

/*-
 * Copyright (c) 2022-2023 Alexandre Joannou
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

package SoC_Map;

// ================================================================
// This module defines the overall 'address map' of the SoC, showing
// the addresses serviced by each slave IP, and which addresses are
// memory vs. I/O.

// ***** WARNING! WARNING! WARNING! *****

// During system integration, this address map should be identical to
// the system interconnect settings (e.g., routing of requests between
// masters and slaves).  This map is also needed by software so that
// it knows how to address various IPs.

// This module contains no state; it just has constants, and so can be
// freely instantiated at multiple places in the SoC module hierarchy
// at no hardware cost.  It allows this map to be defined in one
// place and shared across the SoC.

// ================================================================
// This version of SoC_Map is for the DARPA SSITH GFE

// Our "Near_Mem_IO" corresponds to "CLINT" in Rocket

// ================================================================
// Exports

export  SoC_Map_Struct (..), soc_map_struct;

export  SoC_Map_IFC (..), mkSoC_Map;

export  N_External_Interrupt_Sources;
export  n_external_interrupt_sources;

// ================================================================
// Bluespec library imports

import Routable :: *; // For Range

// ================================================================
// Project imports

import Fabric_Defs :: *;    // Only for type Fabric_Addr

import "BDPI" getenv_as_64hex =
  function Maybe #(Bit #(64)) getEnvInt (String varname);

// ================================================================
// Top-level-struct version of the SoC Map for RISCY-OOO

typedef struct {
   Bit #(64)  near_mem_io_addr_base;
   Bit #(64)  near_mem_io_addr_size;

   Bit #(64)  boot_rom_addr_base;
   Bit #(64)  boot_rom_addr_size;

   Bit #(64)  main_mem_addr_base;
   Bit #(64)  main_mem_addr_size;

   Bit #(64)  pc_reset_value;
   } SoC_Map_Struct
deriving (FShow);

SoC_Map_Struct soc_map_struct =
SoC_Map_Struct {
   near_mem_io_addr_base:  'h_1000_0000,
   near_mem_io_addr_size:  'h_0001_0000,

   boot_rom_addr_base:     'h_7000_0000,
   boot_rom_addr_size:     'h_0000_1000,

   main_mem_addr_base:     'h_C000_0000,
   main_mem_addr_size:     'h_C000_0000,

   pc_reset_value:
     (genC) ? fromMaybe ('h_7000_0000, getEnvInt ("CHERI_BGAS_PC_RESET_VALUE"))
            : 'h_7000_0000 // = boot_rom_addr_base
   };

// ================================================================
// Interface and module for the address map

interface SoC_Map_IFC;
   (* always_ready *)   method  Range#(Wd_Addr)  m_plic_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_near_mem_io_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_ethernet_0_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_dma_0_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_uart_0_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_uart_1_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_gpio_0_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_boot_rom_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_virt_dev_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_ddr4_0_uncached_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_ddr4_0_cached_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_global_bgas_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_bgas_router_conf_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_f2h_addr_range;
   (* always_ready *)   method  Range#(Wd_Addr)  m_mem0_controller_addr_range;

   (* always_ready *)
   method  Bool  m_is_mem_addr (Fabric_Addr addr);

   (* always_ready *)
   method  Bool  m_is_IO_addr (Fabric_Addr addr, Bool imem_not_dmem);

   (* always_ready *)
   method  Bool  m_is_near_mem_IO_addr (Fabric_Addr addr);

   (* always_ready *)   method  Bit #(64)  m_pc_reset_value;
   (* always_ready *)   method  Bit #(64)  m_mtvec_reset_value;
   (* always_ready *)   method  Bit #(64)  m_nmivec_reset_value;
endinterface

// ================================================================

(* synthesize *)
module mkSoC_Map (SoC_Map_IFC);

   // ----------------------------------------------------------------
   // PLIC

   let plic_addr_range = Range {
      base: 'h_0C00_0000,
      size: 'h_0040_0000    // 4M
   };

   // ----------------------------------------------------------------
   // Near_Mem_IO (CLINT)

   let near_mem_io_addr_range = Range {
      base: 'h_1000_0000,
      size: 'h_0001_0000    // 64K
   };

   // ----------------------------------------------------------------
   // Virtual Device

   let virt_dev_addr_range = Range {
      base: 'h_4000_0000,
      size: 'h_0010_0000    // 1M
   };

   // ----------------------------------------------------------------
   // Ethernet 0

   let ethernet_0_addr_range = Range {
      base: 'h_6210_0000,
      size: 'h_0004_0000    // 256K
   };

   // ----------------------------------------------------------------
   // DMA 0

   let dma_0_addr_range = Range {
      base: 'h_6220_0000,
      size: 'h_0001_0000    // 64K
   };

   // ----------------------------------------------------------------
   // UART 0

   let uart_0_addr_range = Range {
      base: 'h_6230_0000,
      size: 'h_0000_1000    // 4K
   };

   // ----------------------------------------------------------------
   // UART 1

   let uart_1_addr_range = Range {
      base: 'h_6230_1000,
      size: 'h_0000_1000    // 4K
   };

   // ----------------------------------------------------------------
   // GPIO 0

   let gpio_0_addr_range = Range {
      base: 'h_6FFF_0000,
      size: 'h_0001_0000    // 64K
   };

   // ----------------------------------------------------------------
   // Boot ROM

   let boot_rom_addr_range = Range {
      base: 'h_7000_0000,
      size: 'h_0000_1000    // 4K
   };

   // ----------------------------------------------------------------
   // DDR memory 0 uncached

   let ddr4_0_uncached_addr_range = Range {
      base: 'h_8000_0000,
      size: 'h_0000_0000    // No uncached ddr4 in this configuraion
   };

   // ----------------------------------------------------------------
   // DDR memory 0 cached

   let ddr4_0_cached_addr_range = Range {
      base: 'h0000_8000_0000,
      size: 'h0001_0000_0000    // 4G
   };

   // ----------------------------------------------------------------
   // BGAS router configuration

   let bgas_router_conf_addr_range = Range {
      base: 'h_0000_0200_0000_0000, // right passed the F2H interface
      size: 'h_0000_0000_0000_1000  // 4K
   };

   // ----------------------------------------------------------------
   // Global BGAS accesses

   // top 20 bits, with MSB indicating global access, and remaining
   // 19 bits indicating the node id
   let global_bgas_addr_range = Range {
      base: 'h_8000_0000_0000_0000,
      size: 'h_FFFF_F000_0000_0000
   };

   // ----------------------------------------------------------------
   // F2H interface

   let f2h_addr_range = Range {
      base: 'h_0000_0100_0000_0000, // just some 1TB aligned base region not already taken
      size: 'h_0000_0100_0000_0000  // 1TB
   };

   // ----------------------------------------------------------------
   // Memory address predicate
   // Identifies memory addresses in the Fabric.
   // (Caches needs this information to cache these addresses.)

   function Bool fn_is_mem_addr (Fabric_Addr addr) =
      inRange (ddr4_0_cached_addr_range, addr);

   function Bool fn_is_IO_addr (Fabric_Addr addr, Bool imem_not_dmem) =
         inRange(ddr4_0_uncached_addr_range, addr)
      || inRange(boot_rom_addr_range, addr)
      || inRange(virt_dev_addr_range, addr)
      || (   (! imem_not_dmem)
          && (   inRange(plic_addr_range, addr)
              || inRange(near_mem_io_addr_range, addr)
              || inRange(f2h_addr_range, addr)
              || inRange(uart_0_addr_range, addr)
              || inRange(uart_1_addr_range, addr)
              || inRange(bgas_router_conf_addr_range, addr)
              || inRange(global_bgas_addr_range, addr)));

   // ----------------------------------------------------------------
   // PC, MTVEC and NMIVEC reset values

   Bit #(64) pc_reset_value =
     (genC) ? fromMaybe ( boot_rom_addr_range.base
                        , getEnvInt ("CHERI_BGAS_PC_RESET_VALUE") )
            : boot_rom_addr_range.base;
   Bit #(64) mtvec_reset_value  = 'h1000;    // TODO
   Bit #(64) nmivec_reset_value = ?;         // TODO

   // ================================================================
   // INTERFACE

   method  Range#(Wd_Addr)  m_plic_addr_range = plic_addr_range;
   method  Range#(Wd_Addr)  m_near_mem_io_addr_range = near_mem_io_addr_range;
   method  Range#(Wd_Addr)  m_ethernet_0_addr_range = ethernet_0_addr_range;
   method  Range#(Wd_Addr)  m_dma_0_addr_range = dma_0_addr_range;
   method  Range#(Wd_Addr)  m_uart_0_addr_range = uart_0_addr_range;
   method  Range#(Wd_Addr)  m_uart_1_addr_range = uart_1_addr_range;
   method  Range#(Wd_Addr)  m_gpio_0_addr_range = gpio_0_addr_range;
   method  Range#(Wd_Addr)  m_boot_rom_addr_range = boot_rom_addr_range;
   method  Range#(Wd_Addr)  m_virt_dev_addr_range = virt_dev_addr_range;
   method  Range#(Wd_Addr)  m_ddr4_0_uncached_addr_range = ddr4_0_uncached_addr_range;
   method  Range#(Wd_Addr)  m_ddr4_0_cached_addr_range = ddr4_0_cached_addr_range;
   method  Range#(Wd_Addr)  m_global_bgas_addr_range = global_bgas_addr_range;
   method  Range#(Wd_Addr)  m_bgas_router_conf_addr_range = bgas_router_conf_addr_range;
   method  Range#(Wd_Addr)  m_f2h_addr_range = f2h_addr_range;
   method  Range#(Wd_Addr)  m_mem0_controller_addr_range = ddr4_0_cached_addr_range;

   method  Bool  m_is_mem_addr (Fabric_Addr addr) = fn_is_mem_addr (addr);

   method  Bool  m_is_IO_addr (Fabric_Addr addr, Bool imem_not_dmem) = fn_is_IO_addr (addr, imem_not_dmem);

   method  Bool  m_is_near_mem_IO_addr (Fabric_Addr addr) = inRange (near_mem_io_addr_range, addr);

   method  Bit #(64)  m_pc_reset_value     = pc_reset_value;
   method  Bit #(64)  m_mtvec_reset_value  = mtvec_reset_value;
   method  Bit #(64)  m_nmivec_reset_value = nmivec_reset_value;
endmodule

// ================================================================
// Interrupt request numbers (== index in to vector of
// interrupt-request lines in Core)

typedef  16  N_External_Interrupt_Sources;
Integer  n_external_interrupt_sources = valueOf (N_External_Interrupt_Sources);

// ================================================================

endpackage

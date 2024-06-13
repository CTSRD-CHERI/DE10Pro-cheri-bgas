//
// SPDX-License-Identifier: BSD-2-Clause
//
// Copyright (c) 2019 A. Theodore Markettos
// Copyright (c) 2021 Alexandre Joannou
// All rights reserved.
//
// This software was developed by SRI International, the University of
// Cambridge Computer Laboratory (Department of Computer Science and
// Technology), and ARM Research under DARPA contract HR0011-18-C-0016
// ("ECATS"), as part of the DARPA SSITH research programme.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
// SUCH DAMAGE.
//

//`define ENABLE_FLASH
`define ENABLE_DDR4A
`define ENABLE_DDR4B
`define ENABLE_DDR4C
`define ENABLE_DDR4D
//`define ENABLE_SI5340
`define ENABLE_FAN_I2C
//`define ENABLE_POWER_MONITOR_I2C
`define ENABLE_TEMP_I2C
//`define ENABLE_GPIO
//`define ENABLE_PCIE
`define ENABLE_QSFP28A
`define ENABLE_QSFP28B
`define ENABLE_QSFP28C
`define ENABLE_QSFP28D
`define ENABLE_HPS

module DE10Pro_cheri_bgas (
  input         CLK_100_B3I
//, input         CLK_50_B2C
//, input         CLK_50_B2L
//, input         CLK_50_B3C
, input         CLK_50_B3I
//, input         CLK_50_B3L
, input         CPU_RESET_n
//, input   [1:0] BUTTON
//, input   [1:0] SW
, output  [3:0] LED
`ifdef ENABLE_FLASH
, output        FLASH_CLK
, output [27:1] FLASH_A
, inout  [15:0] FLASH_D
, output        FLASH_CE_n
, output        FLASH_WE_n
, output        FLASH_OE_n
, output        FLASH_ADV_n
, output        FLASH_RESET_n
, input         FLASH_RDY_BSY_n
`endif // ENABLE_FLASH
`ifdef ENABLE_DDR4A
, input         DDR4A_REFCLK_p
, output [16:0] DDR4A_A
, output  [1:0] DDR4A_BA
, output  [1:0] DDR4A_BG
, output        DDR4A_CK
, output        DDR4A_CK_n
, output        DDR4A_CKE
, inout   [8:0] DDR4A_DQS
, inout   [8:0] DDR4A_DQS_n
, inout  [71:0] DDR4A_DQ
, inout   [8:0] DDR4A_DBI_n
, output        DDR4A_CS_n
, output        DDR4A_RESET_n
, output        DDR4A_ODT
, output        DDR4A_PAR
, input         DDR4A_ALERT_n
, output        DDR4A_ACT_n
//, input         DDR4A_EVENT_n
//, inout         DDR4A_SCL
//, inout         DDR4A_SDA
, input         DDR4A_RZQ
`endif // ENABLE_DDR4A
`ifdef ENABLE_DDR4B
, input         DDR4B_REFCLK_p
, output [16:0] DDR4B_A
, output  [1:0] DDR4B_BA
, output  [1:0] DDR4B_BG
, output        DDR4B_CK
, output        DDR4B_CK_n
, output        DDR4B_CKE
, inout   [8:0] DDR4B_DQS
, inout   [8:0] DDR4B_DQS_n
, inout  [71:0] DDR4B_DQ
, inout   [8:0] DDR4B_DBI_n
, output        DDR4B_CS_n
, output        DDR4B_RESET_n
, output        DDR4B_ODT
, output        DDR4B_PAR
, input         DDR4B_ALERT_n
, output        DDR4B_ACT_n
//, input         DDR4B_EVENT_n
//, inout         DDR4B_SCL
//, inout         DDR4B_SDA
, input         DDR4B_RZQ
`endif // ENABLE_DDR4B
`ifdef ENABLE_DDR4C
, input         DDR4C_REFCLK_p
, output [16:0] DDR4C_A
, output  [1:0] DDR4C_BA
, output  [1:0] DDR4C_BG
, output        DDR4C_CK
, output        DDR4C_CK_n
, output        DDR4C_CKE
, inout   [8:0] DDR4C_DQS
, inout   [8:0] DDR4C_DQS_n
, inout  [71:0] DDR4C_DQ
, inout   [8:0] DDR4C_DBI_n
, output        DDR4C_CS_n
, output        DDR4C_RESET_n
, output        DDR4C_ODT
, output        DDR4C_PAR
, input         DDR4C_ALERT_n
, output        DDR4C_ACT_n
//, input         DDR4C_EVENT_n
//, inout         DDR4C_SCL
//, inout         DDR4C_SDA
, input         DDR4C_RZQ
`endif // ENABLE_DDR4C
`ifdef ENABLE_DDR4D
, input         DDR4D_REFCLK_p
, output [16:0] DDR4D_A
, output  [1:0] DDR4D_BA
, output  [1:0] DDR4D_BG
, output        DDR4D_CK
, output        DDR4D_CK_n
, output        DDR4D_CKE
, inout   [8:0] DDR4D_DQS
, inout   [8:0] DDR4D_DQS_n
, inout  [71:0] DDR4D_DQ
, inout   [8:0] DDR4D_DBI_n
, output        DDR4D_CS_n
, output        DDR4D_RESET_n
, output        DDR4D_ODT
, output        DDR4D_PAR
, input         DDR4D_ALERT_n
, output        DDR4D_ACT_n
//, input         DDR4D_EVENT_n
//, inout         DDR4D_SCL
//, inout         DDR4D_SDA
, input         DDR4D_RZQ
`endif // ENABLE_DDR4D
`ifdef ENABLE_SI5340
, inout         SI5340A0_I2C_SCL
, inout         SI5340A0_I2C_SDA
, input         SI5340A0_INTR
, output        SI5340A0_OE_n
, output        SI5340A0_RST_n
, inout         SI5340A1_I2C_SCL
, inout         SI5340A1_I2C_SDA
, input         SI5340A1_INTR
, output        SI5340A1_OE_n
, output        SI5340A1_RST_n
`endif // ENABLE_SI5340
`ifdef ENABLE_FAN_I2C
, inout         FAN_I2C_SCL
, inout         FAN_I2C_SDA
//, input         FAN_ALERT_n
`endif // ENABLE_FAN_I2C
`ifdef ENABLE_POWER_MONITOR_I2C
, inout         POWER_MONITOR_I2C_SCL
, inout         POWER_MONITOR_I2C_SDA
, input         POWER_MONITOR_ALERT_n
`endif // ENABLE_POWER_MONITOR_I2C
`ifdef ENABLE_TEMP_I2C
, inout         TEMP_I2C_SCL
, inout         TEMP_I2C_SDA
`endif // ENABLE_TEMP_I2C
`ifdef ENABLE_GPIO
, inout   [1:0] GPIO_CLK
, inout   [3:0] GPIO_P
`endif // ENABLE_GPIO
`ifdef ENABLE_PCIE
, inout         PCIE_SMBCLK
, inout         PCIE_SMBDAT
, input         PCIE_REFCLK_p
, output [15:0] PCIE_TX_p
, input  [15:0] PCIE_RX_p
, input         PCIE_PERST_n
, output        PCIE_WAKE_n
`endif // ENABLE_PCIE
`ifdef ENABLE_QSFP28A
, input         QSFP28A_REFCLK_p
, output  [3:0] QSFP28A_TX_p
, input   [3:0] QSFP28A_RX_p
, input         QSFP28A_INTERRUPT_n
, output        QSFP28A_LP_MODE
, input         QSFP28A_MOD_PRS_n
, output        QSFP28A_MOD_SEL_n
, output        QSFP28A_RST_n
, inout         QSFP28A_SCL
, inout         QSFP28A_SDA
`endif // ENABLE_QSFP28A
`ifdef ENABLE_QSFP28B
, input         QSFP28B_REFCLK_p
, output  [3:0] QSFP28B_TX_p
, input   [3:0] QSFP28B_RX_p
, input         QSFP28B_INTERRUPT_n
, output        QSFP28B_LP_MODE
, input         QSFP28B_MOD_PRS_n
, output        QSFP28B_MOD_SEL_n
, output        QSFP28B_RST_n
, inout         QSFP28B_SCL
, inout         QSFP28B_SDA
`endif // ENABLE_QSFP28B
`ifdef ENABLE_QSFP28C
, input         QSFP28C_REFCLK_p
, output  [3:0] QSFP28C_TX_p
, input   [3:0] QSFP28C_RX_p
, input         QSFP28C_INTERRUPT_n
, output        QSFP28C_LP_MODE
, input         QSFP28C_MOD_PRS_n
, output        QSFP28C_MOD_SEL_n
, output        QSFP28C_RST_n
, inout         QSFP28C_SCL
, inout         QSFP28C_SDA
`endif // ENABLE_QSFP28C
`ifdef ENABLE_QSFP28D
, input         QSFP28D_REFCLK_p
, output  [3:0] QSFP28D_TX_p
, input   [3:0] QSFP28D_RX_p
, input         QSFP28D_INTERRUPT_n
, output        QSFP28D_LP_MODE
, input         QSFP28D_MOD_PRS_n
, output        QSFP28D_MOD_SEL_n
, output        QSFP28D_RST_n
, inout         QSFP28D_SCL
, inout         QSFP28D_SDA
`endif // ENABLE_QSFP28D
`ifdef ENABLE_HPS
, input         HPS_USB0_CLK
, output        HPS_USB0_STP
, input         HPS_USB0_DIR
, inout   [7:0] HPS_USB0_DATA
, input         HPS_USB0_NXT
, output        HPS_EMAC0_TX_CLK
, output        HPS_EMAC0_TX_CTL
, input         HPS_EMAC0_RX_CLK
, input         HPS_EMAC0_RX_CTL
, output  [3:0] HPS_EMAC0_TXD
, input   [3:0] HPS_EMAC0_RXD
, inout         HPS_EMAC0_MDIO
, output        HPS_EMAC0_MDC
, output        HPS_UART0_TX
, input         HPS_UART0_RX
, output        HPS_FPGA_UART1_TX
, input         HPS_FPGA_UART1_RX
, output        HPS_SD_CLK
, inout         HPS_SD_CMD
, inout   [3:0] HPS_SD_DATA
, input         HPS_OSC_CLK
, inout         HPS_LED
, inout         HPS_KEY
, inout         HPS_CARD_PRSNT_n
`endif
//, input         EXP_EN
//, inout         UFL_CLKIN_p
//, inout         UFL_CLKIN_n
);

  wire   clk_100;
  assign clk_100 = CLK_100_B3I;
  wire   clk_50;
  assign clk_50 = CLK_50_B3I;
  wire   clk_sys;

  // Reset logic - use done signals (delayed a few cycles) negated
  //////////////////////////////////////////////////////////////////////////////
  // Reset release
  wire ninit_done;
  reg ninit_done_delayed [1:0];
  reset_release reset_release (.ninit_done(ninit_done));
  always @(posedge clk_sys) begin
    ninit_done_delayed[1] <= ninit_done;
    ninit_done_delayed[0] <= ninit_done_delayed[1];
  end
  // DDR reset signal
  wire   ddr_reset;
  assign ddr_reset = ninit_done_delayed[0] || !CPU_RESET_n;
  // Aggregate DDR reset done signals
  wire ddr_reset_done;
  wire ddrb_reset_done;
  wire ddrc_reset_done;
  wire ddrd_reset_done;
  `ifndef ENABLE_DDR4B
  assign ddrb_reset_done = 1;
  `endif
  `ifndef ENABLE_DDR4C
  assign ddrc_reset_done = 1;
  `endif
  `ifndef ENABLE_DDR4D
  assign ddrd_reset_done = 1;
  `endif
  reg ddrb_reset_done_delayed [1:0];
  always @(posedge clk_sys) begin
    ddrb_reset_done_delayed[1] <= ddrb_reset_done;
    ddrb_reset_done_delayed[0] <= ddrb_reset_done_delayed[1];
  end
  reg ddrc_reset_done_delayed [1:0];
  always @(posedge clk_sys) begin
    ddrc_reset_done_delayed[1] <= ddrc_reset_done;
    ddrc_reset_done_delayed[0] <= ddrc_reset_done_delayed[1];
  end
  reg ddrd_reset_done_delayed [1:0];
  always @(posedge clk_sys) begin
    ddrd_reset_done_delayed[1] <= ddrd_reset_done;
    ddrd_reset_done_delayed[0] <= ddrd_reset_done_delayed[1];
  end
  
  // other reset inputs
  wire h2f_reset;
  wire fan_reset_n;
  // combined reset
  wire   soc_reset;
  assign soc_reset =    ninit_done_delayed[0]
                     || !ddrb_reset_done_delayed[0]
							|| !ddrc_reset_done_delayed[0]
							|| !ddrd_reset_done_delayed[0]
                     || !CPU_RESET_n
                     || h2f_reset;

  assign SI5340A0_RST_n = 1'b1;
  assign SI5340A1_RST_n = 1'b1;

  assign SI5340A0_OE_n = 1'b0;
  assign SI5340A1_OE_n = 1'b0;

  wire [3:0] LED_OUT;
  assign LED = ~LED_OUT;

  toplevel soc (
    .reset_reset                                   (soc_reset)
  //, .clk_clk                                       (clk_100)
  , .clk_out_clk                                     (clk_sys)
  `ifdef ENABLE_DDR4B
  , .emif_ddrb_local_reset_req_local_reset_req     (ddr_reset)
  , .emif_ddrb_local_reset_status_local_reset_done (ddrb_reset_done)
  , .emif_ddrb_pll_ref_clk_clk                     (DDR4B_REFCLK_p)
  , .emif_ddrb_oct_oct_rzqin                       (DDR4B_RZQ)
  , .emif_ddrb_mem_mem_ck                          (DDR4B_CK)
  , .emif_ddrb_mem_mem_ck_n                        (DDR4B_CK_n)
  , .emif_ddrb_mem_mem_a                           (DDR4B_A)
  , .emif_ddrb_mem_mem_act_n                       (DDR4B_ACT_n)
  , .emif_ddrb_mem_mem_ba                          (DDR4B_BA)
  , .emif_ddrb_mem_mem_bg                          (DDR4B_BG)
  , .emif_ddrb_mem_mem_cke                         (DDR4B_CKE)
  , .emif_ddrb_mem_mem_cs_n                        (DDR4B_CS_n)
  , .emif_ddrb_mem_mem_odt                         (DDR4B_ODT)
  , .emif_ddrb_mem_mem_reset_n                     (DDR4B_RESET_n)
  , .emif_ddrb_mem_mem_par                         (DDR4B_PAR)
  , .emif_ddrb_mem_mem_alert_n                     (DDR4B_ALERT_n)
  , .emif_ddrb_mem_mem_dqs                         (DDR4B_DQS)
  , .emif_ddrb_mem_mem_dqs_n                       (DDR4B_DQS_n)
  , .emif_ddrb_mem_mem_dq                          (DDR4B_DQ)
  , .emif_ddrb_mem_mem_dbi_n                       (DDR4B_DBI_n)
  //, .emif_ddrb_status_local_cal_success            (_connected_to_emif_ddrb_status_local_cal_success_)
  //, .emif_ddrb_status_local_cal_fail               (_connected_to_emif_ddrb_status_local_cal_fail_)
  `endif // ENABLE_DDR4B
  `ifdef ENABLE_DDR4C
  , .emif_ddrc_local_reset_req_local_reset_req     (ddr_reset)
  , .emif_ddrc_local_reset_status_local_reset_done (ddrc_reset_done)
  , .emif_ddrc_pll_ref_clk_clk                     (DDR4C_REFCLK_p)
  , .emif_ddrc_oct_oct_rzqin                       (DDR4C_RZQ)
  , .emif_ddrc_mem_mem_ck                          (DDR4C_CK)
  , .emif_ddrc_mem_mem_ck_n                        (DDR4C_CK_n)
  , .emif_ddrc_mem_mem_a                           (DDR4C_A)
  , .emif_ddrc_mem_mem_act_n                       (DDR4C_ACT_n)
  , .emif_ddrc_mem_mem_ba                          (DDR4C_BA)
  , .emif_ddrc_mem_mem_bg                          (DDR4C_BG)
  , .emif_ddrc_mem_mem_cke                         (DDR4C_CKE)
  , .emif_ddrc_mem_mem_cs_n                        (DDR4C_CS_n)
  , .emif_ddrc_mem_mem_odt                         (DDR4C_ODT)
  , .emif_ddrc_mem_mem_reset_n                     (DDR4C_RESET_n)
  , .emif_ddrc_mem_mem_par                         (DDR4C_PAR)
  , .emif_ddrc_mem_mem_alert_n                     (DDR4C_ALERT_n)
  , .emif_ddrc_mem_mem_dqs                         (DDR4C_DQS)
  , .emif_ddrc_mem_mem_dqs_n                       (DDR4C_DQS_n)
  , .emif_ddrc_mem_mem_dq                          (DDR4C_DQ)
  , .emif_ddrc_mem_mem_dbi_n                       (DDR4C_DBI_n)
  //, .emif_ddrc_status_local_cal_success            (_connected_to_emif_ddrc_status_local_cal_success_)
  //, .emif_ddrc_status_local_cal_fail               (_connected_to_emif_ddrc_status_local_cal_fail_)
  `endif // ENABLE_DDR4C
  `ifdef ENABLE_DDR4D
  , .emif_ddrd_local_reset_req_local_reset_req     (ddr_reset)
  , .emif_ddrd_local_reset_status_local_reset_done (ddrd_reset_done)
  , .emif_ddrd_pll_ref_clk_clk                     (DDR4D_REFCLK_p)
  , .emif_ddrd_oct_oct_rzqin                       (DDR4D_RZQ)
  , .emif_ddrd_mem_mem_ck                          (DDR4D_CK)
  , .emif_ddrd_mem_mem_ck_n                        (DDR4D_CK_n)
  , .emif_ddrd_mem_mem_a                           (DDR4D_A)
  , .emif_ddrd_mem_mem_act_n                       (DDR4D_ACT_n)
  , .emif_ddrd_mem_mem_ba                          (DDR4D_BA)
  , .emif_ddrd_mem_mem_bg                          (DDR4D_BG)
  , .emif_ddrd_mem_mem_cke                         (DDR4D_CKE)
  , .emif_ddrd_mem_mem_cs_n                        (DDR4D_CS_n)
  , .emif_ddrd_mem_mem_odt                         (DDR4D_ODT)
  , .emif_ddrd_mem_mem_reset_n                     (DDR4D_RESET_n)
  , .emif_ddrd_mem_mem_par                         (DDR4D_PAR)
  , .emif_ddrd_mem_mem_alert_n                     (DDR4D_ALERT_n)
  , .emif_ddrd_mem_mem_dqs                         (DDR4D_DQS)
  , .emif_ddrd_mem_mem_dqs_n                       (DDR4D_DQS_n)
  , .emif_ddrd_mem_mem_dq                          (DDR4D_DQ)
  , .emif_ddrd_mem_mem_dbi_n                       (DDR4D_DBI_n)
  //, .emif_ddrd_status_local_cal_success            (_connected_to_emif_ddrd_status_local_cal_success_)
  //, .emif_ddrd_status_local_cal_fail               (_connected_to_emif_ddrd_status_local_cal_fail_)
  `endif // ENABLE_DDR4D
  `ifdef ENABLE_HPS
  , .emif_hps_ddra_pll_ref_clk_clk                  (DDR4A_REFCLK_p)
  , .emif_hps_ddra_mem_mem_ck                       (DDR4A_CK)
  , .emif_hps_ddra_mem_mem_ck_n                     (DDR4A_CK_n)
  , .emif_hps_ddra_mem_mem_a                        (DDR4A_A)
  , .emif_hps_ddra_mem_mem_act_n                    (DDR4A_ACT_n)
  , .emif_hps_ddra_mem_mem_ba                       (DDR4A_BA)
  , .emif_hps_ddra_mem_mem_bg                       (DDR4A_BG)
  , .emif_hps_ddra_mem_mem_cke                      (DDR4A_CKE)
  , .emif_hps_ddra_mem_mem_cs_n                     (DDR4A_CS_n)
  , .emif_hps_ddra_mem_mem_odt                      (DDR4A_ODT)
  , .emif_hps_ddra_mem_mem_reset_n                  (DDR4A_RESET_n)
  , .emif_hps_ddra_mem_mem_par                      (DDR4A_PAR)
  , .emif_hps_ddra_mem_mem_alert_n                  (DDR4A_ALERT_n)
  , .emif_hps_ddra_mem_mem_dqs                      (DDR4A_DQS)
  , .emif_hps_ddra_mem_mem_dqs_n                    (DDR4A_DQS_n)
  , .emif_hps_ddra_mem_mem_dq                       (DDR4A_DQ)
  , .emif_hps_ddra_mem_mem_dbi_n                    (DDR4A_DBI_n)
  , .emif_hps_ddra_oct_oct_rzqin                    (DDR4A_RZQ)
  , .hps_io_hps_io_phery_usb0_CLK                   (HPS_USB0_CLK)
  , .hps_io_hps_io_phery_usb0_STP                   (HPS_USB0_STP)
  , .hps_io_hps_io_phery_usb0_DIR                   (HPS_USB0_DIR)
  , .hps_io_hps_io_phery_usb0_NXT                   (HPS_USB0_NXT)
  , .hps_io_hps_io_phery_usb0_DATA0                 (HPS_USB0_DATA[0])
  , .hps_io_hps_io_phery_usb0_DATA1                 (HPS_USB0_DATA[1])
  , .hps_io_hps_io_phery_usb0_DATA2                 (HPS_USB0_DATA[2])
  , .hps_io_hps_io_phery_usb0_DATA3                 (HPS_USB0_DATA[3])
  , .hps_io_hps_io_phery_usb0_DATA4                 (HPS_USB0_DATA[4])
  , .hps_io_hps_io_phery_usb0_DATA5                 (HPS_USB0_DATA[5])
  , .hps_io_hps_io_phery_usb0_DATA6                 (HPS_USB0_DATA[6])
  , .hps_io_hps_io_phery_usb0_DATA7                 (HPS_USB0_DATA[7])
  , .hps_io_hps_io_phery_emac0_TX_CLK               (HPS_EMAC0_TX_CLK)
  , .hps_io_hps_io_phery_emac0_RX_CLK               (HPS_EMAC0_RX_CLK)
  , .hps_io_hps_io_phery_emac0_TX_CTL               (HPS_EMAC0_TX_CTL)
  , .hps_io_hps_io_phery_emac0_RX_CTL               (HPS_EMAC0_RX_CTL)
  , .hps_io_hps_io_phery_emac0_TXD0                 (HPS_EMAC0_TXD[0])
  , .hps_io_hps_io_phery_emac0_TXD1                 (HPS_EMAC0_TXD[1])
  , .hps_io_hps_io_phery_emac0_RXD0                 (HPS_EMAC0_RXD[0])
  , .hps_io_hps_io_phery_emac0_RXD1                 (HPS_EMAC0_RXD[1])
  , .hps_io_hps_io_phery_emac0_TXD2                 (HPS_EMAC0_TXD[2])
  , .hps_io_hps_io_phery_emac0_TXD3                 (HPS_EMAC0_TXD[3])
  , .hps_io_hps_io_phery_emac0_RXD2                 (HPS_EMAC0_RXD[2])
  , .hps_io_hps_io_phery_emac0_RXD3                 (HPS_EMAC0_RXD[3])
  , .hps_io_hps_io_gpio_gpio1_io0                   (HPS_LED)
  , .hps_io_hps_io_gpio_gpio1_io1                   (HPS_KEY)
  , .hps_io_hps_io_phery_uart0_TX                   (HPS_UART0_TX)
  , .hps_io_hps_io_phery_uart0_RX                   (HPS_UART0_RX)
  , .hps_io_hps_io_phery_uart1_TX                   (HPS_FPGA_UART1_TX)
  , .hps_io_hps_io_phery_uart1_RX                   (HPS_FPGA_UART1_RX)
  , .hps_io_hps_io_phery_sdmmc_CCLK                 (HPS_SD_CLK)
  , .hps_io_hps_io_phery_sdmmc_CMD                  (HPS_SD_CMD)
  , .hps_io_hps_io_phery_sdmmc_D0                   (HPS_SD_DATA[0])
  , .hps_io_hps_io_phery_sdmmc_D1                   (HPS_SD_DATA[1])
  , .hps_io_hps_io_phery_sdmmc_D2                   (HPS_SD_DATA[2])
  , .hps_io_hps_io_phery_sdmmc_D3                   (HPS_SD_DATA[3])
  , .hps_io_hps_io_hps_ocs_clk                      (HPS_OSC_CLK)
  , .hps_io_hps_io_gpio_gpio1_io20                  (HPS_CARD_PRSNT_n)
  , .hps_io_hps_io_phery_emac0_MDIO                 (HPS_EMAC0_MDIO)
  , .hps_io_hps_io_phery_emac0_MDC                  (HPS_EMAC0_MDC)
  , .hps_h2f_reset_reset                            (h2f_reset)
  `endif // ENABLE_HPS
  `ifdef ENABLE_FAN_I2C
  `ifdef ENABLE_TEMP_I2C
  , .fan_controller_fan_i2c_sda                    (FAN_I2C_SDA)
  , .fan_controller_fan_i2c_scl                    (FAN_I2C_SCL)
  , .fan_controller_temp_i2c_sda                   (TEMP_I2C_SDA)
  , .fan_controller_temp_i2c_scl                   (TEMP_I2C_SCL)
  , .fan_controller_led_led                        (LED_OUT)
  , .fan_controller_reset_source_reset_n           (fan_reset_n)
  `endif // ENABLE_TEMP_I2C
  `endif // ENABLE_FAN_I2C
  `ifdef ENABLE_QSFP28A
  //, .seriallite3_wrapper_coe_link_status_a_coe_link_status           (link_status_A)
  , .seriallite3_wrapper_coe_qsfp28_rx_pins_x_a_coe_qsfp28_rx_pins_x (QSFP28A_RX_p)
  , .seriallite3_wrapper_coe_qsfp28_tx_pins_a_coe_qsfp28_tx_pins     (QSFP28A_TX_p)
  , .seriallite3_wrapper_csi_qsfp_refclk_a_clk                       (QSFP28A_REFCLK_p)
  `endif // ENABLE_QSFP28A
  `ifdef ENABLE_QSFP28B
  //, .seriallite3_wrapper_coe_link_status_b_coe_link_status           (link_status_B)
  , .seriallite3_wrapper_coe_qsfp28_rx_pins_x_b_coe_qsfp28_rx_pins_x (QSFP28B_RX_p)
  , .seriallite3_wrapper_coe_qsfp28_tx_pins_b_coe_qsfp28_tx_pins     (QSFP28B_TX_p)
  , .seriallite3_wrapper_csi_qsfp_refclk_b_clk                       (QSFP28B_REFCLK_p)
  `endif // ENABLE_QSFP28B
  `ifdef ENABLE_QSFP28C
  //, .seriallite3_wrapper_coe_link_status_c_coe_link_status           (link_status_C)
  , .seriallite3_wrapper_coe_qsfp28_rx_pins_x_c_coe_qsfp28_rx_pins_x (QSFP28C_RX_p)
  , .seriallite3_wrapper_coe_qsfp28_tx_pins_c_coe_qsfp28_tx_pins     (QSFP28C_TX_p)
  , .seriallite3_wrapper_csi_qsfp_refclk_c_clk                       (QSFP28C_REFCLK_p)
  `endif // ENABLE_QSFP28C
  `ifdef ENABLE_QSFP28D
  //, .seriallite3_wrapper_coe_link_status_d_coe_link_status           (link_status_D)
  , .seriallite3_wrapper_coe_qsfp28_rx_pins_x_d_coe_qsfp28_rx_pins_x (QSFP28D_RX_p)
  , .seriallite3_wrapper_coe_qsfp28_tx_pins_d_coe_qsfp28_tx_pins     (QSFP28D_TX_p)
  , .seriallite3_wrapper_csi_qsfp_refclk_d_clk                       (QSFP28D_REFCLK_p)
  `endif // ENABLE_QSFP28D
  );

endmodule

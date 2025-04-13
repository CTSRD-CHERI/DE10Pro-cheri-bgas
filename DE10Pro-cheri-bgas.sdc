#-
# SPDX-License-Identifier: BSD-2-Clause
#
# Copyright (c) 2019 A. Theodore Markettos
# Copyright (c) 2021 Alexandre Joannou
# All rights reserved.
#
# This software was developed by SRI International, the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology), and ARM Research under DARPA contract HR0011-18-C-0016
# ("ECATS"), as part of the DARPA SSITH research programme.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#


set_time_format -unit ns -decimal_places 3
create_clock -name MAIN_CLOCK_100 -period 10 [get_ports CLK_100_B3I]
create_clock -name MAIN_CLOCK_50 -period 20 [get_ports CLK_50_B3I]
create_clock -name EMIF_REF_CLOCK -period 3.75 [get_ports DDR4A_REFCLK_p]
create_clock -period "166.667 MHz" [get_ports DDR4B_REFCLK_p]
create_clock -period "166.667 MHz" [get_ports DDR4C_REFCLK_p]
create_clock -period "166.667 MHz" [get_ports DDR4D_REFCLK_p]
set_clock_groups -asynchronous -group {[get_clocks { DDR4B_REFCLK_p }]}
set_clock_groups -asynchronous -group {[get_clocks { DDR4C_REFCLK_p }]}
set_clock_groups -asynchronous -group {[get_clocks { DDR4D_REFCLK_p }]}
set_clock_groups -asynchronous -group { hps_user0_clk }
set_false_path -from [get_ports {CPU_RESET_n}]
set_false_path -from [get_ports {BUTTON[0]}] -to *
set_false_path -from [get_ports {BUTTON[1]}] -to *
set_false_path -from [get_ports {SW[0]}] -to *
set_false_path -from [get_ports {SW[1]}] -to *
set_false_path -from [get_ports {LED[0]}] -to *
set_false_path -from [get_ports {LED[1]}] -to *
set_false_path -from [get_ports {LED[2]}] -to *
set_false_path -from [get_ports {LED[3]}] -to *
set_false_path -from * -to [get_ports {LED[0]}]
set_false_path -from * -to [get_ports {LED[1]}]
set_false_path -from * -to [get_ports {LED[2]}]
set_false_path -from * -to [get_ports {LED[3]}]
set_max_skew -to [get_ports "HPS_EMAC0_MDC"] 2
set_max_skew -to [get_ports "HPS_EMAC0_MDIO"] 2
#source ./jtag.sdc

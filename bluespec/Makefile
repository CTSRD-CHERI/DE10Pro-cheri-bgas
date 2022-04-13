#-
# Copyright (c) 2021 Alexandre Joannou
# All rights reserved.
#
# This software was developed by SRI International and the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
# DARPA SSITH research programme.
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

BSC = bsc
BLUESPECDIR ?= $(shell which $(BSC) | xargs dirname | xargs dirname)/lib

NB_SYSTEMS ?= 1

SRCS  = CHERI_BGAS_Top.bsv

# relevant folders
DE10PROBSVSHELLDIR ?= $(CURDIR)/DE10Pro-bsv-shell
COREDIR ?= $(CURDIR)/Toooba
COREW_DIRS = $(COREDIR)/src_Core/Core:$(COREDIR)/src_Core/CPU:$(COREDIR)/src_Core/ISA:$(COREDIR)/src_Core/PLIC:$(COREDIR)/src_Core/Debug_Module:$(COREDIR)/src_Core/BSV_Additional_Libs:$(COREDIR)/src_Core/RISCY_OOO/procs/RV64G_OOO:$(COREDIR)/src_Core/RISCY_OOO/procs/lib:$(COREDIR)/src_Core/RISCY_OOO/coherence/src:$(COREDIR)/src_Core/RISCY_OOO/fpgautils/lib
WINDCOREIFCDIR ?= $(COREDIR)/libs/WindCoreInterface
CHERICAPLIBDIR ?= $(COREDIR)/libs/cheri-cap-lib
TAGCONTROLLERDIR ?= $(COREDIR)/libs/TagController
RISCVHPMEVENTSDIR ?= $(COREDIR)/libs/RISCV_HPM_Events
TAGCONTROLLER_DIRS = $(TAGCONTROLLERDIR)/TagController:$(TAGCONTROLLERDIR)/TagController/CacheCore
BLUESTUFFDIR ?= $(COREDIR)/libs/BlueStuff
BLUESTUFF_DIRS = $(BLUESTUFFDIR):$(BLUESTUFFDIR)/BlueBasics:$(BLUESTUFFDIR)/BlueUtils:$(BLUESTUFFDIR)/AXI:$(BLUESTUFFDIR)/Stratix10ChipID
RECIPEDIR = $(CURDIR)/Recipe

# search path for bsc imports
BSVPATH = +:$(DE10PROBSVSHELLDIR):$(RECIPEDIR):$(WINDCOREIFCDIR):$(RISCVHPMEVENTSDIR):$(CHERICAPLIBDIR):$(TAGCONTROLLER_DIRS):$(COREW_DIRS):$(BLUESTUFF_DIRS)
BSCFLAGS = -p $(BSVPATH)

# design configuration variables
# Toooba conf flags
BSCFLAGS += -D RV64
BSCFLAGS += -D ISA_PRIV_M -D ISA_PRIV_U -D ISA_PRIV_S -D SV39
BSCFLAGS += -D ISA_I -D ISA_M -D ISA_A -D ISA_F -D ISA_D -D ISA_FD_DIV -D ISA_C
BSCFLAGS += -D FABRIC64
BSCFLAGS += -D Near_Mem_Caches
BSCFLAGS += -D INCLUDE_GDB_CONTROL
BSCFLAGS += -D MEM64
BSCFLAGS += -D PERFORMANCE_MONITORING
# RiscyOOO flags
BSCFLAGS += -D NUM_CORES=1
ifeq ($(NB_SYSTEMS),1)
BSCFLAGS += -D CORE_SMALL
else
#BSCFLAGS += -D CORE_MINI
BSCFLAGS += -D CORE_TINY
endif
BSCFLAGS += -D CACHE_LARGE
BSCFLAGS += -D NO_XILINX
BSCFLAGS += -D XILINX_FP_FMA_LATENCY=3 # TODO why is this needed?
BSCFLAGS += -D XILINX_INT_MUL_LATENCY=2 # TODO why is this needed?
# from SSITH P3
BSCFLAGS += -keep-fires
BSCFLAGS += -aggressive-conditions
BSCFLAGS += -no-warn-action-shadowing
BSCFLAGS += -suppress-warnings G0020
# CHERI / Tag Controller conf and flags
BSCFLAGS += -D CheriMasterIDWidth=1
BSCFLAGS += -D CheriTransactionIDWidth=6
BSCFLAGS += -D USECAP
BSCFLAGS += -D RISCV
BSCFLAGS += -D CAP128
CAPSIZE = 128
TAGS_STRUCT = 0 64
TAGS_ALIGN = 32
# CHERI BGAS flags
BSCFLAGS += -D NB_CHERI_BGAS_SYSTEMS=$(NB_SYSTEMS)

# generated files directories
BUILDDIR ?= $(CURDIR)/build
BDIR ?= $(BUILDDIR)/bdir
SIMDIR ?= $(BUILDDIR)/simdir
VDIR ?= $(CURDIR)/verilogdir
VERILATORDIR ?= $(CURDIR)/verilatordir

BSCFLAGS += -bdir $(BDIR)
BSCFLAGS += +RTS -K128M -RTS
#BSCFLAGS += -show-schedule
#BSCFLAGS += -sched-dot
#BSCFLAGS += -show-range-conflict
#BSCFLAGS += -show-rule-rel \* \*
#BSCFLAGS += -steps-warn-interval n
BSCSIMFLAGS = -simdir $(SIMDIR)
BSCSIMFLAGS += -D NO_TAGTABLE_ZEROING
BSCVERILOGFLAGS = -vdir $(VDIR)
BSCVERILOGFLAGS += -unspecified-to X
BSCVERILOGFLAGS += -opt-undetermined-vals

rtl: $(VDIR)/mkCHERI_BGAS_Top_Sig.v

bluesim: $(SIMDIR)/sim_CHERI_BGAS

$(SIMDIR)/sim_CHERI_BGAS: CHERI_BGAS_Top_Sim.bsv $(SRCS) TagTableStructure.bsv
	mkdir -p $(BDIR)
	mkdir -p $(SIMDIR)
	$(BSC) $(BSCFLAGS) $(BSCSIMFLAGS) -sim -g mkCHERI_BGAS_Top_Sim -u CHERI_BGAS_Top_Sim.bsv
	$(BSC) $(BSCFLAGS) $(BSCSIMFLAGS) -sim -e mkCHERI_BGAS_Top_Sim -u -o $@ $(BLUESTUFFDIR)/BlueUtils/MemSim.c

verilatorsim: $(VDIR)/mkCHERI_BGAS_Top_Sim.v CHERI_BGAS_Top_verilator_sim.cpp
	$(eval TMP_V := $(shell mktemp -t mkCHERI_BGAS_Top_Sim_edited_XXXX.v))
	sed 's/$$imported_//' $(VDIR)/mkCHERI_BGAS_Top_Sim.v > $(TMP_V)
	verilator \
      -Mdir $(VERILATORDIR) \
      --prefix VmkCHERI_BGAS_Top_Sim \
      -o mkCHERI_BGAS_Top_Sim \
      -y $(VDIR) \
      -y $(BLUESPECDIR)/Verilog \
      -y $(BLUESTUFFDIR)/BlueUtils \
      -I$(VDIR) \
      -Wno-WIDTH \
      -Wno-CASEINCOMPLETE \
      -Wno-STMTDLY \
      -Wno-INITIALDLY \
      -Wno-UNSIGNED \
      -Wno-CMPCONST \
      -cc \
      $(TMP_V) \
      $(BLUESTUFFDIR)/BlueUtils/MemSim.v \
      -exe \
      CHERI_BGAS_Top_verilator_sim.cpp \
      $(BLUESTUFFDIR)/BlueUtils/MemSim.c
	#-Wno-fatal
	make -C $(VERILATORDIR) -j -f VmkCHERI_BGAS_Top_Sim.mk mkCHERI_BGAS_Top_Sim

$(VDIR)/mkCHERI_BGAS_Top_Sim.v: $(VDIR)/mkCHERI_BGAS_Top_Sig.v
	mkdir -p $(BDIR)
	mkdir -p $(VDIR)
	$(BSC) $(BSCFLAGS) $(BSCVERILOGFLAGS) -verilog -g mkCHERI_BGAS_Top_Sim -u CHERI_BGAS_Top_Sim.bsv

$(VDIR)/mkCHERI_BGAS_Top_Sig.v: $(SRCS) TagTableStructure.bsv GenerateHPMVector.bsv StatCounters.bsv
	mkdir -p $(BDIR)
	mkdir -p $(VDIR)
	$(BSC) $(BSCFLAGS) $(BSCVERILOGFLAGS) -verilog -g mkCHERI_BGAS_Top_Sig -u CHERI_BGAS_Top.bsv

#.PHONY: TagTableStructure.bsv
TagTableStructure.bsv: $(TAGCONTROLLERDIR)/tagsparams.py
	@echo "INFO: Re-generating CHERI tag controller parameters"
	$^ -v -c $(CAPSIZE) -s $(TAGS_STRUCT:"%"=%) -a $(TAGS_ALIGN) --data-store-base-addr 0xc0000000 -b $@ 0xbfff8000 0x17ffff000
	@echo "INFO: Re-generated CHERI tag controller parameters"

#.PHONY: GenerateHPMVector.bsv
GenerateHPMVector.bsv: $(RISCVHPMEVENTSDIR)/parse_counters.py
	@echo "INFO: Re-generating GenerateHPMVector bluespec file"
	$^ $(RISCVHPMEVENTSDIR)/counters.yaml -m ProcTypes -b $@
	@echo "INFO: Re-generated GenerateHPMVector bluespec file"

#.PHONY: StatCounters.bsv
StatCounters.bsv: $(RISCVHPMEVENTSDIR)/parse_counters.py
	@echo "INFO: Re-generating HPM events struct bluepsec file"
	$^ $(RISCVHPMEVENTSDIR)/counters.yaml -m ProcTypes -s $@
	@echo "INFO: Re-generated HPM events struct bluespec file"

.PHONY: clean mrproper

clean:
	rm -f TagTableStructure.bsv
	rm -f GenerateHPMVector.bsv
	rm -f StatCounters.bsv
	rm -f -r $(BUILDDIR)

mrproper: clean
	rm -f -r $(VDIR)
	rm -f -r $(VERILATORDIR)

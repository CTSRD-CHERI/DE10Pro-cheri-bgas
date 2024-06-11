#-
# Copyright (c) 2021-2022 Alexandre Joannou
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
CORE_NUM ?= 2

# RiscyOOO flags
ifeq ($(NB_SYSTEMS),1)
CORE_SIZE ?= SMALL
else
#BSCFLAGS += -D CORE_MINI
CORE_SIZE ?= TINY
endif

# If we're running in simulation, tell RISCY_Config to include RVFI debug logs.

# $(filter bluesim,$(MAKECMDGOALS)) = return the list of goals in MAKECMDGOALS that are equal to bluesim
# e.g. for `make clean` returns (), for `make bluesim` returns (bluesim,)
# ifneq (x,) = if x is not nothing
# => if (the list of goals in MAKECMDGOALS that are equal to bluesim) is not nothing
# => if `bluesim` is one of the goals, include these defines.
ifneq ($(filter bluesim,$(MAKECMDGOALS)),)
RVFI = true
endif

TOPFILE = $(CURDIR)/CHERI_BGAS_Top.bsv

# relevant folders
DE10PROBSVSHELLDIR ?= $(CURDIR)/DE10Pro-bsv-shell
CORE_DIR ?= $(CURDIR)/Toooba

#================================================================
# Parameter settings for MIT RISCY

include $(CORE_DIR)/builds/Resources/Include_RISCY_Config.mk

VIRTUALDEVICE_DIRS ?= $(CURDIR)/VirtualDevice
RECIPEDIR = $(CURDIR)/Recipe
DMAWINDOWDIR = $(CURDIR)/DmaWindow

# generated files directories
BUILDDIR ?= $(CURDIR)/build
BDIR ?= $(BUILDDIR)/bdir
SIMDIR ?= $(BUILDDIR)/simdir
VDIR ?= $(CURDIR)/verilogdir
VERILATORDIR ?= $(CURDIR)/verilatordir

# search path for bsc imports
EXTRABSVPATH = +:$(DE10PROBSVSHELLDIR):$(RECIPEDIR):$(VIRTUALDEVICE_DIRS):$(DMAWINDOWDIR)
BSCFLAGS = $(BSC_PATH) -p $(EXTRABSVPATH)

# design configuration variables
# Toooba conf flags
BSCFLAGS += $(BSC_COMPILATION_FLAGS)
BSCFLAGS += -D INCLUDE_GDB_CONTROL
BSCFLAGS += -D PERFORMANCE_MONITORING

BSCFLAGS += -D NO_XILINX
BSCFLAGS += -D XILINX_FP_FMA_LATENCY=3 # TODO why is this needed?
BSCFLAGS += -D XILINX_INT_MUL_LATENCY=2 # TODO why is this needed?
# from SSITH P3
BSCFLAGS += -keep-fires
BSCFLAGS += -aggressive-conditions
BSCFLAGS += -no-warn-action-shadowing
BSCFLAGS += -suppress-warnings G0020
# CHERI / Tag Controller conf and flags
ifneq ($(filter bluesim,$(MAKECMDGOALS)),)
# Memory is implicitly zero         => -D NO_TAGTABLE_ZEROING
# We are in simulation              => -D BSIM
# We have already set up RVFI in Include_RISCY_Config.mk
BSCFLAGS += -D NO_TAGTABLE_ZEROING -D BSIM
endif
BSCFLAGS += -D USECAP
BSCFLAGS += -D FABRIC64
CAPSIZE = 128
TAGS_STRUCT = 0 64
TAGS_ALIGN = 32
# CHERI BGAS flags
BSCFLAGS += -D NB_CHERI_BGAS_SYSTEMS=$(NB_SYSTEMS)

TOPFILE_BO = $(BDIR)/$(patsubst %.bsv,%.bo,$(notdir $(TOPFILE)))

BSCFLAGS += -bdir $(BDIR)
BSCFLAGS += +RTS -K128M -RTS
#BSCFLAGS += -show-schedule
#BSCFLAGS += -sched-dot
#BSCFLAGS += -show-range-conflict
#BSCFLAGS += -show-rule-rel \* \*
#BSCFLAGS += -steps-warn-interval n
BSCBSIMFLAGS = -simdir $(SIMDIR)
BSCVERILOGFLAGS = -vdir $(VDIR)
BSCVERILOGFLAGS += -unspecified-to X
BSCVERILOGFLAGS += -opt-undetermined-vals

ifeq (,$(filter clean mrproper,$(MAKECMDGOALS)))
include .depends.mk

.depends.mk: TagTableStructure.bsv GenerateHPMVector.bsv StatCounters.bsv | $(BDIR)
	if ! bluetcl -exec makedepend -elab $(BSCFLAGS) -o $@ $(TOPFILE); then rm -f $@ && false; fi
endif

$(BDIR):
	mkdir -p $(BDIR)

$(BDIR)/%.bo:
	$(info building $@)
	bsc -elab $(BSCFLAGS) $<

rtl: $(VDIR)/mkCHERI_BGAS_Top_Sig.v

bluesim: $(SIMDIR)/sim_CHERI_BGAS

$(SIMDIR)/sim_CHERI_BGAS: CHERI_BGAS_Top_Sim.bsv $(TOPFILE_BO) $(BLUESTUFFDIR)/BlueUtils/MemSim.c $(BLUEUNIXBRIDGESDIR)/BlueUnixFifo.c TagTableStructure.bsv GenerateHPMVector.bsv StatCounters.bsv
	mkdir -p $(BDIR)
	mkdir -p $(SIMDIR)
	$(BSC) $(BSCFLAGS) $(BSCBSIMFLAGS) -sim -g mkCHERI_BGAS_Top_Sim -u CHERI_BGAS_Top_Sim.bsv
	$(BSC) $(BSCFLAGS) $(BSCBSIMFLAGS) -sim -e mkCHERI_BGAS_Top_Sim -u -o $@ $(BLUESTUFFDIR)/BlueUtils/MemSim.c $(BLUESTUFFDIR)/BlueUtils/SimUtils.c $(BLUEUNIXBRIDGESDIR)/BlueUnixFifo.c

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
      $(BLUESTUFFDIR)/BlueUtils/MemSim.c \
      $(BLUESTUFFDIR)/BlueUtils/SimUtils.c
	#-Wno-fatal
	make -C $(VERILATORDIR) -j -f VmkCHERI_BGAS_Top_Sim.mk mkCHERI_BGAS_Top_Sim

$(VDIR)/mkCHERI_BGAS_Top_Sim.v: $(VDIR)/mkCHERI_BGAS_Top_Sig.v
	mkdir -p $(BDIR)
	mkdir -p $(VDIR)
	$(BSC) $(BSCFLAGS) $(BSCVERILOGFLAGS) -verilog -g mkCHERI_BGAS_Top_Sim -u CHERI_BGAS_Top_Sim.bsv

TOOOBA_SOURCE_FILES = $(shell find $(CURDIR)/Toooba/ -type f -name '*.bsv')
$(VDIR)/mkCHERI_BGAS_Top_Sig.v: $(TOPFILE) TagTableStructure.bsv GenerateHPMVector.bsv StatCounters.bsv $(TOOOBA_SOURCE_FILES)
	mkdir -p $(BDIR)
	mkdir -p $(VDIR)
	$(BSC) $(BSCFLAGS) $(BSCVERILOGFLAGS) -verilog -g mkCHERI_BGAS_Top_Sig -u CHERI_BGAS_Top.bsv

#.PHONY: TagTableStructure.bsv
TagTableStructure.bsv: $(TAG_CONTROLLER_DIR)/tagsparams.py
	@echo "INFO: Re-generating CHERI tag controller parameters"
	$^ -v -c $(CAPSIZE) -s $(TAGS_STRUCT:"%"=%) -a $(TAGS_ALIGN) --data-store-base-addr 0xc0000000 -b $@ 0xbfff8000 0x17ffff000
	@echo "INFO: Re-generated CHERI tag controller parameters"

#.PHONY: GenerateHPMVector.bsv
GenerateHPMVector.bsv: $(RISCV_HPM_EVENTS_DIR)/parse_counters.py
	@echo "INFO: Re-generating GenerateHPMVector bluespec file"
	$^ $(RISCV_HPM_EVENTS_DIR)/counters.yaml -m ProcTypes -b $@
	@echo "INFO: Re-generated GenerateHPMVector bluespec file"

#.PHONY: StatCounters.bsv
StatCounters.bsv: $(RISCV_HPM_EVENTS_DIR)/parse_counters.py
	@echo "INFO: Re-generating HPM events struct bluepsec file"
	$^ $(RISCV_HPM_EVENTS_DIR)/counters.yaml -m ProcTypes -s $@
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

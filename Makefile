#-
# Copyright (c) 2021 Alexandre Joannou
# Copyright (c) 2022 Franz Fuchs
# All rights reserved.
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

BSVSRCDIR = $(CURDIR)/bluespec
BLUESTUFFDIR = $(BSVSRCDIR)/Toooba/libs/BlueStuff
export VIPBUNDLEDIR = $(CURDIR)/vipbundle
export VIPBUNDLE = $(VIPBUNDLEDIR)/vipbundle
DE10SERIALLITE3DIR = $(CURDIR)/de10pro-seriallite3
QPF = $(CURDIR)/DE10Pro-cheri-bgas.qpf

# Builds with separate flags can be sandboxed by setting OUTPUT_DIR_POSTFIX before running make.
# This places all Bluespec compiled files (BUILDDIR) and generated Verilog files (VDIR) into folders with the given postfix.
# Compiled FPGA output_files folders, both the current "output_files" and the archival "output_files_<TIMESTAMP>_<GITHASH>", are postfixed too: "output_files$(OUTPUT_DIR_POSTFIX)", "output_files$(OUTPUT_DIR_POSTFIX)_<TIMESTAMP>_<GITHASH>"
OUTPUT_DIR_POSTFIX ?=
OUTPUT_DIR = output_files$(OUTPUT_DIR_POSTFIX)
export BUILDDIR = $(CURDIR)/build$(OUTPUT_DIR_POSTFIX)
export VDIR = $(CURDIR)/cheri-bgas-rtl$(OUTPUT_DIR_POSTFIX)

BSC = bsc
BLUESPECDIR ?= $(shell which $(BSC) | xargs dirname | xargs dirname)/lib

SOFTDIR ?= $(CURDIR)/software
UBOOTBUILDDIR ?= $(SOFTDIR)/uboot_build

BOOTLOADER ?= $(UBOOTBUILDDIR)/u-boot-socfpga/spl/u-boot-spl-dtb.ihex

all: synthesize gen-uboot gen-rbf

gen-uboot: $(BOOTLOADER)
$(BOOTLOADER):
	mkdir -p $(UBOOTBUILDDIR)
	cd $(UBOOTBUILDDIR) && sh $(SOFTDIR)/build_uboot.sh

gen-rbf: $(BOOTLOADER)
	$(eval NOW = $(shell date +"%Y%m%d_%H_%M"))
	$(eval GITREV = $(shell git rev-parse HEAD))
	$(eval GITDIRTY = $(shell git diff --quiet || echo "_dirty"))
	$(eval TAGNAME = $(NOW)-$(GITREV)$(GITDIRTY))
	$(eval OUTDIR = $(OUTPUT_DIR)_$(TAGNAME))
	$(eval TEMPLATENAME = "cheri-bgas-socfpga")
	$(eval SOF = "$(OUTDIR)/DE10Pro-cheri-bgas.sof")
	$(eval OUTNAME = "$(OUTDIR)/$(TEMPLATENAME)-$(TAGNAME)")
	cp -r $(OUTPUT_DIR) $(OUTDIR)
	quartus_pfg -c $(SOF) -o hps=ON -o hps_path=$(BOOTLOADER) $(OUTNAME).rbf

ci-gen-rbf: $(BOOTLOADER)
	$(eval SOF = "$(OUTPUT_DIR)/DE10Pro-cheri-bgas.sof")
	$(eval NOW = $(shell date +"%Y%m%d_%H_%M"))
	$(eval GITREV = $(shell git rev-parse HEAD))
	$(eval GITDIRTY = $(shell git diff --quiet || echo "_dirty"))
	$(eval TAGNAME = $(NOW)-$(GITREV)$(GITDIRTY))
	$(eval OUTNAME = "$(OUTPUT_DIR)/cheri-bgas-socfpga-$(TAGNAME)")
	quartus_pfg -c $(SOF) -o hps=ON -o hps_path=$(BOOTLOADER) $(OUTNAME).rbf

synthesize $(OUTPUT_DIR)/DE10Pro-cheri-bgas.sof &: gen-ip
	BLUESPECDIR=$(BLUESPECDIR) BLUESTUFFDIR=$(BLUESTUFFDIR) DE10SERIALLITE3DIR=$(DE10SERIALLITE3DIR) time quartus_sh --flow compile $(QPF)

gen-ip: $(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl $(addprefix $(DE10SERIALLITE3DIR)/, mkBERT_hw.tcl mkSerialLite3_hw.tcl mkStatusDevice_Status15_hw.tcl)
	BLUESPECDIR=$(BLUESPECDIR) BLUESTUFFDIR=$(BLUESTUFFDIR) DE10SERIALLITE3DIR=$(DE10SERIALLITE3DIR) quartus_ipgenerate $(QPF)

gen-bluespec-quartus-ip: $(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl $(addprefix $(DE10SERIALLITE3DIR)/, mkBERT_hw.tcl mkSerialLite3_hw.tcl mkStatusDevice_Status15_hw.tcl)

$(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl: $(VIPBUNDLE) $(VDIR)/mkCHERI_BGAS_Top_Sig.v
	$(VIPBUNDLEDIR)/vipbundle \
      -f quartus_ip_tcl \
      -o $(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl \
      $(VDIR)/mkCHERI_BGAS_Top_Sig.v

$(DE10SERIALLITE3DIR)/mkBERT_hw.tcl:
	$(MAKE) -C $(DE10SERIALLITE3DIR) generate_bert_tcl
$(DE10SERIALLITE3DIR)/mkSerialLite3_hw.tcl:
	$(MAKE) -C $(DE10SERIALLITE3DIR) generate_seriallite3_tcl
$(DE10SERIALLITE3DIR)/mkStatusDevice_Status15_hw.tcl:
	$(MAKE) -C $(DE10SERIALLITE3DIR) generate_status_dev_15_tcl

$(VIPBUNDLE):
	$(MAKE) -C $(VIPBUNDLEDIR) vipbundle

gen-bluespec-rtl: $(VDIR)/mkCHERI_BGAS_Top_Sig.v

# We defer the 'should we rebuild the .v file' decision to the next level makefile, instead of duplicating the condition it uses here.
$(VDIR)/mkCHERI_BGAS_Top_Sig.v:
	$(MAKE) -C $(BSVSRCDIR) rtl

# Build the simulated SoC with the next level makefile, passing through any build-directory overrides created here
bluesim:
	$(MAKE) -C $(BSVSRCDIR) bluesim

.PHONY: clean mrproper $(VDIR)/mkCHERI_BGAS_Top_Sig.v bluesim

clean-bluespec-rtl:
	$(MAKE) -C $(BSVSRCDIR) clean
	rm -rf $(VDIR)

clean-vipbundle:
	$(MAKE) -C $(VIPBUNDLEDIR) clean

clean-bluespec-quartus-ip:
	rm -f $(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl

mrproper-bert: clean-bert-tcl clean-bert-rtl
mrproper-seriallite3: clean-seriallite3-tcl clean-seriallite3-rtl
mrproper-status_dev_15: clean-status_dev_15-tcl clean-status_dev_15-rtl
clean-bert-tcl:
	$(MAKE) -C $(DE10SERIALLITE3DIR) clean_bert_tcl
clean-seriallite3-tcl:
	$(MAKE) -C $(DE10SERIALLITE3DIR) clean_seriallite3_tcl
clean-status_dev_15-tcl:
	$(MAKE) -C $(DE10SERIALLITE3DIR) clean_status_dev_15_tcl
clean-bert-rtl:
	$(MAKE) -C $(DE10SERIALLITE3DIR) mrproper_bert_rtl
clean-seriallite3-rtl:
	$(MAKE) -C $(DE10SERIALLITE3DIR) mrproper_seriallite3_rtl
clean-status_dev_15-rtl:
	$(MAKE) -C $(DE10SERIALLITE3DIR) mrproper_status_dev_15_rtl

clean-ip-gen:
	rm -rf $(CURDIR)/ip/reset_release/
	rm -rf $(CURDIR)/ip/toplevel/CHERI_BGAS_Top/
	rm -rf $(CURDIR)/ip/toplevel/clock_in/
	rm -rf $(CURDIR)/ip/toplevel/de10_fan/
	rm -rf $(CURDIR)/ip/toplevel/ddrd_mm_clock_crossing_bridge/
	rm -rf $(CURDIR)/ip/toplevel/emif_ddrd/
	rm -rf $(CURDIR)/ip/toplevel/ddrb_mm_clock_crossing_bridge/
	rm -rf $(CURDIR)/ip/toplevel/emif_ddrb/
	rm -rf $(CURDIR)/ip/toplevel/emif_hps_ddra/
	rm -rf $(CURDIR)/ip/toplevel/hps/
	rm -rf $(CURDIR)/ip/toplevel/reset_in/
	rm -rf $(CURDIR)/toplevel/
	rm -rf $(CURDIR)/qdb/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/axi4lite_management_bridge/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/iopll/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/mkBERT_Instance_a/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/mkBERT_Instance_b/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/mkBERT_Instance_c/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/mkBERT_Instance_d/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/mkSerialLite3_Instance_a/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/mkSerialLite3_Instance_b/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/mkSerialLite3_Instance_c/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/mkSerialLite3_Instance_d/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/mkStatusDevice_Instance_Status15_0/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/reset50/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/reset200/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/reset_bridge/
	rm -rf $(CURDIR)/ip/seriallite3_wrapper/seriallite3_wrapper_clock_in/
	rm -rf $(CURDIR)/seriallite3_wrapper/

clean: clean-bluespec-rtl clean-bluespec-quartus-ip clean-vipbundle clean-ip-gen
	rm -rf $(CURDIR)/synth_dumps $(CURDIR)/tmp-clearbox $(UBOOTBUILDDIR)

mrproper-vipbundle:
	$(MAKE) -C $(VIPBUNDLEDIR) mrproper

mrproper-bluespec-rtl:
	$(MAKE) -C $(BSVSRCDIR) mrproper

mrproper: clean mrproper-bluespec-rtl mrproper-vipbundle mrproper-bert mrproper-seriallite3 mrproper-status_dev_15
	rm -rf $(CURDIR)/qdb $(OUTPUT_DIR)

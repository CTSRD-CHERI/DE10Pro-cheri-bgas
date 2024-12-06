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
VIPBUNDLEDIR = $(CURDIR)/vipbundle
VIPBUNDLE = $(VIPBUNDLEDIR)/vipbundle
QPF = $(CURDIR)/DE10Pro-cheri-bgas.qpf
export VDIR = $(CURDIR)/cheri-bgas-rtl

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
	$(eval OUTDIR = output_files_$(TAGNAME))
	$(eval TEMPLATENAME = "cheri-bgas-socfpga")
	$(eval SOF = "$(OUTDIR)/DE10Pro-cheri-bgas.sof")
	$(eval OUTNAME = "$(OUTDIR)/$(TEMPLATENAME)-$(TAGNAME)")
	cp -r output_files $(OUTDIR)
	quartus_pfg -c $(SOF) -o hps=ON -o hps_path=$(BOOTLOADER) $(OUTNAME).rbf

ci-gen-rbf: $(BOOTLOADER)
	$(eval SOF = "output_files/DE10Pro-cheri-bgas.sof")
	$(eval NOW = $(shell date +"%Y%m%d_%H_%M"))
	$(eval GITREV = $(shell git rev-parse HEAD))
	$(eval GITDIRTY = $(shell git diff --quiet || echo "_dirty"))
	$(eval TAGNAME = $(NOW)-$(GITREV)$(GITDIRTY))
	$(eval OUTNAME = "output_files/cheri-bgas-socfpga-$(TAGNAME)")
	quartus_pfg -c $(SOF) -o hps=ON -o hps_path=$(BOOTLOADER) $(OUTNAME).rbf

synthesize output_files/DE10Pro-cheri-bgas.sof &: gen-ip
	BLUESPECDIR=$(BLUESPECDIR) BLUESTUFFDIR=$(BLUESTUFFDIR) time quartus_sh --flow compile $(QPF)

gen-ip: $(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl
	BLUESPECDIR=$(BLUESPECDIR) BLUESTUFFDIR=$(BLUESTUFFDIR) quartus_ipgenerate $(QPF)

gen-bluespec-quartus-ip: $(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl

$(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl: $(VIPBUNDLE) $(VDIR)/mkCHERI_BGAS_Top_Sig.v
	$(VIPBUNDLEDIR)/vipbundle \
      -f quartus_ip_tcl \
      -o $(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl \
      $(VDIR)/mkCHERI_BGAS_Top_Sig.v

$(VIPBUNDLE):
	$(MAKE) -C $(VIPBUNDLEDIR) vipbundle

gen-bluespec-rtl: $(VDIR)/mkCHERI_BGAS_Top_Sig.v

# We defer the 'should we rebuild the .v file' decision to the next level makefile, instead of duplicating the condition it uses here.
$(VDIR)/mkCHERI_BGAS_Top_Sig.v:
	$(MAKE) -C $(BSVSRCDIR) rtl

.PHONY: clean mrproper $(VDIR)/mkCHERI_BGAS_Top_Sig.v

clean-bluespec-rtl:
	$(MAKE) -C $(BSVSRCDIR) clean
	rm -rf $(VDIR)

clean-vipbundle:
	$(MAKE) -C $(VIPBUNDLEDIR) clean

clean-bluespec-quartus-ip:
	rm -f $(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl

clean-ip-gen:
	rm -rf $(CURDIR)/ip/reset_release/
	rm -rf $(CURDIR)/ip/toplevel/CHERI_BGAS_Top/
	rm -rf $(CURDIR)/ip/toplevel/clock_in/
	rm -rf $(CURDIR)/ip/toplevel/ddrd_mm_clock_crossing_bridge/
	rm -rf $(CURDIR)/ip/toplevel/emif_ddrd/
	rm -rf $(CURDIR)/ip/toplevel/ddrb_mm_clock_crossing_bridge/
	rm -rf $(CURDIR)/ip/toplevel/emif_ddrb/
	rm -rf $(CURDIR)/ip/toplevel/emif_hps_ddra/
	rm -rf $(CURDIR)/ip/toplevel/hps/
	rm -rf $(CURDIR)/ip/toplevel/reset_in/
	rm -rf $(CURDIR)/toplevel/
	rm -rf $(CURDIR)/qdb/

clean: clean-bluespec-rtl clean-bluespec-quartus-ip clean-vipbundle clean-ip-gen
	rm -rf $(CURDIR)/synth_dumps $(CURDIR)/tmp-clearbox $(UBOOTBUILDDIR)

mrproper-vipbundle:
	$(MAKE) -C $(VIPBUNDLEDIR) mrproper

mrproper-bluespec-rtl:
	$(MAKE) -C $(BSVSRCDIR) mrproper

mrproper: clean mrproper-bluespec-rtl mrproper-vipbundle
	rm -rf $(CURDIR)/qdb $(CURDIR)/output_files

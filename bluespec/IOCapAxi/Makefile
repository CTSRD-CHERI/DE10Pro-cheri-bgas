# Very simple makefile that pulls in the right include directories for any testbenches
# and builds Bluesim versions.

BUILDDIR = ./build

BLUESTUFFDIR = ../Toooba/libs/BlueStuff
include $(BLUESTUFFDIR)/bluestuff.inc.mk
BSVPATH = +:$(BLUESTUFF_DIRS):./aes:./cap2024_02:./testbenches:./util:../AxiWindow

BSCFLAGS = -bdir $(BUILDDIR) -info-dir $(BUILDDIR) -simdir $(BUILDDIR) -vdir $(BUILDDIR)/verilog -p $(BSVPATH) -u -aggressive-conditions +RTS -K512M -RTS

BSCDIR = $(dir $(shell which bsc))../

$(BUILDDIR)/%.bsim: ./testbenches/%.bsv alwaysRemake
	mkdir -p $(BUILDDIR)
	bsc $(BSCFLAGS) -sim -g $* $< 
	bsc $(BSCFLAGS) -sim -e $* $@

$(BUILDDIR)/verilog/%.v: ./testbenches/%.bsv alwaysRemake
	mkdir -p $(BUILDDIR) $(BUILDDIR)/verilog
	bsc $(BSCFLAGS) -verilog -g $* -u $<

$(BUILDDIR)/%.vsim: $(BUILDDIR)/verilog/%.v ./testbenches/%.cpp alwaysRemake
	verilator --Mdir $(BUILDDIR) --top-module $* --cc --build --exe -o $*.vsim -I$(BSCDIR)lib/Verilog/ $(BSCDIR)lib/Verilator/verilator_config.vlt $(shell echo $(BUILDDIR)/verilog/*.v) ./testbenches/$*.cpp

.PHONY: list alwaysRemake

list:
	@printf "Available targets:\n\tclean\n\t$(BUILDDIR)/%%.bsim\n\t$(BUILDDIR)/verilog/%%.v\n\t$(BUILDDIR)/%%.vsim\n"
	

clean:
	rm -rf $(BUILDDIR)

alwaysRemake:
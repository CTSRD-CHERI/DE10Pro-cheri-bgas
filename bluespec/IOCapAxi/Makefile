# Very simple makefile that pulls in the right include directories for any testbenches
# and builds Bluesim versions.

BUILDDIR = ./build

BLUESTUFFDIR = ../Toooba/libs/BlueStuff
include $(BLUESTUFFDIR)/bluestuff.inc.mk
BSVPATH = +:$(BLUESTUFF_DIRS):./aes:./cap2024_02:./testbenches:./util:../AxiWindow

BSCFLAGS = -bdir $(BUILDDIR) -info-dir $(BUILDDIR) -simdir $(BUILDDIR) -vdir $(BUILDDIR)/verilog -p $(BSVPATH) -u -aggressive-conditions

$(BUILDDIR)/%.out: ./testbenches/%.bsv alwaysRemake
	mkdir -p $(BUILDDIR)
	bsc $(BSCFLAGS) -sim -g $* $< 
	bsc $(BSCFLAGS) -sim -e $* $@

$(BUILDDIR)/%.vout: ./testbenches/%.bsv alwaysRemake
	mkdir -p $(BUILDDIR) $(BUILDDIR)/verilog
	bsc $(BSCFLAGS) -verilog -g $* -u $<
	verilator --Mdir $(BUILDDIR) -sv --top-module $* --cc --build --exe -o $*.vout $(shell echo $(BUILDDIR)/verilog/*.v) ./testbenches/$*.cpp

.PHONY: clean alwaysRemake
clean:
	rm -rf $(BUILDDIR)

alwaysRemake:
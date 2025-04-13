all: $(addprefix $(CURDIR)/tools/, cheri-bgas-fuse-devfs forever-splice jtagvpi_to_fmemdmi openocd fmem)

mrproper: mrproper-cheri-bgas-fuse-devfs mrproper-forever-splice mrproper-jtagvpi_to_fmemdmi mrproper-openocd

####

gen-cheri-bgas-fuse-devfs: $(CURDIR)/tools/cheri-bgas-fuse-devfs

$(CURDIR)/tools/cheri-bgas-fuse-devfs: $(CURDIR)/cheri-bgas-fuse-devfs/cheri-bgas-fuse-devfs
	mkdir -p $(CURDIR)/tools
	cp $< $@

$(CURDIR)/cheri-bgas-fuse-devfs/cheri-bgas-fuse-devfs:
	$(MAKE) -C $(CURDIR)/cheri-bgas-fuse-devfs

mrproper-cheri-bgas-fuse-devfs:
	rm -rf $(CURDIR)/tools/cheri-bgas-fuse-devfs
	$(MAKE) -C $(CURDIR)/cheri-bgas-fuse-devfs clean

####

gen-forever-splice: $(CURDIR)/tools/forever-splice
$(CURDIR)/tools/forever-splice: $(CURDIR)/forever-splice/forever-splice
	mkdir -p $(CURDIR)/tools
	cp $< $@

$(CURDIR)/forever-splice/forever-splice:
	$(MAKE) -C $(CURDIR)/forever-splice

mrproper-forever-splice:
	rm -rf $(CURDIR)/tools/forever-splice
	$(MAKE) -C $(CURDIR)/forever-splice clean

####

gen-jtagvpi_to_fmemdmi: $(CURDIR)/tools/jtagvpi_to_fmemdmi
$(CURDIR)/tools/jtagvpi_to_fmemdmi: $(CURDIR)/jtagvpi_to_fmemdmi/jtagvpi_to_fmemdmi
	mkdir -p $(CURDIR)/tools
	cp $< $@

$(CURDIR)/jtagvpi_to_fmemdmi/jtagvpi_to_fmemdmi:
	$(MAKE) -C $(CURDIR)/jtagvpi_to_fmemdmi

mrproper-jtagvpi_to_fmemdmi:
	rm -rf $(CURDIR)/tools/jtagvpi_to_fmemdmi
	$(MAKE) -C $(CURDIR)/jtagvpi_to_fmemdmi mrproper

####

gen-openocd: $(CURDIR)/tools/openocd
$(CURDIR)/tools/openocd: /tmp/openocd/src/openocd
	mkdir -p $(CURDIR)/tools
	cp $< $@

/tmp/openocd/src/openocd:
	rm -rf /tmp/openocd
	git clone --depth=1 --branch latest https://github.com/openocd-org/openocd.git /tmp/openocd
	# last tested with commit 8a37230
	cd /tmp/openocd && git submodule update --init --recursive && \
	./bootstrap && ./configure --enable-jtag_vpi
	$(MAKE) -C /tmp/openocd

mrproper-openocd:
	rm -rf $(CURDIR)/tools/openocd
	$(MAKE) -C /tmp/openocd clean

####

gen-fmem: $(CURDIR)/tools/fmem
$(CURDIR)/tools/fmem: /tmp/fmem/fmem
	mkdir -p $(CURDIR)/tools
	cp $< $@

/tmp/fmem/fmem:
	rm -rf /tmp/fmem
	git clone --depth=1 --branch main https://github.com/CTSRD-CHERI/fmem.git /tmp/fmem
	cd /tmp/fmem && git submodule update --init --recursive && \
	$(MAKE) -C /tmp/fmem

mrproper-fmem:
	rm -rf $(CURDIR)/tools/fmem
	$(MAKE) -C /tmp/fmem clean

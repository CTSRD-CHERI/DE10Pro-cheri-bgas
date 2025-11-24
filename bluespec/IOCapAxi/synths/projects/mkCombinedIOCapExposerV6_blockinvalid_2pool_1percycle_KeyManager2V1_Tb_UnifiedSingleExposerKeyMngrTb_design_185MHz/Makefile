
# GNU flavored Makefile

project_name = mkCombinedIOCapExposerV6_blockinvalid_2pool_1percycle_KeyManager2V1_Tb_UnifiedSingleExposerKeyMngrTb_design

basic_verilog_deps = ../../verilog/input_harness.v ../../verilog/output_harness.v $(project_name).v $(project_name).qpf $(project_name).qsf $(project_name).sdc
bluespec_generated_verilog_deps = $(wildcard ../build/verilog/*.v)
bsc_verilog_deps = $(wildcard $(BLUESPECDIR)/Verilog/*.v)
quartus_verilog_deps = $(wildcard ../../verilog/Verilog.Quartus/*.v)

.PRECIOUS: ./output_files/$(project_name).sta.rpt

./output_files/$(project_name).sta.rpt: $(basic_verilog_deps) $(bluespec_generated_verilog_deps) $(bsc_verilog_deps) $(quartus_verilog_deps)
	quartus_sh --flow compile $(project_name)


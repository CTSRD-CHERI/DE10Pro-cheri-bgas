
# GNU flavored Makefile

project_name = null_UnifiedSingleExposerKeyMngrTb_UnifiedSingleExposerKeyMngrTb_design

basic_verilog_deps = ../../verilog/input_harness.v ../../verilog/output_harness.v ../../verilog/null_UnifiedSingleExposerKeyMngrTb.v $(project_name).qpf $(project_name).qsf $(project_name).sdc

.PRECIOUS: ./output_files/$(project_name).sta.rpt

./output_files/$(project_name).sta.rpt: $(basic_verilog_deps)
	quartus_sh --flow compile $(project_name)


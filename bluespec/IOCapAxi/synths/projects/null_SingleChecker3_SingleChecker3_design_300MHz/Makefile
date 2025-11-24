
# GNU flavored Makefile

project_name = null_SingleChecker3_SingleChecker3_design

basic_verilog_deps = ../../verilog/input_harness.v ../../verilog/output_harness.v ../../verilog/null_SingleChecker3.v $(project_name).qpf $(project_name).qsf $(project_name).sdc

.PRECIOUS: ./output_files/$(project_name).sta.rpt

./output_files/$(project_name).sta.rpt: $(basic_verilog_deps)
	quartus_sh --flow compile $(project_name)


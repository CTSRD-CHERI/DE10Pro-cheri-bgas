#ifndef TB_H
#define TB_H

#include <verilated.h>
#include <cstdint>
#include <vector>

#define FMT_HEADER_ONLY
#include "fmt/format.h"

#include "dtl/dtl.hpp"

// TbOutput MUST be parameterized on 

struct TestBase {
    virtual ~TestBase() = default;

    virtual bool run() = 0;
};

struct TestParams {
    const char* testName;
    int argc;
    char** argv;
    uint64_t endTime;
};

template<class DUT, class TbOutput>
concept ValidTbOutput = requires(DUT dut, TbOutput out) {
    {out == out} -> std::convertible_to<bool>;
    {pull_output(dut, out)} -> std::convertible_to<void>;
} && fmt::formattable<TbOutput>;

template<class DUT, class TbInput, class TbOutput>
    requires ValidTbOutput<DUT, TbOutput>
struct CycleTest : TestBase {
    TestParams params;
    std::vector<TbInput> inputs;
    std::vector<TbOutput> expectedOutputs;

    CycleTest(
        TestParams params,
        std::vector<TbInput> inputs,
        std::vector<TbOutput> expectedOutputs
    ) : params(params), inputs(inputs), expectedOutputs(expectedOutputs) {}
    virtual ~CycleTest() override = default;

    /**
     * Create and run a DUT using this test's parameters
     */
    std::vector<TbOutput> getOutputs() {
        // Step through the input vector in order
        size_t input_idx = 0;

        auto outputs = std::vector<TbOutput>();

        {
            VerilatedContext ctx{};
            ctx.commandArgs(params.argc, params.argv);    // remember args
            // Make a design-under-test
            DUT dut{&ctx};

            uint64_t main_time = 0;
            // initial conditions in order to generate appropriate edges on
            // reset
            dut.RST_N = 1;
            dut.CLK = 0;

            while ((!ctx.gotFinish()) && (main_time <= params.endTime) ) { // $finish executed
                if (main_time == 2) {
                    dut.RST_N = 0;    // assert reset
                }
                else if (main_time == 7) {
                    dut.RST_N = 1;    // deassert reset
                }

                // Toggle clock - goes up at 5, 15, 25, 35...
                if ((main_time % 10) == 5) {
                    dut.CLK = 1;
                }
                // Goes down at 10, 20, 30, 40...
                else if ((main_time % 10) == 0) {
                    dut.CLK = 0;
                    // ... and we set the inputs and pull outputs at this time too.

                    // Gather input. By default apply a null input.
                    TbInput input{};

                    // If the next input in the queue has a .time value set to the current time,
                    // use that instead.
                    if (input_idx >= inputs.size()) {
                        // Don't pull any more inputs
                    } else if (inputs[input_idx].time == main_time) {
                        input = inputs[input_idx];
                        input_idx++;
                    } else if (inputs[input_idx].time < main_time) {
                        throw std::runtime_error("Input had out-of-order indices");
                    }

                    // Actually apply the input to the DUT.
                    push_input(dut, input);
                    
                    // Now pull out the outputs.
                    TbOutput output{};
                    output.time = main_time;
                    pull_output(dut, output);

                    // Only remember non-zero outputs
                    if (output.is_notable()) {
                        outputs.push_back(std::move(output));
                    }
                }

                dut.eval();
                main_time++;
            }

            dut.final();    // Done simulating
            // end of DUT lifetime, gets destructed
            // end of ctx lifetime, gets destructed
        }

        return outputs;
    }

    /**
     * Check a DUT produces the given outputs when stimulated with the given inputs.
     * Returns true if it did, false if it didn't.
     * Prints a diff of the outputs if it didn't match.
     */
    virtual bool run() override {
        fprintf(stderr, "\033[1;33mTest: %s\033[0m\n", params.testName);

        auto outputs = getOutputs();

        if (expectedOutputs == outputs) {
            fprintf(stderr, "\033[1;32mTest-Success\033[0m\n");
            return true;
        }

        fprintf(stderr, "\033[1;31mTest-Failure: Output Diff\033[0m\n");

        dtl::Diff<TbOutput, std::vector<TbOutput>> diff(expectedOutputs, outputs);
        diff.compose();
        
        for (std::pair<TbOutput, dtl::elemInfo> sesElem : diff.getSes().getSequence()) {
            switch (sesElem.second.type) {
                case dtl::SES_ADD:
                    fmt::print(stderr, "\033[32m++++++++++\n{}\n++++++++++\033[0m\n", sesElem.first);
                    break;
                case dtl::SES_DELETE:
                    fmt::print(stderr, "\033[91m----------\n{}\n----------\033[0m\n", sesElem.first);
                    break;
                case dtl::SES_COMMON:
                    fmt::print(stderr, "{}\n", sesElem.first);
                    break;
            }
        }

        return false;
    }
};

#endif // TB_H
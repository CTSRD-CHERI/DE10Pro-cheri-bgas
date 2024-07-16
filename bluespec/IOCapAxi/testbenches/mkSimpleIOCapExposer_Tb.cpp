#include <verilated.h>
#include "VmkSimpleIOCapExposer_Tb.h"

#include "key_manager.h"
#include "exposer.h"

#define FMT_HEADER_ONLY
#include "fmt/format.h"

#include "dtl/dtl.hpp"
#include <random>

using namespace shimmed_exposer;

// Helper function
template<class T>
std::optional<T> some(T t) {
    return std::optional(t);
}

struct TestParams {
    const char* testName;
    int argc;
    char** argv;
    uint64_t endTime;
};

ShimmedExposerOutputs runDut(TestParams& params, ShimmedExposerInputs inputs) {
    // Step through the input vector in order
    size_t input_idx = 0;

    auto outputs = ShimmedExposerOutputs();

    {
        VerilatedContext ctx{};
        ctx.commandArgs(params.argc, params.argv);    // remember args
        // Make a design-under-test
        VmkSimpleIOCapExposer_Tb dut{&ctx};

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
                ShimmedExposerInput input{};

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
                // TODO
                // ShimmedExposerOutput output{};
                // output.time = main_time;
                // pull_output(dut, output);

                // // Only remember non-zero outputs
                // if (output.readResp || output.writeResp || output.keyResponse || output.newEpochRequest) {
                //     outputs.push_back(std::move(output));
                // }
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
bool checkDut(TestParams params, ShimmedExposerInputs inputs, ShimmedExposerOutputs expectedOut) {
    fprintf(stderr, "\033[1;33mTest: %s\033[0m\n", params.testName);

    auto outputs = runDut(params, inputs);

    // TODO equality, printing

    // if (expectedOut == outputs) {
    //     fprintf(stderr, "\033[1;32mTest-Success\033[0m\n");
    //     return true;
    // }

    // fprintf(stderr, "\033[1;31mTest-Failure: Output Diff\033[0m\n");

    // dtl::Diff<ShimmedExposerOutput, ShimmedExposerOutputs> diff(expectedOut, outputs);
    // diff.compose();
    
    // for (std::pair<ShimmedExposerOutput, dtl::elemInfo> sesElem : diff.getSes().getSequence()) {
    //     switch (sesElem.second.type) {
    //         case dtl::SES_ADD:
    //             fmt::print(stderr, "\033[32m++++++++++\n{}\n++++++++++\033[0m\n", sesElem.first);
    //             break;
    //         case dtl::SES_DELETE:
    //             fmt::print(stderr, "\033[91m----------\n{}\n----------\033[0m\n", sesElem.first);
    //             break;
    //         case dtl::SES_COMMON:
    //             fmt::print(stderr, "{}\n", sesElem.first);
    //             break;
    //     }
    // }

    return false;
}

int main(int argc, char** argv) {

}
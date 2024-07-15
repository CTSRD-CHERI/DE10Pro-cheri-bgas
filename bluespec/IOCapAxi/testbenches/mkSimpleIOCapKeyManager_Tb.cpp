#include <verilated.h>
#include "VmkSimpleIOCapKeyManager_Tb.h"

#include "key_manager.h"

using namespace key_manager;

std::vector<KeyManagerOutput> checkDut(int argc, char** argv, std::vector<KeyManagerInput> inputs, uint64_t end_time) {
    // Step through the input vector in order
    size_t input_idx = 0;

    auto outputs = std::vector<KeyManagerOutput>();

    {
        VerilatedContext ctx{};
        ctx.commandArgs(argc, argv);    // remember args
        // Make a design-under-test
        VmkSimpleIOCapKeyManager_Tb dut{&ctx};

        uint64_t main_time = 0;
        // initial conditions in order to generate appropriate edges on
        // reset
        dut.RST_N = 1;
        dut.CLK = 0;

        while ((!ctx.gotFinish()) && (main_time <= end_time) ) { // $finish executed
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
                KeyManagerInput input{};

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
                KeyManagerOutput output{};
                output.time = main_time;
                pull_output(dut, output);

                if (output.axiReadResp || output.axiWriteResp || output.keyResponse || output.newEpochRequest) {
                    outputs.push_back(std::move(output));
                }

                // then compare the outputs to the expected.
                // TODO
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

int main(int argc, char** argv) {
    auto outputs = checkDut(
        argc, argv,
        {
            // Initialize a key's contents
            KeyManagerInput {
                .time = 100,
                .writeReq = std::optional(AxiWriteReq {
                    .address = 0x1040,
                    .data = 0xdeadbeef,
                    .write_enable = 0b1111,
                }),
            },
            KeyManagerInput {
                .time = 110,
                .writeReq = std::optional(AxiWriteReq {
                    .address = 0x1044,
                    .data = 0xdeadbeef,
                    .write_enable = 0b1111,
                }),
            },
            KeyManagerInput {
                .time = 120,
                .writeReq = std::optional(AxiWriteReq {
                    .address = 0x1048,
                    .data = 0xf2edbeef,
                    .write_enable = 0b1111,
                }),
            },
            KeyManagerInput {
                .time = 130,
                .writeReq = std::optional(AxiWriteReq {
                    .address = 0x104C,
                    .data = 0xf1edbeef,
                    .write_enable = 0b1111,
                }),
            },
            // Request it (should still be null because that status hasn't been set)
            KeyManagerInput {
                .time = 140,
                .keyRequest = std::optional(0x4),
                .readReq = std::optional(AxiReadReq {
                    .address = 0x40,
                })
            },
            // Set the status to valid
            KeyManagerInput {
                .time = 150,
                .writeReq = std::optional(AxiWriteReq {
                    .address = 0x40,
                    .data = 0x1,
                    .write_enable = 0b1111,
                })
            },
            // Then request it again
            KeyManagerInput {
                .time = 160,
                .keyRequest = std::optional(0x4),
                .readReq = std::optional(AxiReadReq {
                    .address = 0x40,
                })
            }
        },
        // Run for 1k cycles
        1000 * 10
    );

    printf("Outputs\n");
    for (auto& output : outputs) {
        printf("\ntime: %lu\n", output.time);
        if (output.newEpochRequest) {
            printf("requested new epoch: %u\n", output.newEpochRequest.value());
        }
        if (output.keyResponse) {
            auto resp = output.keyResponse.value();
            if (resp.key) {
                printf("finished key access: key %u is valid 0x%08lx%08lx\n", resp.keyId, resp.key.value().top, resp.key.value().bottom);
            } else {
                printf("finished key access: key %u is invalid\n", resp.keyId);
            }
        }
        if (output.axiReadResp) {
            auto resp = output.axiReadResp.value();
            if (resp.good) {
                printf("read succeeded, returning 0x%08x\n", resp.data);
            } else {
                printf("read failed\n");
            }
        }
        if (output.axiWriteResp) {
            if (output.axiWriteResp.value().good) {
                printf("write succeeded\n");
            } else {
                printf("write failed\n");
            }
        }
    }
}
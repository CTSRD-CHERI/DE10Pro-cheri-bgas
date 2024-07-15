#include <verilated.h>
#include "VmkSimpleIOCapKeyManager_Tb.h"

#include "key_manager.h"

#define FMT_HEADER_ONLY
#include "fmt/format.h"

using namespace key_manager;

struct TestParams {
    const char* testName;
    int argc;
    char** argv;
    uint64_t endTime;
};

KeyManagerOutputs runDut(TestParams& params, KeyManagerInputs inputs) {
    // Step through the input vector in order
    size_t input_idx = 0;

    auto outputs = KeyManagerOutputs();

    {
        VerilatedContext ctx{};
        ctx.commandArgs(params.argc, params.argv);    // remember args
        // Make a design-under-test
        VmkSimpleIOCapKeyManager_Tb dut{&ctx};

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

                // Only remember non-zero outputs
                if (output.readResp || output.writeResp || output.keyResponse || output.newEpochRequest) {
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
 * Check a 
 */
int checkDut(TestParams params, KeyManagerInputs inputs, KeyManagerOutputs expectedOut) {
    printf("Test: %s\n", params.testName);

    auto outputs = runDut(params, inputs);

    if (outputs == expectedOut) {
        printf("Success\n");
        return 0;
    }

    printf("Failure - diff Outputs\n");
    for (auto& output : outputs) {
        fmt::print("{}\n", output);
        // printf("\ntime: %lu\n", output.time);
        // if (output.newEpochRequest) {
        //     printf("requested new epoch: %u\n", output.newEpochRequest.value());
        // }
        // if (output.keyResponse) {
        //     auto resp = output.keyResponse.value();
        //     if (resp.key) {
        //         printf("finished key access: key %u is valid 0x%08lx%08lx\n", resp.keyId, resp.key.value().top, resp.key.value().bottom);
        //     } else {
        //         printf("finished key access: key %u is invalid\n", resp.keyId);
        //     }
        // }
        // if (output.readResp) {
        //     auto resp = output.readResp.value();
        //     if (resp.good) {
        //         printf("read succeeded, returning 0x%08x\n", resp.data);
        //     } else {
        //         printf("read failed\n");
        //     }
        // }
        // if (output.writeResp) {
        //     if (output.writeResp.value().good) {
        //         printf("write succeeded\n");
        //     } else {
        //         printf("write failed\n");
        //     }
        // }
    }

    return 1;
}

int main(int argc, char** argv) {
    return checkDut(
        TestParams {
            .testName="Write and enable key over AXI",
            .argc = argc,
            .argv = argv,
             // Run for 1k cycles
            .endTime = 1000 * 10
        },
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
        // expected output
        {
            // Key writes must succeed, there are four of them
            KeyManagerOutput {
                .time = 120,
                .writeResp = std::optional(AxiWriteResp {
                    .good = true,
                }),
            },
            KeyManagerOutput {
                .time = 130,
                .writeResp = std::optional(AxiWriteResp {
                    .good = true,
                }),
            },
            KeyManagerOutput {
                .time = 140,
                .writeResp = std::optional(AxiWriteResp {
                    .good = true,
                }),
            },
            KeyManagerOutput {
                .time = 150,
                .writeResp = std::optional(AxiWriteResp {
                    .good = true,
                }),
            },
            // The key status read request should complete immediately (2 cycle latency from 140 when enqueued)
            // The data should be zero, because the key hasn't been activated yet
            KeyManagerOutput {
                .time = 160,
                .readResp = std::optional(AxiReadResp {
                    .good = true,
                    .data = 0,
                }),
            },
            // The key status write request should complete immediately (2 cycle latency from 150 when enqueued)
            KeyManagerOutput {
                .time = 170,
                .writeResp = std::optional(AxiWriteResp {
                    .good = true,
                }),
            },
            // Get the key response back from BRAM after 4 cycles, and get the readReq of that status back too.
            // the key was invalid at the time of request, so it's invalid i.e. std::nullopt here.
            // the writeResp from the last cycle was for enabling it, so the readResp says it *is* valid.
            KeyManagerOutput {
                .time = 180,
                .keyResponse = std::optional(KeyResponse {
                    .keyId = 0x4,
                    .key = std::nullopt,
                }),
                // .readResp = std::optional(AxiReadResp {
                //     .good = true,
                //     .data = 0x1,
                // })
            },
            // Get the second key response back from BRAM after 4 cycles
            // The key was valid at 160 at the time of request, so it's valid here
            KeyManagerOutput {
                .time = 200,
                .keyResponse = std::optional(KeyResponse {
                    .keyId = 0x4,
                    .key = std::optional(Key {
                        .top = 0xf1edbeeff2edbeef,
                        .bottom = 0xdeadbeefdeadbeef,
                    }),
                }),
            },
        }
    );
}
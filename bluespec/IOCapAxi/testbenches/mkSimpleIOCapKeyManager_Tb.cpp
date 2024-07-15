#include <verilated.h>
#include "VmkSimpleIOCapKeyManager_Tb.h"

#include "key_manager.h"

#define FMT_HEADER_ONLY
#include "fmt/format.h"

#include "dtl/dtl.hpp"

using namespace key_manager;

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
 * Check a DUT produces the given outputs when stimulated with the given inputs.
 * Returns true if it did, false if it didn't.
 * Prints a diff of the outputs if it didn't match.
 */
bool checkDut(TestParams params, KeyManagerInputs inputs, KeyManagerOutputs expectedOut) {
    printf("\033[1;33mTest: %s\033[0m\n", params.testName);

    auto outputs = runDut(params, inputs);

    if (expectedOut == outputs) {
        printf("\033[1;32mTest-Success\033[0m\n");
        return true;
    }

    printf("\033[1;31mTest-Failure: Output Diff\033[0m\n");

    dtl::Diff<KeyManagerOutput, KeyManagerOutputs> diff(expectedOut, outputs);
    diff.compose();
    
    for (std::pair<KeyManagerOutput, dtl::elemInfo> sesElem : diff.getSes().getSequence()) {
        switch (sesElem.second.type) {
            case dtl::SES_ADD:
                fmt::print("\033[32m++++++++++\n{}\n++++++++++\033[0m\n", sesElem.first);
                break;
            case dtl::SES_DELETE:
                fmt::print("\033[91m----------\n{}\n----------\033[0m\n", sesElem.first);
                break;
            case dtl::SES_COMMON:
                fmt::print("{}\n", sesElem.first);
                break;
        }
    }

    return false;
}

int main(int argc, char** argv) {
    int success = EXIT_SUCCESS;

    {
        TestParams params = TestParams {
            .testName="Write and enable key over AXI",
            .argc = argc,
            .argv = argv,
             // Run for 1k cycles
            .endTime = 1000 * 10
        };
        
        KeyManagerInputs inputs = {
            // Initialize a key's contents
            KeyManagerInput {
                .time = 100,
                .writeReq = some(AxiWriteReq {
                    .address = 0x1040,
                    .data = 0xdeadbeef,
                    .write_enable = 0b1111,
                }),
            },
            KeyManagerInput {
                .time = 110,
                .writeReq = some(AxiWriteReq {
                    .address = 0x1044,
                    .data = 0xdeadbeef,
                    .write_enable = 0b1111,
                }),
            },
            KeyManagerInput {
                .time = 120,
                .writeReq = some(AxiWriteReq {
                    .address = 0x1048,
                    .data = 0xf2edbeef,
                    .write_enable = 0b1111,
                }),
            },
            KeyManagerInput {
                .time = 130,
                .writeReq = some(AxiWriteReq {
                    .address = 0x104C,
                    .data = 0xf1edbeef,
                    .write_enable = 0b1111,
                }),
            },
            // Request it (should still be null because that status hasn't been set)
            KeyManagerInput {
                .time = 140,
                .keyRequest = some(0x4),
                .readReq = some(AxiReadReq {
                    .address = 0x40,
                })
            },
            // Set the status to valid
            KeyManagerInput {
                .time = 150,
                .writeReq = some(AxiWriteReq {
                    .address = 0x40,
                    .data = 0x1,
                    .write_enable = 0b1111,
                })
            },
            // Then request it again
            KeyManagerInput {
                .time = 160,
                .keyRequest = some(0x4),
                .readReq = some(AxiReadReq {
                    .address = 0x40,
                })
            }
        };
        KeyManagerOutputs expectedOut = {
            // Key writes must succeed, there are four of them
            KeyManagerOutput {
                .time = 120,
                .writeResp = some(AxiWriteResp {
                    .good = true,
                }),
            },
            KeyManagerOutput {
                .time = 130,
                .writeResp = some(AxiWriteResp {
                    .good = true,
                }),
            },
            KeyManagerOutput {
                .time = 140,
                .writeResp = some(AxiWriteResp {
                    .good = true,
                }),
            },
            KeyManagerOutput {
                .time = 150,
                .writeResp = some(AxiWriteResp {
                    .good = true,
                }),
            },
            // The key status read request should complete immediately (2 cycle latency from 140 when enqueued)
            // The data should be zero, because the key hasn't been activated yet
            KeyManagerOutput {
                .time = 160,
                .readResp = some(AxiReadResp {
                    .good = true,
                    .data = 0,
                }),
            },
            // The key status write request should complete immediately (2 cycle latency from 150 when enqueued)
            KeyManagerOutput {
                .time = 170,
                .writeResp = some(AxiWriteResp {
                    .good = true,
                }),
            },
            // Get the key response back from BRAM after 4 cycles, and get the readReq of that status back too.
            // the key was invalid at the time of request, so it's invalid i.e. std::nullopt here.
            // the writeResp from the last cycle was for enabling it, so the readResp says it *is* valid.
            KeyManagerOutput {
                .time = 180,
                .keyResponse = some(KeyResponse {
                    .keyId = 0x4,
                    .key = std::nullopt,
                }),
                .readResp = some(AxiReadResp {
                    .good = true,
                    .data = 0x1,
                })
            },
            // Get the second key response back from BRAM after 4 cycles
            // The key was valid at 160 at the time of request, so it's valid here
            KeyManagerOutput {
                .time = 200,
                .keyResponse = some(KeyResponse {
                    .keyId = 0x4,
                    .key = some(Key {
                        .top = 0xf1edbeeff2edbeef,
                        .bottom = 0xdeadbeefdeadbeef,
                    }),
                }),
            },
        };

        if (!checkDut(params, inputs, expectedOut)) {
            success = EXIT_FAILURE;
        }
    }

    {
        TestParams params = TestParams {
            .testName="Write and enable key over AXI - event-based construction",
            .argc = argc,
            .argv = argv,
             // Run for 1k cycles
            .endTime = 1000 * 10
        };

        KeyManagerInputsMaker inputs{};
        KeyManagerOutputsMaker outputs{};

        // Initialize a key's contents - all write responses are delayed by 2 cycles and must return true
        inputs[100].writeReq = some(AxiWriteReq {
            .address = 0x1040,
            .data = 0xdeadbeef,
            .write_enable = 0b1111,
        });
        outputs[120].writeResp = some(AxiWriteResp { .good = true });

        inputs[110].writeReq = some(AxiWriteReq {
            .address = 0x1044,
            .data = 0xdeadbeef,
            .write_enable = 0b1111,
        });
        outputs[130].writeResp = some(AxiWriteResp { .good = true });

        inputs[120].writeReq = some(AxiWriteReq {
            .address = 0x1048,
            .data = 0xf2edbeef,
            .write_enable = 0b1111,
        });
        outputs[140].writeResp = some(AxiWriteResp { .good = true });

        inputs[130].writeReq = some(AxiWriteReq {
            .address = 0x104C,
            .data = 0xf1edbeef,
            .write_enable = 0b1111,
        });
        outputs[150].writeResp = some(AxiWriteResp { .good = true });

        // Request the key's status through the keyRequests (includes key value) and AXI read (status only) ports.
        inputs[140].keyRequest = some(0x4);
        inputs[140].readReq = some(AxiReadReq { .address = 0x40 });
        // The keyResponse will arrive in 4 cycles (the latency of the BRAM) and will have the validity-at-point-of-request
        // i.e. the key data will be invalid, because at tick 140 we hadn't set it valid yet.
        outputs[180].keyResponse = some(KeyResponse { .keyId = 0x4, .key = std::nullopt });
        // The AXI read response will return with 2 cycle latency and will be 0 (key invalid)
        outputs[160].readResp = some(AxiReadResp { .good = true, .data = 0 });

        // Try setting the key's status to Valid, now the data has gone through.
        inputs[150].writeReq = some(AxiWriteReq {
            .address = 0x40,
            .data = 0x1,
            .write_enable = 0b1111,
        });
        // The AXI write response will return with 2 cycle latency
        outputs[170].writeResp = some(AxiWriteResp { .good = true });

        // Try reading the key status in both ways again
        inputs[160].keyRequest = some(0x4);
        inputs[160].readReq = some(AxiReadReq { .address = 0x40 });
        // The keyResponse will arrive in 4 cycles (the latency of the BRAM) and will have the validity-at-point-of-request
        // i.e. the key data will be invalid, because at tick 140 we hadn't set it valid yet.
        outputs[200].keyResponse = some(KeyResponse {
            .keyId = 0x4,
            .key = some(Key {
                .top = 0xf1edbeeff2edbeef,
                .bottom = 0xdeadbeefdeadbeef,
            }),
        });
        // The AXI read response will return with 2 cycle latency and will be 1 (key valid)
        outputs[180].readResp = some(AxiReadResp { .good = true, .data = 1 });

        if (!checkDut(params, inputs.asVec(), outputs.asVec())) {
            success = EXIT_FAILURE;
        }
    }

    // {
    //     TestParams params = TestParams {
    //         .testName = "Invalidation Epochs",
    //         .argc = argc,
    //         .argv = argv,
    //          // Run for 1k cycles
    //         .endTime = 1000 * 10
    //     };
    //     KeyManagerInputsMaker inputs{};
    //     KeyManagerOutputsMaker outputs{};

    //     if (!checkDut(params, inputs.asVec(), outputs.asVec())) {
    //         success = EXIT_FAILURE;
    //     }
    // }

    {
        TestParams params = TestParams {
            .testName = "Key Request - 1/cycle Throughput",
            .argc = argc,
            .argv = argv,
             // Run until t = 3000
            .endTime = 3000
        };
        KeyManagerInputsMaker inputs{};
        KeyManagerOutputsMaker outputs{};

        // Request a key on every cycle - the BRAM should be able to sustain this throughput.
        // for (uint16_t i = 0; i < 256; i++) { // Do 10 for now to avoid log spamming
        for (uint16_t i = 0; i < 10; i++) {
            // Request the given key (should always be invalid)
            inputs[100 + (i * 10)].keyRequest = some(i);
            // Get the given key back 4 cycles later
            outputs[140 + (i * 10)].keyResponse = some(KeyResponse {
                .keyId = i,
                .key = std::nullopt,
            });
        }

        if (!checkDut(params, inputs.asVec(), outputs.asVec())) {
            success = EXIT_FAILURE;
        }
    }

    // Template for further test creation
    /*
    {
        TestParams params = TestParams {
            .testName = ,
            .argc = argc,
            .argv = argv,
             // Run for 1k cycles
            .endTime = 1000 * 10
        };
        KeyManagerInputsMaker inputs{};
        KeyManagerOutputsMaker outputs{};

        if (!checkDut(params, inputs.asVec(), outputs.asVec())) {
            success = EXIT_FAILURE;
        }
    }
    */
    
    return success;
}
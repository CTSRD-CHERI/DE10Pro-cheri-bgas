#include <verilated.h>
#include "VmkSimpleIOCapKeyManager_Tb.h"
#include <cstdint>
#include <optional>

using Epoch = uint8_t;
using KeyId = uint16_t; // 9 bits

struct Key {
    uint64_t top;
    uint64_t bottom;
};

struct KeyResponse {
    KeyId keyId;
    std::optional<Key> key;
};

// 13-bit address, 32-bit data AXI4-Lite
using AxiAddress = uint16_t; // 13-bit address
using AxiData = uint32_t;

enum class AXI4_Resp: uint8_t {
    Okay = 0b00,
    ExOkay = 0b01,
    SlvErr = 0b10,
    DecErr = 0b11,
};

union AWFlit {
    // struct {
    //     uint16_t prot : 3;
    //     uint16_t addr : 13;
    // };
    uint16_t flit;
};

union WFlit {
    // Can't bitpack here because gcc forces fields to be byte-aligned!
    // struct {
    //     uint8_t wstrb : 4;
    //     AxiData  data  : 32;
    //     uint32_t       : 28;
    // };
    uint64_t flit;
};

union BFlit {
    // struct {
    //     uint8_t        : 6;
    //     AXI4_Resp resp : 2;
    // };
    uint8_t flit;
};

union ARFlit {
    // struct {
    //     uint16_t prot : 3;
    //     uint16_t addr : 13;
    // };
    uint16_t flit;
};

union RFlit {
    // struct {
    //     AXI4_Resp resp : 2;
    //     AxiData   data : 32;
    //     uint32_t       : 30;
    // };
    uint64_t flit;
};

struct AxiWriteReq {
    AxiAddress address;
    uint32_t data;
    uint8_t write_enable;

    AWFlit aw_flit() {
        AWFlit flit{};
        constexpr uint16_t prot = 0b000;
        flit.flit = ((this->address & 0x1FFFu) << 3) | (prot << 0);
        return flit;
    }
    WFlit w_flit() {
        WFlit flit{};
        // flit.flit = 0xacacacacacacacac;
        flit.flit = (0xacacacaul << 36) | (uint64_t(this->data) << 4) | (uint64_t(this->write_enable) & 0xF);
        return flit;
    }
};

struct AxiWriteResp {
    bool good;

    AxiWriteResp(BFlit flit) : good((flit.flit & 0b11) == uint8_t(AXI4_Resp::Okay)) {}
};

struct AxiReadReq {
    AxiAddress address; // 13 bits

    ARFlit ar_flit() {
        ARFlit flit{};
        constexpr uint16_t prot = 0b000;
        flit.flit = ((this->address & 0x1FFFu) << 3) | (prot << 0);
        return flit;
    }
};

struct AxiReadResp {
    bool good;
    AxiData data;

    AxiReadResp(RFlit flit) : good((flit.flit & 0b11) == uint8_t(AXI4_Resp::Okay)), data(flit.flit >> 2) {}
};

struct KeyManagerInput {
    uint64_t time;

    // TODO
    // bool bumpPerfCounterGoodWrite;
    // bool bumpPerfCounterBadWrite;
    // bool bumpPerfCounterGoodRead;
    // bool bumpPerfCounterBadRead;

    std::optional<KeyId> keyRequest;
    std::optional<Epoch> finishedEpoch;
    std::optional<AxiWriteReq> writeReq;
    std::optional<AxiReadReq> readReq;
};

struct KeyManagerOutput {
    uint64_t time;
    std::optional<Epoch> newEpochRequest;
    std::optional<KeyResponse> keyResponse;

    std::optional<AxiReadResp> axiReadResp;
    std::optional<AxiWriteResp> axiWriteResp;
};

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

                #define PUT(name, value) do {                  \
                    dut.EN_keyMgr32_## name ##_put = 1;        \
                    dut.keyMgr32_## name ##_put_val = (value); \
                    assert(dut.RDY_keyMgr32_## name ##_put);   \
                    assert(dut.keyMgr32_## name ##_canPut);    \
                } while(0);
                #define NOPUT(name) dut.EN_keyMgr32_## name ##_put = 0;

                if (input.keyRequest) {
                    PUT(keyRequests, input.keyRequest.value());
                } else {
                    NOPUT(keyRequests);
                }

                if (input.finishedEpoch) {
                    PUT(finishedEpochs, input.finishedEpoch.value());
                } else {
                    NOPUT(finishedEpochs);
                }

                if (input.readReq) {
                    PUT(hostFacingSlave_ar, input.readReq.value().ar_flit().flit);
                } else {
                    NOPUT(hostFacingSlave_ar);
                }

                if (input.writeReq) {
                    PUT(hostFacingSlave_aw, input.writeReq.value().aw_flit().flit);
                    PUT(hostFacingSlave_w,  input.writeReq.value().w_flit().flit);
                } else {
                    NOPUT(hostFacingSlave_aw);
                    NOPUT(hostFacingSlave_w);
                }

                #undef NOPUT
                #undef PUT

                // Now pull out the outputs.
                #define CANPEEK(from) (dut.RDY_keyMgr32_## from ##_peek)
                #define POP(from, into) \
                    assert(dut.keyMgr32_## from ##_canPeek); \
                    assert(dut.RDY_keyMgr32_## from ##_drop); \
                    dut.EN_keyMgr32_## from ##_drop = 1; \
                    into = dut.keyMgr32_## from ##_peek;
                #define NOPOP(from) \
                    dut.EN_keyMgr32_## from ##_drop = 0; \

                KeyManagerOutput output{};
                output.time = main_time;

                if (CANPEEK(newEpochRequests)) {
                    Epoch newEpochRequest;
                    POP(newEpochRequests, newEpochRequest);
                    output.newEpochRequest = std::optional(newEpochRequest);
                } else {
                    NOPOP(newEpochRequests);
                }

                if (dut.RDY_keyMgr32_keyResponses_peek_fst && dut.RDY_keyMgr32_keyResponses_peek_snd) {
                    assert(dut.keyMgr32_keyResponses_canPeek);
                    assert(dut.RDY_keyMgr32_keyResponses_drop);
                    dut.EN_keyMgr32_keyResponses_drop = 1;

                    KeyResponse resp{};
                    resp.keyId = dut.keyMgr32_keyResponses_peek_fst;
                    // The tag on the Maybe is the most-significant-bit
                    // i.e. peek_snd[128], which will be in the bottom bit of the fifth 32-bit item
                    // The tag will be 0 for Invalid and 1 for Valid
                    if (dut.keyMgr32_keyResponses_peek_snd[4] & 1) {
                        // tag was 1 => valid
                        uint64_t top = (uint64_t(dut.keyMgr32_keyResponses_peek_snd[3]) << 32) | uint64_t(dut.keyMgr32_keyResponses_peek_snd[2]);
                        uint64_t bot = (uint64_t(dut.keyMgr32_keyResponses_peek_snd[1]) << 32) | uint64_t(dut.keyMgr32_keyResponses_peek_snd[0]);
                        resp.key = std::optional{Key{.top=top, .bottom=bot}};
                    } else {
                        // tag was 0 => invalid
                        resp.key = std::nullopt;
                    }

                    output.keyResponse = resp;
                } else {
                    dut.EN_keyMgr32_keyResponses_drop = 0;
                }

                if (CANPEEK(hostFacingSlave_r)) {
                    RFlit rflit;
                    POP(hostFacingSlave_r, rflit.flit);
                    output.axiReadResp = std::optional(AxiReadResp(rflit));
                } else {
                    NOPOP(hostFacingSlave_r);
                }

                if (CANPEEK(hostFacingSlave_b)) {
                    BFlit bflit;
                    POP(hostFacingSlave_b, bflit.flit);
                    output.axiWriteResp = std::optional(AxiWriteResp(bflit));
                } else {
                    NOPOP(hostFacingSlave_b);
                }

                #undef NOPOP
                #undef POP
                #undef CANPEEK

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
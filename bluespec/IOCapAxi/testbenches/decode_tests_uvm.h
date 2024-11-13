#ifndef DECODE_TESTS_UVM_H
#define DECODE_TESTS_UVM_H

#include <deque>

#include "tb_bitfields.h"

#include "tb.h"
#include "capabilities.h"
#include "util.h"

// Helper macros for generating stimulus
#define CANPUT_INPUT(name) ((dut.RDY_## name ##_put != 0) && (dut. name ##_canPut != 0))
#define PUT_INPUT(name, value) do {                  \
    dut.EN_## name ##_put = 1;        \
    dut. name ##_put_val = (value); \
    assert(dut.RDY_## name ##_put);   \
    assert(dut. name ##_canPut);    \
} while(0);
#define NOPUT_INPUT(name) dut.EN_## name ##_put = 0;
#define CANPEEK_INPUT(from) (dut.EN_## from ##_put)
#define PEEK_INPUT(from) dut. from ##_put_val
#define CANPEEK_OUTPUT(from) (dut.from ##_canPeek)
#define CANPOP_OUTPUT(from) (dut.RDY_## from ##_peek)
#define PEEK_OUTPUT(from) dut. from ##_peek
#define POP_OUTPUT(from, into) \
    assert(dut. from ##_canPeek); \
    assert(dut.RDY_## from ##_drop); \
    dut.EN_## from ##_drop = 1; \
    into = dut. from ##_peek;
#define NOPOP_OUTPUT(from) \
    dut.EN_## from ##_drop = 0; \

template<class DUT>
class ManyRandomValidCaps_11 : public StimulusGenerator<DUT> {
protected:
    uint64_t count;
    uint64_t end_tick;
    ThroughputTracker throughput;

    virtual U128 random_cap(std::mt19937& rng) {
        U128 key = U128::random(rng);
        // TODO randomize perms
        auto cap = random_initial_resource_cap_11(rng, key, 111, CCapPerms_Write);
        return U128::from_le(cap.data);
    }

public:
    ManyRandomValidCaps_11(uint64_t count) : count(count), end_tick(~0), throughput() {}
    virtual ~ManyRandomValidCaps_11() = default;

    virtual std::string name() override {
        return fmt::format("{} random valid 2024_11 caps", count);
    }
    virtual void driveInputsForTick(std::mt19937& rng, DUT& dut, uint64_t tick) override {
        NOPUT_INPUT(stimulusIn);
        if (count > 0) {
            throughput.trackCycleWithAvailableInput();
            if (CANPUT_INPUT(stimulusIn)) {
                auto cap128 = random_cap(rng).verilate();

                PUT_INPUT(stimulusIn, cap128);
                throughput.trackAccepted();

                count--;
                
                if (count == 0) {
                    // Give it 100 cycles to push everything out
                    end_tick = tick + 1000;
                }
            }
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        return (count == 0 && tick > end_tick);
    }
    virtual void dump_stats() override {
        fmt::println(stderr, "throughput, {}", throughput.asDouble());
    }
};


template<class DUT>
class ManyRandomValidCaps_02 : public ManyRandomValidCaps_11<DUT> {
protected:
    virtual U128 random_cap(std::mt19937& rng) override {
        U128 key = U128::random(rng);
        // TODO randomize perms
        auto cap = random_initial_resource_cap_02(rng, key, 111, CCapPerms_Write);
        return U128::from_le(cap.data);
    }
public:
    ManyRandomValidCaps_02(uint64_t count) : ManyRandomValidCaps_11<DUT>(count) {}
    virtual ~ManyRandomValidCaps_02() = default;

    virtual std::string name() override {
        return fmt::format("{} random valid 2024_02 caps", this->count);
    }
};

template<class DUT>
class ManyRandomBits : public ManyRandomValidCaps_11<DUT> {
protected:
    virtual U128 random_cap(std::mt19937& rng) override {
        return U128::random(rng);
    }
public:
    ManyRandomBits(uint64_t count) : ManyRandomValidCaps_11<DUT>(count) {}
    virtual ~ManyRandomBits() = default;

    virtual std::string name() override {
        return fmt::format("{} random bits", this->count);
    }
};

template<class DUT>
class DecoderScoreboard_11 : public Scoreboard<DUT> {
    // tick_initiated = the tick on which the capability was put-ed into the unit
    std::deque<LatencyTracked<decoder::CapCheckResult_Tuple2_CapPerms_CapRange>> expected;
    std::vector<uint64_t> latency;

protected:
    virtual CCapResult read_base_len_perms(U128& cap128, uint64_t& base, uint64_t& len, bool& len_64, CCapPerms& perms) {
        auto cap = CCap2024_11 {
            .signature = {0},
            .data = {0}
        };
        cap128.to_le(cap.data);


        CCapResult res = ccap2024_11_read_range(&cap, &base, &len, &len_64);
        if (res != CCapResult_Success) {
            return res;
        }
        return ccap2024_11_read_perms(&cap, &perms);
    }

public:
    DecoderScoreboard_11() = default;
    virtual ~DecoderScoreboard_11() = default;

    virtual void monitorAndScore(DUT& dut, uint64_t tick) {
        if (CANPEEK_INPUT(stimulusIn)) {
            auto cap128_v = PEEK_INPUT(stimulusIn);
            U128 cap128 = U128::from_verilated(cap128_v);

            uint64_t base = 0xdeadbeef;
            uint64_t len = 0xdeadbeef;
            bool len_64 = false;
            CCapPerms perms;

            if (read_base_len_perms(cap128, base, len, len_64, perms) != CCapResult_Success) {
                expected.push_back(LatencyTracked {
                    .tick_initiated=tick,
                    .value=decoder::CapCheckResult_Tuple2_CapPerms_CapRange {
                        .failTag = 1,
                    }
                });
            } else {
                expected.push_back(LatencyTracked {
                    .tick_initiated=tick,
                    .value=decoder::CapCheckResult_Tuple2_CapPerms_CapRange {
                        .succTop = (base + len),
                        .succTopTopBit = (len_64),
                        .succBase = base,
                        .succPerms = uint8_t(uint8_t(perms) - 1),
                        .failTag = 0,
                    }
                });
            }
        }

        if (CANPOP_OUTPUT(stimulusOut)) {
            VlWide<5> result_v;
            POP_OUTPUT(stimulusOut, result_v);
            auto result = decoder::CapCheckResult_Tuple2_CapPerms_CapRange::unpack(stdify_array(result_v));
            if (expected.empty()) {
                throw test_failure(fmt::format("DecoderScoreboard got unexpected output:\nexpected None\nall expected: {{}}\ngot: {}\n", result));
            } else {
                auto expectedVal = expected.front();
                expected.pop_front();
                bool different = false;
                if (expectedVal.value.failTag != result.failTag) {
                    different = true;
                } else if (expectedVal.value.failTag == 0 && result.failTag == 0 && expectedVal.value != result) {
                    different = true;
                }

                if (different) {
                    throw test_failure(fmt::format("DecoderScoreboard got unexpected output:\nexpected:     {}\nall expected: {}\ngot:          {}\n", expectedVal.value, expected, result));
                }

                latency.push_back(tick - expectedVal.tick_initiated);
            }
        } else {
            NOPOP_OUTPUT(stimulusOut);
        }
    }

    // Should raise a test_failure on failure
    virtual void endTest() override {
        if (
            !expected.empty()
        ) {
            throw test_failure(fmt::format(
                "DecoderScoreboard expected more outputs: {}\n", expected
            ));
        }
    }
    #define STRINGIFY(x) STRINGIFY2(x)
    #define STRINGIFY2(x) #x
    #define DUMP_MEAN_OF(x) fmt::println(stderr, STRINGIFY(x) ", {}", mean_of(x));
    virtual void dump_stats() override {
        DUMP_MEAN_OF(latency);
    }
    #undef DUMP_MEAN_OF
    #undef STRINGIFY2
    #undef STRINGIFY
};

template<class DUT>
class DecoderScoreboard_02 : public DecoderScoreboard_11<DUT> {
protected:
    virtual CCapResult read_base_len_perms(U128& cap128, uint64_t& base, uint64_t& len, bool& len_64, CCapPerms& perms) override {
        auto cap = CCap2024_02 {
            .signature = {0},
            .data = {0}
        };
        cap128.to_le(cap.data);


        CCapResult res = ccap2024_02_read_range(&cap, &base, &len, &len_64);
        if (res != CCapResult_Success) {
            return res;
        }
        return ccap2024_02_read_perms(&cap, &perms);
    }
public:
    DecoderScoreboard_02() = default;
    virtual ~DecoderScoreboard_02() = default;
};

template<class DUT>
class DecoderUVMishTest_11: public UVMishTest<DUT> {
public:
    DecoderUVMishTest_11(StimulusGenerator<DUT>* stimulus) :
        UVMishTest<DUT>(
            new DecoderScoreboard_11<DUT>(),
            stimulus
        ) {}
};

template<class DUT>
class DecoderUVMishTest_02: public UVMishTest<DUT> {
public:
    DecoderUVMishTest_02(StimulusGenerator<DUT>* stimulus) :
        UVMishTest<DUT>(
            new DecoderScoreboard_02<DUT>(),
            stimulus
        ) {}
};

#endif // DECODE_TESTS_UVM_H
#ifndef EXPOSER_TESTS_UVM_H
#define EXPOSER_TESTS_UVM_H

#include "key_manager.h"
#include "exposer.h"

#include "tb.h"
#include "capabilities.h"
#include "util.h"

#include <fmt/ranges.h>

#include <deque>
#include <memory>
#include <random>

struct AxiParams {
    uint64_t address;
    uint8_t transfer_width;
    uint8_t n_transfers;
};

template<CapType ctype>
struct ValidCapWithRange {
    CapStruct<ctype> cap;
    uint64_t cap_base;
    uint64_t cap_len;
    bool cap_is_almighty;
    CCapPerms perms;

    ValidCapWithRange(CapStruct<ctype> cap) : cap(cap) {
        if (this->cap.read_range(&this->cap_base, &this->cap_len, &this->cap_is_almighty) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap2024_XX_read_range");
        }
        if (this->cap.read_perms(&this->perms) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap2024_XX_read_perms");
        }
    }

    AxiParams valid_transfer_params(uint8_t transfer_width, uint8_t n_transfers) const {
        if (!cap_is_almighty) {
            if (cap_len == 0) {
                return AxiParams {
                    .address = cap_base,
                    .transfer_width = 1,
                    .n_transfers = 1,
                };
            }
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error(fmt::format("transfer_width = 0, cap_len = {}", cap_len));
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error(fmt::format("n_transfers = 0, transfer_width = {} cap_len = {}", transfer_width, cap_len));
            }
        }
        return AxiParams {
            .address = cap_base,
            .transfer_width = transfer_width,
            .n_transfers = n_transfers,
        };
    }
};

// Copy of ValidCapWithRange that has fallbacks for if the cap fails read_range
template<CapType ctype>
struct MaybeValidCapWithRange {
    CapStruct<ctype> cap;
    uint64_t cap_base;
    uint64_t cap_len;
    bool cap_is_almighty;
    CCapPerms perms;

    MaybeValidCapWithRange(CapStruct<ctype> cap) : cap(cap) {
        if (this->cap.read_range(&this->cap_base, &this->cap_len, &this->cap_is_almighty) != CCapResult_Success) {
            cap_base = 0xdeafbeef;
            cap_len = 0xdeadbeef;
            cap_is_almighty = false;
        }

        // should always be able to read perms
        if (this->cap.read_perms(&this->perms) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap2024_XX_read_perms");
        }
    }

    AxiParams maybe_valid_transfer_params(uint8_t transfer_width, uint8_t n_transfers) const {
        if (!cap_is_almighty) {
            if (cap_len == 0) {
                return AxiParams {
                    .address = cap_base,
                    .transfer_width = 1,
                    .n_transfers = 1,
                };
            }
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error(fmt::format("transfer_width = 0, cap_len = {}", cap_len));
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error(fmt::format("n_transfers = 0, transfer_width = {} cap_len = {}", transfer_width, cap_len));
            }
        }
        return AxiParams {
            .address = cap_base,
            .transfer_width = transfer_width,
            .n_transfers = n_transfers,
        };
    }
};

/**
 * UVM-style testing is more dynamic than fixed per-cycle expected-input expected-output testing.
 * This implementation uses two components per test: a Stimulus Generator to twiddle the inputs on the DUT,
 * and a Scoreboard which monitors the inputs and outputs of the DUT to determine if the behaviour was correct.
 * This structure allows the same Scoreboard to be reused for different tests, and the Scoreboard is designed to be timing-independent.
 * The Stimulus Generator changes for each test, and usually just specifies the kinds of transactions tested
 * (e.g. a simple test may say "shove this queue of AW transactions into the unit as fast as possible")
 * which don't react to the output of the DUT (e.g. the DUT will expect to pass transactions on to memory and have them complete at some point - those completions aren't pre-planned.)
 * KeyManagerShimStimulus and SanitizedMemStimulus are convenience interfaces which objects can implement to observe the outputs of the DUT and dynamically generate relevant inputs.
 * 
 * The ExposerStimulus stimulus generator class is the base class for ShimmedExposer UVM tests,
 * and subclasses should pass instances of KeyManagerShimStimulus and SanitizedMemStimulus to its base constructor.
 */

// Helper macros for generating stimulus
#define CANPUT_INPUT(name) ((dut.RDY_## name ##_put != 0) && (dut. name ##_canPut != 0))
#define PUT_INPUT(name, value) do {                  \
    dut.EN_## name ##_put = 1;        \
    dut. name ##_put_val = (value); \
    assert(dut.RDY_## name ##_put);   \
    assert(dut. name ##_canPut);    \
} while(0);
#define NOPUT_INPUT(name) dut.EN_## name ##_put = 0;
#define CANPEEK_OUTPUT(from) (dut.from ##_canPeek)
#define PEEK_OUTPUT(from) dut. from ##_peek
#define POP_OUTPUT(from, into) \
    assert(dut. from ##_canPeek); \
    assert(dut.RDY_## from ##_drop); \
    dut.EN_## from ##_drop = 1; \
    into = dut. from ##_peek;
#define NOPOP_OUTPUT(from) \
    dut.EN_## from ##_drop = 0;

/**
 * Generic stimulus generator for the key manager parts of ShimmedExposer DUTs.
 * Dynamically reacts to the DUT outputs (e.g. requests for key data) to generate inputs
 * (e.g. responding with the requested key data).
 * Intended for use inside a ExposerStimulus.
 */
template<class DUT, KeyMngrVersion V>
class KeyManagerShimStimulus {
public:
    std::unordered_map<key_manager::KeyId, U128> secrets; // Fake keymanager proxy.

    KeyManagerShimStimulus() : secrets() {}
    // Observe the key manager inputs (epoch fulfilments and key responses)
    virtual void driveInputsForKeyMgr(DUT& dut, uint64_t tick) = 0;
    virtual void dump_toml_stats(FILE* stats){}
};

/**
 * Implementation of KeyManagerShimStimulus that assumes revocation never occurs.
 * Simply responds to key requests with the contents of the `secrets` field of the parent class.
 */
template<class DUT, KeyMngrVersion V>
class BasicKeyManagerShimStimulus;

template<class DUT>
class BasicKeyManagerShimStimulus<DUT, KeyMngrV1>: public KeyManagerShimStimulus<DUT, KeyMngrV1> {
    std::deque<key_manager::KeyResponse> keyResponses;
public:
    BasicKeyManagerShimStimulus() : KeyManagerShimStimulus<DUT, KeyMngrV1>() {}
    virtual void driveInputsForKeyMgr(DUT& dut, uint64_t tick) {
        if (!keyResponses.empty() && CANPUT_INPUT(keyStoreShim_keyResponses)) {
            auto keyResponse = keyResponses.front();
            keyResponses.pop_front();
            assert(key_manager::Tuple2_KeyId_MaybeKey::unpack(keyResponse.asBluespec().pack()) == keyResponse.asBluespec());
            PUT_INPUT(keyStoreShim_keyResponses, verilate_array(keyResponse.asBluespec().pack()));
        } else {
            NOPUT_INPUT(keyStoreShim_keyResponses);
        }

        if (CANPEEK_OUTPUT(keyStoreShim_keyRequests)) {
            key_manager::KeyId requested = 0;
            POP_OUTPUT(keyStoreShim_keyRequests, requested);
            if (this->secrets.contains(requested)) {
                keyResponses.push_back(key_manager::KeyResponse {
                    .keyId = requested,
                    .key = this->secrets[requested]
                });
            } else {
                keyResponses.push_back(key_manager::KeyResponse {
                    .keyId = requested,
                    .key = std::nullopt
                });
            }
        }
    }
};

template<class DUT>
class BasicKeyManagerShimStimulus<DUT, KeyMngrV2>: public KeyManagerShimStimulus<DUT, KeyMngrV2> {
    std::deque<key_manager::KeyResponse> keyResponses;
    std::unordered_map<key_manager::KeyId, int64_t> refcounts;
public:
    BasicKeyManagerShimStimulus() : KeyManagerShimStimulus<DUT, KeyMngrV2>() {}
    virtual void driveInputsForKeyMgr(DUT& dut, uint64_t tick) {
        if (!keyResponses.empty() && CANPUT_INPUT(keyStoreShim_keyResponses)) {
            auto keyResponse = keyResponses.front();
            keyResponses.pop_front();
            assert(key_manager::Tuple2_KeyId_MaybeKey::unpack(keyResponse.asBluespec().pack()) == keyResponse.asBluespec());
            PUT_INPUT(keyStoreShim_keyResponses, verilate_array(keyResponse.asBluespec().pack()));
        } else {
            NOPUT_INPUT(keyStoreShim_keyResponses);
        }

        if (CANPEEK_OUTPUT(keyStoreShim_keyRequests)) {
            key_manager::KeyId requested = 0;
            POP_OUTPUT(keyStoreShim_keyRequests, requested);
            if (this->secrets.contains(requested)) {
                keyResponses.push_back(key_manager::KeyResponse {
                    .keyId = requested,
                    .key = this->secrets[requested]
                });
            } else {
                keyResponses.push_back(key_manager::KeyResponse {
                    .keyId = requested,
                    .key = std::nullopt
                });
            }
        } else {
            NOPOP_OUTPUT(keyStoreShim_keyRequests);
        }

        if (CANPEEK_OUTPUT(keyStoreShim_rValve_Increment)) {
            key_manager::KeyId requested = 0;
            POP_OUTPUT(keyStoreShim_rValve_Increment, requested);
            if (this->refcounts.contains(requested)) {
                this->refcounts[requested] += 1;
            } else {
                this->refcounts[requested] = 1;
            }
        } else {
            NOPOP_OUTPUT(keyStoreShim_rValve_Increment);
        }
        if (CANPEEK_OUTPUT(keyStoreShim_rValve_Decrement)) {
            key_manager::KeyId requested = 0;
            POP_OUTPUT(keyStoreShim_rValve_Decrement, requested);
            if (this->refcounts.contains(requested)) {
                this->refcounts[requested] -= 1;
            } else {
                this->refcounts[requested] = -1;
            }
            if (this->refcounts[requested] < 0) {
                throw test_failure("Decremented a refcount below zero");
            }
        } else {
            NOPOP_OUTPUT(keyStoreShim_rValve_Decrement);
        }
        if (CANPEEK_OUTPUT(keyStoreShim_wValve_Increment)) {
            key_manager::KeyId requested = 0;
            POP_OUTPUT(keyStoreShim_wValve_Increment, requested);
            if (this->refcounts.contains(requested)) {
                this->refcounts[requested] += 1;
            } else {
                this->refcounts[requested] = 1;
            }
        } else {
            NOPOP_OUTPUT(keyStoreShim_wValve_Increment);
        }
        if (CANPEEK_OUTPUT(keyStoreShim_wValve_Decrement)) {
            key_manager::KeyId requested = 0;
            POP_OUTPUT(keyStoreShim_wValve_Decrement, requested);
            if (this->refcounts.contains(requested)) {
                this->refcounts[requested] -= 1;
            } else {
                this->refcounts[requested] = -1;
            }
            if (this->refcounts[requested] < 0) {
                throw test_failure("Decremented a refcount below zero");
            }
        } else {
            NOPOP_OUTPUT(keyStoreShim_wValve_Decrement);
        }
    }
};

/**
 * Generic stimulus generator for the sanitized memory outputs of ShimmedExposer DUTs.
 * Intended for use inside a ExposerStimulus.
 */
template<class DUT>
class SanitizedMemStimulus {
public:
    // Observe the sanitized AW/W/AR outputs and drive the sanitize B/R inputs in response
    virtual void driveBAndRInputs(DUT& dut, uint64_t tick) = 0;
    virtual void dump_toml_stats(FILE* stats){}
};

/**
 * Implementation of SanitizedMemStimulus.
 * Immediately responds to AW and AR requests from the memory outputs with no cycle delay.
 */
template<class DUT>
class BasicSanitizedMemStimulus : public SanitizedMemStimulus<DUT> {
    // Flits which have arrived before their corresponding AW
    std::deque<axi::SanitizedAxi::WFlit_data32> unexpectedWFlits;
    std::deque<uint64_t> wFlitsExpectedPerBurst;
    std::deque<axi::SanitizedAxi::BFlit_id4> pendingBInputs;
    std::deque<axi::SanitizedAxi::BFlit_id4> bInputs;
    std::deque<axi::SanitizedAxi::RFlit_id4_data32> rInputs;

    ThroughputTracker b_throughput;
    ThroughputTracker r_throughput;
public:
    virtual void driveBAndRInputs(DUT& dut, uint64_t tick) {
        NOPUT_INPUT(exposer4x32_sanitizedOut_b);
        if (!bInputs.empty()) {
            if (CANPUT_INPUT(exposer4x32_sanitizedOut_b)) {
                auto bInput = bInputs.front();
                bInputs.pop_front();
                PUT_INPUT(exposer4x32_sanitizedOut_b, bInput.pack());
                b_throughput.trackAccepted();
            }
            b_throughput.trackCycleWithAvailableInput();
        }

        NOPUT_INPUT(exposer4x32_sanitizedOut_r);
        if (!rInputs.empty()) {
            if (CANPUT_INPUT(exposer4x32_sanitizedOut_r)) {
                auto rInput = rInputs.front();
                rInputs.pop_front();
                PUT_INPUT(exposer4x32_sanitizedOut_r, rInput.pack());
                r_throughput.trackAccepted();
            }
            r_throughput.trackCycleWithAvailableInput();
        }

        if (CANPEEK_OUTPUT(exposer4x32_sanitizedOut_aw)) {
            VlWide<4> flit;
            POP_OUTPUT(exposer4x32_sanitizedOut_aw, flit);
            auto awFlit = axi::SanitizedAxi::AWFlit_id4_addr64_user0::unpack(stdify_array(flit));
            wFlitsExpectedPerBurst.push_back(awFlit.awlen + 1);
            pendingBInputs.push_back(axi::SanitizedAxi::BFlit_id4 {
                .bresp = (uint8_t)axi::AXI4_Resp::Okay,
                .bid = awFlit.awid
            });
        }

        if (CANPEEK_OUTPUT(exposer4x32_sanitizedOut_w)) {
            uint64_t flit;
            POP_OUTPUT(exposer4x32_sanitizedOut_w, flit);
            auto wFlit = axi::SanitizedAxi::WFlit_data32::unpack(flit);
            unexpectedWFlits.push_back(wFlit);
        }

        // Resolve W flits
        while (!wFlitsExpectedPerBurst.empty() && !unexpectedWFlits.empty()) {
            auto wFlit = unexpectedWFlits.front();
            unexpectedWFlits.pop_front();

            if (wFlitsExpectedPerBurst.front() == 0) {
                throw std::logic_error("BasicSanitizedMemStimulus had a burst of 0 flits expected");
            } else if (wFlitsExpectedPerBurst.front() == 1) {
                // There should be a pendingBInput as well, enqueue that for sending
                if (pendingBInputs.empty()) {
                    throw std::logic_error("BasicSanitizedMemStimulus popped off wFlitsExpectedPerBurst without a corresponding B input for that burst");
                }
                bInputs.push_back(pendingBInputs.front());
                pendingBInputs.pop_front();
                wFlitsExpectedPerBurst.pop_front();
                assert(wFlit.wlast == 1);
            } else {
                wFlitsExpectedPerBurst.front() -= 1;
                assert(wFlit.wlast == 0);
            }
        }

        if (CANPEEK_OUTPUT(exposer4x32_sanitizedOut_ar)) {
            VlWide<4> flit;
            POP_OUTPUT(exposer4x32_sanitizedOut_ar, flit);
            auto arFlit = axi::SanitizedAxi::ARFlit_id4_addr64_user0::unpack(stdify_array(flit));
            // Assume the arFlits use 32-bit data
            for (uint32_t i = 0; i < arFlit.arlen + 1; i++) {
                rInputs.push_back(axi::SanitizedAxi::RFlit_id4_data32 {
                    .rlast = (uint8_t)((i == arFlit.arlen) ? 1 : 0),
                    .rresp = (uint8_t)axi::AXI4_Resp::Okay,
                    .rdata = i, // TODO better RDATA generation
                    .rid = arFlit.arid,
                });
            }
        }
    }
    virtual void dump_toml_stats(FILE* stats) override {
        fmt::println(stats, "b_throughput = {}", b_throughput.asDouble());
        fmt::println(stats, "r_throughput = {}", r_throughput.asDouble());
    }
};

/**
 * Base class for all ShimmedExposer stimulus generators
 */
template<class DUT, CapType ctype, KeyMngrVersion V>
class ExposerStimulus : public StimulusGenerator<DUT> {
public:
    std::unique_ptr<KeyManagerShimStimulus<DUT, V>> keyMgr;
protected:
    std::unique_ptr<SanitizedMemStimulus<DUT>> sanitizedMem;

    std::deque<axi::IOCapAxi::AWFlit_id4_addr64_user3> awInputs;
    std::deque<axi::IOCapAxi::WFlit_data32> wInputs;
    std::deque<axi::IOCapAxi::ARFlit_id4_addr64_user3> arInputs;

    ThroughputTracker aw_throughput;
    ThroughputTracker w_throughput;
    ThroughputTracker ar_throughput;

    /// Use these functions in subclasses!

    ValidCapWithRange<ctype> test_legacy_random_initial_resource_cap(std::mt19937& rng, uint32_t secret_id, CCapPerms perms) {
        return ValidCapWithRange(CapStruct<ctype>::legacy_random_initial_resource_cap(rng, keyMgr->secrets[secret_id], secret_id, perms));
    }
    ValidCapWithRange<ctype> test_librust_random_valid_cap(std::mt19937& rng, uint32_t secret_id, int n_cavs=-1) {
        CCapU128 secret_key;
        keyMgr->secrets[secret_id].to_le(secret_key);
        return ValidCapWithRange(CapStruct<ctype>::librust_rand_valid_cap(rng, &secret_key, &secret_id, n_cavs));
    }
    MaybeValidCapWithRange<ctype> test_librust_random_edge_case_cap(std::mt19937& rng, uint32_t secret_id, uintptr_t edge_case) {
        CCapU128 secret_key;
        keyMgr->secrets[secret_id].to_le(secret_key);
        return MaybeValidCapWithRange(CapStruct<ctype>::librust_rand_edge_case_cap(rng, &secret_key, &secret_id, edge_case));
    }
    void enqueueReadBurst(CapStruct<ctype>& cap, AxiParams& axi_params, uint8_t id) {
        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);

        arInputs.push_back(axi::IOCapAxi::ARFlit_id4_addr64_user3 {
            .aruser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .arlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .araddr = axi_params.address,
            .arid = (uint8_t)(id & 0xF),
        });
        arInputs.push_back(axi::IOCapAxi::packCap1_ar(cap128, sig128));
        arInputs.push_back(axi::IOCapAxi::packCap2_ar(cap128, sig128));
        arInputs.push_back(axi::IOCapAxi::packCap3_ar(cap128, sig128));
    }
    void enqueueWriteBurst(CapStruct<ctype>& cap, AxiParams& axi_params, uint8_t id) {
        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);

        awInputs.push_back(axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = (uint8_t)(id & 0xF),
        });
        awInputs.push_back(axi::IOCapAxi::packCap1_aw(cap128, sig128));
        awInputs.push_back(axi::IOCapAxi::packCap2_aw(cap128, sig128));
        awInputs.push_back(axi::IOCapAxi::packCap3_aw(cap128, sig128));

        for (int i = 0; i < axi_params.n_transfers; i++) {
            wInputs.push_back(axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            });
        }
    }

public:
    ExposerStimulus(KeyManagerShimStimulus<DUT, V>* keyMgr, SanitizedMemStimulus<DUT>* sanitizedMem) :
        keyMgr(keyMgr), sanitizedMem(sanitizedMem), awInputs(), wInputs(), arInputs() {}

    virtual ~ExposerStimulus() = default;
    virtual void driveInputsForTick(std::mt19937& rng, DUT& dut, uint64_t tick) {
        keyMgr->driveInputsForKeyMgr(dut, tick);
        sanitizedMem->driveBAndRInputs(dut, tick);

        NOPUT_INPUT(exposer4x32_iocapsIn_axiSignals_aw);
        if (!awInputs.empty()) {
            if (CANPUT_INPUT(exposer4x32_iocapsIn_axiSignals_aw)) {
                auto awInput = awInputs.front();
                awInputs.pop_front();
                PUT_INPUT(exposer4x32_iocapsIn_axiSignals_aw, verilate_array(awInput.pack()));
                aw_throughput.trackAccepted();
            }
            aw_throughput.trackCycleWithAvailableInput();
        }

        NOPUT_INPUT(exposer4x32_iocapsIn_axiSignals_w);
        if (!wInputs.empty()) {
            if (CANPUT_INPUT(exposer4x32_iocapsIn_axiSignals_w)) {
                auto wInput = wInputs.front();
                wInputs.pop_front();
                PUT_INPUT(exposer4x32_iocapsIn_axiSignals_w, wInput.pack());
                w_throughput.trackAccepted();
            }
            w_throughput.trackCycleWithAvailableInput();
        }

        NOPUT_INPUT(exposer4x32_iocapsIn_axiSignals_ar);
        if (!arInputs.empty()) {
            if (CANPUT_INPUT(exposer4x32_iocapsIn_axiSignals_ar)) {
                auto arInput = arInputs.front();
                arInputs.pop_front();
                PUT_INPUT(exposer4x32_iocapsIn_axiSignals_ar, verilate_array(arInput.pack()));
                ar_throughput.trackAccepted();
            }
            ar_throughput.trackCycleWithAvailableInput();
        }
    }
    virtual void dump_toml_stats(FILE* stats) override {
        fmt::println(stats, "aw_throughput = {}", aw_throughput.asDouble());
        fmt::println(stats, "w_throughput = {}", w_throughput.asDouble());
        fmt::println(stats, "ar_throughput = {}", ar_throughput.asDouble());
        sanitizedMem->dump_toml_stats(stats);
        keyMgr->dump_toml_stats(stats);
    }
};

// Used for B and R flits which may be associated with unauthenticated (capability was bad) AW/AR flits or correct AW/AR flits which received a response from the sanitized side
template<typename T>
struct LatencyTrackedWithAuthCorrectness {
    uint64_t tick_initiated;
    bool was_correct;
    T value;
};

template <typename T> class fmt::formatter<LatencyTrackedWithAuthCorrectness<T>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (LatencyTrackedWithAuthCorrectness<T> const& x, Context& ctx) const {
        return format_to(ctx.out(), "{{ .tick_initiated = {}, .was_correct = {}, .value = {} }}", x.tick_initiated, x.was_correct, x.value);
    }
};

/**
 * Scoreboard for ShimmedExposer tests.
 * Can be instantiated directly or subclassed.
 * Does not do anything to handle revocation, but revocation can be simulated by modifying the KeyID -> Key map `secrets` the scoreboard uses
 * to determine if incoming requests will be valid or not.
 */
template<class DUT, CapType ctype, KeyMngrVersion V>
class BaseExposerScoreboard : public Scoreboard<DUT> {
protected:
    std::unordered_map<key_manager::KeyId, U128>& secrets; // Fake keymanager proxy. THIS SCOREBOARD ASSUMES KEYS DONT CHANGE

    // Early versions of the exposer will always pass transactions through, even if it later registers them as "invalid" with the performance counters.
    bool expectPassthroughInvalidTransactions;

    std::vector<axi::IOCapAxi::AWFlit_id4_addr64_user3> awInProgress;
    // tick_initiated = the tick on which the last AW flit was put-ed into the unit
    std::deque<LatencyTracked<axi::SanitizedAxi::AWFlit_id4_addr64_user0>> expectedAw;

    // W flits received on the input where we don't know if their associated AWs were correctly authed.
    // The DUT cannot possibly know if W flits in this queue are correctly authed or not.
    std::deque<LatencyTracked<axi::IOCapAxi::WFlit_data32>> wInProgress;
    // The status of future groups of W flits. e.g. if you recieve a valid AW transaction specifying 3 W flits, store (true, 3) so the next 3 W flits received will automatically be counted as valid.
    std::deque<std::pair<bool, uint64_t>> wflitValidity; // tuple[0] = is valid, tuple[1] = count
    // The W flits we expect to see on the output, i.e. only valid ones
    std::deque<LatencyTracked<axi::SanitizedAxi::WFlit_data32>> expectedW;

    std::vector<axi::IOCapAxi::ARFlit_id4_addr64_user3> arInProgress;
    // tick_initiated = the tick on which the last AW flit was put-ed into the unit
    std::deque<LatencyTracked<axi::SanitizedAxi::ARFlit_id4_addr64_user0>> expectedAr;

    // B and R responses have no ordering guarantees *except* B is ordered w.r.t. B when the IDs are the same, ditto for R.
    // => for each, keep a map of ID -> ordered list of responses.
    // Whenever we receive a response, look in the map at the given ID. if the vector is empty, there wasn't supposed to be a response. Otherwise the first pop()-ed element must match.
    // tick_initiated = the tick on which the 
    std::array<std::deque<LatencyTrackedWithAuthCorrectness<axi::IOCapAxi::BFlit_id4>>, 16> expectedB;
    std::array<std::deque<LatencyTrackedWithAuthCorrectness<axi::IOCapAxi::RFlit_id4_data32>>, 16> expectedR;

    uint64_t expectedGoodWrite = 0;
    uint64_t expectedBadWrite = 0;
    uint64_t expectedBadWriteBadCap = 0;
    uint64_t expectedBadWriteGoodCapBadRange = 0;
    uint64_t expectedGoodRead = 0;
    uint64_t expectedBadRead = 0;
    uint64_t expectedBadReadBadCap = 0;
    uint64_t expectedBadReadGoodCapBadRange = 0;
    uint64_t signalledGoodWrite = 0;
    uint64_t signalledBadWrite = 0;
    uint64_t signalledGoodRead = 0;
    uint64_t signalledBadRead = 0;

    std::vector<ShimmedExposerInput<V>> inputs;
    std::vector<ShimmedExposerOutput<V>> outputs;

    std::vector<uint64_t> aw_aw_latency;
    std::vector<uint64_t> aw_b_latency;
    std::vector<uint64_t> ar_ar_latency;
    std::vector<uint64_t> ar_r_latency;
    std::vector<uint64_t> w_w_latency;
    std::vector<uint64_t> b_b_latency;
    std::vector<uint64_t> r_r_latency;

    void resolveAwFlit(uint64_t tick, std::optional<axi::IOCapAxi::AWFlit_id4_addr64_user3> newIncomingFlit) {
        if (newIncomingFlit) {
            awInProgress.push_back(newIncomingFlit.value());
            if (awInProgress.size() == 1 && expectPassthroughInvalidTransactions) {
                // No matter what, this AW transaction *will* be passed through, so do it immediately
                if (awInProgress[0].awuser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Start) {
                    throw test_failure(fmt::format("BaseExposerScoreboard got nonsensical initial aw flit - incorrect user flags {}", awInProgress));
                }
                expectedAw.push_back({
                    tick,
                    axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
                        .awregion = awInProgress[0].awregion,
                        .awqos = awInProgress[0].awqos,
                        .awprot = awInProgress[0].awprot,
                        .awcache = awInProgress[0].awcache,
                        .awlock = awInProgress[0].awlock,
                        .awburst = awInProgress[0].awburst,
                        .awsize = awInProgress[0].awsize,
                        .awlen = awInProgress[0].awlen,
                        .awaddr = awInProgress[0].awaddr,
                        .awid = awInProgress[0].awid,
                    }
                });
                // and expect the right number of W flits to be passed through
                wflitValidity.push_back(std::make_pair(true, axi::len_to_n_transfers(awInProgress[0].awlen)));
            }
        }
        if (awInProgress.size() > 4) {
            throw test_failure(fmt::format("BaseExposerScoreboard got nonsensical set of aw flits - too many somehow? {}", awInProgress));
        }
        if (awInProgress.size() == 4) {
            if (
                (awInProgress[0].awuser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Start) ||
                (awInProgress[1].awuser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap1) ||
                (awInProgress[2].awuser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap2) ||
                (awInProgress[3].awuser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap3)
            ) {
                throw test_failure(fmt::format("BaseExposerScoreboard got nonsensical set of aw flits - incorrect user flags {}", awInProgress));
            }
            U128 data{.top = 0, .bottom = 0};
            U128 sig{.top = 0, .bottom = 0};
            unpackCap1_aw(data, sig, awInProgress[1]);
            unpackCap2_aw(data, sig, awInProgress[2]);
            unpackCap3_aw(data, sig, awInProgress[3]);
            CapStruct<ctype> cap;
            sig.to_le(cap.signature);
            data.to_le(cap.data);
            uint32_t secret_key_id;

            // Find the address range 
            uint64_t axiBase = awInProgress[0].awaddr;
            uint64_t axiTop;
            try {
                axiTop = axiBase + axi::burst_byte_length((axi::AXI4_Burst)awInProgress[0].awburst, awInProgress[0].awsize, awInProgress[0].awlen);
            } catch (std::runtime_error& ex) {
                throw test_failure(fmt::format("BaseExposerScoreboard got nonsensical set of aw flits - {} - {}", awInProgress, ex.what()));
            }
            
            uint64_t base = 0;
            uint64_t len = 0;
            bool len64 = false;
            CCapPerms perms = CCapPerms_ReadWrite;
            bool capIsValid = (cap.read_secret_id(&secret_key_id) == CCapResult_Success) &&
                                (cap.read_range(&base, &len, &len64) == CCapResult_Success) &&
                                (cap.read_perms(&perms) == CCapResult_Success) && 
                                ((perms & CCapPerms_Write) != 0);
            if (capIsValid && secrets.contains(secret_key_id & 0xFF)) {
                CCapU128 secret_key;
                secrets[secret_key_id & 0xFF].to_le(secret_key);
                capIsValid = (cap.check_signature(&secret_key) == CCapResult_Success);
            } else {
                capIsValid = false;
            }

            // TODO if the capability extends over the top of the 64-bit addrspace, how to handle

            // TODO this might not work if axiBase+axiLen = end of addrspace?
            bool rangeIsValid = len64 || (axiBase >= base && (axiTop - base) <= len);
            // The performance counters should reflect the validity of the capability/access in all cases
            if (capIsValid && rangeIsValid) {
                expectedGoodWrite++;
            } else {
                expectedBadWrite++;
                if (!capIsValid) {
                    expectedBadWriteBadCap++;
                } else {
                    if (expectedBadWriteGoodCapBadRange == 0) {
                        fmt::println(stderr, "Bad range: cap 0x{:x} .. 0x{:x}, axi {:x} .. {:x}", base, base+len, axiBase, axiTop);
                    }
                    expectedBadWriteGoodCapBadRange++;
                }
            }
            // If the capability and ranges are valid,
            // expect an AW flit to come out *and* the right number of W flits!
            // If `expectPassthroughInvalidTransactions` is true then these will already be expected, so do nothing.
            if (!expectPassthroughInvalidTransactions) {
                if (capIsValid && rangeIsValid) {
                    expectedAw.push_back({
                        tick,
                        axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
                            .awregion = awInProgress[0].awregion,
                            .awqos = awInProgress[0].awqos,
                            .awprot = awInProgress[0].awprot,
                            .awcache = awInProgress[0].awcache,
                            .awlock = awInProgress[0].awlock,
                            .awburst = awInProgress[0].awburst,
                            .awsize = awInProgress[0].awsize,
                            .awlen = awInProgress[0].awlen,
                            .awaddr = awInProgress[0].awaddr,
                            .awid = awInProgress[0].awid,
                        }
                    });
                    // and expect the right number of W flits to be passed through
                    wflitValidity.push_back(std::make_pair(true, axi::len_to_n_transfers(awInProgress[0].awlen)));
                } else {
                    // Otherwise expect a B flit with a BAD response to come out
                    expectedB[awInProgress[0].awid].push_back({
                        tick,
                        false, // This is from an invalid AW flit, not a valid B response from the sanitized side
                        axi::IOCapAxi::BFlit_id4 {
                            .bresp = (uint8_t)axi::AXI4_Resp::SlvErr,
                            .bid = awInProgress[0].awid
                        }
                    });
                    // and drop the W flits
                    wflitValidity.push_back(std::make_pair(false, axi::len_to_n_transfers(awInProgress[0].awlen)));
                }
            }

            // We've now handled the set of AW flits, and can drop them
            awInProgress.clear();
        }
    }

    void resolveArFlit(uint64_t tick, std::optional<axi::IOCapAxi::ARFlit_id4_addr64_user3> newIncomingFlit) {
        if (newIncomingFlit) {
            arInProgress.push_back(newIncomingFlit.value());
            if (arInProgress.size() == 1 && expectPassthroughInvalidTransactions) {
                // No matter what, this AR transaction *will* be passed through, so do it immediately
                if (arInProgress[0].aruser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Start) {
                    throw test_failure(fmt::format("BaseExposerScoreboard got nonsensical initial ar flit - incorrect user flags {}", awInProgress));
                }
                expectedAr.push_back({
                    tick,
                    axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
                        .arregion = arInProgress[0].arregion,
                        .arqos = arInProgress[0].arqos,
                        .arprot = arInProgress[0].arprot,
                        .arcache = arInProgress[0].arcache,
                        .arlock = arInProgress[0].arlock,
                        .arburst = arInProgress[0].arburst,
                        .arsize = arInProgress[0].arsize,
                        .arlen = arInProgress[0].arlen,
                        .araddr = arInProgress[0].araddr,
                        .arid = arInProgress[0].arid,
                    }
                });
            }
        }
        if (arInProgress.size() > 4) {
            throw test_failure(fmt::format("BaseExposerScoreboard got nonsensical set of ar flits - too many somehow? {}", arInProgress));
        }
        if (arInProgress.size() == 4) {
            if (
                (arInProgress[0].aruser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Start) ||
                (arInProgress[1].aruser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap1) ||
                (arInProgress[2].aruser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap2) ||
                (arInProgress[3].aruser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap3)
            ) {
                throw test_failure(fmt::format("BaseExposerScoreboard got nonsensical set of ar flits - incorrect user flags {}", arInProgress));
            }
            U128 data{.top = 0, .bottom = 0};
            U128 sig{.top = 0, .bottom = 0};
            unpackCap1_ar(data, sig, arInProgress[1]);
            unpackCap2_ar(data, sig, arInProgress[2]);
            unpackCap3_ar(data, sig, arInProgress[3]);
            CapStruct<ctype> cap;
            sig.to_le(cap.signature);
            data.to_le(cap.data);
            uint32_t secret_key_id;

            // Find the address range 
            uint64_t axiBase = arInProgress[0].araddr;
            uint64_t axiTop;
            try {
                axiTop = axiBase + axi::burst_byte_length((axi::AXI4_Burst)arInProgress[0].arburst, arInProgress[0].arsize, arInProgress[0].arlen);
            } catch (std::runtime_error& ex) {
                throw test_failure(fmt::format("BaseExposerScoreboard got nonsensical set of ar flits - {} - {}", arInProgress, ex.what()));
            }
            
            uint64_t base = 0;
            uint64_t len = 0;
            bool len64 = false;
            CCapPerms perms = CCapPerms_ReadWrite;
            bool capIsValid = (cap.read_secret_id(&secret_key_id) == CCapResult_Success) &&
                                (cap.read_range(&base, &len, &len64) == CCapResult_Success) &&
                                (cap.read_perms(&perms) == CCapResult_Success) && 
                                ((perms & CCapPerms_Read) != 0);
            if (capIsValid && secrets.contains(secret_key_id & 0xFF)) {
                CCapU128 secret_key;
                secrets[secret_key_id & 0xFF].to_le(secret_key);
                capIsValid = (cap.check_signature(&secret_key) == CCapResult_Success);
            } else {
                capIsValid = false;
            }

            // TODO this might not work if axiBase+axiLen = end of addrspace?
            bool rangeIsValid = len64 || (axiBase >= base && axiTop <= (base + len));
            // The performance counters should reflect the validity of the capability/access in all cases
            if (capIsValid && rangeIsValid) {
                expectedGoodRead++;
            } else {
                expectedBadRead++;
                if (!capIsValid) {
                    expectedBadReadBadCap++;
                } else {
                    expectedBadReadGoodCapBadRange++;
                }
            }
            // If the capability and ranges are valid, expect an AR flit to come out
            // If `expectPassthroughInvalidTransactions` is true then these will already be expected, so do nothing.
            if (!expectPassthroughInvalidTransactions) {
                if (capIsValid && rangeIsValid) {
                    expectedAr.push_back({
                        tick,
                        axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
                            .arregion = arInProgress[0].arregion,
                            .arqos = arInProgress[0].arqos,
                            .arprot = arInProgress[0].arprot,
                            .arcache = arInProgress[0].arcache,
                            .arlock = arInProgress[0].arlock,
                            .arburst = arInProgress[0].arburst,
                            .arsize = arInProgress[0].arsize,
                            .arlen = arInProgress[0].arlen,
                            .araddr = arInProgress[0].araddr,
                            .arid = arInProgress[0].arid,
                        }
                    });
                } else {
                    // Otherwise expect a B flit with a BAD response to come out
                    expectedR[arInProgress[0].arid].push_back({
                        tick,
                        false, // This is from an invalid AR flit, not a valid R response from the sanitized side
                        axi::IOCapAxi::RFlit_id4_data32 {
                            .rlast = 1,
                            .rresp = (uint8_t)axi::AXI4_Resp::SlvErr,
                            .rdata = 0xaaaaaaaa, // Bluespec uses this to mean ?
                            .rid = arInProgress[0].arid,
                        }
                    });
                }
            }

            // We've now handled the set of AR flits, and can drop them
            arInProgress.clear();
        }
    }

    void resolveWFlits(uint64_t tick, std::optional<axi::IOCapAxi::WFlit_data32> newIncomingFlit) {
        if (newIncomingFlit) {
            wInProgress.push_back({
                tick,
                newIncomingFlit.value()
            });
        }
        // If there are outstanding future groups of W flits, compare them to the wInProgress.
        while (!wflitValidity.empty() && !wInProgress.empty()) {
            auto [groupValid, groupLen] = *wflitValidity.begin();
            while (!wInProgress.empty() && groupLen > 0) {
                const LatencyTracked<axi::IOCapAxi::WFlit_data32> iocapFlit = wInProgress.front();
                wInProgress.pop_front();
                // Handle the w flit: if it's in a valid group make a new expected flit
                if (groupValid) {
                    expectedW.push_back({
                        iocapFlit.tick_initiated,
                        axi::SanitizedAxi::WFlit_data32 {
                            .wlast = iocapFlit.value.wlast,
                            .wstrb = iocapFlit.value.wstrb,
                            .wdata = iocapFlit.value.wdata,
                        }
                    });
                }
                // Otherwise drop it and do nothing!
                groupLen--;
            }
            // If we exhausted the group, pop it off
            if (groupLen == 0) {
                wflitValidity.pop_front();
            } else {
                // Otherwise, update the new length
                wflitValidity.front() = std::make_pair(groupValid, groupLen);
            }
        }
        if (!wflitValidity.empty() && !wInProgress.empty()) {
            throw std::logic_error("Screwed up resolveWFlits");
        }
    }

    virtual void monitorAndScoreKeyManager(DUT& dut, uint64_t tick, KeyMngrShimInput<V>& inputKeyManager, KeyMngrShimOutput<V>& outputKeyManager) = 0;

public:
    BaseExposerScoreboard(std::unordered_map<key_manager::KeyId, U128>& secrets, bool expectPassthroughInvalidTransactions = false) : secrets(secrets),
        expectPassthroughInvalidTransactions(expectPassthroughInvalidTransactions),
        awInProgress(),
        expectedAw(),
        wInProgress(),
        wflitValidity(),
        expectedW(),
        arInProgress(),
        expectedAr(),
        expectedB(),
        expectedR() {}
    virtual ~BaseExposerScoreboard() = default;
    // Should raise a test_failure on failure
    virtual void monitorAndScore(DUT& dut, uint64_t tick) {
        ShimmedExposerOutput<V> output{0};
        output.time = tick;
        pull_output(dut, output); // TODO apply backpressure?
        if (output.is_notable())
            outputs.push_back(output);

        if (output.clean_flit_aw) {
            if (expectedAw.empty()) {
                throw test_failure(fmt::format("BaseExposerScoreboard got unexpected aw flit:\nexpected None\ngot: {}\n", output.clean_flit_aw.value()));
            } else {
                auto expected = expectedAw.front();
                expectedAw.pop_front();
                if (output.clean_flit_aw.value() != expected.value) {
                    throw test_failure(fmt::format("BaseExposerScoreboard got unexpected aw flit:\nexpected: {}\ngot: {}\n", expected, output.clean_flit_aw.value()));
                }
                aw_aw_latency.push_back(tick - expected.tick_initiated);
            }
        }

        if (output.clean_flit_ar) {
            if (expectedAr.empty()) {
                throw test_failure(fmt::format("BaseExposerScoreboard got unexpected ar flit:\nexpected None\ngot: {}\n", output.clean_flit_ar.value()));
            } else {
                auto expected = expectedAr.front();
                expectedAr.pop_front();
                if (output.clean_flit_ar.value() != expected.value) {
                    throw test_failure(fmt::format("BaseExposerScoreboard got unexpected ar flit:\nexpected: {}\ngot: {}\n", expected, output.clean_flit_ar.value()));
                }
                ar_ar_latency.push_back(tick - expected.tick_initiated);
            }
        }

        if (output.clean_flit_w) {
            if (expectedW.empty()) {
                throw test_failure(fmt::format("BaseExposerScoreboard got unexpected w flit:\nexpected None, had {} unresolved and {} groups\ngot: {}\n", wInProgress, wflitValidity, output.clean_flit_w.value()));
            } else {
                auto expected = expectedW.front();
                expectedW.pop_front();
                if (output.clean_flit_w.value() != expected.value) {
                    throw test_failure(fmt::format("BaseExposerScoreboard got unexpected w flit:\nexpected {}, had {} unresolved and {} groups\ngot: {}\n", expected, wInProgress, wflitValidity, output.clean_flit_w.value()));
                }
                // TODO: note that this is the latency of entering the pipe e.g. the latency from the input port being *ready* to it coming out the other end. Not the latency from the input becoming available.
                w_w_latency.push_back(tick - expected.tick_initiated);
            }
        }

        if (output.iocap_flit_b) {
            auto& expectedForId = expectedB[output.iocap_flit_b.value().bid];
            if (expectedForId.empty()) {
                throw test_failure(fmt::format("BaseExposerScoreboard got unexpected b flit:\nexpected None\nall expected: {}\ngot: {}\n", expectedB, output.iocap_flit_b.value()));
            } else {
                auto expected = expectedForId.front();
                expectedForId.pop_front();
                if (output.iocap_flit_b.value() != expected.value) {
                    throw test_failure(fmt::format("BaseExposerScoreboard got unexpected b flit:\nexpected: {}\nall expected: {}\ngot: {}\n", expected, expectedB, output.iocap_flit_b.value()));
                }
                if (expected.was_correct) {
                    b_b_latency.push_back(tick - expected.tick_initiated);
                } else {
                    aw_b_latency.push_back(tick - expected.tick_initiated);
                }
            }
        }

        if (output.iocap_flit_r) {
            auto& expectedForId = expectedR[output.iocap_flit_r.value().rid];
            if (expectedForId.empty()) {
                throw test_failure(fmt::format("BaseExposerScoreboard got unexpected r flit:\nexpected None\nall expected: {}\ngot: {}\n", expectedR, output.iocap_flit_r.value()));
            } else {
                auto expected = expectedForId.front();
                expectedForId.pop_front();
                if (output.iocap_flit_r.value() != expected.value) {
                    throw test_failure(fmt::format("BaseExposerScoreboard got unexpected r flit:\nexpected: {}\nall expected: {}\ngot: {}\n", expected, expectedR, output.iocap_flit_r.value()));
                }
                if (expected.was_correct) {
                    r_r_latency.push_back(tick - expected.tick_initiated);
                } else {
                    ar_r_latency.push_back(tick - expected.tick_initiated);
                }
            }
        }

        ShimmedExposerInput<V> input{0};
        input.time = tick;
        observe_input(dut, input);
        if (input.is_notable())
            inputs.push_back(input);

        // Add incoming B and R flits from the sanitized-side to the list of expected outputs
        if (input.clean_flit_b) {
            auto expected = input.clean_flit_b.value();
            expectedB[expected.bid].push_back({
                tick, 
                true, // This is from a valid B flit from the sanitized side
                axi::IOCapAxi::BFlit_id4 {
                    .bresp = expected.bresp,
                    .bid = expected.bid
                }
            });
        }
        if (input.clean_flit_r) {
            auto expected = input.clean_flit_r.value();
            expectedR[expected.rid].push_back({
                tick,
                true, // This is from a valid R flit from the sanitized side
                axi::IOCapAxi::RFlit_id4_data32 {
                    .rlast = expected.rlast,
                    .rresp = expected.rresp,
                    .rdata = expected.rdata,
                    .rid = expected.rid
                }
            });
        }

        // Resolve AW flits to see if there's a new valid/invalid burst
        // If invalid, it should return a B flit. If we received a staitized B flit with the same ID on the same cycle, that one comes first in the ordering because it must be the result of a valid AW on a previous cycle/came before.
        resolveAwFlit(tick, input.iocap_flit_aw);
        // Resolve AR flits to see if there's a new valid/invalid burst
        // Same as above for R flits
        resolveArFlit(tick, input.iocap_flit_ar);

        // Add incoming W flits from the IOCap-side to the list of expected output W flits, if possible.
        resolveWFlits(tick, input.iocap_flit_w);

        this->monitorAndScoreKeyManager(dut, tick, input.keyManager, output.keyManager);
    }
    // Should raise a test_failure on failure
    virtual void endTest() override {
        // TODO log inputs and outputs
        if (
            !this->awInProgress.empty() ||
            !this->expectedAw.empty() ||
            !this->wInProgress.empty() ||
            !this->expectedW.empty() ||
            !this->arInProgress.empty() ||
            !this->expectedAr.empty() ||
            // Check foreach ID in {B, R}
            std::any_of(this->expectedB.begin(), this->expectedB.end(), [](auto& expectedForId) { return !expectedForId.empty(); }) ||
            std::any_of(this->expectedR.begin(), this->expectedR.end(), [](auto& expectedForId) { return !expectedForId.empty(); }) ||
            (this->expectedGoodWrite != this->signalledGoodWrite) ||
            (this->expectedBadWrite != this->signalledBadWrite) ||
            (this->expectedGoodRead != this->signalledGoodRead) ||
            (this->expectedBadRead != this->signalledBadRead)
        ) {
            throw test_failure(fmt::format(
                "BaseExposerScoreboard unexpected outcome:\n"
                "aw: {} in progress, {} expected\n"
                "w: {} in progress, {} expected\n"
                "ar: {} in progress, {} expected\n"
                "b: {}\n"
                "r: {}\n"
                "perf counters exp/act:\n"
                "good write {}/{}\n"
                "bad write {}/{}\n"
                "bad write bad cap {}\n"
                "bad write good cap bad range {}\n"
                "good read {}/{}\n"
                "bad read {}/{}\n"
                "bad read bad cap {}\n"
                "bad read good cap bad range {}\n"
                ,
                this->awInProgress, this->expectedAw,
                this->wInProgress, this->expectedW,
                this->arInProgress, this->expectedAr,
                this->expectedB,
                this->expectedR,
                this->expectedGoodWrite, this->signalledGoodWrite,
                this->expectedBadWrite, this->signalledBadWrite,
                this->expectedBadWriteBadCap, this->expectedBadWriteGoodCapBadRange,
                this->expectedGoodRead, this->signalledGoodRead,
                this->expectedBadRead, this->signalledBadRead,
                this->expectedBadReadBadCap, this->expectedBadReadGoodCapBadRange
            ));
        }
    }
    #define STRINGIFY(x) STRINGIFY2(x)
    #define STRINGIFY2(x) #x
    #define DUMP_MEAN_OF(x) fmt::println(stats, STRINGIFY(x) "_mean = {}", mean_of(x));
    virtual void dump_toml_stats(FILE* stats) override {
        DUMP_MEAN_OF(aw_aw_latency);
        DUMP_MEAN_OF(aw_b_latency);
        DUMP_MEAN_OF(ar_ar_latency);
        DUMP_MEAN_OF(ar_r_latency);
        DUMP_MEAN_OF(w_w_latency);
        DUMP_MEAN_OF(b_b_latency);
        DUMP_MEAN_OF(r_r_latency);
        fmt::println(stats, "exp_valid_write = {}", expectedGoodWrite);
        fmt::println(stats, "exp_invalid_write = {}", expectedBadWrite);
        fmt::println(stats, "exp_invalid_write_bad_cap = {}", expectedBadWriteBadCap);
        fmt::println(stats, "exp_invalid_write_good_cap_bad_range = {}", expectedBadWriteGoodCapBadRange);
        fmt::println(stats, "exp_valid_read = {}", expectedGoodRead);
        fmt::println(stats, "exp_invalid_read = {}", expectedBadRead);
        fmt::println(stats, "exp_invalid_read_bad_cap = {}", expectedBadReadBadCap);
        fmt::println(stats, "exp_invalid_read_good_cap_bad_range = {}", expectedBadReadGoodCapBadRange);
        fmt::println(stats, "valid_cap_ratio = {}", (double(expectedGoodWrite+expectedGoodRead))/(double(expectedGoodWrite+expectedGoodRead+expectedBadWrite+expectedBadRead)));
    }
    #undef DUMP_MEAN_OF
    #undef STRINGIFY2
    #undef STRINGIFY
};

template<class DUT, CapType ctype, KeyMngrVersion V>
class ExposerScoreboard;

template<class DUT, CapType ctype>
class ExposerScoreboard<DUT, ctype, KeyMngrV1> : public BaseExposerScoreboard<DUT, ctype, KeyMngrV1> {
protected:
    std::deque<key_manager::Epoch> expectedEpochCompletions;

    virtual void onKeyMngrNewEpoch(key_manager::Epoch nextEpoch) {
        fmt::println(stderr, "Note - ExposerScoreboard<KeyMngrV1> base class doesn't automatically handle keys when getting new key epoch.");
        expectedEpochCompletions.push_back((nextEpoch - 1) & 1);
    }
    virtual void onKeyMngrKeyResponse(key_manager::KeyResponse response) {
        std::optional<U128> expected = std::nullopt;
        if (this->secrets.contains(response.keyId)) {
            expected = this->secrets[response.keyId];
        }
        if (expected != response.key) {
            throw test_failure(fmt::format("ExposerScoreboard<KeyMngrV1> base class saw a new value {} for key ID {} that doesn't match expected value {}", response.key, response.keyId, expected));
        }
    }

    virtual void monitorAndScoreKeyManager(DUT& dut, uint64_t tick, KeyMngrShimInput<KeyMngrV1>& inputKeyManager, KeyMngrShimOutput<KeyMngrV1>& outputKeyManager) override{
        if (outputKeyManager.finishedEpoch) {
            if (expectedEpochCompletions.empty()) {
                throw test_failure(fmt::format("ExposerScoreboard<KeyMngrV1> got unexpected finishedEpoch:\nexpected None\ngot: {}\n", outputKeyManager.finishedEpoch.value()));
            } else {
                auto expected = expectedEpochCompletions.front();
                expectedEpochCompletions.pop_front();
                if (outputKeyManager.finishedEpoch.value() != expected) {
                    throw test_failure(fmt::format("ExposerScoreboard<KeyMngrV1> got unexpected finishedEpoch:\nexpected: {}\ngot: {}\n", expected, outputKeyManager.finishedEpoch.value()));
                }
            }
        }

        if (outputKeyManager.bumpPerfCounterGoodWrite) {
            this->signalledGoodWrite++;
        }
        if (outputKeyManager.bumpPerfCounterBadWrite) {
            this->signalledBadWrite++;
        }
        if (outputKeyManager.bumpPerfCounterGoodRead) {
            this->signalledGoodRead++;
        }
        if (outputKeyManager.bumpPerfCounterBadRead) {
            this->signalledBadRead++;
        }

        if (inputKeyManager.newEpochRequest) {
            onKeyMngrNewEpoch(inputKeyManager.newEpochRequest.value());
        }

        if (inputKeyManager.keyResponse) {
            onKeyMngrKeyResponse(inputKeyManager.keyResponse.value());
        }
    }
public:
    ExposerScoreboard(std::unordered_map<key_manager::KeyId, U128>& secrets, bool expectPassthroughInvalidTransactions = false) :
        BaseExposerScoreboard<DUT, ctype, KeyMngrV1>(secrets, expectPassthroughInvalidTransactions),
        expectedEpochCompletions() {}
    virtual ~ExposerScoreboard() override = default;
    // Should raise a test_failure on failure
    virtual void endTest() override {
        // TODO log inputs and outputs
        if (
            !expectedEpochCompletions.empty() ||
            !this->awInProgress.empty() ||
            !this->expectedAw.empty() ||
            !this->wInProgress.empty() ||
            !this->expectedW.empty() ||
            !this->arInProgress.empty() ||
            !this->expectedAr.empty() ||
            // Check foreach ID in {B, R}
            std::any_of(this->expectedB.begin(), this->expectedB.end(), [](auto& expectedForId) { return !expectedForId.empty(); }) ||
            std::any_of(this->expectedR.begin(), this->expectedR.end(), [](auto& expectedForId) { return !expectedForId.empty(); }) ||
            (this->expectedGoodWrite != this->signalledGoodWrite) ||
            (this->expectedBadWrite != this->signalledBadWrite) ||
            (this->expectedGoodRead != this->signalledGoodRead) ||
            (this->expectedBadRead != this->signalledBadRead)
        ) {
            throw test_failure(fmt::format(
                "BaseExposerScoreboard unexpected outcome:\n"
                "epoch completions: {}\n"
                "aw: {} in progress, {} expected\n"
                "w: {} in progress, {} expected\n"
                "ar: {} in progress, {} expected\n"
                "b: {}\n"
                "r: {}\n"
                "perf counters exp/act:\n"
                "good write {}/{}\n"
                "bad write {}/{}\n"
                "bad write bad cap {}\n"
                "bad write good cap bad range {}\n"
                "good read {}/{}\n"
                "bad read {}/{}\n"
                "bad read bad cap {}\n"
                "bad read good cap bad range {}\n"
                ,
                expectedEpochCompletions,
                this->awInProgress, this->expectedAw,
                this->wInProgress, this->expectedW,
                this->arInProgress, this->expectedAr,
                this->expectedB,
                this->expectedR,
                this->expectedGoodWrite, this->signalledGoodWrite,
                this->expectedBadWrite, this->signalledBadWrite,
                this->expectedBadWriteBadCap, this->expectedBadWriteGoodCapBadRange,
                this->expectedGoodRead, this->signalledGoodRead,
                this->expectedBadRead, this->signalledBadRead,
                this->expectedBadReadBadCap, this->expectedBadReadGoodCapBadRange
            ));
        }
    }
};

template<class DUT, CapType ctype>
class ExposerScoreboard<DUT, ctype, KeyMngrV2> : public BaseExposerScoreboard<DUT, ctype, KeyMngrV2> {
protected:
    // TODO test the rwValve_IncDecrement

    // virtual void onKeyMngrNewEpoch(key_manager::Epoch nextEpoch) {
    //     fmt::println(stderr, "Note - ExposerScoreboard<KeyMngrV1> base class doesn't automatically handle keys when getting new key epoch.");
    //     expectedEpochCompletions.push_back((nextEpoch - 1) & 1);
    // }
    virtual void onKeyMngrKeyResponse(key_manager::KeyResponse response) {
        std::optional<U128> expected = std::nullopt;
        if (this->secrets.contains(response.keyId)) {
            expected = this->secrets[response.keyId];
        }
        if (expected != response.key) {
            throw test_failure(fmt::format("ExposerScoreboard<KeyMngrV2> base class saw a new value {} for key ID {} that doesn't match expected value {}", response.key, response.keyId, expected));
        }
    }

    virtual void monitorAndScoreKeyManager(DUT& dut, uint64_t tick, KeyMngrShimInput<KeyMngrV2>& inputKeyManager, KeyMngrShimOutput<KeyMngrV2>& outputKeyManager) override {
        /*if (outputKeyManager.finishedEpoch) {
            if (expectedEpochCompletions.empty()) {
                throw test_failure(fmt::format("ExposerScoreboard<KeyMngrV1> got unexpected finishedEpoch:\nexpected None\ngot: {}\n", outputKeyManager.finishedEpoch.value()));
            } else {
                auto expected = expectedEpochCompletions.front();
                expectedEpochCompletions.pop_front();
                if (outputKeyManager.finishedEpoch.value() != expected) {
                    throw test_failure(fmt::format("ExposerScoreboard<KeyMngrV1> got unexpected finishedEpoch:\nexpected: {}\ngot: {}\n", expected, outputKeyManager.finishedEpoch.value()));
                }
            }
        }*/

        if (outputKeyManager.bumpPerfCounterGoodWrite) {
            this->signalledGoodWrite++;
        }
        if (outputKeyManager.bumpPerfCounterBadWrite) {
            this->signalledBadWrite++;
        }
        if (outputKeyManager.bumpPerfCounterGoodRead) {
            this->signalledGoodRead++;
        }
        if (outputKeyManager.bumpPerfCounterBadRead) {
            this->signalledBadRead++;
        }

        if (inputKeyManager.keyResponse) {
            onKeyMngrKeyResponse(inputKeyManager.keyResponse.value());
        }

        
    }
public:
    ExposerScoreboard(std::unordered_map<key_manager::KeyId, U128>& secrets, bool expectPassthroughInvalidTransactions = false) :
        BaseExposerScoreboard<DUT, ctype, KeyMngrV2>(secrets, expectPassthroughInvalidTransactions)
        {}
    virtual ~ExposerScoreboard() override = default;
    /*
    // Should raise a test_failure on failure
    virtual void endTest() override {
        // TODO log inputs and outputs
        if (
            !expectedEpochCompletions.empty() ||
            !this->awInProgress.empty() ||
            !this->expectedAw.empty() ||
            !this->wInProgress.empty() ||
            !this->expectedW.empty() ||
            !this->arInProgress.empty() ||
            !this->expectedAr.empty() ||
            // Check foreach ID in {B, R}
            std::any_of(this->expectedB.begin(), this->expectedB.end(), [](auto& expectedForId) { return !expectedForId.empty(); }) ||
            std::any_of(this->expectedR.begin(), this->expectedR.end(), [](auto& expectedForId) { return !expectedForId.empty(); }) ||
            (this->expectedGoodWrite != this->signalledGoodWrite) ||
            (this->expectedBadWrite != this->signalledBadWrite) ||
            (this->expectedGoodRead != this->signalledGoodRead) ||
            (this->expectedBadRead != this->signalledBadRead)
        ) {
            throw test_failure(fmt::format(
                "BaseExposerScoreboard unexpected outcome:\n"
                "epoch completions: {}\n"
                "aw: {} in progress, {} expected\n"
                "w: {} in progress, {} expected\n"
                "ar: {} in progress, {} expected\n"
                "b: {}\n"
                "r: {}\n"
                "perf counters exp/act:\n"
                "good write {}/{}\n"
                "bad write {}/{}\n"
                "bad write bad cap {}\n"
                "bad write good cap bad range {}\n"
                "good read {}/{}\n"
                "bad read {}/{}\n"
                "bad read bad cap {}\n"
                "bad read good cap bad range {}\n"
                ,
                expectedEpochCompletions,
                this->awInProgress, this->expectedAw,
                this->wInProgress, this->expectedW,
                this->arInProgress, this->expectedAr,
                this->expectedB,
                this->expectedR,
                this->expectedGoodWrite, this->signalledGoodWrite,
                this->expectedBadWrite, this->signalledBadWrite,
                this->expectedBadWriteBadCap, this->expectedBadWriteGoodCapBadRange,
                this->expectedGoodRead, this->signalledGoodRead,
                this->expectedBadRead, this->signalledBadRead,
                this->expectedBadReadBadCap, this->expectedBadReadGoodCapBadRange
            ));
        }
    }*/
};

template<class DUT, CapType ctype = CapType::Cap2024_02, KeyMngrVersion V = KeyMngrV1>
class ExposerUVMishTest: public UVMishTest<DUT> {
public:
    ExposerUVMishTest(ExposerStimulus<DUT, ctype, V>* stimulus, bool expectPassthroughInvalidTransactions = false) :
        UVMishTest<DUT>(
            new ExposerScoreboard<DUT, ctype, V>(stimulus->keyMgr->secrets, expectPassthroughInvalidTransactions),
            stimulus
        ) {}
};

template<class DUT, CapType ctype, KeyMngrVersion V>
class UVMValidKeyValidInitialCapValidAccess : public ExposerStimulus<DUT, ctype, V> {
    CCapPerms perms;
    
public:
    virtual ~UVMValidKeyValidInitialCapValidAccess() = default;
    virtual std::string name() override {
        return fmt::format("Valid-Key Valid-Cap Valid-{}", ccap_perms_str(perms));
    }
    UVMValidKeyValidInitialCapValidAccess(CCapPerms perms) : ExposerStimulus<DUT, ctype, V>(
        new BasicKeyManagerShimStimulus<DUT, V>(),
        new BasicSanitizedMemStimulus<DUT>()
    ), perms(perms) {}
    virtual void setup(std::mt19937& rng) override {
        const uint8_t axi_id = 0b1011;
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(rng);

        this->keyMgr->secrets[secret_id] = key;
        auto cap_data = this->test_legacy_random_initial_resource_cap(rng, secret_id, perms);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
        if (perms & CCapPerms_Read) {
            this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
        }
        if (perms & CCapPerms_Write) {
            this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // 100 cycles should do it
        return tick >= 1000;
    }
};

template<class DUT, CapType ctype, KeyMngrVersion V>
class UVMValidKeyValidInitialCapOOBAccess : public ExposerStimulus<DUT, ctype, V> {
    CCapPerms perms;
    
public:
    virtual ~UVMValidKeyValidInitialCapOOBAccess() = default;
    virtual std::string name() override {
        return fmt::format("Valid-Key Valid-Cap OOB-{}", ccap_perms_str(perms));
    }
    UVMValidKeyValidInitialCapOOBAccess(CCapPerms perms) : ExposerStimulus<DUT, ctype, V>(
        new BasicKeyManagerShimStimulus<DUT, V>(),
        new BasicSanitizedMemStimulus<DUT>()
    ), perms(perms) {}
    virtual void setup(std::mt19937& rng) override {        
        const uint8_t axi_id = 0b1011;
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(rng);

        this->keyMgr->secrets[secret_id] = key;
        auto cap_data = this->test_legacy_random_initial_resource_cap(rng, secret_id, perms);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
        axi_params.address = cap_data.cap_base - 4096;
        if (perms & CCapPerms_Read) {
            this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
        }
        if (perms & CCapPerms_Write) {
            this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // 100 cycles should do it
        return tick >= 1000;
    }
};

template<class DUT, CapType ctype, KeyMngrVersion V>
class UVMInvalidKeyAccess : public ExposerStimulus<DUT, ctype, V> {
    CCapPerms perms;
    
public:
    virtual ~UVMInvalidKeyAccess() = default;
    virtual std::string name() override {
        return fmt::format("Invalid-Key {}", ccap_perms_str(perms));
    }
    UVMInvalidKeyAccess(CCapPerms perms) : ExposerStimulus<DUT, ctype, V>(
        new BasicKeyManagerShimStimulus<DUT, V>(),
        new BasicSanitizedMemStimulus<DUT>()
    ), perms(perms) {}
    virtual void setup(std::mt19937& rng) override {        
        const uint8_t axi_id = 0b1011;
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(rng);

        this->keyMgr->secrets[secret_id] = key;
        // Use the wrong secret key ID
        auto cap_data = this->test_legacy_random_initial_resource_cap(rng, 90, perms);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
        axi_params.address = cap_data.cap_base - 4096;
        if (perms & CCapPerms_Read) {
            this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
        }
        if (perms & CCapPerms_Write) {
            this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // 100 cycles should do it
        return tick >= 1000;
    }
};

template<class DUT, CapType ctype, KeyMngrVersion V>
class UVMValidKeyValidCapBadPerms : public ExposerStimulus<DUT, ctype, V> {
public:
    virtual ~UVMValidKeyValidCapBadPerms() = default;
    virtual std::string name() override {
        return "Valid-Key Valid-Cap BadPerms";
    }
    UVMValidKeyValidCapBadPerms() : ExposerStimulus<DUT, ctype, V>(
        new BasicKeyManagerShimStimulus<DUT, V>(),
        new BasicSanitizedMemStimulus<DUT>()
    ) {}
    virtual void setup(std::mt19937& rng) override {        
        const uint8_t axi_id = 0b1011;
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(rng);

        this->keyMgr->secrets[secret_id] = key;
        {
            auto cap_data = this->test_legacy_random_initial_resource_cap(rng, secret_id, CCapPerms_Read);
            auto axi_params = cap_data.valid_transfer_params(32, 20);
            this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
        }
        {
            auto cap_data = this->test_legacy_random_initial_resource_cap(rng, secret_id, CCapPerms_Write);
            auto axi_params = cap_data.valid_transfer_params(32, 20);
            this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // 100 cycles should do it
        return tick >= 1000;
    }
};

template<class DUT, CapType ctype, KeyMngrVersion V>
class UVMValidKeyBadSigCap : public ExposerStimulus<DUT, ctype, V> {
public:
    virtual ~UVMValidKeyBadSigCap() = default;
    virtual std::string name() override {
        return "Valid-Key BadSig-Cap";
    }
    UVMValidKeyBadSigCap() : ExposerStimulus<DUT, ctype, V>(
        new BasicKeyManagerShimStimulus<DUT, V>(),
        new BasicSanitizedMemStimulus<DUT>()
    ) {}
    virtual void setup(std::mt19937& rng) override {        
        const uint8_t axi_id = 0b1011;
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(rng);

        this->keyMgr->secrets[secret_id] = key;
        const U128 badSignature = {
            .top = 0x01020304050607,
            .bottom = 0x08090a0b0c0d0e0f,
        };
        {
            auto cap_data = this->test_legacy_random_initial_resource_cap(rng, secret_id, CCapPerms_Read);
            badSignature.to_le(cap_data.cap.signature);
            auto axi_params = cap_data.valid_transfer_params(32, 20);
            this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
        }
        {
            auto cap_data = this->test_legacy_random_initial_resource_cap(rng, secret_id, CCapPerms_Write);
            badSignature.to_le(cap_data.cap.signature);
            auto axi_params = cap_data.valid_transfer_params(32, 20);
            this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // 100 cycles should do it
        return tick >= 1000;
    }
};

template<class DUT, CapType ctype>
class UVMTransactionsBetweenRevocations_KeyMngrV1 : public ExposerStimulus<DUT, ctype, KeyMngrV1> {
    uint64_t n_revocations;
    uint8_t epoch = 0;
public:
    virtual ~UVMTransactionsBetweenRevocations_KeyMngrV1() = default;
    virtual std::string name() override {
        return fmt::format("Valid-Key Valid-Cap Valid-ReadWrite with {} revocations", n_revocations);
    }
    UVMTransactionsBetweenRevocations_KeyMngrV1(uint64_t n_revocations) : ExposerStimulus<DUT, ctype, KeyMngrV1>(
        new BasicKeyManagerShimStimulus<DUT, KeyMngrV1>(),
        new BasicSanitizedMemStimulus<DUT>()
    ), n_revocations(n_revocations) {}
    virtual void driveInputsForTick(std::mt19937& rng, DUT& dut, uint64_t tick) {
        if (tick % 5000 == 0) {
            // Enqueue 450 cycles worth of transactions, including creating secret keys
            // TODO multiple transactions, mixing key ids?

            const key_manager::KeyId secret_id = 111;
            const U128 key = U128::random(rng);
            const uint8_t axi_id = 0b1011;

            this->keyMgr->secrets[secret_id] = key;
            {
                auto cap_data = this->test_legacy_random_initial_resource_cap(rng, secret_id, CCapPerms_Read);
                auto axi_params = cap_data.valid_transfer_params(32, 10);
                this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
            }
            {
                auto cap_data = this->test_legacy_random_initial_resource_cap(rng, secret_id, CCapPerms_Write);
                auto axi_params = cap_data.valid_transfer_params(32, 10);
                this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
            }
        }

        ExposerStimulus<DUT, ctype, KeyMngrV1>::driveInputsForTick(rng, dut, tick);

        if (tick % 5000 == 4500) {
            // Transactions should be over, drive the revocation signal
            if (!this->awInputs.empty() || !this->arInputs.empty() || !this->wInputs.empty()) {
                throw std::logic_error(
                    fmt::format("Can't revoke yet - transactions still outstanding {} {} {}",
                        this->awInputs,
                        this->arInputs,
                        this->wInputs
                    )
                );
            }

            if (CANPUT_INPUT(keyStoreShim_newEpochRequests)) {
                epoch = (epoch + 1) & 1;
                PUT_INPUT(keyStoreShim_newEpochRequests, epoch);
            } else {
                throw std::logic_error("Couldn't put into newEpochRequests!");
            }

            // Delete secret keys
            this->keyMgr->secrets.clear();
        } else {
            NOPUT_INPUT(keyStoreShim_newEpochRequests);
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // Each revocation = 450 cycles of transactions then 50 for revocation
        // Plus some spare cycles - TODO why???
        return tick >= (5000 * (n_revocations) + 350);
    }
};

template<class DUT, CapType ctype, KeyMngrVersion V>
class UVMStreamOfNValidTransactions : public ExposerStimulus<DUT, ctype, V> {
    CCapPerms perms;
    uint64_t n_transactions;
    uint8_t n_data_flits_per_transaction;

    uint64_t final_tick = 0;
    
public:
    virtual ~UVMStreamOfNValidTransactions() = default;
    virtual std::string name() override {
        return fmt::format("Stream of {} {} transactions ({} data flits each)", n_transactions, ccap_perms_str(perms), n_data_flits_per_transaction);
    }
    UVMStreamOfNValidTransactions(CCapPerms perms, uint64_t n_transactions, uint8_t n_data_flits_per_transaction) : ExposerStimulus<DUT, ctype, V>(
        new BasicKeyManagerShimStimulus<DUT, V>(),
        new BasicSanitizedMemStimulus<DUT>()
    ), perms(perms), n_transactions(n_transactions), n_data_flits_per_transaction(n_data_flits_per_transaction) {}
    virtual void setup(std::mt19937& rng) override {
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(rng);

        this->keyMgr->secrets[secret_id] = key;
        for (uint64_t i = 0; i < n_transactions; i++) {
            uint8_t axi_id = i & 0xF;
            auto cap_data = this->test_legacy_random_initial_resource_cap(rng, secret_id, perms);
            auto axi_params = cap_data.valid_transfer_params(32, n_data_flits_per_transaction);
            if (perms & CCapPerms_Read) {
                this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
            }
            if (perms & CCapPerms_Write) {
                this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
            }
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        if (final_tick == 0) {
            if (this->awInputs.empty() && this->wInputs.empty() && this->arInputs.empty()) {
                // Give 1000 cycles of buffer
                final_tick = tick + 10000;
            }
            return false;
        } else {
            return tick >= final_tick;
        }
    }
};

template<class DUT, CapType ctype, KeyMngrVersion V>
class UVMStreamOfNLibRustValidTransactions : public ExposerStimulus<DUT, ctype, V> {
    uint64_t n_transactions;
    int n_cavs = -1;

    uint64_t final_tick = 0;
    
public:
    virtual ~UVMStreamOfNLibRustValidTransactions() = default;
    virtual std::string name() override {
        if (n_cavs == -1) {
            return fmt::format("Stream of {} librust random valid {} transactions", n_transactions, ctype);
        } else {
            return fmt::format("Stream of {} librust random valid {} {}-caveat transactions", n_transactions, ctype, n_cavs);
        }
    }
    UVMStreamOfNLibRustValidTransactions(uint64_t n_transactions, int n_cavs = -1) : ExposerStimulus<DUT, ctype, V>(
        new BasicKeyManagerShimStimulus<DUT, V>(),
        new BasicSanitizedMemStimulus<DUT>()
    ), n_transactions(n_transactions), n_cavs(n_cavs) {
        if (n_cavs < -1 || n_cavs > 2) {
            throw std::runtime_error(fmt::format("Cannot have a stream of {}-caveat transactions - invalid caveat count", n_cavs));
        }
    }
    virtual void setup(std::mt19937& rng) override {
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(rng);

        this->keyMgr->secrets[secret_id] = key;
        for (uint64_t i = 0; i < n_transactions; i++) {
            uint8_t axi_id = i & 0xF;
            auto cap_data = this->test_librust_random_valid_cap(rng, secret_id, n_cavs);
            auto axi_params = cap_data.valid_transfer_params(32, 20);
            if (cap_data.perms & CCapPerms_Read) {
                this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
            }
            if (cap_data.perms & CCapPerms_Write) {
                this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
            }
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        if (final_tick == 0) {
            if (this->awInputs.empty() && this->wInputs.empty() && this->arInputs.empty()) {
                // Give 10000 cycles of buffer
                final_tick = tick + 100000;
            }
            return false;
        } else {
            return tick >= final_tick;
        }
    }
};

template<class DUT, CapType ctype, KeyMngrVersion V>
class UVMStreamOfNLibRustEdgeCaseTransactions : public ExposerStimulus<DUT, ctype, V> {
    uint64_t n_transactions;
    uintptr_t edge_case;

    uint64_t final_tick = 0;
    
public:
    virtual ~UVMStreamOfNLibRustEdgeCaseTransactions() = default;
    virtual std::string name() override {
        return fmt::format("Stream of {} librust random edge case {} {} transactions", n_transactions, CapStruct<ctype>::librust_rand_edge_case_str(edge_case), ctype);
    }
    UVMStreamOfNLibRustEdgeCaseTransactions(uint64_t n_transactions, uintptr_t edge_case) : ExposerStimulus<DUT, ctype, V>(
        new BasicKeyManagerShimStimulus<DUT, V>(),
        new BasicSanitizedMemStimulus<DUT>()
    ), n_transactions(n_transactions), edge_case(edge_case) {}
    virtual void setup(std::mt19937& rng) override {
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(rng);

        this->keyMgr->secrets[secret_id] = key;
        for (uint64_t i = 0; i < n_transactions; i++) {
            uint8_t axi_id = i & 0xF;
            auto cap_data = this->test_librust_random_edge_case_cap(rng, secret_id, edge_case);
            auto axi_params = cap_data.maybe_valid_transfer_params(32, 20);
            if (cap_data.perms & CCapPerms_Read) {
                this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
            }
            if (cap_data.perms & CCapPerms_Write) {
                this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
            }
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        if (final_tick == 0) {
            if (this->awInputs.empty() && this->wInputs.empty() && this->arInputs.empty()) {
                // Give 10000 cycles of buffer
                final_tick = tick + 100000;
            }
            return false;
        } else {
            return tick >= final_tick;
        }
    }
};



#undef POP_OUTPUT
#undef PEEK_OUTPUT
#undef CANPEEK_OUTPUT
#undef NOPUT_INPUT
#undef PUT_INPUT
#undef CANPUT_INPUT

template<class TheDUT, CapType ctype, KeyMngrVersion V>
constexpr std::vector<TestBase*> basicExposerUvmTests(bool expectPassthroughInvalidTransactions) {
    std::vector<TestBase*> tests = {
        // UVM-style testing
        // TODO add tests for above todos, consider revocation
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, ctype, V>(CCapPerms_Read),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, ctype, V>(CCapPerms_Write),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, ctype, V>(CCapPerms_ReadWrite),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapOOBAccess<TheDUT, ctype, V>(CCapPerms_Read),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapOOBAccess<TheDUT, ctype, V>(CCapPerms_Write),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidCapBadPerms<TheDUT, ctype, V>(),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyBadSigCap<TheDUT, ctype, V>(),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMInvalidKeyAccess<TheDUT, ctype, V>(CCapPerms_ReadWrite),
            expectPassthroughInvalidTransactions
        ),

        new ExposerUVMishTest(
            new UVMStreamOfNValidTransactions<TheDUT, ctype, V>(
                CCapPerms_ReadWrite,
                /* n_transactions */ 100,
                /* n_data_flits_per_transaction */ 1
            ),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNValidTransactions<TheDUT, ctype, V>(
                CCapPerms_ReadWrite,
                /* n_transactions */ 100,
                /* n_data_flits_per_transaction */ 4
            ),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNValidTransactions<TheDUT, ctype, V>(
                CCapPerms_ReadWrite,
                /* n_transactions */ 100,
                /* n_data_flits_per_transaction */ 8
            ),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNValidTransactions<TheDUT, ctype, V>(
                CCapPerms_ReadWrite,
                /* n_transactions */ 100,
                /* n_data_flits_per_transaction */ 16
            ),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNValidTransactions<TheDUT, ctype, V>(
                CCapPerms_ReadWrite,
                /* n_transactions */ 100,
                /* n_data_flits_per_transaction */ 32
            ),
            expectPassthroughInvalidTransactions
        ),

        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000),
            expectPassthroughInvalidTransactions
        ),

        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000, /* n_cavs */ 0),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000, /* n_cavs */ 1),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000, /* n_cavs */ 2),
            expectPassthroughInvalidTransactions
        ),
    };

    if constexpr (V == KeyMngrV1) {
        tests.push_back(
            // 5 cycles of revocations
            // TODO test this with valid and invalid transactions!
            // TODO figure out how to do more accurate UVM revocation testing
            new ExposerUVMishTest(
                new UVMTransactionsBetweenRevocations_KeyMngrV1<TheDUT, ctype>(5),
                expectPassthroughInvalidTransactions
            )
        );
    }

    for (auto edge_case = 0; edge_case < ccap2024_11_rand_edge_case_num(); edge_case++) {
        tests.push_back(
            new ExposerUVMishTest(
                new UVMStreamOfNLibRustEdgeCaseTransactions<TheDUT, ctype, V>(10'000, edge_case),
                expectPassthroughInvalidTransactions
            )
        );
    }

    return tests;
}

#endif // EXPOSER_TESTS_UVM_H
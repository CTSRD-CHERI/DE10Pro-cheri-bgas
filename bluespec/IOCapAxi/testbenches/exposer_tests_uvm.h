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
#define PUT_INPUT(name, value) do { \
    dut.EN_## name ##_put = 1;      \
    dut. name ##_put_val = (value); \
    assert(dut.RDY_## name ##_put); \
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
#define WRITE_INPUT(name, value) do {      \
    dut.EN_## name ##___05Fwrite = 1;      \
    dut. name ##___05Fwrite_x = (value);   \
    assert(dut.RDY_## name ##___05Fwrite); \
} while(0);
#define NOWRITE_INPUT(name) \
    dut.EN_## name ##___05Fwrite = 0;

/**
 * Generic stimulus generator for the key manager parts of ShimmedExposer DUTs.
 * Dynamically reacts to the DUT outputs (e.g. requests for key data) to generate inputs
 * (e.g. responding with the requested key data).
 * Intended for use inside a ExposerStimulus.
 */
template<class DUT, KeyMngrVersion V>
class KeyManagerShimStimulus {
public:
    // The secrets stored in the key manager. TODO: Cannot change throughout the run.
    std::unordered_map<key_manager::KeyId, U128> secrets;

    KeyManagerShimStimulus() : secrets() {}
    // Assume the contents of `secrets` will not change, set up any pending transactions to populate the key store
    virtual void setup() {}
    // Has the internal key manager hardware been set up with the secrets?
    virtual bool isReady() {
        // By default, a fake key manager shim is always ready
        return true;
    }
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
    virtual void driveInputsForKeyMgr(DUT& dut, uint64_t tick) override {
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
    virtual void driveInputsForKeyMgr(DUT& dut, uint64_t tick) override {
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

template<class DUT>
class BasicKeyManagerShimStimulus<DUT, KeyMngrV2_AsDUT>: public KeyManagerShimStimulus<DUT, KeyMngrV2_AsDUT> {
    std::deque<std::pair<axi::AxiLite::AWFlit_addr13_user0, axi::AxiLite::WFlit_data32_user0>> pendingWrites;
    uint64_t pendingResponses;
public:
    BasicKeyManagerShimStimulus() : KeyManagerShimStimulus<DUT, KeyMngrV2_AsDUT>() {}
    virtual void setup() override {
        // Memory map:
// [0x0, 0x10, 0x20, 0x30, 0x40... 0x1000) = read/write key status
// [0x1000, 0x1010, 0x1020... 0x2000)      = write key values
// [0x1000, 0x1008, 0x1010, 0x1018]        = read performance counters
//                                           - good write
//                                           - bad write
//                                           - good read
//                                           - bad read
        for (auto& [key_id, key_val] : this->secrets) {
            auto key_data = key_val.stdify();
            const uint16_t KEY_STATUS_ADDR = 0 + (key_id * 16);
            const uint16_t KEY_DATA_ADDR = 0x1000 + (key_id * 16);
            // four writes of key data
            pendingWrites.push_back(std::make_pair(
                axi::AxiLite::AWFlit_addr13_user0 {
                    .awprot=0,
                    .awaddr=uint16_t(KEY_DATA_ADDR + 0)
                },
                axi::AxiLite::WFlit_data32_user0 {
                    .wstrb=0b1111,
                    .wdata=key_data[0],
                }
            ));
            pendingWrites.push_back(std::make_pair(
                axi::AxiLite::AWFlit_addr13_user0 {
                    .awprot=0,
                    .awaddr=uint16_t(KEY_DATA_ADDR + 4)
                },
                axi::AxiLite::WFlit_data32_user0 {
                    .wstrb=0b1111,
                    .wdata=key_data[1],
                }
            ));
            pendingWrites.push_back(std::make_pair(
                axi::AxiLite::AWFlit_addr13_user0 {
                    .awprot=0,
                    .awaddr=uint16_t(KEY_DATA_ADDR + 8)
                },
                axi::AxiLite::WFlit_data32_user0 {
                    .wstrb=0b1111,
                    .wdata=key_data[2],
                }
            ));
            pendingWrites.push_back(std::make_pair(
                axi::AxiLite::AWFlit_addr13_user0 {
                    .awprot=0,
                    .awaddr=uint16_t(KEY_DATA_ADDR + 12)
                },
                axi::AxiLite::WFlit_data32_user0 {
                    .wstrb=0b1111,
                    .wdata=key_data[3],
                }
            ));
            // one write of key status
            pendingWrites.push_back(std::make_pair(
                axi::AxiLite::AWFlit_addr13_user0 {
                    .awprot=0,
                    .awaddr=KEY_STATUS_ADDR,
                },
                axi::AxiLite::WFlit_data32_user0 {
                    .wstrb=0b1111,
                    .wdata=1, // keyvalid
                }
            ));
        }
        pendingResponses = pendingWrites.size();
    }
    virtual bool isReady() override {
        return pendingResponses == 0;
    }
    virtual void driveInputsForKeyMgr(DUT& dut, uint64_t tick) override {
        if (!pendingWrites.empty() && CANPUT_INPUT(keyStore_aw) && CANPUT_INPUT(keyStore_w)) {
            auto [awFlit, wFlit] = pendingWrites.front();

            PUT_INPUT(keyStore_aw, awFlit.pack());
            PUT_INPUT(keyStore_w, wFlit.pack());
            
            pendingWrites.pop_front();
        } else {
            NOPUT_INPUT(keyStore_aw);
            NOPUT_INPUT(keyStore_w);
        }

        if (CANPEEK_OUTPUT(keyStore_b)) {
            char bFlitPacked;
            POP_OUTPUT(keyStore_b, bFlitPacked);
            auto bFlit = axi::AxiLite::BFlit_user0::unpack(bFlitPacked);
            pendingResponses--;
            if (bFlit.bresp != 0) {
                throw test_failure("KeyStore Write returned error");
            }
        } else {
            NOPOP_OUTPUT(keyStore_b);
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
                PUT_INPUT(exposer4x32_sanitizedOut_b, bInput.pack());
                b_throughput.trackAccepted();
                bInputs.pop_front();
            }
            b_throughput.trackCycleWithAvailableInput();
        }

        NOPUT_INPUT(exposer4x32_sanitizedOut_r);
        if (!rInputs.empty()) {
            if (CANPUT_INPUT(exposer4x32_sanitizedOut_r)) {
                auto rInput = rInputs.front();
                PUT_INPUT(exposer4x32_sanitizedOut_r, rInput.pack());
                r_throughput.trackAccepted();
                rInputs.pop_front();
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

            unexpectedWFlits.pop_front();
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
    // Make different stimuli set their ticks relative to the first tick where the test could actually start
    // Initially a number that's far too big, but not near the 64-bit limit, so that it can be used as a proxy while the key manager is still being inited.
    // Even while the key manager is being inited, firstTickWhereKeyMgrReady + x will always be a long way in the future, but will never overflow uint64_t
    uint64_t firstTickWhereKeyMgrReady = 4'000'000'000'000;
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
    ValidCapWithRange<ctype> test_librust_random_valid_cap(std::mt19937& rng, uint32_t secret_id, int n_cavs=-1, std::optional<CCapPerms> perms = std::nullopt){
        CCapU128 secret_key;
        keyMgr->secrets[secret_id].to_le(secret_key);
        CCapPerms* perms_ptr = (perms.has_value()) ? &perms.value() : nullptr;
        return ValidCapWithRange(CapStruct<ctype>::librust_rand_valid_cap(rng, &secret_key, &secret_id, perms_ptr, n_cavs));
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
    virtual void setup(std::mt19937& rng) override {
        keyMgr->setup();
    }
    virtual void driveInputsForTick(std::mt19937& rng, DUT& dut, uint64_t tick) override {
        if (!keyMgr->isReady()) {
            keyMgr->driveInputsForKeyMgr(dut, tick);
            return;
        } else if (firstTickWhereKeyMgrReady == 4'000'000'000'000) {
            firstTickWhereKeyMgrReady = tick;
        }

        keyMgr->driveInputsForKeyMgr(dut, tick);
        sanitizedMem->driveBAndRInputs(dut, tick);

        NOPUT_INPUT(exposer4x32_iocapsIn_axiSignals_aw);
        if (!awInputs.empty()) {
            if (CANPUT_INPUT(exposer4x32_iocapsIn_axiSignals_aw)) {
                auto awInput = awInputs.front();
                PUT_INPUT(exposer4x32_iocapsIn_axiSignals_aw, verilate_array(awInput.pack()));
                aw_throughput.trackAccepted();
                awInputs.pop_front();
            }
            aw_throughput.trackCycleWithAvailableInput();
        }

        NOPUT_INPUT(exposer4x32_iocapsIn_axiSignals_w);
        if (!wInputs.empty()) {
            if (CANPUT_INPUT(exposer4x32_iocapsIn_axiSignals_w)) {
                auto wInput = wInputs.front();
                PUT_INPUT(exposer4x32_iocapsIn_axiSignals_w, wInput.pack());
                w_throughput.trackAccepted();
                wInputs.pop_front();
            }
            w_throughput.trackCycleWithAvailableInput();
        }

        NOPUT_INPUT(exposer4x32_iocapsIn_axiSignals_ar);
        if (!arInputs.empty()) {
            if (CANPUT_INPUT(exposer4x32_iocapsIn_axiSignals_ar)) {
                auto arInput = arInputs.front();
                PUT_INPUT(exposer4x32_iocapsIn_axiSignals_ar, verilate_array(arInput.pack()));
                ar_throughput.trackAccepted();
                arInputs.pop_front();
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

template<class AddrFlit, class DataFlit>
struct AxiTxn {
    key_manager::KeyId keyId;
    bool valid;
    // Can only be true if valid
    bool addrForwardedDownstream;
    // Can only be true if valid && addrForwardedDownstream && (nDataFlitsForwarded == nDataFlits)
    // - In the write case, B flit has to be sent downstream->upstream after all W flits sent upstream->downstream
    // - In the read case, final R flit has to be sent downstream->upstream after all other R flits sent downstream->upstream
    bool completionArrivedFromDownstream;
    // Can only be true if !valid || (valid && addrForwardedDownstream && (nDataFlitsForwarded == nDataFlits))
    bool completionSentUpstream;

    // Number of data flits we expect to see for this transaction
    // If the txn is invalid and a read, this should be one - there's exactly one flit that needs forwarding
    uint64_t nDataFlits;
    uint64_t nDataFlitsForwarded;

    // tick_initiated = the tick on which the final addr flit arrived from upstream and was put into the DUT
    LatencyTracked<AddrFlit> upstreamAddr;
    // tick_initiated = the tick on which each data flit arrived from (upstream if write else downstream) and was put into the DUT
    std::vector<LatencyTracked<DataFlit>> data;

    AxiTxn(key_manager::KeyId, bool valid, uint64_t nDataFlits, LatencyTracked<AddrFlit> upstreamAddr) :
        keyId(keyId),
        valid(valid),
        addrForwardedDownstream(false),
        completionArrivedFromDownstream(false),
        completionSentUpstream(false),
        nDataFlits(nDataFlits), nDataFlitsForwarded(0),
        upstreamAddr(upstreamAddr), data() {}

    bool hasAllDataFlits() const {
        return data.size() == nDataFlits;
    }
    void pushDataFlit(LatencyTracked<DataFlit> flit) {
        if (hasAllDataFlits()) {
            throw std::runtime_error("tried to add too many data flits to an AxiTxn");
        }
        data.push_back(flit);
    }
};

template <class AddrFlit, class DataFlit> class fmt::formatter<AxiTxn<AddrFlit, DataFlit>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (AxiTxn<AddrFlit, DataFlit> const& x, Context& ctx) const {
        return format_to(ctx.out(), "{{ .keyId = {}, .valid = {}, .addrFwdedDownstream = {}, .completionArrivedDownstream = {}, .completionSentUpstream = {}, nDataFlits = {}, .nDataFlitsFwded = {}, .upstreamAddr = {}, .data = <{}>{} }}",
            x.keyId,
            x.valid,
            x.addrForwardedDownstream,
            x.completionArrivedFromDownstream,
            x.completionSentUpstream,
            x.nDataFlits,
            x.nDataFlitsForwarded,
            x.upstreamAddr,
            x.data.size(),
            x.data
        );
    }
};

template<class AddrFlit, class DataFlit>
class TxnPerKeyScoreboard {
public:
    std::deque<AxiTxn<AddrFlit, DataFlit>> txns;
    // Data flits from upstream that aren't attached to any txn yet
    // Can only be from upstream, because downstream can't send packets unsolicited
    std::deque<LatencyTracked<DataFlit>> dataFlitsPendingTxn;

    void cleanupTxns(bool isRead) {
        while (!txns.empty()) {
            auto& txn = txns.front();
            if (!txn.completionSentUpstream) {
                break;
            }

            // txn.completionSentUpstream = true

            if (txn.valid) {
                // we can assume txn.nDataFlits == txn.nDataFlitsForwarded == txn.data.size()
                txns.pop_front();
            }
            // For invalid txns we can't make that assumption - we have to check
            // txn.data.size() == txn.nDataFlits
            // in case e.g. a write transaction sent the completion upstream early when they knew it was bad,
            // even though the data flits from upstream hadn't been captured.
            // We can delete it ONLY if we have captured all the data flits we need.
            if (!txn.valid) {
                bool canPop = isRead ? true : (txn.data.size() == txn.nDataFlits);
                if (canPop) {
                    txns.pop_front();
                } else {
                    break;
                }
            }
        }
    }

public:
    TxnPerKeyScoreboard() : txns(), dataFlitsPendingTxn() {}

    void recvDataFlit(LatencyTracked<DataFlit> flit, bool isRead) {
        for (auto& txn : txns) {
            if (!txn.hasAllDataFlits()) {
                txn.pushDataFlit(flit);
                // If the txn is invalid and has already sent a completion upstream, adding a flit
                // can cause it to be completely finished and popped off.
                cleanupTxns(isRead);
                return;
            }
        }
        dataFlitsPendingTxn.push_back(flit);
    }
    void pushUnconfirmedTxn(key_manager::KeyId keyId, bool valid, uint64_t nDataFlits, LatencyTracked<AddrFlit> upstreamAddr) {
        auto txn = AxiTxn<AddrFlit, DataFlit>(keyId, valid, nDataFlits, upstreamAddr);
        while (!dataFlitsPendingTxn.empty() && !txn.hasAllDataFlits()) {
            txn.pushDataFlit(dataFlitsPendingTxn.front());
            dataFlitsPendingTxn.pop_front();
        }
        txns.push_back(txn);
    }
    void checkAndFwdAddrFlit(std::function<void(LatencyTracked<AddrFlit>&)> checkAddrFlit) {
        for (auto& txn : txns) {
            // Skip all txns that don't need to or already have forwarded their address
            if (txn.addrForwardedDownstream || !txn.valid) {
                continue;
            }
            checkAddrFlit(txn.upstreamAddr);
            txn.addrForwardedDownstream = true;
            return;
        }
        throw test_failure("Forwarded unexpected addr flit:\nno valid txns waiting to forward");
    }
    // When you see a data flit getting forwarded (either from upstream->downstream for writes, or downstream->upstream for reads)
    void checkAndFwdDataFlit_validBypassInvalid(std::function<void(LatencyTracked<DataFlit>&)> checkDataFlit) {
        for (auto& txn : txns) {
            // Skip all txns that don't need to or already have forwarded all data flits
            if (txn.nDataFlitsForwarded == txn.nDataFlits) {
                continue;
            }
            if (!txn.valid) {
                // Even if there are invalid writes ahead of a valid write, we are allowed to forward the data for the valid write.
                continue;
            }

            if (!txn.addrForwardedDownstream) {
                // May not be an AXI error in all cases - e.g. write data can be forwarded before the aw - but in practice this shouldn't happen
                throw test_failure("Forwarded unexpected data flit:\nnext txn hadn't forwarded address yet");
            }
            if (txn.data.size() <= txn.nDataFlitsForwarded) {
                throw test_failure("Forwarded unexpected data flit:\nnext txn didn't have a new data flit to confirm");
            }
            checkDataFlit(txn.data[txn.nDataFlitsForwarded]);
            txn.nDataFlitsForwarded += 1;

            return;
        }
        throw test_failure("Forwarded unexpected data flit:\nno txns with pending data");
    }
    // For reads, where read responses for valid txns can't bypass invalid ones under the same txnid.
    // The callback can receive nullopt when the next transaction is invalid so should return an error response
    void checkAndFwdDataFlit_noValidBypassInvalid(std::function<void(LatencyTracked<DataFlit>*)> checkDataFlit) {
        for (auto& txn : txns) {
            // Skip all txns that don't need to or already have forwarded all data flits
            if (txn.nDataFlitsForwarded == txn.nDataFlits) {
                continue;
            }

            if (txn.valid) {
                if (!txn.addrForwardedDownstream) {
                    // May not be an AXI error in all cases - e.g. write data can be forwarded before the aw - but in practice this shouldn't happen
                    throw test_failure("Forwarded unexpected data flit:\nnext txn hadn't forwarded address yet");
                }
                if (txn.data.size() <= txn.nDataFlitsForwarded) {
                    throw test_failure("Forwarded unexpected data flit:\nnext txn didn't have a new data flit to confirm");
                }
                checkDataFlit(&txn.data[txn.nDataFlitsForwarded]);
                txn.nDataFlitsForwarded += 1;
                // fmt::println(stderr, "checkAndFwdDataFlit {} {}", txn.nDataFlitsForwarded, txn.data[txn.nDataFlitsForwarded - 1]);
            } else {
                // We should forward exactly one data - the completion
                checkDataFlit(nullptr);
                txn.nDataFlitsForwarded += 1;
                // fmt::println(stderr, "checkAndFwdDataFlit {} nullptr", txn.nDataFlitsForwarded);
            }

            return;
        }
        throw test_failure("Forwarded unexpected data flit:\nno txns with pending data");
    }
    void checkCompletionFlitFromDownstream(bool isRead) {
        for (auto& txn : txns) {
            // Skip all txns that don't need or already have a completion from downstream
            if (!txn.valid || txn.completionArrivedFromDownstream) {
                continue;
            }
            if (!txn.addrForwardedDownstream) {
                throw test_failure("Received unexpected completion from downstream:\nthe next valid txn hasn't forwarded the address flit yet");
            }
            if (!isRead && txn.nDataFlitsForwarded < txn.nDataFlits) {
                // In the write case, B flit can only arrive from downstream after all W flits forwarded upstream->downstream
                // In the read case, final R flit may arrive before other flits are forwarded downstream->upstream
                throw test_failure("Recevied unexpected completion from downstream:\nthe next valid txn hasn't forwarded all data flits yet");
            }
            txn.completionArrivedFromDownstream = true;
            return;
        }
        throw test_failure("Recieved unexpected completion from downstream:\nthere aren't any valid txns awaiting such");
    }
    void checkCompletionFlitSentUpstream(std::function<void(AxiTxn<AddrFlit, DataFlit>&)> checkTxn, bool isRead) {
        // fmt::println(stderr, "checkCompletionFlitSentUpstream {}", txns);
        for (auto& txn : txns) {
            // Skip any txns which have already sent completions upstream
            if (txn.completionSentUpstream) {
                continue;
            }
            if (txn.valid && !txn.completionArrivedFromDownstream) {
                throw test_failure("Sent unexpected completion upstream:\nthe next transaction is valid + hasn't received a completion from downstream");
            }
            if (isRead && txn.nDataFlitsForwarded < txn.nDataFlits) {
                // In the write case, B flit may be forwarded upstream before all W flits have arrived from upstream- (TODO IS THIS TRUE??)
                // In the read case, final R flit must be forwarded upstreama after other flits are forwarded upstream
                throw test_failure("Sent unexpected completion upstream:\nthe next valid txn hasn't forwarded all data flits yet");
            }
            checkTxn(txn);
            txn.completionSentUpstream = true;
            // We have completed a txn, we might need to pop it off...
            cleanupTxns(isRead);
            return;
        }
        throw test_failure("Sent unexpected completion upstream:\nthere aren't any uncompleted txns");
    }
    void invalidateFromKey(key_manager::KeyId keyId, bool isRead) {
        for (auto& txn : txns) {
            if (txn.keyId == keyId && txn.valid && !txn.addrForwardedDownstream) {
                txn.valid = false;
                if (isRead) {
                    txn.nDataFlits = 1;
                }
            } 
        }
        cleanupTxns(isRead); // TODO maybe this won't ever do anything...
    }
    bool empty() const {
        return txns.empty() && dataFlitsPendingTxn.empty();
    }
};

template <class AddrFlit, class DataFlit> class fmt::formatter<TxnPerKeyScoreboard<AddrFlit, DataFlit>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (TxnPerKeyScoreboard<AddrFlit, DataFlit> const& x, Context& ctx) const {
        return format_to(ctx.out(), "\n\t{{ .txns = {}, .dataFlitsPendingTxn = {} }}\n", x.txns, x.dataFlitsPendingTxn);
    }
};

class WriteTxnScoreboard {
    using AddrFlit = axi::SanitizedAxi::AWFlit_id4_addr64_user0;
    using DataFlit = axi::SanitizedAxi::WFlit_data32;

public:
    std::array<TxnPerKeyScoreboard<AddrFlit, DataFlit>, 16> txnIdBoards;
    // (txnId, n-data-flits-expected)
    std::deque<std::pair<uint8_t, uint64_t>> expectedIncomingDataFlitTarget;
    // (txnId, n-data-flits-expected)
    std::deque<std::pair<uint8_t, uint64_t>> expectedOutgoingDataFlitTarget;

    // tick_initiated = the tick on which the flit arrived from downstream
    // tick_completed = the tick on which the flit exits to upstream
    // only contains responses from confirmed-valid transactions (the only resps to be passed through from downstream->upstream), not error ones.
    std::array<std::deque<LatencyTracked<axi::IOCapAxi::BFlit_id4>>, 16> expectedBFromDownstream;
    // tick_initiated = the tick on which the flit arrived from upstream
    // Flits which arrived before their AW was processed, so we don't know what txnid they have.
    // This can happen for e.g. short transactions with 1 data-flit per.
    std::deque<LatencyTracked<DataFlit>> dataFlitsPendingTxnId;

    std::vector<uint64_t> aw_aw_latency;
    // This is meaningless - it depends on bottlenecks
    // std::vector<uint64_t> aw_b_latency;
    std::vector<uint64_t> w_w_latency;
    std::vector<uint64_t> b_b_latency;

    void expectIncomingDataFlits(uint8_t txnId, uint64_t nDataFlits) {
        expectedIncomingDataFlitTarget.push_back({
            txnId, nDataFlits
        });
        while (!dataFlitsPendingTxnId.empty() && !expectedIncomingDataFlitTarget.empty()) {
            auto dFlit = dataFlitsPendingTxnId.front();
            auto& [txnId, nDataFlits] = expectedIncomingDataFlitTarget.front();
            // fmt::println(stderr, "recvW {} {} {}", nDataFlits, txnId, flit);
            txnIdBoards[txnId].recvDataFlit(dFlit, /* isRead */ false);
            nDataFlits -= 1;
            if (nDataFlits == 0) {
                expectedIncomingDataFlitTarget.pop_front();
            }
            dataFlitsPendingTxnId.pop_front();
        }
    }
    void recvWFlitFromUpstream(uint64_t tick, axi::IOCapAxi::WFlit_data32& flit) {
        LatencyTracked<DataFlit> dFlit = {
            tick,
            axi::SanitizedAxi::WFlit_data32 {
                .wlast = flit.wlast,
                .wstrb = flit.wstrb,
                .wdata = flit.wdata
            }
        };
        if (expectedIncomingDataFlitTarget.empty()) {
            dataFlitsPendingTxnId.push_back(dFlit);
        } else {
            auto& [txnId, nDataFlits] = expectedIncomingDataFlitTarget.front();
            // fmt::println(stderr, "recvW {} {} {}", nDataFlits, txnId, flit);
            txnIdBoards[txnId].recvDataFlit(dFlit, /* isRead */ false);
            nDataFlits -= 1;
            if (nDataFlits == 0) {
                expectedIncomingDataFlitTarget.pop_front();
            }
        }
    }
    void pushUnconfirmedTxn(uint8_t txnId, key_manager::KeyId keyId, bool valid, uint64_t nDataFlits, LatencyTracked<AddrFlit> upstreamAddr) {
        txnIdBoards[txnId].pushUnconfirmedTxn(keyId, valid, nDataFlits, upstreamAddr);
    }
    void checkAndFwdAwFlit(uint64_t tick, AddrFlit& awFlit) {   
        try {
            txnIdBoards[awFlit.awid].checkAndFwdAddrFlit([&](auto& expected){
                if (awFlit != expected.value) {
                    throw test_failure(fmt::format("Forwarded unexpected aw flit:\nexpected: {}", expected));
                }
                aw_aw_latency.push_back(tick - expected.tick_initiated);
            });
        } catch (test_failure& e) {
            throw test_failure(fmt::format("{}\ngot: {}\n", e.what(), awFlit));
        }
        
        expectedOutgoingDataFlitTarget.push_back({
            awFlit.awid, axi::len_to_n_transfers(awFlit.awlen)
        });
    }
    void checkAndFwdWFlit(uint64_t tick, DataFlit& wFlit) {
        try {
            if (expectedOutgoingDataFlitTarget.empty()) {
                throw test_failure("Forwarding unexpected w flit:\nno addr flit to associate with");
            }
            auto& [txnId, nDataFlits] = expectedOutgoingDataFlitTarget.front();

            txnIdBoards[txnId].checkAndFwdDataFlit_validBypassInvalid([&](auto expected) {
                if (wFlit != expected.value) {
                    throw test_failure(fmt::format("Forwarding unexpected w flit:\nexpected: {}", expected.value));
                }
                // TODO: note that this is the latency of entering the pipe e.g. the latency from the input port being *ready* to it coming out the other end.
                // Not the latency from the input becoming available.
                w_w_latency.push_back(tick - expected.tick_initiated);
            });

            nDataFlits -= 1;
            if (nDataFlits == 0) {
                expectedOutgoingDataFlitTarget.pop_front();
            }
        } catch (test_failure& e) {
            throw test_failure(fmt::format("{}\ngot: {}\ntxns: {}\n", e.what(), wFlit, txnIdBoards));
        }

    }
    void checkBFlitFromDownstream(uint64_t tick, axi::SanitizedAxi::BFlit_id4& bFlit) {
        try{
            txnIdBoards[bFlit.bid].checkCompletionFlitFromDownstream(/* isRead */ false);
        } catch (test_failure& e) {
            throw test_failure(fmt::format("{}\ngot: {}\ntxns: {}\n", e.what(), bFlit, txnIdBoards));
        }
        expectedBFromDownstream[bFlit.bid].push_back({
            tick, 
            axi::IOCapAxi::BFlit_id4 {
                .bresp = bFlit.bresp,
                .bid = bFlit.bid
            }
        });
    }
    void checkBFlitSentUpstream(uint64_t tick, axi::IOCapAxi::BFlit_id4& bFlit) {
        try {
            txnIdBoards[bFlit.bid].checkCompletionFlitSentUpstream([&](auto& expectedTxn) {
                if (expectedTxn.valid) {
                    auto expectedFlit = expectedBFromDownstream[bFlit.bid].front();
                    
                    if (bFlit != expectedFlit.value) {
                        throw test_failure(fmt::format("Sent unexpected b flit upstream:\nexpected: {}", expectedFlit.value));
                    }

                    b_b_latency.push_back(tick - expectedFlit.tick_initiated);

                    expectedBFromDownstream[bFlit.bid].pop_front();
                } else {
                    auto expectedFlit = axi::IOCapAxi::BFlit_id4 {
                        .bresp = (uint8_t)axi::AXI4_Resp::SlvErr,
                        .bid = bFlit.bid
                    };
                    if (bFlit != expectedFlit) {
                        throw test_failure(fmt::format("Sent unexpected b flit upstream:\nexpected: {}", expectedFlit));
                    }
                }
            }, /* isRead */ false);
        } catch (test_failure& e) {
            throw test_failure(fmt::format("{}\ngot: {}\ntxns: {}\n", e.what(), bFlit, txnIdBoards));
        }
    }
    void invalidateFromKey(key_manager::KeyId keyId) {
        for (auto& board : txnIdBoards) {
            board.invalidateFromKey(keyId, /* isRead */ false);
        }
    }
    bool empty() const {
        if (std::any_of(this->txnIdBoards.begin(), this->txnIdBoards.end(), [](auto& board) { return !board.empty(); })) {
            return false;
        }
        if (std::any_of(this->expectedBFromDownstream.begin(), this->expectedBFromDownstream.end(), [](auto& stream) { return !stream.empty(); })) {
            return false;
        }
        return expectedIncomingDataFlitTarget.empty() && expectedOutgoingDataFlitTarget.empty();
    }
};
    
template <> class fmt::formatter<WriteTxnScoreboard> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (WriteTxnScoreboard const& x, Context& ctx) const {
        return format_to(ctx.out(), "{{ {}, .expectedBFromDownstream = {}, .expectedIncoming = {}, .expectedOutgoing = {} }}", x.txnIdBoards, x.expectedBFromDownstream, x.expectedIncomingDataFlitTarget, x.expectedOutgoingDataFlitTarget);
    }
};

class ReadTxnScoreboard {
    using AddrFlit = axi::SanitizedAxi::ARFlit_id4_addr64_user0;
    using DataFlit = axi::IOCapAxi::RFlit_id4_data32;

public:
    std::array<TxnPerKeyScoreboard<AddrFlit, DataFlit>, 16> txnIdBoards;

    std::vector<uint64_t> ar_ar_latency;
    // This is meaningless - it's affected by transaction-to-transaction dependencies
    // std::vector<uint64_t> ar_r_latency;
    std::vector<uint64_t> r_r_latency;

    void pushUnconfirmedTxn(uint8_t txnId, key_manager::KeyId keyId, bool valid, uint64_t nDataFlits, LatencyTracked<AddrFlit> upstreamAddr) {
        if (!valid) {
            // invalid txns need to forward exactly one flit - the error confirmation
            nDataFlits = 1;
        }
        txnIdBoards[txnId].pushUnconfirmedTxn(keyId, valid, nDataFlits, upstreamAddr);
    }
    void checkAndFwdArFlit(uint64_t tick, AddrFlit& arFlit) {
        try {
            txnIdBoards[arFlit.arid].checkAndFwdAddrFlit([&](auto& expected) {
                if (arFlit != expected.value) {
                    throw test_failure(fmt::format("Forwarded unexpected ar flit:\nexpected: {}", expected.value));
                }
                ar_ar_latency.push_back(tick - expected.tick_initiated);
            });
        } catch (test_failure& e) {
            throw test_failure(fmt::format("{}\ngot: {}\ntxns: {}\n", e.what(), arFlit, txnIdBoards));
        }
    }
    void recvRFlitFromDownstream(uint64_t tick, axi::SanitizedAxi::RFlit_id4_data32& rFlit) {
        try {
            txnIdBoards[rFlit.rid].recvDataFlit({
                tick,
                axi::IOCapAxi::RFlit_id4_data32 {
                    .rlast = rFlit.rlast,
                    .rresp = rFlit.rresp,
                    .rdata = rFlit.rdata,
                    .rid = rFlit.rid
                }
            }, /* isRead */ true);
            if (rFlit.rlast) {
                txnIdBoards[rFlit.rid].checkCompletionFlitFromDownstream(/* isRead */ true);
            }
        } catch (test_failure& e) {
            throw test_failure(fmt::format("{}\ngot: {}\ntxns: {}\n", e.what(), rFlit, txnIdBoards));
        }
    }
    void checkAndFwdRFlit(uint64_t tick, DataFlit& rFlit) {
        try {
            txnIdBoards[rFlit.rid].checkAndFwdDataFlit_noValidBypassInvalid([&](auto nullablePtrToExpected) {
                if (nullablePtrToExpected) {
                    auto& expectedFlit = *nullablePtrToExpected;
                    
                    if (rFlit != expectedFlit.value) {
                        throw test_failure(fmt::format("Sent unexpected r flit upstream:\nexpected: {}", expectedFlit.value));
                    }

                    r_r_latency.push_back(tick - expectedFlit.tick_initiated);
                } else {
                    // expected an error flit
                    auto expectedFlit = axi::IOCapAxi::RFlit_id4_data32 {
                        .rlast = 1,
                        .rresp = (uint8_t)axi::AXI4_Resp::SlvErr,
                        .rdata = 0xaaaaaaaa, // Bluespec uses this to mean ?
                        .rid = rFlit.rid,
                    };
                    if (rFlit != expectedFlit) {
                        throw test_failure(fmt::format("Sent unexpected r flit upstream:\nexpected: {}", expectedFlit));
                    }
                }
            });
            if (rFlit.rlast) {
                // We've already checked if expectedTxn is valid or not, if the rFlit didn't match the txn validity then the above checkAndFwdDataFlit call wouldn't have succeeded
                txnIdBoards[rFlit.rid].checkCompletionFlitSentUpstream([&](auto& expectedTxn) {}, /* isRead */ true);
            }
        } catch (test_failure& e) {
            throw test_failure(fmt::format("{}\ngot: {}\ntxns: {}\n", e.what(), rFlit, txnIdBoards));
        }
    }
    void invalidateFromKey(key_manager::KeyId keyId) {
        for (auto& board : txnIdBoards) {
            board.invalidateFromKey(keyId, /* isRead */ true);
        }
    }
    bool empty() const {
        if (std::any_of(this->txnIdBoards.begin(), this->txnIdBoards.end(), [](auto& board) { return !board.empty(); })) {
            return false;
        }
        return true;
    }
};

template <> class fmt::formatter<ReadTxnScoreboard> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (ReadTxnScoreboard const& x, Context& ctx) const {
        return format_to(ctx.out(), "{}", x.txnIdBoards);
    }
};

/**
 * Scoreboard for ShimmedExposer tests.
 * Can be instantiated directly or subclassed.
 * Does not do anything to handle revocation, but revocation can be simulated by modifying the KeyID -> Key map `secrets` the scoreboard uses
 * to determine if incoming requests will be valid or not.
 * 
 * By AXI convention, Upstream = the Manager who speaks IOCapAxi, Downstream = the Subordinate who speaks plain AXI.
 */
template<class DUT, CapType ctype, KeyMngrVersion V>
class BaseExposerScoreboard : public Scoreboard<DUT> {
protected:
    std::unordered_map<key_manager::KeyId, U128>& secrets; // Fake keymanager proxy. THIS SCOREBOARD ASSUMES KEYS DONT CHANGE

    // Early versions of the exposer will always pass transactions through, even if it later registers them as "invalid" with the performance counters.
    bool expectPassthroughInvalidTransactions;

    std::vector<axi::IOCapAxi::AWFlit_id4_addr64_user3> awInProgress;
    WriteTxnScoreboard wTxns;

    std::vector<axi::IOCapAxi::ARFlit_id4_addr64_user3> arInProgress;
    ReadTxnScoreboard rTxns;

    // We can't understand what should be good/bad ahead of time, because txns could be cancelled while still in progress.
    // Monitor the actual outputs of the exposer, which are checked by the read/write txnscoreboards, and check the performance counters against *those*.
    // NOTE: this means if a valid txn stalls out, it won't be "confirmed" and therefore will show up as "expected bad" instead of "expected good"
    uint64_t totalWriteTxns;
    uint64_t totalReadTxns;
    // Confirmed i.e. were valid at the point they passed out of the exposed.
    // signalledGoodWrite should match this.
    uint64_t confirmedWriteTxns;
    uint64_t confirmedReadTxns;
    // expectedBad{Read,Write} = total{Read,Write} - confirmed{Read,Write}

    uint64_t signalledGoodWrite = 0;
    uint64_t signalledBadWrite = 0;
    uint64_t signalledGoodRead = 0;
    uint64_t signalledBadRead = 0;

    std::vector<ShimmedExposerInput<V>> inputs;
    std::vector<ShimmedExposerOutput<V>> outputs;

    void resolveAwFlit(uint64_t tick, std::optional<axi::IOCapAxi::AWFlit_id4_addr64_user3> newIncomingFlit) {
        if (newIncomingFlit) {
            awInProgress.push_back(newIncomingFlit.value());
            // No matter what, we're going to get some data flits in and they're going to be related to this txnId.
            // Call this early so we can process W flits coming in while handling the rest of the AW
            if (awInProgress.size() == 1) {
                uint8_t txnId = awInProgress[0].awid;
                uint64_t nDataFlits = axi::len_to_n_transfers(awInProgress[0].awlen);
                wTxns.expectIncomingDataFlits(txnId, nDataFlits);
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
            // The performance counters should reflect the validity of the capability/access in all cases.
            // We can't predict the goodness of a txn ahead of time - it could be cancelled after it comes through!
            totalWriteTxns++;
            // If the capability and ranges are valid,
            // expect an AW flit to come out *and* the right number of W flits!
            uint8_t txnId = awInProgress[0].awid;
            uint64_t nDataFlits = axi::len_to_n_transfers(awInProgress[0].awlen);
            wTxns.pushUnconfirmedTxn(
                txnId,
                secret_key_id,
                (capIsValid && rangeIsValid) || expectPassthroughInvalidTransactions, // valid
                nDataFlits,
                {
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
                }
            );

            // We've now handled the set of AW flits, and can drop them
            awInProgress.clear();
        }
    }

    void resolveArFlit(uint64_t tick, std::optional<axi::IOCapAxi::ARFlit_id4_addr64_user3> newIncomingFlit) {
        if (newIncomingFlit) {
            arInProgress.push_back(newIncomingFlit.value());
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
            // We can't predict the goodness of a txn ahead of time - it could be cancelled after it comes through!
            totalReadTxns++;
            bool isValid = (capIsValid && rangeIsValid) || expectPassthroughInvalidTransactions;
            // If the capability and ranges are valid, expect an AR flit to come out
            rTxns.pushUnconfirmedTxn(
                arInProgress[0].arid,
                secret_key_id,
                (capIsValid && rangeIsValid) || expectPassthroughInvalidTransactions, // valid
                axi::len_to_n_transfers(arInProgress[0].arlen),
                {
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
                }
            );

            // We've now handled the set of AR flits, and can drop them
            arInProgress.clear();
        }
    }

    virtual void monitorAndScoreKeyManager(DUT& dut, uint64_t tick, KeyMngrShimInput<V>& inputKeyManager, KeyMngrShimOutput<V>& outputKeyManager) = 0;

public:
    BaseExposerScoreboard(std::unordered_map<key_manager::KeyId, U128>& secrets, bool expectPassthroughInvalidTransactions = false) : secrets(secrets),
        expectPassthroughInvalidTransactions(expectPassthroughInvalidTransactions) {}
    virtual ~BaseExposerScoreboard() = default;
    // Should raise a test_failure on failure
    virtual void monitorAndScore(DUT& dut, uint64_t tick) {
        ShimmedExposerOutput<V> output{0};
        output.time = tick;
        pull_output(dut, output); // TODO apply backpressure?
        if (output.is_notable())
            outputs.push_back(output);

        if (output.clean_flit_aw) {
            confirmedWriteTxns++;
            wTxns.checkAndFwdAwFlit(tick, output.clean_flit_aw.value());
        }

        if (output.clean_flit_ar) {
            confirmedReadTxns++;
            rTxns.checkAndFwdArFlit(tick, output.clean_flit_ar.value());
        }

        if (output.clean_flit_w) {
            wTxns.checkAndFwdWFlit(tick, output.clean_flit_w.value());
        }

        if (output.iocap_flit_b) {
            wTxns.checkBFlitSentUpstream(tick, output.iocap_flit_b.value());
        }

        if (output.iocap_flit_r) {
            rTxns.checkAndFwdRFlit(tick, output.iocap_flit_r.value());
        }

        ShimmedExposerInput<V> input{0};
        input.time = tick;
        observe_input(dut, input);
        if (input.is_notable())
            inputs.push_back(input);

        // Add incoming B and R flits from the sanitized-side to the list of expected outputs
        if (input.clean_flit_b) {
            wTxns.checkBFlitFromDownstream(tick, input.clean_flit_b.value());
        }
        if (input.clean_flit_r) {
            rTxns.recvRFlitFromDownstream(tick, input.clean_flit_r.value());
        }

        // Resolve AW flits to see if there's a new valid/invalid burst
        // If invalid, it should return a B flit. If we received a staitized B flit with the same ID on the same cycle, that one comes first in the ordering because it must be the result of a valid AW on a previous cycle/came before.
        resolveAwFlit(tick, input.iocap_flit_aw);
        // Resolve AR flits to see if there's a new valid/invalid burst
        // Same as above for R flits
        resolveArFlit(tick, input.iocap_flit_ar);

        // Add incoming W flits from the IOCap-side to the list of expected output W flits, if possible.
        if (input.iocap_flit_w) {
            wTxns.recvWFlitFromUpstream(tick, input.iocap_flit_w.value());
        }

        this->monitorAndScoreKeyManager(dut, tick, input.keyManager, output.keyManager);
    }
    // Should raise a test_failure on failure
    virtual void endTest() override {
        // TODO log inputs and outputs
        if (
            !this->wTxns.empty() ||
            !this->rTxns.empty() ||
            (this->confirmedWriteTxns != this->signalledGoodWrite) ||
            (this->totalWriteTxns - this->confirmedWriteTxns != this->signalledBadWrite) ||
            (this->confirmedReadTxns != this->signalledGoodRead) ||
            (this->totalReadTxns - this->confirmedReadTxns != this->signalledBadRead)
        ) {
            throw test_failure(fmt::format(
                "BaseExposerScoreboard unexpected outcome:\n"
                "wTxns: {}\n"
                "rTxns: {}\n"
                "perf counters exp/act:\n"
                "good write {}/{}\n"
                "bad write {}/{}\n"
                "good read {}/{}\n"
                "bad read {}/{}\n"
                ,
                this->wTxns, this->rTxns,
                this->confirmedWriteTxns, this->signalledGoodWrite,
                this->totalWriteTxns - this->confirmedWriteTxns, this->signalledBadWrite,
                this->confirmedReadTxns, this->signalledGoodRead,
                this->totalReadTxns - this->confirmedReadTxns, this->signalledBadRead
            ));
        }
    }
    #define STRINGIFY(x) STRINGIFY2(x)
    #define STRINGIFY2(x) #x
    #define DUMP_MEAN_OF(name, x) fmt::println(stats, STRINGIFY(name) "_mean = {}", mean_of(x));
    virtual void dump_toml_stats(FILE* stats) override {
        DUMP_MEAN_OF(aw_aw_latency, wTxns.aw_aw_latency);
        DUMP_MEAN_OF(ar_ar_latency, rTxns.ar_ar_latency);
        DUMP_MEAN_OF(w_w_latency, wTxns.w_w_latency);
        DUMP_MEAN_OF(b_b_latency, wTxns.b_b_latency);
        DUMP_MEAN_OF(r_r_latency, rTxns.r_r_latency);
        fmt::println(stats, "total_write = {}", totalWriteTxns);
        fmt::println(stats, "confirmed_write = {}", confirmedWriteTxns);
        fmt::println(stats, "exp_invalid_write = {}", totalWriteTxns - confirmedWriteTxns);
        fmt::println(stats, "perf_valid_write = {}", signalledGoodWrite);
        fmt::println(stats, "perf_invalid_write = {}", signalledBadWrite);
        fmt::println(stats, "total_read = {}", totalReadTxns);
        fmt::println(stats, "confirmed_read = {}", confirmedReadTxns);
        fmt::println(stats, "exp_invalid_read = {}", totalReadTxns - confirmedReadTxns);
        fmt::println(stats, "perf_valid_read = {}", signalledGoodRead);
        fmt::println(stats, "perf_invalid_read = {}", signalledBadRead);
        fmt::println(stats, "valid_txn_ratio = {}", (double(confirmedWriteTxns + confirmedReadTxns))/(double(totalWriteTxns + totalReadTxns)));
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
            !this->wTxns.empty() ||
            !this->rTxns.empty() ||
            (this->confirmedWriteTxns != this->signalledGoodWrite) ||
            (this->totalWriteTxns - this->confirmedWriteTxns != this->signalledBadWrite) ||
            (this->confirmedReadTxns != this->signalledGoodRead) ||
            (this->totalReadTxns - this->confirmedReadTxns != this->signalledBadRead)
        ) {
            throw test_failure(fmt::format(
                "BaseExposerScoreboard unexpected outcome:\n"
                "epoch completions: {}\n"
                "wTxns: {}\n"
                "rTxns: {}\n"
                "perf counters exp/act:\n"
                "good write {}/{}\n"
                "bad write {}/{}\n"
                "good read {}/{}\n"
                "bad read {}/{}\n"
                ,
                this->wTxns, this->rTxns,
                this->confirmedWriteTxns, this->signalledGoodWrite,
                this->totalWriteTxns - this->confirmedWriteTxns, this->signalledBadWrite,
                this->confirmedReadTxns, this->signalledGoodRead,
                this->totalReadTxns - this->confirmedReadTxns, this->signalledBadRead
            ));
        }
    }
};

template<class DUT, CapType ctype>
class ExposerScoreboard<DUT, ctype, KeyMngrV2> : public BaseExposerScoreboard<DUT, ctype, KeyMngrV2> {
    // std::optional<key_manager::KeyId> killOnNextCycle;
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
        // if (killOnNextCycle.has_value()) {
        //     // it is now the next cycle :)
        //     // fmt::println(stderr, "{} kill {}", tick, killOnNextCycle.value());
        //     this->wTxns.invalidateFromKey(killOnNextCycle.value());
        //     this->rTxns.invalidateFromKey(killOnNextCycle.value());
        // }

        if (inputKeyManager.killKeyMessage.has_value()) {
            this->wTxns.invalidateFromKey(inputKeyManager.killKeyMessage.value());
            this->rTxns.invalidateFromKey(inputKeyManager.killKeyMessage.value());
        } else {
            // killOnNextCycle = std::nullopt;
        }

        if (outputKeyManager.bumpPerfCounterGoodWrite) {
            fmt::println("V\tPerfGood\tWrite");
            this->signalledGoodWrite++;
        }
        if (outputKeyManager.bumpPerfCounterBadWrite) {
            fmt::println("V\tPerfBad\tWrite");
            this->signalledBadWrite++;
        }
        if (outputKeyManager.bumpPerfCounterGoodRead) {
            fmt::println("V\tPerfGood\tRead");
            this->signalledGoodRead++;
        }
        if (outputKeyManager.bumpPerfCounterBadRead) {
            fmt::println("V\tPerfBad\tRead");
            this->signalledBadRead++;
        }

        if (inputKeyManager.keyResponse) {
            onKeyMngrKeyResponse(inputKeyManager.keyResponse.value());
        }

        
    }
public:
    ExposerScoreboard(std::unordered_map<key_manager::KeyId, U128>& secrets, bool expectPassthroughInvalidTransactions = false) :
        BaseExposerScoreboard<DUT, ctype, KeyMngrV2>(secrets, expectPassthroughInvalidTransactions)
        // killOnNextCycle(std::nullopt)
        {}
    virtual ~ExposerScoreboard() override = default;
};

template<class DUT, CapType ctype>
class ExposerScoreboard<DUT, ctype, KeyMngrV2_AsDUT> : public BaseExposerScoreboard<DUT, ctype, KeyMngrV2_AsDUT> {
    virtual void monitorAndScoreKeyManager(
        DUT& dut, uint64_t tick,
        KeyMngrShimInput<KeyMngrV2_AsDUT>& inputKeyManager, KeyMngrShimOutput<KeyMngrV2_AsDUT>& outputKeyManager
    ) override {
        this->signalledGoodWrite = outputKeyManager.debugGoodWrite;
        this->signalledBadWrite = outputKeyManager.debugBadWrite;
        this->signalledGoodRead = outputKeyManager.debugGoodRead;
        this->signalledBadRead = outputKeyManager.debugBadRead;
    }
public:
    ExposerScoreboard(std::unordered_map<key_manager::KeyId, U128>& secrets, bool expectPassthroughInvalidTransactions = false) :
        BaseExposerScoreboard<DUT, ctype, KeyMngrV2_AsDUT>(secrets, expectPassthroughInvalidTransactions)
        {}
    virtual ~ExposerScoreboard() override = default;
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

        ExposerStimulus<DUT, ctype, V>::setup(rng);
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // 100 cycles should do it
        return tick >= this->firstTickWhereKeyMgrReady + 1000;
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

        ExposerStimulus<DUT, ctype, V>::setup(rng);
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // 100 cycles should do it
        return tick >= this->firstTickWhereKeyMgrReady + 1000;
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

        ExposerStimulus<DUT, ctype, V>::setup(rng);
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // 100 cycles should do it
        return tick >= this->firstTickWhereKeyMgrReady + 1000;
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
        
        ExposerStimulus<DUT, ctype, V>::setup(rng);
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // 100 cycles should do it
        return tick >= this->firstTickWhereKeyMgrReady + 1000;
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
        
        ExposerStimulus<DUT, ctype, V>::setup(rng);
    }
    virtual bool shouldFinish(uint64_t tick) override {
        // 100 cycles should do it
        return tick >= this->firstTickWhereKeyMgrReady + 1000;
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
        return tick >= this->firstTickWhereKeyMgrReady + (5000 * (n_revocations) + 350);
    }
};

// Over the sweep of {delay = 4-33 cycles}
// - start a transaction with {0,1,2} caveats
// - at +delay cycles, send the cancel signal
template<class DUT, CapType ctype>
class UVMSimpleRevokeWhileChecking_KeyMngrV2 : public ExposerStimulus<DUT, ctype, KeyMngrV2> {
    int n_cavs;
public:
    virtual ~UVMSimpleRevokeWhileChecking_KeyMngrV2() = default;
    virtual std::string name() override {
        return fmt::format("Single Valid-Key Valid-Cap Valid-ReadWrite {}-cav, revoked while in progress", n_cavs);
    }
    UVMSimpleRevokeWhileChecking_KeyMngrV2(int n_cavs) : ExposerStimulus<DUT, ctype, KeyMngrV2>(
        new BasicKeyManagerShimStimulus<DUT, KeyMngrV2>(),
        new BasicSanitizedMemStimulus<DUT>()
    ), n_cavs(n_cavs) {}
    virtual void driveInputsForTick(std::mt19937& rng, DUT& dut, uint64_t tick) {
        const key_manager::KeyId secret_id = 111;
        uint64_t iteration = (tick / 1000);
        if (iteration < 30) {
            if (tick % 1000 == 100){
                // Enqueue a transaction, including creating secret keys

                const U128 key = U128::random(rng);
                const uint8_t axi_id = 0b1011;

                this->keyMgr->secrets[secret_id] = key;
                auto cap_data = this->test_librust_random_valid_cap(rng, secret_id, n_cavs, CCapPerms_ReadWrite);
                auto axi_params = cap_data.valid_transfer_params(32, 1);
                this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
                this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
            }
        }

        ExposerStimulus<DUT, ctype, KeyMngrV2>::driveInputsForTick(rng, dut, tick);

        if (iteration < 30) {
            uint64_t delay = 40 + iteration * 10;

            if ((tick % 1000) == 100 + delay) {
                // TODO gate this behind Konata
                fmt::println(stdout, "V\tKillKey\t{}", secret_id);
                // fmt::println(stderr, "delay {} killing", delay);
                // We have finished key retrieval, revoke
                WRITE_INPUT(keyStoreShim_killKeyMessage, secret_id);
                // don't actually get rid of the key.
            } else {
                NOWRITE_INPUT(keyStoreShim_killKeyMessage);
            }
        }
    }
    virtual bool shouldFinish(uint64_t tick) override {
        return tick >= this->firstTickWhereKeyMgrReady + 1000 * 30 + 1000;
    }
};

// TODO version of SimpleRevokeWhenChecking_KeyMngrV2

template<class DUT, CapType ctype, KeyMngrVersion V>
class UVMStreamOfNValidTransactions : public ExposerStimulus<DUT, ctype, V> {
    CCapPerms perms;
    uint64_t n_transactions;
    uint8_t n_data_flits_per_transaction;

    uint64_t final_tick = 0;
    
public:
    virtual ~UVMStreamOfNValidTransactions() = default;
    virtual std::string name() override {
        return fmt::format("Stream of {} {} {}-flit transactions", n_transactions, ccap_perms_str(perms), n_data_flits_per_transaction);
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
        
        ExposerStimulus<DUT, ctype, V>::setup(rng);
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
    uint8_t n_data_flits_per_transaction;
    int n_cavs = -1;

    uint64_t final_tick = 0;
    
public:
    virtual ~UVMStreamOfNLibRustValidTransactions() = default;
    virtual std::string name() override {
        if (n_cavs == -1) {
            return fmt::format("Stream of {} librust random valid {} {}-flit transactions", n_transactions, ctype, n_data_flits_per_transaction);
        } else {
            return fmt::format("Stream of {} librust random valid {} {}-caveat {}-flit transactions", n_transactions, ctype, n_cavs, n_data_flits_per_transaction);
        }
    }
    UVMStreamOfNLibRustValidTransactions(uint64_t n_transactions, uint8_t n_data_flits_per_transaction, int n_cavs = -1) : ExposerStimulus<DUT, ctype, V>(
        new BasicKeyManagerShimStimulus<DUT, V>(),
        new BasicSanitizedMemStimulus<DUT>()
    ), n_transactions(n_transactions), n_data_flits_per_transaction(n_data_flits_per_transaction), n_cavs(n_cavs) {
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
            auto axi_params = cap_data.valid_transfer_params(32, n_data_flits_per_transaction);
            if (cap_data.perms & CCapPerms_Read) {
                this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
            }
            if (cap_data.perms & CCapPerms_Write) {
                this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
            }
        }
        
        ExposerStimulus<DUT, ctype, V>::setup(rng);
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
        
        ExposerStimulus<DUT, ctype, V>::setup(rng);
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



#undef NOWRITE_INPUT
#undef WRITE_INPUT
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
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000, /* n_data_flits_per_transaction */ 4),
            expectPassthroughInvalidTransactions
        ),

        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000, /* n_data_flits_per_transaction */ 4, /* n_cavs */ 0),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000, /* n_data_flits_per_transaction */ 4, /* n_cavs */ 1),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000, /* n_data_flits_per_transaction */ 4, /* n_cavs */ 2),
            expectPassthroughInvalidTransactions
        ),

        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000, /* n_data_flits_per_transaction */ 1, /* n_cavs */ 0),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000, /* n_data_flits_per_transaction */ 1, /* n_cavs */ 1),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, ctype, V>(10'000, /* n_data_flits_per_transaction */ 1, /* n_cavs */ 2),
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
    } else if constexpr (V == KeyMngrV2) {
        tests.push_back(
            new ExposerUVMishTest(
                new UVMSimpleRevokeWhileChecking_KeyMngrV2<TheDUT, ctype>(0),
                expectPassthroughInvalidTransactions
            )
        );
        tests.push_back(
            new ExposerUVMishTest(
                new UVMSimpleRevokeWhileChecking_KeyMngrV2<TheDUT, ctype>(1),
                expectPassthroughInvalidTransactions
            )
        );
        tests.push_back(
            new ExposerUVMishTest(
                new UVMSimpleRevokeWhileChecking_KeyMngrV2<TheDUT, ctype>(2),
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
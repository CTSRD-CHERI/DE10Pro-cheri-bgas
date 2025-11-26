#ifndef EXPOSER_H
#define EXPOSER_H

#include "tb_bitfields.h"
#include "key_manager.h"
#include "tb.h"

#include "util.h"

#include <array>
#include <algorithm>
#include <cstdint>
#include <optional>

namespace exposer {

    struct ExposerInput {
        uint64_t time;

        std::optional<axi::IOCapAxi::AWFlit_id4_addr64_user3> iocap_flit_aw;
        std::optional<axi::IOCapAxi::WFlit_data32> iocap_flit_w;
        std::optional<axi::IOCapAxi::ARFlit_id4_addr64_user3> iocap_flit_ar;

        std::optional<axi::SanitizedAxi::BFlit_id4> clean_flit_b;
        std::optional<axi::SanitizedAxi::RFlit_id4_data32> clean_flit_r;
        
        bool operator==(const ExposerInput&) const = default;
        bool is_notable() const {
            return (iocap_flit_aw) || (iocap_flit_w) || (iocap_flit_ar) || (clean_flit_b) || (clean_flit_r);
        }
    };

    struct ExposerOutput {
        uint64_t time;

        std::optional<axi::IOCapAxi::BFlit_id4> iocap_flit_b;
        std::optional<axi::IOCapAxi::RFlit_id4_data32> iocap_flit_r;

        std::optional<axi::SanitizedAxi::AWFlit_id4_addr64_user0> clean_flit_aw;
        std::optional<axi::SanitizedAxi::WFlit_data32> clean_flit_w;
        std::optional<axi::SanitizedAxi::ARFlit_id4_addr64_user0> clean_flit_ar;

        bool operator==(const ExposerOutput&) const = default;
        bool is_notable() const {
            return (iocap_flit_b) || (iocap_flit_r) || (clean_flit_aw) || (clean_flit_ar) || (clean_flit_w);
        }
    };
}

template <> class fmt::formatter<exposer::ExposerInput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (exposer::ExposerInput const& x, Context& ctx) const {
        return format_to(ctx.out(), "ExposerInput {{\n\t.time = {},\n\t.iocap_flit_aw = {},\n\t.iocap_flit_w = {},\n\t.iocap_flit_ar = {},\n\t.clean_flit_b = {},\n\t.clean_flit_r = {}\n}}", x.time, x.iocap_flit_aw, x.iocap_flit_w, x.iocap_flit_ar, x.clean_flit_b, x.clean_flit_r);
    }
};

template <> class fmt::formatter<exposer::ExposerOutput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (exposer::ExposerOutput const& x, Context& ctx) const {
        return format_to(ctx.out(), "ExposerOutput {{\n\t.time = {},\n\t.iocap_flit_b = {},\n\t.iocap_flit_r = {},\n\t.clean_flit_aw = {},\n\t.clean_flit_w = {},\n\t.clean_flit_ar = {}\n}}", x.time, x.iocap_flit_b, x.iocap_flit_r, x.clean_flit_aw, x.clean_flit_w, x.clean_flit_ar);
    }
};

enum KeyMngrVersion {
    KeyMngrV1,
    KeyMngrV2,
    KeyMngrV2_AsDUT_MMIO32,
};

template<KeyMngrVersion V>
struct KeyMngrShimInput;

template<>
struct KeyMngrShimInput<KeyMngrV1> {
    std::optional<key_manager::Epoch> newEpochRequest;
    std::optional<key_manager::KeyResponse> keyResponse;

    bool operator==(const KeyMngrShimInput<KeyMngrV1>&) const = default;
    bool is_notable() const {
        return (newEpochRequest) || (keyResponse);
    }
};
template <> class fmt::formatter<KeyMngrShimInput<KeyMngrV1>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (KeyMngrShimInput<KeyMngrV1> const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyMngrShimInput<KeyMngrV1> {{ .newEpochRequest = {}, .keyResponse = {} }}", x.newEpochRequest, x.keyResponse);
    }
};

template<>
struct KeyMngrShimInput<KeyMngrV2> {
    std::optional<key_manager::KeyResponse> keyResponse;
    std::optional<key_manager::KeyId> killKeyMessage;

    bool operator==(const KeyMngrShimInput<KeyMngrV2>&) const = default;
    bool is_notable() const {
        return (killKeyMessage) || (keyResponse);
    }
};
template <> class fmt::formatter<KeyMngrShimInput<KeyMngrV2>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (KeyMngrShimInput<KeyMngrV2> const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyMngrShimInput<KeyMngrV2> {{ .killKeyMessage = {}, .keyResponse = {} }}", x.killKeyMessage, x.keyResponse);
    }
};

// When the KeyMngr is an actual DUT, we only shim the AXI inputs
template<>
struct KeyMngrShimInput<KeyMngrV2_AsDUT_MMIO32> {
    std::optional<axi::AxiLite::ARFlit_addr13_user0> ar;
    std::optional<axi::AxiLite::AWFlit_addr13_user0> aw;
    std::optional<axi::AxiLite::WFlit_data32_user0> w;

    bool operator==(const KeyMngrShimInput<KeyMngrV2_AsDUT_MMIO32>&) const = default;
    bool is_notable() const {
        return (ar.has_value()) || (aw.has_value()) || (w.has_value());

    }
};
template <> class fmt::formatter<KeyMngrShimInput<KeyMngrV2_AsDUT_MMIO32>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (KeyMngrShimInput<KeyMngrV2_AsDUT_MMIO32> const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyMngrShimInput<KeyMngrV2_AsDUT_MMIO32> {{ .ar = {}, .aw = {}, .w = {} }}", x.ar, x.aw, x.w);
    }
};

template<KeyMngrVersion V>
struct ShimmedExposerInput : exposer::ExposerInput {
    KeyMngrShimInput<V> keyManager;

    bool operator==(const ShimmedExposerInput&) const = default;
    bool is_notable() const {
        return exposer::ExposerInput::is_notable() || (keyManager.is_notable());
    }
};
template<KeyMngrVersion V> class fmt::formatter<ShimmedExposerInput<V>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (ShimmedExposerInput<V> const& x, Context& ctx) const {
        return format_to(ctx.out(), "ShimmedExposerInput {{\n\t.exposer = {},\n\t.keyManager = {}\n}}", (exposer::ExposerInput)x, x.keyManager);
    }
};

template<KeyMngrVersion V>
using ShimmedExposerInputs = std::vector<ShimmedExposerInput<V>>;
template<KeyMngrVersion V>
using ShimmedExposerInputsMaker = TimeSeriesMaker<ShimmedExposerInput<V>>;

template<KeyMngrVersion V>
struct KeyMngrShimOutput;

template<>
struct KeyMngrShimOutput<KeyMngrV1> {
    bool bumpPerfCounterGoodWrite;
    bool bumpPerfCounterBadWrite;
    bool bumpPerfCounterGoodRead;
    bool bumpPerfCounterBadRead;

    std::optional<key_manager::KeyId> keyRequest;
    std::optional<key_manager::Epoch> finishedEpoch;

    bool operator==(const KeyMngrShimOutput<KeyMngrV1>&) const = default;
    bool is_notable() {
        return (bumpPerfCounterGoodWrite) || (bumpPerfCounterBadWrite) || (bumpPerfCounterGoodRead) || (bumpPerfCounterBadRead) || (keyRequest) || (finishedEpoch);
    }
};
template <> class fmt::formatter<KeyMngrShimOutput<KeyMngrV1>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (KeyMngrShimOutput<KeyMngrV1> const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyMngrShimOutput<KeyMngrV1> {{ .bumpPerfCounterGoodWrite = {}, ....BadWrite = {}, ....GoodRead = {}, ....BadRead = {}, .keyRequest = {}, .finishedEpoch = {} }}", x.bumpPerfCounterGoodWrite, x.bumpPerfCounterBadWrite, x.bumpPerfCounterGoodRead, x.bumpPerfCounterBadRead, x.keyRequest, x.finishedEpoch);
    }
};

template<>
struct KeyMngrShimOutput<KeyMngrV2> {
    bool bumpPerfCounterGoodWrite;
    bool bumpPerfCounterBadWrite;
    bool bumpPerfCounterGoodRead;
    bool bumpPerfCounterBadRead;

    std::optional<key_manager::KeyId> keyRequest;
    std::optional<key_manager::KeyId> rValve_Increment;
    std::optional<key_manager::KeyId> rValve_Decrement;
    std::optional<key_manager::KeyId> wValve_Increment;
    std::optional<key_manager::KeyId> wValve_Decrement;

    bool operator==(const KeyMngrShimOutput<KeyMngrV2>&) const = default;
    bool is_notable() {
        return (bumpPerfCounterGoodWrite) || (bumpPerfCounterBadWrite) ||
                (bumpPerfCounterGoodRead) || (bumpPerfCounterBadRead) ||
                (keyRequest) ||
                (rValve_Increment) || (rValve_Decrement) ||
                (wValve_Increment) || (wValve_Decrement);
    }
};
template <> class fmt::formatter<KeyMngrShimOutput<KeyMngrV2>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (KeyMngrShimOutput<KeyMngrV2> const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyMngrShimOutput<KeyMngrV2> {{ .bumpPerfCounterGoodWrite = {}, ....BadWrite = {}, ....GoodRead = {}, ....BadRead = {}, .keyRequest = {}, .rValve_Inc = {}, ...r_Dec = {}, ...w_Inc = {}, ...wDec = {} }}", x.bumpPerfCounterGoodWrite, x.bumpPerfCounterBadWrite, x.bumpPerfCounterGoodRead, x.bumpPerfCounterBadRead, x.keyRequest, x.rValve_Increment, x.rValve_Decrement, x.wValve_Increment, x.wValve_Decrement);
    }
};

// When the KeyMngr is an actual DUT, we can still inspect the innards?
template<>
struct KeyMngrShimOutput<KeyMngrV2_AsDUT_MMIO32> {
    std::optional<axi::AxiLite::RFlit_data32_user0> r;
    std::optional<axi::AxiLite::BFlit_user0> b;


    // bool bumpPerfCounterGoodWrite;
    // bool bumpPerfCounterBadWrite;
    // bool bumpPerfCounterGoodRead;
    // bool bumpPerfCounterBadRead;

    // std::optional<key_manager::KeyId> keyRequest;
    // std::optional<key_manager::KeyId> rValve_Increment;
    // std::optional<key_manager::KeyId> rValve_Decrement;
    // std::optional<key_manager::KeyId> wValve_Increment;
    // std::optional<key_manager::KeyId> wValve_Decrement;

    key_manager2::KeyStatuses debugKeyStatuses;
    key_manager2::refcountpipe::MaybeKeyId debugEnableKey;
    key_manager2::refcountpipe::MaybeKeyId debugKillKey;
    uint64_t debugGoodWrite;
    uint64_t debugBadWrite;
    uint64_t debugGoodRead;
    uint64_t debugBadRead;

    bool operator==(const KeyMngrShimOutput<KeyMngrV2_AsDUT_MMIO32>&) const = default;
    bool is_notable() {
        return (b.has_value()) || (r.has_value());
        // return (bumpPerfCounterGoodWrite) || (bumpPerfCounterBadWrite) ||
        //         (bumpPerfCounterGoodRead) || (bumpPerfCounterBadRead) ||
        //         (keyRequest) ||
        //         (rValve_Increment) || (rValve_Decrement) ||
        //         (wValve_Increment) || (wValve_Decrement);
    }
};
template <> class fmt::formatter<KeyMngrShimOutput<KeyMngrV2_AsDUT_MMIO32>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (KeyMngrShimOutput<KeyMngrV2_AsDUT_MMIO32> const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyMngrShimOutput<KeyMngrV2_AsDUT_MMIO32> {{ .r = {}, .b = {} }}", x.r, x.b);
    }
};


template<KeyMngrVersion V>
struct ShimmedExposerOutput : exposer::ExposerOutput {
    KeyMngrShimOutput<V> keyManager;

    bool operator==(const ShimmedExposerOutput&) const = default;
    bool is_notable() {
        return (exposer::ExposerOutput::is_notable()) || (keyManager.is_notable());
    }
};
template<KeyMngrVersion V> class fmt::formatter<ShimmedExposerOutput<V>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (ShimmedExposerOutput<V> const& x, Context& ctx) const {
        return format_to(ctx.out(), "ShimmedExposerOutput {{\n\t.exposer = {},\n\t.keyManager = {}\n}}", (exposer::ExposerOutput)x, x.keyManager);
    }
};
template<KeyMngrVersion V>
using ShimmedExposerOutputs = std::vector<ShimmedExposerOutput<V>>;
template<KeyMngrVersion V>
using ShimmedExposerOutputsMaker = TimeSeriesMaker<ShimmedExposerOutput<V>>;

/**
 * Apply a ShimmedExposerInput to a Verilator device-under-test.
 * The DUT must have adhere to the Bluespec `SimpleIOCapExposerTb` interface, with keyStoreShim and exposer4x32 sub-interfaces.
 * 
 * If the given input requests a certain line be held up, e.g. if it sets the iocap_flit_w field, the 
 * DUT must be able to receive the `put` i.e. the RDY_exposer4x32_iocapsIn_axiSignals_w_put and exposer4x32_iocapsIn_axiSignals_w_canPut booleans must be True.
 * Otherwise an assertion failure is thrown. TODO better error handling.
 */
template<class DUT>
void push_input(DUT& dut, const exposer::ExposerInput& input) {
    #define PUT(name, value) do {                  \
        dut.EN_## name ##_put = 1;        \
        dut. name ##_put_val = (value); \
        assert(dut.RDY_## name ##_put);   \
        assert(dut. name ##_canPut);    \
    } while(0);
    #define NOPUT(name) dut.EN_## name ##_put = 0;

    if (input.iocap_flit_aw) {
        auto flit = verilate_array(input.iocap_flit_aw.value().pack());
        PUT(exposer4x32_iocapsIn_axiSignals_aw, flit);
    } else {
        NOPUT(exposer4x32_iocapsIn_axiSignals_aw);
    }

    if (input.iocap_flit_w) {
        PUT(exposer4x32_iocapsIn_axiSignals_w, input.iocap_flit_w.value().pack());
    } else {
        NOPUT(exposer4x32_iocapsIn_axiSignals_w);
    }
    
    if (input.iocap_flit_ar) {
        auto flit = verilate_array(input.iocap_flit_ar.value().pack());
        PUT(exposer4x32_iocapsIn_axiSignals_ar, flit);
    } else {
        NOPUT(exposer4x32_iocapsIn_axiSignals_ar);
    }

    if (input.clean_flit_b) {
        auto flit = input.clean_flit_b.value().pack();
        PUT(exposer4x32_sanitizedOut_b, flit);
    } else {
        NOPUT(exposer4x32_sanitizedOut_b);
    }

    if (input.clean_flit_r) {
        auto flit = input.clean_flit_r.value().pack();
        PUT(exposer4x32_sanitizedOut_r, flit);
    } else {
        NOPUT(exposer4x32_sanitizedOut_r);
    }

    #undef NOPUT
    #undef PUT
}
template<class DUT>
void push_input(DUT& dut, const KeyMngrShimInput<KeyMngrV1>& input) {
    #define PUT(name, value) do {                  \
        dut.EN_## name ##_put = 1;        \
        dut. name ##_put_val = (value); \
        assert(dut.RDY_## name ##_put);   \
        assert(dut. name ##_canPut);    \
    } while(0);
    #define NOPUT(name) dut.EN_## name ##_put = 0;

    if (input.newEpochRequest) {
        PUT(keyStoreShim_newEpochRequests, input.newEpochRequest.value());
    } else {
        NOPUT(keyStoreShim_newEpochRequests);
    }

    if (input.keyResponse) {
        PUT(keyStoreShim_keyResponses, verilate_array(input.keyResponse.value().asBluespec().pack()));
    } else {
        NOPUT(keyStoreShim_keyResponses);
    }

    #undef NOPUT
    #undef PUT
}
template<class DUT>
void push_input(DUT& dut, const ShimmedExposerInput<KeyMngrV1>& input) {
    push_input(dut, (const exposer::ExposerInput&)input);
    push_input(dut, input.keyManager);
}

template<class DUT>
void observe_input(DUT& dut, exposer::ExposerInput& input) {
    #define CANPEEK(from) (dut.EN_## from ##_put)
    #define PEEK(from) dut. from ##_put_val

    if (CANPEEK(exposer4x32_iocapsIn_axiSignals_aw)) {
        input.iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3::unpack(stdify_array(PEEK(exposer4x32_iocapsIn_axiSignals_aw)));
    }

    if (CANPEEK(exposer4x32_iocapsIn_axiSignals_w)) {
        input.iocap_flit_w = axi::IOCapAxi::WFlit_data32::unpack(PEEK(exposer4x32_iocapsIn_axiSignals_w));
    }

    if (CANPEEK(exposer4x32_iocapsIn_axiSignals_ar)) {
        input.iocap_flit_ar = axi::IOCapAxi::ARFlit_id4_addr64_user3::unpack(stdify_array(PEEK(exposer4x32_iocapsIn_axiSignals_ar)));
    }

    if (CANPEEK(exposer4x32_sanitizedOut_b)) {
        input.clean_flit_b = axi::SanitizedAxi::BFlit_id4::unpack(PEEK(exposer4x32_sanitizedOut_b));
    }

    if (CANPEEK(exposer4x32_sanitizedOut_r)) {
        input.clean_flit_r = axi::SanitizedAxi::RFlit_id4_data32::unpack(PEEK(exposer4x32_sanitizedOut_r));
    }

    #undef PEEK
    #undef CANPEEK
}

template<class DUT>
void observe_input(DUT& dut, KeyMngrShimInput<KeyMngrV1>& input) {
    #define CANPEEK(from) (dut.EN_## from ##_put)
    #define PEEK(from) dut. from ##_put_val

    if (CANPEEK(keyStoreShim_newEpochRequests)) {
        input.newEpochRequest = PEEK(keyStoreShim_newEpochRequests);
    }

    if (CANPEEK(keyStoreShim_keyResponses)) {
        input.keyResponse = key_manager::KeyResponse::fromBluespec(
            key_manager::Tuple2_KeyId_MaybeKey::unpack(stdify_array(PEEK(keyStoreShim_keyResponses)))
        );
    }

    #undef PEEK
    #undef CANPEEK
}

template<class DUT>
void observe_input(DUT& dut, ShimmedExposerInput<KeyMngrV1>& input) {
    observe_input(dut, (exposer::ExposerInput&)input);
    observe_input(dut, input.keyManager);
}

template<class DUT>
void observe_input(DUT& dut, KeyMngrShimInput<KeyMngrV2>& input) {
    #define CANPEEK(from) (dut.EN_## from ##_put)
    #define PEEK(from) dut. from ##_put_val
    #define CANPEEK_WRITE(name) (dut.EN_## name ##___05Fwrite)
    #define PEEK_WRITE(name) (dut.name ##___05Fwrite_x)

    if (CANPEEK_WRITE(keyStoreShim_killKeyMessage)) {
        input.killKeyMessage = PEEK_WRITE(keyStoreShim_killKeyMessage);
    }

    if (CANPEEK(keyStoreShim_keyResponses)) {
        input.keyResponse = key_manager::KeyResponse::fromBluespec(
            key_manager::Tuple2_KeyId_MaybeKey::unpack(stdify_array(PEEK(keyStoreShim_keyResponses)))
        );
    }

    #undef PEEK_WRITE
    #undef CANPEEK_WRITE
    #undef PEEK
    #undef CANPEEK
}

template<class DUT>
void observe_input(DUT& dut, ShimmedExposerInput<KeyMngrV2>& input) {
    observe_input(dut, (exposer::ExposerInput&)input);
    observe_input(dut, input.keyManager);
}

template<class DUT>
void observe_input(DUT& dut, KeyMngrShimInput<KeyMngrV2_AsDUT_MMIO32>& input) {
    #define CANPEEK(from) (dut.EN_## from ##_put)
    #define PEEK(from) dut. from ##_put_val
    #define CANPEEK_WRITE(name) (dut.EN_## name ##___05Fwrite)
    #define PEEK_WRITE(name) (dut.name ##___05Fwrite_x)

    if (CANPEEK(keyStore_aw)) {
        input.aw = axi::AxiLite::AWFlit_addr13_user0::unpack(PEEK(keyStore_aw));
    }

    if (CANPEEK(keyStore_w)) {
        input.w = axi::AxiLite::WFlit_data32_user0::unpack(PEEK(keyStore_w));
    }

    if (CANPEEK(keyStore_ar)) {
        input.ar = axi::AxiLite::ARFlit_addr13_user0::unpack(PEEK(keyStore_ar));
    }

    #undef PEEK_WRITE
    #undef CANPEEK_WRITE
    #undef PEEK
    #undef CANPEEK
}

template<class DUT>
void observe_input(DUT& dut, ShimmedExposerInput<KeyMngrV2_AsDUT_MMIO32>& input) {
    observe_input(dut, (exposer::ExposerInput&)input);
    observe_input(dut, input.keyManager);
}

/**
 * Pull from the outputs of a Verilator device-under-test to fill a ShimmedExposerOutput.
 * The DUT must have adhere to the Bluespec `SimpleIOCapExposerTb` interface, with keyStoreShim and exposer4x32 sub-interfaces.
 * 
 * All outputs will be pulled from if they have any content.
 * If the output is peekable, e.g. if dut.RDY_keyMgr32_hostFacingSlave_r == 1,
 * dut.keyMgr32_hostFacingSlave_r_canPeek and RDY_keyMgr32_hostfacingSlave_r_drop must both be truthy.
 */
template<class DUT>
void pull_output(DUT& dut, exposer::ExposerOutput& output) {
    #define CANPEEK(from) (dut.RDY_## from ##_peek)
    #define POP(from, into) \
        assert(dut. from ##_canPeek); \
        assert(dut.RDY_## from ##_drop); \
        dut.EN_## from ##_drop = 1; \
        into = dut. from ##_peek;
    #define NOPOP(from) \
        dut.EN_## from ##_drop = 0; \

    if (CANPEEK(exposer4x32_iocapsIn_axiSignals_b)) {
        uint8_t bflit;
        POP(exposer4x32_iocapsIn_axiSignals_b, bflit);
        output.iocap_flit_b = std::optional(axi::IOCapAxi::BFlit_id4::unpack(bflit));
    } else {
        NOPOP(exposer4x32_iocapsIn_axiSignals_b);
    }
    
    if (CANPEEK(exposer4x32_iocapsIn_axiSignals_r)) {
        uint64_t rflit;
        POP(exposer4x32_iocapsIn_axiSignals_r, rflit);
        output.iocap_flit_r = std::optional(axi::IOCapAxi::RFlit_id4_data32::unpack(rflit));
    } else {
        NOPOP(exposer4x32_iocapsIn_axiSignals_r);
    }

    if (CANPEEK(exposer4x32_sanitizedOut_aw)) {
        VlWide<4> flit;
        POP(exposer4x32_sanitizedOut_aw, flit);
        output.clean_flit_aw = std::optional(axi::SanitizedAxi::AWFlit_id4_addr64_user0::unpack(stdify_array(flit)));
    } else {
        NOPOP(exposer4x32_sanitizedOut_aw);
    }

    if (CANPEEK(exposer4x32_sanitizedOut_w)) {
        uint64_t flit;
        POP(exposer4x32_sanitizedOut_w, flit);
        output.clean_flit_w = std::optional(axi::SanitizedAxi::WFlit_data32::unpack(flit));
    } else {
        NOPOP(exposer4x32_sanitizedOut_w);
    }

    if (CANPEEK(exposer4x32_sanitizedOut_ar)) {
        VlWide<4> flit;
        POP(exposer4x32_sanitizedOut_ar, flit);
        output.clean_flit_ar = std::optional(axi::SanitizedAxi::ARFlit_id4_addr64_user0::unpack(stdify_array(flit)));
    } else {
        NOPOP(exposer4x32_sanitizedOut_ar);
    }

    #undef NOPOP
    #undef POP
    #undef CANPEEK
}

template<class DUT>
void pull_output(DUT& dut, KeyMngrShimOutput<KeyMngrV1>& output) {
    #define CANPEEK(from) (dut.RDY_## from ##_peek)
    #define POP(from, into) \
        assert(dut. from ##_canPeek); \
        assert(dut.RDY_## from ##_drop); \
        dut.EN_## from ##_drop = 1; \
        into = dut. from ##_peek;
    #define NOPOP(from) \
        dut.EN_## from ##_drop = 0; \

    if (CANPEEK(keyStoreShim_keyRequests)){
        key_manager::KeyId keyRequest;
        POP(keyStoreShim_keyRequests, keyRequest);
        output.keyRequest = std::optional(keyRequest);
    } else {
        NOPOP(keyStoreShim_keyRequests);
    }

    if (CANPEEK(keyStoreShim_finishedEpochs)){
        key_manager::Epoch finishedEpoch;
        POP(keyStoreShim_finishedEpochs, finishedEpoch);
        output.finishedEpoch = std::optional(finishedEpoch);
    } else {
        NOPOP(keyStoreShim_finishedEpochs);
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterGoodWrite___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterGoodWrite___05Fread) {
        output.bumpPerfCounterGoodWrite = true;
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterBadWrite___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterBadWrite___05Fread) {
        output.bumpPerfCounterBadWrite = true;
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterGoodRead___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterGoodRead___05Fread) {
        output.bumpPerfCounterGoodRead = true;
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterBadRead___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterBadRead___05Fread) {
        output.bumpPerfCounterBadRead = true;
    }

    #undef NOPOP
    #undef POP
    #undef CANPEEK
}

template<class DUT>
void pull_output(DUT& dut, ShimmedExposerOutput<KeyMngrV1>& output) {
    pull_output(dut, (exposer::ExposerOutput&)output);
    pull_output(dut, output.keyManager);
}

template<class DUT>
void pull_output(DUT& dut, KeyMngrShimOutput<KeyMngrV2>& output) {
    #define CANPEEK(from) (dut.RDY_## from ##_peek)
    #define POP(from, into) \
        assert(dut. from ##_canPeek); \
        assert(dut.RDY_## from ##_drop); \
        dut.EN_## from ##_drop = 1; \
        into = dut. from ##_peek;
    #define NOPOP(from) \
        dut.EN_## from ##_drop = 0; \

    if (CANPEEK(keyStoreShim_keyRequests)){
        key_manager::KeyId keyRequest;
        POP(keyStoreShim_keyRequests, keyRequest);
        output.keyRequest = std::optional(keyRequest);
    } else {
        NOPOP(keyStoreShim_keyRequests);
    }

    if (CANPEEK(keyStoreShim_rValve_Increment)){
        key_manager::KeyId key;
        POP(keyStoreShim_rValve_Increment, key);
        output.rValve_Increment = std::optional(key);
    } else {
        NOPOP(keyStoreShim_rValve_Increment);
    }

    if (CANPEEK(keyStoreShim_rValve_Decrement)){
        key_manager::KeyId key;
        POP(keyStoreShim_rValve_Decrement, key);
        output.rValve_Decrement = std::optional(key);
    } else {
        NOPOP(keyStoreShim_rValve_Decrement);
    }

    if (CANPEEK(keyStoreShim_wValve_Increment)){
        key_manager::KeyId key;
        POP(keyStoreShim_wValve_Increment, key);
        output.rValve_Increment = std::optional(key);
    } else {
        NOPOP(keyStoreShim_wValve_Increment);
    }

    if (CANPEEK(keyStoreShim_wValve_Decrement)){
        key_manager::KeyId key;
        POP(keyStoreShim_wValve_Decrement, key);
        output.rValve_Decrement = std::optional(key);
    } else {
        NOPOP(keyStoreShim_wValve_Decrement);
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterGoodWrite___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterGoodWrite___05Fread) {
        output.bumpPerfCounterGoodWrite = true;
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterBadWrite___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterBadWrite___05Fread) {
        output.bumpPerfCounterBadWrite = true;
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterGoodRead___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterGoodRead___05Fread) {
        output.bumpPerfCounterGoodRead = true;
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterBadRead___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterBadRead___05Fread) {
        output.bumpPerfCounterBadRead = true;
    }

    #undef NOPOP
    #undef POP
    #undef CANPEEK
}

template<class DUT>
void pull_output(DUT& dut, ShimmedExposerOutput<KeyMngrV2>& output) {
    pull_output(dut, (exposer::ExposerOutput&)output);
    pull_output(dut, output.keyManager);
}


template<class DUT>
void pull_output(DUT& dut, KeyMngrShimOutput<KeyMngrV2_AsDUT_MMIO32>& output) {
    #define CANPEEK(from) (dut.RDY_## from ##_peek)
    #define POP(from, into) \
        assert(dut. from ##_canPeek); \
        assert(dut.RDY_## from ##_drop); \
        dut.EN_## from ##_drop = 1; \
        into = dut. from ##_peek;
    #define NOPOP(from) \
        dut.EN_## from ##_drop = 0; \

    if (CANPEEK(keyStore_r)){
        uint64_t rFlitPacked;
        POP(keyStore_r, rFlitPacked);
        output.r = std::optional(axi::AxiLite::RFlit_data32_user0::unpack(rFlitPacked));
    } else {
        NOPOP(keyStore_r);
    }

    if (CANPEEK(keyStore_b)){
        char bFlitPacked;
        POP(keyStore_b, bFlitPacked);
        output.b = std::optional(axi::AxiLite::BFlit_user0::unpack(bFlitPacked));
    } else {
        NOPOP(keyStore_b);
    }

    output.debugKeyStatuses = key_manager2::KeyStatuses::unpack(stdify_array(dut.debugKeyState___05Fread));
    output.debugEnableKey = key_manager2::refcountpipe::MaybeKeyId::unpack(dut.debugEnableKey___05Fread);
    output.debugKillKey = key_manager2::refcountpipe::MaybeKeyId::unpack(dut.debugKillKey___05Fread);
    output.debugGoodWrite = dut.debugGoodWrite___05Fread;
    output.debugBadWrite = dut.debugBadWrite___05Fread;
    output.debugGoodRead = dut.debugGoodRead___05Fread;
    output.debugBadRead = dut.debugBadRead___05Fread;

    #undef NOPOP
    #undef POP
    #undef CANPEEK
}

template<class DUT>
void pull_output(DUT& dut, ShimmedExposerOutput<KeyMngrV2_AsDUT_MMIO32>& output) {
    pull_output(dut, (exposer::ExposerOutput&)output);
    pull_output(dut, output.keyManager);
}

#endif // EXPOSER_H
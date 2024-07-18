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

template<size_t N>
auto verilate_array(const std::array<uint32_t, N>& from) {
    VlWide<N> vl{};
    std::copy(from.begin(), from.end(), vl.m_storage);
    return vl;
}

template<size_t N>
auto stdify_array(const VlWide<N> from) {
    std::array<uint32_t, N> arr{};
    std::copy(from.m_storage, from.m_storage + N, arr.begin());
    return arr;
}

namespace exposer {

    struct ExposerInput {
        uint64_t time;

        std::optional<axi::IOCapAxi::AWFlit_id4_addr64_user3> iocap_flit_aw;
        std::optional<axi::IOCapAxi::WFlit_data32> iocap_flit_w;
        std::optional<axi::IOCapAxi::ARFlit_id4_addr64_user3> iocap_flit_ar;

        std::optional<axi::SanitizedAxi::BFlit_id4> clean_flit_b;
        std::optional<axi::SanitizedAxi::RFlit_id4_data32> clean_flit_r;
        
        bool operator==(const ExposerInput&) const = default;
    };

    struct ExposerOutput {
        uint64_t time;

        std::optional<axi::IOCapAxi::BFlit_id4> iocap_flit_b;
        std::optional<axi::IOCapAxi::RFlit_id4_data32> iocap_flit_r;

        std::optional<axi::SanitizedAxi::AWFlit_id4_addr64_user0> clean_flit_aw;
        std::optional<axi::SanitizedAxi::WFlit_data32> clean_flit_w;
        std::optional<axi::SanitizedAxi::ARFlit_id4_addr64_user0> clean_flit_ar;

        bool operator==(const ExposerOutput&) const = default;
        bool is_notable() {
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

struct KeyMngrShimInput {
    std::optional<key_manager::Epoch> newEpochRequest;
    std::optional<key_manager::KeyResponse> keyResponse;

    bool operator==(const KeyMngrShimInput&) const = default;
};
template <> class fmt::formatter<KeyMngrShimInput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (KeyMngrShimInput const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyMngrShimInput {{ .newEpochRequest = {}, .keyResponse = {} }}", x.newEpochRequest, x.keyResponse);
    }
};

struct ShimmedExposerInput : exposer::ExposerInput {
    KeyMngrShimInput keyManager;

    bool operator==(const ShimmedExposerInput&) const = default;
};
template <> class fmt::formatter<ShimmedExposerInput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (ShimmedExposerInput const& x, Context& ctx) const {
        return format_to(ctx.out(), "ShimmedExposerInput {{\n\t.exposer = {},\n\t.keyManager = {}\n}}", (exposer::ExposerInput)x, x.keyManager);
    }
};


using ShimmedExposerInputs = std::vector<ShimmedExposerInput>;
using ShimmedExposerInputsMaker = TimeSeriesMaker<ShimmedExposerInput>;

struct KeyMngrShimOutput {
    bool bumpPerfCounterGoodWrite;
    bool bumpPerfCounterBadWrite;
    bool bumpPerfCounterGoodRead;
    bool bumpPerfCounterBadRead;

    std::optional<key_manager::KeyId> keyRequest;
    std::optional<key_manager::Epoch> finishedEpoch;

    bool operator==(const KeyMngrShimOutput&) const = default;
    bool is_notable() {
        return (bumpPerfCounterGoodWrite) || (bumpPerfCounterBadWrite) || (bumpPerfCounterGoodRead) || (bumpPerfCounterBadRead) || (keyRequest) || (finishedEpoch);
    }
};
template <> class fmt::formatter<KeyMngrShimOutput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (KeyMngrShimOutput const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyMngrShimOutput {{ .bumpPerfCounterGoodWrite = {}, ....BadWrite = {}, ....GoodRead = {}, ....BadRead = {}, .keyRequest = {}, .finishedEpoch = {} }}", x.bumpPerfCounterGoodWrite, x.bumpPerfCounterBadWrite, x.bumpPerfCounterGoodRead, x.bumpPerfCounterBadRead, x.keyRequest, x.finishedEpoch);
    }
};
struct ShimmedExposerOutput : exposer::ExposerOutput {
    KeyMngrShimOutput keyManager;

    bool operator==(const ShimmedExposerOutput&) const = default;
    bool is_notable() {
        return (exposer::ExposerOutput::is_notable()) || (keyManager.is_notable());
    }
};
template <> class fmt::formatter<ShimmedExposerOutput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (ShimmedExposerOutput const& x, Context& ctx) const {
        return format_to(ctx.out(), "ShimmedExposerOutput {{\n\t.exposer = {},\n\t.keyManager = {}\n}}", (exposer::ExposerOutput)x, x.keyManager);
    }
};
using ShimmedExposerOutputs = std::vector<ShimmedExposerOutput>;
using ShimmedExposerOutputsMaker = TimeSeriesMaker<ShimmedExposerOutput>;

/**
 * Apply a ShimmedExposerInput to a Verilator device-under-test.
 * The DUT must have adhere to the Bluespec `SimpleIOCapExposerTb` interface, with keyStoreShim and exposer4x32 sub-interfaces.
 * 
 * If the given input requests a certain line be held up, e.g. if it sets the iocap_flit_w field, the 
 * DUT must be able to receive the `put` i.e. the RDY_exposer4x32_iocapsIn_axiSignals_w_put and exposer4x32_iocapsIn_axiSignals_w_canPut booleans must be True.
 * Otherwise an assertion failure is thrown. TODO better error handling.
 */
template<class DUT>
void push_input(DUT& dut, const ShimmedExposerInput& input) {
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

    if (input.keyManager.newEpochRequest) {
        PUT(keyStoreShim_newEpochRequests, input.keyManager.newEpochRequest.value());
    } else {
        NOPUT(keyStoreShim_newEpochRequests);
    }

    if (input.keyManager.keyResponse) {
        PUT(keyStoreShim_keyResponses, verilate_array(input.keyManager.keyResponse.value().asBluespec().pack()));
    } else {
        NOPUT(keyStoreShim_keyResponses);
    }

    #undef NOPUT
    #undef PUT
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
void pull_output(DUT& dut, ShimmedExposerOutput& output) {
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

    if (CANPEEK(keyStoreShim_keyRequests)){
        key_manager::KeyId keyRequest;
        POP(keyStoreShim_keyRequests, keyRequest);
        output.keyManager.keyRequest = std::optional(keyRequest);
    } else {
        NOPOP(keyStoreShim_keyRequests);
    }

    if (CANPEEK(keyStoreShim_finishedEpochs)){
        key_manager::Epoch finishedEpoch;
        POP(keyStoreShim_finishedEpochs, finishedEpoch);
        output.keyManager.finishedEpoch = std::optional(finishedEpoch);
    } else {
        NOPOP(keyStoreShim_finishedEpochs);
    }

    #undef NOPOP
    #undef POP
    #undef CANPEEK
}

#endif // EXPOSER_H
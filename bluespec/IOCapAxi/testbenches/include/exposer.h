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

struct KeyMngrV1ShimInput {
    std::optional<key_manager::Epoch> newEpochRequest;
    std::optional<key_manager::KeyResponse> keyResponse;

    bool operator==(const KeyMngrV1ShimInput&) const = default;
    bool is_notable() const {
        return (newEpochRequest) || (keyResponse);
    }
};
template <> class fmt::formatter<KeyMngrV1ShimInput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (KeyMngrV1ShimInput const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyMngrV1ShimInput {{ .newEpochRequest = {}, .keyResponse = {} }}", x.newEpochRequest, x.keyResponse);
    }
};

template<class KeyMgrShim>
struct ShimmedExposerInput : exposer::ExposerInput {
    KeyMgrShim keyManager;

    bool operator==(const ShimmedExposerInput&) const = default;
    bool is_notable() const {
        return exposer::ExposerInput::is_notable() || (keyManager.is_notable());
    }
};
template<class KeyMgrShim> class fmt::formatter<ShimmedExposerInput<KeyMgrShim>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (ShimmedExposerInput<KeyMgrShim> const& x, Context& ctx) const {
        return format_to(ctx.out(), "ShimmedExposerInput {{\n\t.exposer = {},\n\t.keyManager = {}\n}}", (exposer::ExposerInput)x, x.keyManager);
    }
};

template<class KeyMgrShim>
using ShimmedExposerInputs = std::vector<ShimmedExposerInput<KeyMgrShim>>;
template<class KeyMgrShim>
using ShimmedExposerInputsMaker = TimeSeriesMaker<ShimmedExposerInput<KeyMgrShim>>;

struct KeyMngrV1ShimOutput {
    bool bumpPerfCounterGoodWrite;
    bool bumpPerfCounterBadWrite;
    bool bumpPerfCounterGoodRead;
    bool bumpPerfCounterBadRead;

    std::optional<key_manager::KeyId> keyRequest;
    std::optional<key_manager::Epoch> finishedEpoch;

    bool operator==(const KeyMngrV1ShimOutput&) const = default;
    bool is_notable() {
        return (bumpPerfCounterGoodWrite) || (bumpPerfCounterBadWrite) || (bumpPerfCounterGoodRead) || (bumpPerfCounterBadRead) || (keyRequest) || (finishedEpoch);
    }
};
template <> class fmt::formatter<KeyMngrV1ShimOutput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (KeyMngrV1ShimOutput const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyMngrV1ShimOutput {{ .bumpPerfCounterGoodWrite = {}, ....BadWrite = {}, ....GoodRead = {}, ....BadRead = {}, .keyRequest = {}, .finishedEpoch = {} }}", x.bumpPerfCounterGoodWrite, x.bumpPerfCounterBadWrite, x.bumpPerfCounterGoodRead, x.bumpPerfCounterBadRead, x.keyRequest, x.finishedEpoch);
    }
};
template<class KeyMgrShim>
struct ShimmedExposerOutput : exposer::ExposerOutput {
    KeyMgrShim keyManager;

    bool operator==(const ShimmedExposerOutput&) const = default;
    bool is_notable() {
        return (exposer::ExposerOutput::is_notable()) || (keyManager.is_notable());
    }
};
template<class KeyMgrShim> class fmt::formatter<ShimmedExposerOutput<KeyMgrShim>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (ShimmedExposerOutput<KeyMgrShim> const& x, Context& ctx) const {
        return format_to(ctx.out(), "ShimmedExposerOutput {{\n\t.exposer = {},\n\t.keyManager = {}\n}}", (exposer::ExposerOutput)x, x.keyManager);
    }
};
template<class KeyMgrShim>
using ShimmedExposerOutputs = std::vector<ShimmedExposerOutput<KeyMgrShim>>;
template<class KeyMgrShim>
using ShimmedExposerOutputsMaker = TimeSeriesMaker<ShimmedExposerOutput<KeyMgrShim>>;

/**
 * Apply a ShimmedExposerInput to a Verilator device-under-test.
 * The DUT must have adhere to the Bluespec `SimpleIOCapExposerTb` interface, with keyStoreShim and exposer4x32 sub-interfaces.
 * 
 * If the given input requests a certain line be held up, e.g. if it sets the iocap_flit_w field, the 
 * DUT must be able to receive the `put` i.e. the RDY_exposer4x32_iocapsIn_axiSignals_w_put and exposer4x32_iocapsIn_axiSignals_w_canPut booleans must be True.
 * Otherwise an assertion failure is thrown. TODO better error handling.
 */
template<class DUT>
void push_input(DUT& dut, const ShimmedExposerInput<KeyMngrV1ShimInput>& input) {
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

template<class DUT>
void observe_input(DUT& dut, ShimmedExposerInput<KeyMngrV1ShimInput>& input) {
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

    if (CANPEEK(keyStoreShim_newEpochRequests)) {
        input.keyManager.newEpochRequest = PEEK(keyStoreShim_newEpochRequests);
    }

    if (CANPEEK(keyStoreShim_keyResponses)) {
        input.keyManager.keyResponse = key_manager::KeyResponse::fromBluespec(
            key_manager::Tuple2_KeyId_MaybeKey::unpack(stdify_array(PEEK(keyStoreShim_keyResponses)))
        );
    }

    #undef PEEK
    #undef CANPEEK
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
void pull_output(DUT& dut, ShimmedExposerOutput<KeyMngrV1ShimOutput>& output) {
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

    if (dut.RDY_keyStoreShim_bumpedPerfCounterGoodWrite___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterGoodWrite___05Fread) {
        output.keyManager.bumpPerfCounterGoodWrite = true;
        fmt::println("pull_output good write at {}", output.time);
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterBadWrite___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterBadWrite___05Fread) {
        output.keyManager.bumpPerfCounterBadWrite = true;
        fmt::println("pull_output bad write at {}", output.time);
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterGoodRead___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterGoodRead___05Fread) {
        output.keyManager.bumpPerfCounterGoodRead = true;
        fmt::println("pull_output good read at {}", output.time);
    }

    if (dut.RDY_keyStoreShim_bumpedPerfCounterBadRead___05Fread &&
        dut.keyStoreShim_bumpedPerfCounterBadRead___05Fread) {
        output.keyManager.bumpPerfCounterBadRead = true;
        fmt::println("pull_output bad read at {}", output.time);
    }

    #undef NOPOP
    #undef POP
    #undef CANPEEK
}

#endif // EXPOSER_H
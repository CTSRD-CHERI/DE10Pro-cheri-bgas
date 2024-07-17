#ifndef EXPOSER_H
#define EXPOSER_H

#include "tb_bitfields/axi.h"
#include "key_manager.h"
#include "tb.h"

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
    };

    struct ExposerOutput {
        uint64_t time;

        std::optional<axi::IOCapAxi::BFlit_id4> iocap_flit_b;
        std::optional<axi::IOCapAxi::RFlit_id4_data32> iocap_flit_r;

        std::optional<axi::SanitizedAxi::AWFlit_id4_addr64_user0> clean_flit_aw;
        std::optional<axi::SanitizedAxi::WFlit_data32> clean_flit_w;
        std::optional<axi::SanitizedAxi::ARFlit_id4_addr64_user0> clean_flit_ar;

        bool is_notable() {
            return (iocap_flit_b) || (iocap_flit_r) || (clean_flit_aw) || (clean_flit_ar) || (clean_flit_w);
        }
    };
}

struct KeyMngrShimInput {
    std::optional<key_manager::Epoch> newEpochRequest;
    std::optional<key_manager::KeyResponse> keyResponse;
};
struct ShimmedExposerInput : exposer::ExposerInput {
    KeyMngrShimInput keyManager;
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

    bool is_notable() {
        return (bumpPerfCounterGoodWrite) || (bumpPerfCounterBadWrite) || (bumpPerfCounterGoodRead) || (bumpPerfCounterBadRead) || (keyRequest) || (finishedEpoch);
    }
};
struct ShimmedExposerOutput : exposer::ExposerOutput {
    KeyMngrShimOutput keyManager;

    bool is_notable() {
        return (exposer::ExposerOutput::is_notable()) || (keyManager.is_notable());
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
void push_input<DUT, ShimmedExposerInput>(DUT& dut, const ShimmedExposerInput& input) {
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
void pull_output<DUT, ShimmedExposerOutput>(DUT& dut, ShimmedExposerOutput& output) {
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
        output.exposer.iocap_flit_b = std::optional(axi::IOCapAxi::BFlit_id4::unpack(bflit));
    } else {
        NOPOP(exposer4x32_iocapsIn_axiSignals_b);
    }
    
    if (CANPEEK(exposer4x32_iocapsIn_axiSignals_r)) {
        uint64_t rflit;
        POP(exposer4x32_iocapsIn_axiSignals_r, rflit);
        output.exposer.iocap_flit_r = std::optional(axi::IOCapAxi::RFlit_id4_data32::unpack(rflit));
    } else {
        NOPOP(exposer4x32_iocapsIn_axiSignals_r);
    }

    if (CANPEEK(exposer4x32_sanitizedOut_aw)) {
        VlWide<4> flit;
        POP(exposer4x32_sanitizedOut_aw, flit);
        output.exposer.clean_flit_aw = std::optional(axi::SanitizedAxi::AWFlit_id4_addr64_user0::unpack(stdify_array(flit)));
    } else {
        NOPOP(exposer4x32_sanitizedOut_aw);
    }

    if (CANPEEK(exposer4x32_sanitizedOut_w)) {
        uint64_t flit;
        POP(exposer4x32_sanitizedOut_w, flit);
        output.exposer.clean_flit_w = std::optional(axi::SanitizedAxi::WFlit_data32::unpack(flit));
    } else {
        NOPOP(exposer4x32_sanitizedOut_w);
    }

    if (CANPEEK(exposer4x32_sanitizedOut_ar)) {
        VlWide<4> flit;
        POP(exposer4x32_sanitizedOut_ar, flit);
        output.exposer.clean_flit_ar = std::optional(axi::SanitizedAxi::ARFlit_id4_addr64_user0::unpack(stdify_array(flit)));
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
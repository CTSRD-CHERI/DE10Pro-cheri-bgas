#ifndef EXPOSER_H
#define EXPOSER_H

#include "axi.h"
#include "key_manager.h"

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

namespace exposer {

    struct ExposerInput {
        uint64_t time;

        std::optional<axi::IOCapAxi::AWFlit_id4_addr64_user3> iocap_flit_aw;
        std::optional<axi::IOCapAxi::WFlit_data32> iocap_flit_w;
        std::optional<axi::IOCapAxi::ARFlit_id4_addr64_user3> iocap_flit_ar;

        std::optional<axi::SanitizedAxi::WFlit_data32> clean_flit_w;
        std::optional<axi::SanitizedAxi::RFlit_id4_data32> clean_flit_r;
    };

    struct ExposerOutput {
        uint64_t time;

        std::optional<axi::IOCapAxi::WFlit_data32> iocap_flit_w;
        std::optional<axi::IOCapAxi::RFlit_id4_data32> iocap_flit_r;

        std::optional<axi::SanitizedAxi::AWFlit_id4_addr64_user0> clean_flit_aw;
        std::optional<axi::SanitizedAxi::WFlit_data32> clean_flit_w;
        std::optional<axi::SanitizedAxi::ARFlit_id4_addr64_user0> clean_flit_ar;
    };
}

namespace shimmed_exposer {
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
    };
    struct ShimmedExposerOutput : exposer::ExposerOutput {
        KeyMngrShimOutput keyManager;
    };
    using ShimmedExposerOutputs = std::vector<ShimmedExposerOutput>;
    using ShimmedExposerOutputsMaker = TimeSeriesMaker<ShimmedExposerOutput>;

    /**
     * Apply a ShimmedExposerInput to a Verilator device-under-test.
     * The DUT must have adhere to the Bluespec `SimpleIOCapExposerTb` interface, with keyStoreShim and exposer4x32 sub-interfaces.
     * 
     * If the given input requests a certain line be held up, e.g. if it sets the readReq field, the 
     * DUT must be able to receive the `put` i.e. the RDY_keyMgr32_readReq_put and keyMgr32_readReq_canPut booleans must be True.
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
            VlWide<4> flit = verilate_array(input.iocap_flit_aw.value().pack());
            PUT(exposer4x32_iocapsIn_axiSignals_aw, flit);
        } else {
            NOPUT(exposer4x32_iocapsIn_axiSignals_aw);
        }

        // TODO key manager shim, other flits

        #undef NOPUT
        #undef PUT
    }
}

#endif // EXPOSER_H
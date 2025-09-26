#ifndef KEY_MANAGER2_H
#define KEY_MANAGER2_H

#include "axi.h"
#include "tb_bitfields.h"
#include "tb.h"

#include "util.h"

#include <cstdint>
#include <optional>

#define FMT_HEADER_ONLY
#include "fmt/format.h"

namespace key_manager2 {

    using KeyId = uint8_t; // 8 bits
    using Key = U128;

    namespace refcountpipe {
        // Interactions with the RefCountPipe consist of 
        // keyStatusShim {
        //      interface ReadOnly#(Maybe#(KeyId)) tryConfirmingRevokeKey;
        //      interface WriteOnly#(KeyId) keyToStartRevoking;
        // }
        // dut {
            // interface IOCapAxi_KeyManager2_RefCountPipe#(numeric type n_valves);
            //     interface Vector#(n_valves, IOCapAxi_KeyManager2_RefCountPipe_ValveIfc) valvePorts;

                // interface IOCapAxi_KeyManager2_RefCountPipe_ValveIfc;
                //     // Used by the valve to report key ID transaction-starts to the KeyManager
                //     interface Sink#(KeyId) keyIncrementRefcountRequest;
                //     // Used by the valve to report key ID transaction-ends to the KeyManager
                //     interface Sink#(KeyId) keyDecrementRefcountRequest;
                // endinterface
            // endinterface
        // }

        struct ValveInput {
            std::optional<KeyId> keyIncrementRefcountRequest;
            std::optional<KeyId> keyDecrementRefcountRequest;

            bool operator==(const ValveInput& other) const = default;
        };

        struct KeyStatusInput {
            std::optional<KeyId> keyToStartRevoking;

            bool operator==(const KeyStatusInput& other) const = default;
        };

        struct KeyStatusOutput {
            std::optional<KeyId> tryConfirmingRevokeKey;

            bool operator==(const KeyStatusOutput& other) const = default;
        };

        struct RefCountPipe_2Valves_Input {
            uint64_t time;

            KeyStatusInput keyStatus;
            ValveInput valve0;
            ValveInput valve1;

            bool operator==(const RefCountPipe_2Valves_Input& other) const = default;
        };

        struct RefCountPipe_Output {
            uint64_t time;

            KeyStatusOutput keyStatus;

            bool operator==(const RefCountPipe_Output& other) const = default;
            bool is_notable() {
                return (keyStatus.tryConfirmingRevokeKey.has_value());
            }
        };

        using RefCountPipe_2Valve_Inputs = std::vector<RefCountPipe_2Valves_Input>;
        using RefCountPipe_2Valve_InputsMaker = TimeSeriesMaker<RefCountPipe_2Valves_Input>;

        using RefCountPipe_Outputs = std::vector<RefCountPipe_Output>;
        using RefCountPipe_OutputsMaker = TimeSeriesMaker<RefCountPipe_Output>;
    };
}

/**
 * Apply a RefCountPipe_2Valves_Inputs to a Verilator device-under-test.
 * The DUT must have adhere to the Bluespec interface which provides two sub-interfaces:
    interface IOCapAxi_KeyManager2_RefCountPipe#(2) dut;
    interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc_Shim keyStatusShim;
 * 
 * If the given input requests a certain line be held up, e.g. if it sets the readReq field, the 
 * DUT must be able to receive the `put` i.e. the RDY_dut_readReq_put and dut_readReq_canPut booleans must be True.
 * Otherwise an assertion failure is thrown. TODO better error handling.
 */
template<class DUT>
void push_input(DUT& dut, const key_manager2::refcountpipe::RefCountPipe_2Valves_Input& input) {
    #define PUT(name, value) do {             \
        dut.EN_## name ##_put = 1;        \
        dut.name ##_put_val = (value); \
        assert(dut.RDY_## name ##_put);   \
        assert(dut.name ##_canPut);    \
    } while(0);
    #define NOPUT(name) dut.EN_## name ##_put = 0;

    #define WRITE(name, value) do {                \
        dut.EN_## name ##___05Fwrite = 1;      \
        dut.name ##___05Fwrite_x = (value); \
        assert(dut.RDY_## name ##___05Fwrite); \
    } while(0);
    #define NOWRITE(name) dut.EN_## name ##___05Fwrite = 0;

    if (input.keyStatus.keyToStartRevoking) {
        WRITE(keyStatusShim_keyToStartRevoking, input.keyStatus.keyToStartRevoking.value());
    } else {
        NOWRITE(keyStatusShim_keyToStartRevoking);
    }

    if (input.valve0.keyIncrementRefcountRequest) {
        PUT(dut_valvePorts_0_keyIncrementRefcountRequest, input.valve0.keyIncrementRefcountRequest.value());
    } else {
        NOPUT(dut_valvePorts_0_keyIncrementRefcountRequest);
    }
    if (input.valve0.keyDecrementRefcountRequest) {
        PUT(dut_valvePorts_0_keyDecrementRefcountRequest, input.valve0.keyDecrementRefcountRequest.value());
    } else {
        NOPUT(dut_valvePorts_0_keyDecrementRefcountRequest);
    }

    if (input.valve1.keyIncrementRefcountRequest) {
        PUT(dut_valvePorts_1_keyIncrementRefcountRequest, input.valve1.keyIncrementRefcountRequest.value());
    } else {
        NOPUT(dut_valvePorts_1_keyIncrementRefcountRequest);
    }
    if (input.valve1.keyDecrementRefcountRequest) {
        PUT(dut_valvePorts_1_keyDecrementRefcountRequest, input.valve1.keyDecrementRefcountRequest.value());
    } else {
        NOPUT(dut_valvePorts_1_keyDecrementRefcountRequest);
    }

    #undef NOWRITE
    #undef WRITE
    #undef NOPUT
    #undef PUT
}

/**
 * Pull from the outputs of a Verilator device-under-test to fill a KeyManagerOutput.
 * The DUT must have adhere to the Bluespec interface which provides two sub-interfaces:
    interface IOCapAxi_KeyManager2_RefCountPipe#(2) dut;
    interface IOCapAxi_KeyManager2_KeyStatePipe_RefCountPipeIfc_Shim keyStatusShim;
 * 
 * All outputs will be pulled from if they have any content.
 * If the output is peekable, e.g. if dut.RDY_dut_hostFacingSlave_r == 1,
 * dut.dut_hostFacingSlave_r_canPeek and RDY_dut_hostfacingSlave_r_drop must both be truthy.
 */
template<class DUT>
void pull_output(DUT& dut, key_manager2::refcountpipe::RefCountPipe_Output& output) {
    #define CANREAD(from) (dut.RDY_## from ##___05Fread)
    #define READ(from, into) \
        assert(dut.RDY_## from ##___05Fread); \
        into = dut.from ##___05Fread;

    if (CANREAD(keyStatusShim_tryConfirmingRevokeKey)) {
        uint16_t val;
        READ(keyStatusShim_tryConfirmingRevokeKey, val);
        key_manager2::refcountpipe::MaybeKeyId id = key_manager2::refcountpipe::MaybeKeyId::unpack(val);
        if (id.keyIdValid) {
            output.keyStatus.tryConfirmingRevokeKey = id.keyId;
        } else {
            output.keyStatus.tryConfirmingRevokeKey = std::nullopt;
        }
    } else {
        output.keyStatus.tryConfirmingRevokeKey = std::nullopt;
    }

    #undef READ
    #undef CANREAD
}

template <> class fmt::formatter<key_manager2::refcountpipe::ValveInput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager2::refcountpipe::ValveInput const& key, Context& ctx) const {
        return format_to(ctx.out(), "ValveInput {{ .keyIncrementRefcountRequest = {}, .keyDecrementRefcountRequest = {} }}", key.keyIncrementRefcountRequest, key.keyDecrementRefcountRequest);
    }
};

template <> class fmt::formatter<key_manager2::refcountpipe::KeyStatusInput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager2::refcountpipe::KeyStatusInput const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyStatusInput {{ .keyToStartRevoking = {} }}", x.keyToStartRevoking);
    }
};

template <> class fmt::formatter<key_manager2::refcountpipe::KeyStatusOutput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager2::refcountpipe::KeyStatusOutput const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyStatusOutput {{ .tryConfirmingRevokeKey = {} }}", x.tryConfirmingRevokeKey);
    }
};

template <> class fmt::formatter<key_manager2::refcountpipe::RefCountPipe_2Valves_Input> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager2::refcountpipe::RefCountPipe_2Valves_Input const& x, Context& ctx) const {
        return format_to(ctx.out(), "RefCountPipe_2Valves_Input {{ .time = {}, .keyStatus = {}, .valve0 = {}, .valve1 = {} }}", x.time, x.keyStatus, x.valve0, x.valve1);
    }
};

template <> class fmt::formatter<key_manager2::refcountpipe::RefCountPipe_Output> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager2::refcountpipe::RefCountPipe_Output const& x, Context& ctx) const {
        return format_to(ctx.out(), "RefCountPipe_Output {{ .time = {}, .keyStatus = {} }}", x.time, x.keyStatus);
    }
};

#endif // KEY_MANAGER2_H
#ifndef KEY_MANAGER_INCL
#define KEY_MANAGER_INCL 1

#include <cstdint>
#include <optional>

#define FMT_HEADER_ONLY
#include "fmt/format.h"

/**
 * Container for items of type T where (T::time) is a uint64_t, allowing Python defaultdict-style creation.
 * e.g. from an empty Maker, `maker[100].blah = "blah";` will construct a T, map it to time 100 and set `t.time = 100`, then return a reference for the user to modify.
 * The asVec() function converts it to a vector.
 * 
 */
template<class T>
class TimeSeriesMaker {
    std::map<uint64_t, T> elems;

public:

    std::vector<T> asVec() {
        std::vector<T> v;
        for (const auto& [time, elem] : elems) {
            v.push_back(elem);
        }
        return v;
    }

    T& operator[](const uint64_t& key) {
        auto iter = elems.find(key);
        if (iter == elems.end()) {
            T elem{};
            elem.time = key;
            elems[key] = elem;
            return elems[key];
        }
        return (*iter).second;
    }
};

namespace key_manager {

    using Epoch = uint8_t;
    using KeyId = uint16_t; // 9 bits

    struct Key {
        uint64_t top;
        uint64_t bottom;

        bool operator==(const Key& other) const {
            return (this->top == other.top) &&
                (this->bottom == other.bottom);
        }
    };

    struct KeyResponse {
        KeyId keyId;
        std::optional<Key> key;

        bool operator==(const KeyResponse& other) const {
            return (this->keyId == other.keyId) &&
                (this->key == other.key);
        }
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

    // Can't bitpack these flits because sometimes gcc forces fields to be byte-aligned!
    struct AWFlit {
        // struct {
        //     uint16_t prot : 3;
        //     uint16_t addr : 13;
        // };
        uint16_t flit;
        
        bool operator==(const AWFlit& other) const {
            return this->flit == other.flit;
        }
    };

    struct WFlit {
        // struct {
        //     uint8_t wstrb : 4;
        //     AxiData  data  : 32;
        //     uint32_t       : 28;
        // };
        uint64_t flit;

        bool operator==(const WFlit& other) const {
            return this->flit == other.flit;
        }
    };

    struct BFlit {
        // struct {
        //     uint8_t        : 6;
        //     AXI4_Resp resp : 2;
        // };
        uint8_t flit;
        
        bool operator==(const BFlit& other) const {
            return this->flit == other.flit;
        }
    };

    struct ARFlit {
        // struct {
        //     uint16_t prot : 3;
        //     uint16_t addr : 13;
        // };
        uint16_t flit;
        
        bool operator==(const ARFlit& other) const {
            return this->flit == other.flit;
        }
    };

    struct RFlit {
        // struct {
        //     AXI4_Resp resp : 2;
        //     AxiData   data : 32;
        //     uint32_t       : 30;
        // };

        uint64_t flit;
        
        bool operator==(const RFlit& other) const {
            return this->flit == other.flit;
        }
    };

    struct AxiWriteReq {
        AxiAddress address;
        uint32_t data;
        uint8_t write_enable;

        AWFlit aw_flit() const {
            AWFlit flit{};
            constexpr uint16_t prot = 0b000;
            flit.flit = ((this->address & 0x1FFFu) << 3) | (prot << 0);
            return flit;
        }
        WFlit w_flit() const {
            WFlit flit{};
            // flit.flit = 0xacacacacacacacac;
            flit.flit = (0xacacacaul << 36) | (uint64_t(this->data) << 4) | (uint64_t(this->write_enable) & 0xF);
            return flit;
        }

        bool operator==(const AxiWriteReq& other) const {
            return (this->address == other.address) &&
                (this->data == other.data) &&
                (this->write_enable == other.write_enable);
        }
    };

    struct AxiWriteResp {
        bool good;

        // Don't use a constructor for this because then we can't brace-initialize
        static AxiWriteResp from_flit(BFlit flit) {
            return AxiWriteResp {
                .good = ((flit.flit & 0b11) == uint8_t(AXI4_Resp::Okay)),
            };
        }

        bool operator==(const AxiWriteResp& other) const {
            return this->good == other.good;
        }
    };

    struct AxiReadReq {
        AxiAddress address; // 13 bits

        ARFlit ar_flit() const {
            ARFlit flit{};
            constexpr uint16_t prot = 0b000;
            flit.flit = ((this->address & 0x1FFFu) << 3) | (prot << 0);
            return flit;
        }

        bool operator==(const AxiReadReq& other) const {
            return this->address == other.address;
        }
    };

    struct AxiReadResp {
        bool good;
        AxiData data;
        
        // Don't use a constructor for this because then we can't brace-initialize
        static AxiReadResp from_flit(RFlit flit) {
            return AxiReadResp {
                .good = ((flit.flit & 0b11) == uint8_t(AXI4_Resp::Okay)),
                .data = uint32_t(flit.flit >> 2),
            };
        }

        bool operator==(const AxiReadResp& other) const {
            return (this->good == other.good) &&
                (this->data == other.data);
        }
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

        bool operator==(const KeyManagerInput& other) const {
            return (this->time == other.time) && 
                (this->keyRequest == other.keyRequest) && 
                (this->finishedEpoch == other.finishedEpoch) && 
                (this->writeReq == other.writeReq) && 
                (this->readReq == other.readReq);
        }
    };

    using KeyManagerInputs = std::vector<KeyManagerInput>;
    using KeyManagerInputsMaker = TimeSeriesMaker<KeyManagerInput>;

    struct KeyManagerOutput {
        uint64_t time;
        std::optional<Epoch> newEpochRequest;
        std::optional<KeyResponse> keyResponse;

        std::optional<AxiReadResp> readResp;
        std::optional<AxiWriteResp> writeResp;

        bool operator==(const KeyManagerOutput& other) const {
            return (this->time == other.time) && 
                (this->newEpochRequest == other.newEpochRequest) && 
                (this->keyResponse == other.keyResponse) && 
                (this->readResp == other.readResp) && 
                (this->writeResp == other.writeResp);
        }
    };

    using KeyManagerOutputs = std::vector<KeyManagerOutput>;
    using KeyManagerOutputsMaker = TimeSeriesMaker<KeyManagerOutput>;


    /**
     * Apply a KeyManagerInput to a Verilator device-under-test.
     * The DUT must have adhere to the Bluespec interface which provides a sub-interface:
     * `interface IOCap_KeyManager#(32) keyMgr32;`
     * i.e. there must be a keyMgr32 submodule which adheres to the IOCap_KeyManager#(32) interface.
     * 
     * If the given KeyManagerInput requests a certain line be held up, e.g. if it sets the readReq field, the 
     * DUT must be able to receive the `put` i.e. the RDY_keyMgr32_readReq_put and keyMgr32_readReq_canPut booleans must be True.
     * Otherwise an assertion failure is thrown. TODO better error handling.
     */
    template<class DUT>
    void push_input(DUT& dut, const KeyManagerInput& input) {
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
    }

    /**
     * Pull from the outputs of a Verilator device-under-test to fill a KeyManagerOutput.
     * The DUT must have adhere to the Bluespec interface which provides a sub-interface:
     * `interface IOCap_KeyManager#(32) keyMgr32;`
     * i.e. there must be a keyMgr32 submodule which adheres to the IOCap_KeyManager#(32) interface.
     * 
     * All outputs will be pulled from if they have any content.
     * If the output is peekable, e.g. if dut.RDY_keyMgr32_hostFacingSlave_r == 1,
     * dut.keyMgr32_hostFacingSlave_r_canPeek and RDY_keyMgr32_hostfacingSlave_r_drop must both be truthy.
     */
    template<class DUT>
    void pull_output(DUT& dut, KeyManagerOutput& output) {
        #define CANPEEK(from) (dut.RDY_keyMgr32_## from ##_peek)
        #define POP(from, into) \
            assert(dut.keyMgr32_## from ##_canPeek); \
            assert(dut.RDY_keyMgr32_## from ##_drop); \
            dut.EN_keyMgr32_## from ##_drop = 1; \
            into = dut.keyMgr32_## from ##_peek;
        #define NOPOP(from) \
            dut.EN_keyMgr32_## from ##_drop = 0; \

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
            output.readResp = std::optional(AxiReadResp::from_flit(rflit));
        } else {
            NOPOP(hostFacingSlave_r);
        }

        if (CANPEEK(hostFacingSlave_b)) {
            BFlit bflit;
            POP(hostFacingSlave_b, bflit.flit);
            output.writeResp = std::optional(AxiWriteResp::from_flit(bflit));
        } else {
            NOPOP(hostFacingSlave_b);
        }

        #undef NOPOP
        #undef POP
        #undef CANPEEK
    }

}

template <class T> class fmt::formatter<std::optional<T>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (std::optional<T> const& x, Context& ctx) const {
        if (x.has_value()) {
            return format_to(ctx.out(), "Some({})", x.value());
        } else {
            return format_to(ctx.out(), "None");
        }
    }
};

template <> class fmt::formatter<key_manager::Key> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager::Key const& key, Context& ctx) const {
        return format_to(ctx.out(), "0x{:08x}{:08x}", key.top, key.bottom);
    }
};

template <> class fmt::formatter<key_manager::KeyResponse> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager::KeyResponse const& key, Context& ctx) const {
        return format_to(ctx.out(), "KeyResponse {{ .keyId = {}, .key = {} }}", key.keyId, key.key);
    }
};

template <> class fmt::formatter<key_manager::AxiWriteReq> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager::AxiWriteReq const& x, Context& ctx) const {
        return format_to(ctx.out(), "AxiWriteReq {{ .address = 0x{:08x}, .data = 0x{:08x}, .write_enable = 0b{:04b} }}", x.address, x.data, x.write_enable);
    }
};

template <> class fmt::formatter<key_manager::AxiReadReq> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager::AxiReadReq const& x, Context& ctx) const {
        return format_to(ctx.out(), "AxiReadReq {{ .address = 0x{:08x} }}", x.address);
    }
};

template <> class fmt::formatter<key_manager::AxiWriteResp> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager::AxiWriteResp const& x, Context& ctx) const {
        return format_to(ctx.out(), "AxiWriteResp {{ .good = {} }}", x.good);
    }
};

template <> class fmt::formatter<key_manager::AxiReadResp> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager::AxiReadResp const& x, Context& ctx) const {
        return format_to(ctx.out(), "AxiReadResp {{ .good = {}, .data = 0x{:08x} }}", x.good, x.data);
    }
};

template <> class fmt::formatter<key_manager::KeyManagerInput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager::KeyManagerInput const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyManagerInput {{\n\t.time = {},\n\t.keyRequest = {},\n\t.finishedEpoch = {},\n\t.readReq = {},\n\t.writeReq = {},\n}}", x.time, x.keyRequest, x.finishedEpoch, x.readReq, x.writeReq);
    }
};

template <> class fmt::formatter<key_manager::KeyManagerOutput> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (key_manager::KeyManagerOutput const& x, Context& ctx) const {
        return format_to(ctx.out(), "KeyManagerOutput {{\n\t.time = {},\n\t.keyResponse = {},\n\t.newEpochRequest = {},\n\t.readResp = {},\n\t.writeResp = {},\n}}", x.time, x.keyResponse, x.newEpochRequest, x.readResp, x.writeResp);
    }
};

#endif // KEY_MANAGER_INCL
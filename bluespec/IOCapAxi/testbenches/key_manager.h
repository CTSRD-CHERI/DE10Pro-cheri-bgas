#include <cstdint>
#include <optional>

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
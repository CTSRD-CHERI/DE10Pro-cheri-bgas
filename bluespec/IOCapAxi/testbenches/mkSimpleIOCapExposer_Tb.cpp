#include <verilated.h>
#include "VmkSimpleIOCapExposer_Tb.h"

#include "key_manager.h"
#include "exposer.h"

#include "tb.h"
#include "capabilities.h"
#include "util.h"

#include <fmt/ranges.h>

#include <random>

struct AxiParams {
    uint64_t address;
    uint8_t transfer_width;
    uint8_t n_transfers;
};

struct CapWithRange {
    CCap2024_02 cap;
    uint64_t cap_base;
    uint64_t cap_len;
    bool cap_is_almighty;

    AxiParams valid_transfer_params(uint8_t transfer_width, uint8_t n_transfers) const {
        if (!cap_is_almighty) {
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error("Bad cap_len");
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error("Bad cap_len");
            }
        }
        return AxiParams {
            .address = cap_base,
            .transfer_width = transfer_width,
            .n_transfers = n_transfers,
        };
    }
};

template<class DUT>
struct ExposerCycleTest : public CycleTest<DUT, ShimmedExposerInput, ShimmedExposerOutput> {
    virtual CapWithRange test_random_initial_resource_cap(const U128& key, uint32_t secret_id, CCapPerms perms) {
        CapWithRange data{};

        data.cap = random_initial_resource_cap(this->rng, key, secret_id, perms);
        if (ccap_read_range(&data.cap, &data.cap_base, &data.cap_len, &data.cap_is_almighty) != Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        
        return data;
    }
};

template<class DUT>
struct ValidKeyValidCapValidWrite : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "Valid-Key Valid-Cap Valid-Write";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        auto cap_data = this->test_random_initial_resource_cap(key, 111, CCapPerms::Write);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
       
        U128 cap128 = U128::from_le(cap_data.cap.data);
        U128 sig128 = U128::from_le(cap_data.cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // Overall the authenticated flit passes through four fifos:
        // - At 145 it passes through a mkSizedBypassFIFOF awIn.out (0 cycle delay)
        // - into awPreCheckBuffer, a mkFIFOF (1 cycle), exits @ 155
        // - the keyResponse applies immediately
        // - into awChecker.checkRequest, a mkFIFOF (1 cycle) exits @ 165
        // It then splits into parallel lanes, because the key was valid:
        // - decodeIn (mkFIFOF, 2 cycle)  - sigCheckIn (mkFIFOF, 1 cycle) exits @ 175
        // - decodeOut (mkFIFOF, 2 cycle) - AES (8? cycles)
        // -           resps (mkFIFOF, 1 cycle) enters @ 255, exits @ 265
        //             we set the perf counter in check_aw and enqueue into awOut @265
        // -           awOut (mkFIFOF, 1 cycle) exits @ 275
        // -           we pick it up at 280
        outputs[270].keyManager.bumpPerfCounterGoodWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < axi_params.n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[400].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[420].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct ValidKeyValidCapValidRead : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "Valid-Key Valid-Cap Valid-Read";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random readable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        auto cap_data = this->test_random_initial_resource_cap(key, 111, CCapPerms::Read);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
       
        U128 cap128 = U128::from_le(cap_data.cap.data);
        U128 sig128 = U128::from_le(cap_data.cap.signature);

        // TODO
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_ar = axi::IOCapAxi::ARFlit_id4_addr64_user3 {
            .aruser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .arlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .araddr = axi_params.address,
            .arid = 0b1011,
        };
        inputs[110].iocap_flit_ar = axi::IOCapAxi::packCap1_ar(cap128, sig128);
        inputs[120].iocap_flit_ar = axi::IOCapAxi::packCap2_ar(cap128, sig128);
        inputs[130].iocap_flit_ar = axi::IOCapAxi::packCap3_ar(cap128, sig128);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        
        // Then it will at some point later shove it out, while bumping the bad-read counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterGoodRead = true;
        outputs[280].clean_flit_ar = axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .arlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .araddr = axi_params.address,
            .arid = 0b1011,
        };

        // Get the given amount of read responses
        for (int i = 0; i < axi_params.n_transfers; i++) {
            inputs[280 + (i * 10)].clean_flit_r = axi::SanitizedAxi::RFlit_id4_data32 {
                .rlast = (i == axi_params.n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
            // R pass through a separate pipeline stage to register completions
            // see rule recv_r
            // => latency = 20
            outputs[280 + 20 + (i * 10)].iocap_flit_r = axi::IOCapAxi::RFlit_id4_data32 {
                .rlast = (i == axi_params.n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
        }

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct ValidReadThenValidWrite : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "Valid-Key Valid-Cap Valid-Read then Valid-Key Valid-Cap Valid-Write";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random read/write capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        auto cap_data = this->test_random_initial_resource_cap(key, 111, CCapPerms::ReadWrite);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
       
        U128 cap128 = U128::from_le(cap_data.cap.data);
        U128 sig128 = U128::from_le(cap_data.cap.signature);
        
        // Send the flits to authenticate the first access
        inputs[100].iocap_flit_ar = axi::IOCapAxi::ARFlit_id4_addr64_user3 {
            .aruser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .arlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .araddr = axi_params.address,
            .arid = 0b1011,
        };
        inputs[110].iocap_flit_ar = axi::IOCapAxi::packCap1_ar(cap128, sig128);
        inputs[120].iocap_flit_ar = axi::IOCapAxi::packCap2_ar(cap128, sig128);
        inputs[130].iocap_flit_ar = axi::IOCapAxi::packCap3_ar(cap128, sig128);

        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);
        // Send the given amount of write data flits
        for (int i = 0; i < axi_params.n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            /// TODO should be blocked until the transaction has been measured as good
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over.
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };

        // Then it will at some point later shove it out, while bumping the counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterGoodRead = true;
        outputs[280].clean_flit_ar = axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .arlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .araddr = axi_params.address,
            .arid = 0b1011,
        };

        // Get the given amount of read responses
        for (int i = 0; i < axi_params.n_transfers; i++) {
            inputs[280 + (i * 10)].clean_flit_r = axi::SanitizedAxi::RFlit_id4_data32 {
                .rlast = (i == axi_params.n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
            // R pass through a separate pipeline stage to register completions
            // see rule recv_r
            // => latency = 20
            outputs[280 + 20 + (i * 10)].iocap_flit_r = axi::IOCapAxi::RFlit_id4_data32 {
                .rlast = (i == axi_params.n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
        }


        // The write starts processing just after the read.
        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[160].keyManager.keyRequest = 111;
        // and we will hand it over.
        inputs[180].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key,
        };
        // Because they are separated by a single cycle, the keyresponse for the *first* access will cover *both* transactions.
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // The delay will be short, because it will realize the key is invalid and short-circuit the loss
        outputs[270].keyManager.bumpPerfCounterGoodWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };

        // Eventually we get a write response
        inputs[400].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[420].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct OOBWrite_Passthrough : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "Valid-Key Valid-Cap OOB-Write - Passthrough";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = initial_resource_cap_exact(key, 0xdeadbeef0000, 0x1000, false, 111, CCapPerms::Write);
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 4;
        // Start in bounds, end OOB
        uint64_t access_base =  0xdeadbeef0000 - 0x1000*2;
        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = access_base,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterBadWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = access_base,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            // TODO shouldn't be passed through, the write was invalid!
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[400].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[420].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct OOBRead_Passthrough : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "Valid-Key Valid-Cap OOB-Read - Passthrough";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = initial_resource_cap_exact(key, 0xdeadbeef0000, 0x1000, false, 111, CCapPerms::Read);
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 4;
        // Start in bounds, end OOB
        uint64_t access_base =  0xdeadbeef0000 - 0x1000*2;

        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_ar = axi::IOCapAxi::ARFlit_id4_addr64_user3 {
            .aruser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(transfer_width),
            .arlen = axi::n_transfers_to_len(n_transfers),
            .araddr = access_base,
            .arid = 0b1011,
        };
        inputs[110].iocap_flit_ar = axi::IOCapAxi::packCap1_ar(cap128, sig128);
        inputs[120].iocap_flit_ar = axi::IOCapAxi::packCap2_ar(cap128, sig128);
        inputs[130].iocap_flit_ar = axi::IOCapAxi::packCap3_ar(cap128, sig128);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterBadRead = true;
        outputs[280].clean_flit_ar = axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(transfer_width),
            .arlen = axi::n_transfers_to_len(n_transfers),
            .araddr = access_base,
            .arid = 0b1011,
        };

        // Get the given amount of read responses
        for (int i = 0; i < n_transfers; i++) {
            inputs[280 + (i * 10)].clean_flit_r = axi::SanitizedAxi::RFlit_id4_data32 {
                .rlast = (i == n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
            // R pass through a separate pipeline stage to register completions
            // see rule recv_r
            // => latency = 20
            outputs[280 + 20 + (i * 10)].iocap_flit_r = axi::IOCapAxi::RFlit_id4_data32 {
                .rlast = (i == n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
        }

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct MismatchedPerms_Passthrough : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "Valid-Key Valid-Cap BadPerms - Passthrough";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        auto cap_data = this->test_random_initial_resource_cap(key, 111, CCapPerms::Read);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
       
        U128 cap128 = U128::from_le(cap_data.cap.data);
        U128 sig128 = U128::from_le(cap_data.cap.signature);
        
        // Attempt a write with a read capability
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterBadWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < axi_params.n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[400].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[420].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct InvalidSig_Passthrough : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "Valid-Key BadSig-Cap Write - Passthrough";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        U128 cap{
            .top = 0x00abcdef'11abcdef,
            .bottom = 0x22abcdef'33abcdef
        };
        U128 sig{
            .top = 0x44abcdef'55abcdef,
            .bottom = 0x66abcdef'77abcdef
        };

        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(32),
            .awlen = axi::n_transfers_to_len(1),
            .awaddr = 0xdeadbeefdeadbeef,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap, sig);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap, sig);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap, sig);
        // This will arrive at 135, exiting the AddressChannelCapUnwrapper at 145

        // The above capability claims to use KeyId 111 (TODO actually put it through a cap lib and check)
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = sig
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // Overall the authenticated flit passes through four fifos:
        // - At 145 it passes through a mkSizedBypassFIFOF awIn.out (0 cycle delay)
        // - into awPreCheckBuffer, a mkFIFOF (1 cycle), exits @ 155
        // - the keyResponse applies immediately
        // - into awChecker.checkRequest, a mkFIFOF (1 cycle) exits @ 165
        // It then splits into parallel lanes, because the key was valid:
        // - decodeIn (mkFIFOF, 2 cycle)  - sigCheckIn (mkFIFOF, 1 cycle) exits @ 175
        // - decodeOut (mkFIFOF, 2 cycle) - AES (8? cycles)
        // -           resps (mkFIFOF, 1 cycle) enters @ 255, exits @ 265
        //             we set the perf counter in check_aw and enqueue into awOut @265
        // -           awOut (mkFIFOF, 1 cycle) exits @ 275
        // -           we pick it up at 280
        outputs[270].keyManager.bumpPerfCounterBadWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(32),
            .awlen = axi::n_transfers_to_len(1),
            .awaddr = 0xdeadbeefdeadbeef,
            .awid = 0b1011,
        };

        inputs[140].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
            .wlast = 1,
            .wstrb = 0b1111,
            .wdata = 0xfefefefe,
        };
        outputs[150].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
            .wlast = 1,
            .wstrb = 0b1111,
            .wdata = 0xfefefefe,
        };

        // Eventually we get a write response
        inputs[400].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[420].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct NewEpoch_NoAccesses : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "New Epoch - No Accesses";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        inputs[100].keyManager.newEpochRequest = 1;
        outputs[120].keyManager.finishedEpoch = 0;

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct NewEpoch_PreAccess : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "New Epoch - Pre-Access";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = random_initial_resource_cap(this->rng, key, 111, CCapPerms::Write);
        uint64_t cap_base = 0;
        uint64_t cap_len = 0;
        bool cap_is_almighty = false;
        if (ccap_read_range(&cap, &cap_base, &cap_len, &cap_is_almighty) != Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 20;
        if (!cap_is_almighty) {
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error("Bad cap_len");
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error("Bad cap_len");
            }
        }

        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // Just before it receives the final flit, it will also receive a new epoch request.
        inputs[120].keyManager.newEpochRequest = 1;
        // It will complete the epoch because it's not currently processing anything.
        // More specifically, it hasn't started requesting or using key manager data which is marked by the epoch.
        outputs[140].keyManager.finishedEpoch = 0;

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over
        // NOTE: THIS MAY BE A VALUE FROM THE CURRENT OR NEXT EPOCH!
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // Overall the authenticated flit passes through four fifos:
        // - At 145 it passes through a mkSizedBypassFIFOF awIn.out (0 cycle delay)
        // - into awPreCheckBuffer, a mkFIFOF (1 cycle), exits @ 155
        // - the keyResponse applies immediately
        // - into awChecker.checkRequest, a mkFIFOF (1 cycle) exits @ 165
        // It then splits into parallel lanes, because the key was valid:
        // - decodeIn (mkFIFOF, 2 cycle)  - sigCheckIn (mkFIFOF, 1 cycle) exits @ 175
        // - decodeOut (mkFIFOF, 2 cycle) - AES (8? cycles)
        // -           resps (mkFIFOF, 1 cycle) enters @ 255, exits @ 265
        //             we set the perf counter in check_aw and enqueue into awOut @265
        // -           awOut (mkFIFOF, 1 cycle) exits @ 275
        // -           we pick it up at 280
        outputs[270].keyManager.bumpPerfCounterGoodWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[600].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[620].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct NewEpoch_SameCycle : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "New Epoch - Same Cycle as Access";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = random_initial_resource_cap(this->rng, key, 111, CCapPerms::Write);
        uint64_t cap_base = 0;
        uint64_t cap_len = 0;
        bool cap_is_almighty = false;
        if (ccap_read_range(&cap, &cap_base, &cap_len, &cap_is_almighty) != Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 20;
        if (!cap_is_almighty) {
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error("Bad cap_len");
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error("Bad cap_len");
            }
        }

        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // When it receives the final flit, it will also receive a new epoch request.
        inputs[130].keyManager.newEpochRequest = 1;
        // It will not respond, completing the finish, until the transaction finishes.

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over
        // NOTE: THIS MAY BE A VALUE FROM THE CURRENT OR NEXT EPOCH!
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // Overall the authenticated flit passes through four fifos:
        // - At 145 it passes through a mkSizedBypassFIFOF awIn.out (0 cycle delay)
        // - into awPreCheckBuffer, a mkFIFOF (1 cycle), exits @ 155
        // - the keyResponse applies immediately
        // - into awChecker.checkRequest, a mkFIFOF (1 cycle) exits @ 165
        // It then splits into parallel lanes, because the key was valid:
        // - decodeIn (mkFIFOF, 2 cycle)  - sigCheckIn (mkFIFOF, 1 cycle) exits @ 175
        // - decodeOut (mkFIFOF, 2 cycle) - AES (8? cycles)
        // -           resps (mkFIFOF, 1 cycle) enters @ 255, exits @ 265
        //             we set the perf counter in check_aw and enqueue into awOut @265
        // -           awOut (mkFIFOF, 1 cycle) exits @ 275
        // -           we pick it up at 280
        outputs[270].keyManager.bumpPerfCounterGoodWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[600].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[620].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[620].keyManager.finishedEpoch = 0;

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct NewEpoch_PostAccess : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "New Epoch - Post-Access";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = random_initial_resource_cap(this->rng, key, 111, CCapPerms::Write);
        uint64_t cap_base = 0;
        uint64_t cap_len = 0;
        bool cap_is_almighty = false;
        if (ccap_read_range(&cap, &cap_base, &cap_len, &cap_is_almighty) != Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 20;
        if (!cap_is_almighty) {
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error("Bad cap_len");
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error("Bad cap_len");
            }
        }

        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // Just after it receives the final flit, it will also receive a new epoch request.
        inputs[140].keyManager.newEpochRequest = 1;
        // The request is in progress => won't complete the epoch until

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over
        // NOTE: THIS MAY BE A VALUE FROM THE CURRENT OR NEXT EPOCH!
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // Overall the authenticated flit passes through four fifos:
        // - At 145 it passes through a mkSizedBypassFIFOF awIn.out (0 cycle delay)
        // - into awPreCheckBuffer, a mkFIFOF (1 cycle), exits @ 155
        // - the keyResponse applies immediately
        // - into awChecker.checkRequest, a mkFIFOF (1 cycle) exits @ 165
        // It then splits into parallel lanes, because the key was valid:
        // - decodeIn (mkFIFOF, 2 cycle)  - sigCheckIn (mkFIFOF, 1 cycle) exits @ 175
        // - decodeOut (mkFIFOF, 2 cycle) - AES (8? cycles)
        // -           resps (mkFIFOF, 1 cycle) enters @ 255, exits @ 265
        //             we set the perf counter in check_aw and enqueue into awOut @265
        // -           awOut (mkFIFOF, 1 cycle) exits @ 275
        // -           we pick it up at 280
        outputs[270].keyManager.bumpPerfCounterGoodWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[600].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[620].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[620].keyManager.finishedEpoch = 0;

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct NewEpoch_BetweenAccesses : public ExposerCycleTest<DUT> {
    virtual std::string_view name() override {
        return "Valid-Key Valid-Cap Valid-Read; New Epoch; Invalid-Key Write - Passthrough";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = random_initial_resource_cap(this->rng, key, 111, CCapPerms::ReadWrite);
        uint64_t cap_base = 0;
        uint64_t cap_len = 0;
        bool cap_is_almighty = false;
        if (ccap_read_range(&cap, &cap_base, &cap_len, &cap_is_almighty) != Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 20;
        if (!cap_is_almighty) {
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error("Bad cap_len");
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error("Bad cap_len");
            }
        }

        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the first access
        inputs[100].iocap_flit_ar = axi::IOCapAxi::ARFlit_id4_addr64_user3 {
            .aruser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(transfer_width),
            .arlen = axi::n_transfers_to_len(n_transfers),
            .araddr = cap_base,
            .arid = 0b1011,
        };
        inputs[110].iocap_flit_ar = axi::IOCapAxi::packCap1_ar(cap128, sig128);
        inputs[120].iocap_flit_ar = axi::IOCapAxi::packCap2_ar(cap128, sig128);
        inputs[130].iocap_flit_ar = axi::IOCapAxi::packCap3_ar(cap128, sig128);

        inputs[100 + 10].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };
        inputs[110 + 10].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120 + 10].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130 + 10].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);
        // Send the given amount of write data flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            /// TODO should be blocked until the transaction has been measured as good
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // When it receives the final flit of the first access, just before the final flit of the second access,
        // it will also receive a new epoch request.
        inputs[130].keyManager.newEpochRequest = 1;
        // This epoch will not complete until the *first* has completed, and the second will be delayed until the epoch completes

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over
        // NOTE: THIS MAY BE A VALUE FROM THE CURRENT OR NEXT EPOCH!
        // For the first access, return *valid*.
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterGoodRead = true;
        outputs[280].clean_flit_ar = axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(transfer_width),
            .arlen = axi::n_transfers_to_len(n_transfers),
            .araddr = cap_base,
            .arid = 0b1011,
        };

        // Get the given amount of read responses
        for (int i = 0; i < n_transfers; i++) {
            inputs[280 + (i * 10)].clean_flit_r = axi::SanitizedAxi::RFlit_id4_data32 {
                .rlast = (i == n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
            // R pass through a separate pipeline stage to register completions
            // see rule recv_r
            // => latency = 20
            outputs[280 + 20 + (i * 10)].iocap_flit_r = axi::IOCapAxi::RFlit_id4_data32 {
                .rlast = (i == n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
        }
        // Only once the .rlast read has arrived has the transaction, and the epoch, completed.
        outputs[280 + 20 + ((n_transfers - 1) * 10)].keyManager.finishedEpoch = 0;
        // The write can now be processed by the exposer.
        uint64_t write_check_start = 280 + 20 + ((n_transfers - 1) * 10);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[write_check_start + 10].keyManager.keyRequest = 111;
        // and we will hand it over
        // NOTE: THIS MAY BE A VALUE FROM THE CURRENT OR NEXT EPOCH!
        // For the second access, return *invalid*.
        inputs[write_check_start + 10 + 20].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = std::nullopt,
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // The delay will be short, because it will realize the key is invalid and short-circuit the loss
        outputs[write_check_start + 60].keyManager.bumpPerfCounterBadWrite = true;
        // TODO shouldn't come out
        outputs[write_check_start + 70].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };

        // Eventually we get a write response
        inputs[write_check_start + 200].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[write_check_start + 220].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

// Template for further test creation

// template<class DUT>
// struct TODO : public ExposerCycleTest<DUT> {
//     virtual std::string_view name() override {
//         return "TODO";
//     }
//     virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
//         ShimmedExposerInputsMaker inputs;
//         ShimmedExposerOutputsMaker outputs;
//
//         // TODO
//
//         return {inputs.asVec(), outputs.asVec()};
//     }
// };

int main(int argc, char** argv) {
    int success = EXIT_SUCCESS;

    std::vector<ExposerCycleTest<VmkSimpleIOCapExposer_Tb>*> tests = {
        // Test valid caps are accepted
        new ValidKeyValidCapValidWrite<VmkSimpleIOCapExposer_Tb>(),
        new ValidKeyValidCapValidRead<VmkSimpleIOCapExposer_Tb>(),
        new ValidReadThenValidWrite<VmkSimpleIOCapExposer_Tb>(),
        // TODO Test caps with invalid keys are rejected
        // TODO test valid cap with 1 cav
        // TODO test valid cap with 2 cav
        // Test valid cap with out-of-cap-bounds access
        new OOBWrite_Passthrough<VmkSimpleIOCapExposer_Tb>(),
        new OOBRead_Passthrough<VmkSimpleIOCapExposer_Tb>(),
        // Test valid cap with mismatched perms
        new MismatchedPerms_Passthrough<VmkSimpleIOCapExposer_Tb>(),
        // Test invalid caps (i.e. bad signatures) with valid keys are rejected
        new InvalidSig_Passthrough<VmkSimpleIOCapExposer_Tb>(),

        // TODO test that invalid caps don't let their flits through (contingent on switch flip)
        // TODO test the above for reads and writes

        // TODO test inbalanced completions > starts behaviour

        // Test new-epoch when no accesses are pending
        new NewEpoch_NoAccesses<VmkSimpleIOCapExposer_Tb>(),
        // Test new-epoch when an access hasn't started checking
        new NewEpoch_PreAccess<VmkSimpleIOCapExposer_Tb>(),
        // New-epoch while processing an access (either at the same cycle as the access starts, or while it's processing)
        // will delay the completion of the epoch to after the access completes
        new NewEpoch_SameCycle<VmkSimpleIOCapExposer_Tb>(),
        new NewEpoch_PostAccess<VmkSimpleIOCapExposer_Tb>(),
        // New-Epoch between accesses that arrive on consecutive cycles will delay the second until the first has completed
        new NewEpoch_BetweenAccesses<VmkSimpleIOCapExposer_Tb>(),
        // TODO test the above for reads and writes
    };
    for (auto* test : tests) {
        if (!test->run(argc, argv)) {
            success = EXIT_FAILURE;
        }
    }

    return success;
}
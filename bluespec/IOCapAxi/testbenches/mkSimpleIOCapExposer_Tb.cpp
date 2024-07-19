#include <verilated.h>
#include "VmkSimpleIOCapExposer_Tb.h"

#include "key_manager.h"
#include "exposer.h"

#include "tb.h"
#include "capabilities.h"
#include "util.h"

#include <fmt/ranges.h>

#include <random>

using ExposerCycleTest = CycleTest<VmkSimpleIOCapExposer_Tb, ShimmedExposerInput, ShimmedExposerOutput>;

int main(int argc, char** argv) {
    int success = EXIT_SUCCESS;

    // Test caps with invalid keys are rejected

    // Test valid caps are accepted
    {
        TestParams params = TestParams {
            .testName = "Test Valid-Key Valid-Cap Write - Passthrough",
            .argc = argc,
            .argv = argv,
             // Run for 1k cycles
            .endTime = 1000 * 10
        };

        uint32_t seed = 10298293;
        std::mt19937 rng{seed};

        ShimmedExposerInputsMaker inputs{};
        ShimmedExposerOutputsMaker outputs{};

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(rng);
        CCap2024_02 cap = random_initial_resource_cap(rng, key, 111, CCapPerms::Write);
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

        fmt::println("U128 key = {}", key);
        fmt::println("cap.data = {:02x}", fmt::join(cap.data, ", "));
        fmt::println("U128 cap.data = {}", cap128);
        fmt::println("cap.sig = {:02x}", fmt::join(cap.signature, ", "));
        fmt::println("U128 cap.sig = {}", sig128);
        
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
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = 1,
                .wstrb = 0b1111,
                .wdata = 0xfefefefe,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = 1,
                .wstrb = 0b1111,
                .wdata = 0xfefefefe,
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

        if (!ExposerCycleTest(params, inputs.asVec(), outputs.asVec()).run()) {
            success = EXIT_FAILURE;
        }
    }

    // Test invalid caps with valid keys are rejected
    {
        TestParams params = TestParams {
            .testName = "Test Valid-Key Invalid-Cap Write - Passthrough",
            .argc = argc,
            .argv = argv,
             // Run for 1k cycles
            .endTime = 1000 * 10
        };
        ShimmedExposerInputsMaker inputs{};
        ShimmedExposerOutputsMaker outputs{};

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

        if (!ExposerCycleTest(params, inputs.asVec(), outputs.asVec()).run()) {
            success = EXIT_FAILURE;
        }
    }

    // TODO test that invalid caps don't let their flits through (contingent on switch flip)
    // TODO test valid cap
    // TODO test valid cap with 1 cav
    // TODO test valid cap with 2 cav
    // TODO test valid cap with out-of-cap-bounds access
    // TODO test valid cap with mismatched perms
    // TODO test the above for reads and writes

    // TODO test inbalanced completions > starts

    // TODO test revocation when no accesses are pending
    // TODO test revocation when accesses are pending but not using the key
    // TODO test revocation when an access hasn't started checking but uses the same key
    // TODO test revocation when an access has started checking but uses the same key
    // TODO test the above for reads and writes


    // {
    //     TestParams params = TestParams {
    //         .testName = ,
    //         .argc = argc,
    //         .argv = argv,
    //          // Run for 1k cycles
    //         .endTime = 1000 * 10
    //     };
    //     ShimmedExposerInputsMaker inputs{};
    //     ShimmedExposerOutputsMaker outputs{};

    //     if (!ExposerCycleTest(params, inputs.asVec(), outputs.asVec()).run()) {
    //         success = EXIT_FAILURE;
    //     }
    // }

    return success;
}
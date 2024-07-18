#include <verilated.h>
#include "VmkSimpleIOCapExposer_Tb.h"

#include "key_manager.h"
#include "exposer.h"

#include "tb.h"
#include "util.h"

#include <random>

using ExposerCycleTest = CycleTest<VmkSimpleIOCapExposer_Tb, ShimmedExposerInput, ShimmedExposerOutput>;

int main(int argc, char** argv) {
    int success = EXIT_SUCCESS;

    {
        TestParams params = TestParams {
            .testName = "Test Passthrough Invalid Write",
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
        inputs[140].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
            .wlast = 1,
            .wstrb = 0b1111,
            .wdata = 0xfefefefe,
        };

        if (!ExposerCycleTest(params, inputs.asVec(), outputs.asVec()).run()) {
            success = EXIT_FAILURE;
        }
    }

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
#include <verilated.h>
#include "VmkCombinedIOCapExposerV6_blockinvalid_2pool_1percycle_KeyManager2V1_Tb.h"

#include "exposer_tests_uvm.h"
#include "exposer_tests_legacy.h"

using TheDUT = VmkCombinedIOCapExposerV6_blockinvalid_2pool_1percycle_KeyManager2V1_Tb;
constexpr bool expectPassthroughInvalidTransactions = false;

int main(int argc, char** argv) {
    auto tests = basicExposerUvmTests<TheDUT, CapType::Cap2024_11, KeyMngrV2_AsDUT_MMIO32>(expectPassthroughInvalidTransactions);

    return tb_main(tests, argc, argv);
}
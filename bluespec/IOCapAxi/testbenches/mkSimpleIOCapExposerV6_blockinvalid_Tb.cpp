#include <verilated.h>
#include "VmkSimpleIOCapExposerV6_blockinvalid_Tb.h"

#include "exposer_tests_uvm.h"
#include "exposer_tests_legacy.h"

using TheDUT = VmkSimpleIOCapExposerV6_blockinvalid_Tb;
constexpr bool expectPassthroughInvalidTransactions = false;

int main(int argc, char** argv) {
    auto tests = basicExposerUvmTests<TheDUT, CapType::Cap2024_11, KeyMngrV2>(expectPassthroughInvalidTransactions);

    return tb_main(tests, argc, argv);
}
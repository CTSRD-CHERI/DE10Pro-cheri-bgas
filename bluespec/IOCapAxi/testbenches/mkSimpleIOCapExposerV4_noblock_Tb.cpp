#include <verilated.h>
#include "VmkSimpleIOCapExposerV4_noblock_Tb.h"

#include "exposer_tests_uvm.h"
#include "exposer_tests_legacy.h"

using TheDUT = VmkSimpleIOCapExposerV4_noblock_Tb;
constexpr bool expectPassthroughInvalidTransactions = true;

int main(int argc, char** argv) {
    auto tests = basicExposerUvmTests<TheDUT, CapType::Cap2024_11, KeyMngrV1>(expectPassthroughInvalidTransactions);

    return tb_main(tests, argc, argv);
}
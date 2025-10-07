#include <verilated.h>
#include "VmkSimpleIOCapExposerV5_blockinvalid_Tb.h"

#include "exposer_tests_uvm.h"
#include "exposer_tests_legacy.h"

using TheDUT = VmkSimpleIOCapExposerV5_blockinvalid_Tb;
constexpr bool expectPassthroughInvalidTransactions = false;

int main(int argc, char** argv) {
    std::vector<TestBase*> tests = {
        // UVM-style testing
        // TODO add tests for above todos, consider revocation
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_11, KeyMngrV2>(CCapPerms_Read),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_11, KeyMngrV2>(CCapPerms_Write),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_11, KeyMngrV2>(CCapPerms_ReadWrite),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapOOBAccess<TheDUT, CapType::Cap2024_11, KeyMngrV2>(CCapPerms_Read),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapOOBAccess<TheDUT, CapType::Cap2024_11, KeyMngrV2>(CCapPerms_Write),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidCapBadPerms<TheDUT, CapType::Cap2024_11, KeyMngrV2>(),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyBadSigCap<TheDUT, CapType::Cap2024_11, KeyMngrV2>(),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMInvalidKeyAccess<TheDUT, CapType::Cap2024_11, KeyMngrV2>(CCapPerms_ReadWrite),
            expectPassthroughInvalidTransactions
        ),
        // // 5 cycles of revocations
        // // TODO test this with valid and invalid transactions!
        // // TODO figure out how to do more accurate UVM revocation testing
        // new ExposerUVMishTest(
        //     new UVMTransactionsBetweenRevocations_KeyMngrV2<TheDUT, CapType::Cap2024_11>(5),
        //     expectPassthroughInvalidTransactions
        // ),

        new ExposerUVMishTest(
            new UVMStreamOfNValidTransactions<TheDUT, CapType::Cap2024_11, KeyMngrV2>(CCapPerms_ReadWrite, 100),
            expectPassthroughInvalidTransactions
        ),

        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, CapType::Cap2024_11, KeyMngrV2>(100'000),
            expectPassthroughInvalidTransactions
        ),

        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, CapType::Cap2024_11, KeyMngrV2>(10'000, /* n_cavs */ 0),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, CapType::Cap2024_11, KeyMngrV2>(10'000, /* n_cavs */ 1),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMStreamOfNLibRustValidTransactions<TheDUT, CapType::Cap2024_11, KeyMngrV2>(10'000, /* n_cavs */ 2),
            expectPassthroughInvalidTransactions
        ),
    };

    for (auto edge_case = 0; edge_case < ccap2024_11_rand_edge_case_num(); edge_case++) {
        tests.push_back(
            new ExposerUVMishTest(
                new UVMStreamOfNLibRustEdgeCaseTransactions<TheDUT, CapType::Cap2024_11, KeyMngrV2>(10'000, edge_case),
                expectPassthroughInvalidTransactions
            )
        );
    }

    return tb_main(tests, argc, argv);
}
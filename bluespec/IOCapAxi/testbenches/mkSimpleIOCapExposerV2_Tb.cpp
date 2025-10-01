#include <verilated.h>
#include "VmkSimpleIOCapExposerV2_Tb.h"

#include "exposer_tests_uvm.h"
#include "exposer_tests_legacy.h"

using TheDUT = VmkSimpleIOCapExposerV2_Tb;

int main(int argc, char** argv) {
    std::vector<TestBase*> tests = {
        // Test valid caps are accepted - DONE below
        // new ValidKeyValidCapValidWrite<TheDUT>(),
        // new ValidKeyValidCapValidRead<TheDUT>(),
        // new ValidReadThenValidWrite<TheDUT>(),
        // TODO Test caps with invalid keys are rejected - DONE below
        // Test valid cap with out-of-cap-bounds access - DONE below
        // new OOBWrite_Passthrough<TheDUT>(),
        // new OOBRead_Passthrough<TheDUT>(),
        // Test valid cap with mismatched perms - DONE below
        // new MismatchedPerms_Passthrough<TheDUT>(),
        // Test invalid caps (i.e. bad signatures) with valid keys are rejected - DONE below
        // new InvalidSig_Passthrough<TheDUT>(),

        // TODO test valid cap with 1 cav
        // TODO test valid cap with 2 cav
        // TODO test inbalanced completions > starts behaviour

        /*

        // Test new-epoch when no accesses are pending
        new NewEpoch_NoAccesses<TheDUT>(),
        // Test new-epoch when an access hasn't started checking
        new NewEpoch_PreAccess<TheDUT>(),
        // New-epoch while processing an access (either at the same cycle as the access starts, or while it's processing)
        // will delay the completion of the epoch to after the access completes
        new NewEpoch_SameCycle<TheDUT>(),
        new NewEpoch_PostAccess<TheDUT>(),
        // New-Epoch between accesses that arrive on consecutive cycles will delay the second until the first has completed
        new NewEpoch_BetweenAccesses<TheDUT>(),
        // TODO test the above for reads and writes

        */



        // UVM-style testing
        // TODO add tests for above todos, consider revocation
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_Read)
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_Write)
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_ReadWrite)
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapOOBAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_Read)
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapOOBAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_Write)
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidCapBadPerms<TheDUT, CapType::Cap2024_02, KeyMngrV1>()
        ),
        new ExposerUVMishTest(
            new UVMValidKeyBadSigCap<TheDUT, CapType::Cap2024_02, KeyMngrV1>()
        ),
        new ExposerUVMishTest(
            new UVMInvalidKeyAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_ReadWrite)
        ),
        // 5 cycles of revocations
        // TODO test this with valid and invalid transactions!
        // TODO figure out how to do more accurate UVM revocation testing
        new ExposerUVMishTest(
            new UVMTransactionsBetweenRevocations_KeyMngrV1<TheDUT, CapType::Cap2024_02>(5)
        ),

        new ExposerUVMishTest(
            new UVMStreamOfNValidTransactions<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_ReadWrite, 100)
        )
    };

    return tb_main(tests, argc, argv);
}
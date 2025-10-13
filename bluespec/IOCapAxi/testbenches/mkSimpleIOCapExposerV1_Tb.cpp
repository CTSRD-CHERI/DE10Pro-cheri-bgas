#include <verilated.h>
#include "VmkSimpleIOCapExposerV1_Tb.h"

#include "exposer_tests_uvm.h"
#include "exposer_tests_legacy.h"

using TheDUT = VmkSimpleIOCapExposerV1_Tb;

TestBase* exposerUVMishTestAssumeInvalidPassthrough(ExposerStimulus<TheDUT, CapType::Cap2024_02, KeyMngrV1>* stimulus) {
    return new ExposerUVMishTest<TheDUT, CapType::Cap2024_02, KeyMngrV1>(stimulus, /* expectPassthroughInvalidTransactions = */ true);

}

int main(int argc, char** argv) {
    std::vector<TestBase*> tests = {
        // Test valid caps are accepted
        new ValidKeyValidCapValidWrite<TheDUT>(),
        new ValidKeyValidCapValidRead<TheDUT>(),
        new ValidReadThenValidWrite<TheDUT>(),
        // UVM-style testing
        exposerUVMishTestAssumeInvalidPassthrough(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_Read)
        ),
        exposerUVMishTestAssumeInvalidPassthrough(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_Write)
        ),
        exposerUVMishTestAssumeInvalidPassthrough(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_ReadWrite)
        ),
    
        // Test caps with invalid keys are rejected
        exposerUVMishTestAssumeInvalidPassthrough(
            new UVMInvalidKeyAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_ReadWrite)
        ),

        // TODO test valid cap with 1 cav
        // TODO test valid cap with 2 cav
        // Test valid cap with out-of-cap-bounds access - Assume pasthrough
        new OOBWrite_Passthrough<TheDUT>(),
        new OOBRead_Passthrough<TheDUT>(),

        exposerUVMishTestAssumeInvalidPassthrough(
            new UVMValidKeyValidInitialCapOOBAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_Read)
        ),
        exposerUVMishTestAssumeInvalidPassthrough(
            new UVMValidKeyValidInitialCapOOBAccess<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_Write)
        ),

        // Test valid cap with mismatched perms - DONE below
        new MismatchedPerms_Passthrough<TheDUT>(),
        exposerUVMishTestAssumeInvalidPassthrough(
            new UVMValidKeyValidCapBadPerms<TheDUT, CapType::Cap2024_02, KeyMngrV1>()
        ),

        // Test invalid caps (i.e. bad signatures) with valid keys are rejected - DONE below
        new InvalidSig_Passthrough<TheDUT>(),
        exposerUVMishTestAssumeInvalidPassthrough(
            new UVMValidKeyBadSigCap<TheDUT, CapType::Cap2024_02, KeyMngrV1>()
        ),

        // TODO test inbalanced completions > starts behaviour

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

        // 5 cycles of revocations
        // TODO test this with valid and invalid transactions!
        exposerUVMishTestAssumeInvalidPassthrough(
            new UVMTransactionsBetweenRevocations_KeyMngrV1<TheDUT, CapType::Cap2024_02>(5)
        ),

        exposerUVMishTestAssumeInvalidPassthrough(
            new UVMStreamOfNValidTransactions<TheDUT, CapType::Cap2024_02, KeyMngrV1>(CCapPerms_ReadWrite, 100)
        )
    };

    // TODO upgrade to new test suite?

    return tb_main(tests, argc, argv);
}
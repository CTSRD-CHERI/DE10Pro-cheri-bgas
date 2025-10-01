#include <verilated.h>
#include "VmkIOCapAxi_KeyManager2_RefCountPipe_TwoValve_Tb.h"

#include "key_manager2.h"
#include "tb.h"
#include "util.h"

#include <random>

using namespace key_manager2::refcountpipe;

template<class DUT>
using RefCountPipe2ValveCycleTest = CycleTest<DUT, RefCountPipe_2Valves_Input, RefCountPipe_Output>;

// TODO this DEFINITELY won't work for more complex setups with a cycle test. output too unpredictable + doesn't matter anyway + L + ratio

template<class DUT>
struct IncDecRefcountTest : public RefCountPipe2ValveCycleTest<DUT> {
    virtual std::string name() override {
        return "Send increment and decrement events for keys and watch for revocation events";
    }
    virtual std::pair<RefCountPipe_2Valve_Inputs, RefCountPipe_Outputs> stimuli() {
        RefCountPipe_2Valve_InputsMaker inputs;
        RefCountPipe_OutputsMaker outputs;

        // It should take 128 cycles to zero-init the BRAM.
        constexpr size_t START = 150 * 10;

        // increment 5x
        inputs[START + 00].valve1.keyIncrementRefcountRequest = 1;
        inputs[START + 10].valve0.keyIncrementRefcountRequest = 1;
        inputs[START + 20].valve1.keyIncrementRefcountRequest = 1;
        inputs[START + 30].valve0.keyIncrementRefcountRequest = 1;
        inputs[START + 40].valve1.keyIncrementRefcountRequest = 1;
        
        // decrement 5x, overlapping with increment
        inputs[START + 20].valve0.keyDecrementRefcountRequest = 1;
        inputs[START + 30].valve1.keyDecrementRefcountRequest = 1;
        inputs[START + 40].valve0.keyDecrementRefcountRequest = 1;
        inputs[START + 50].valve1.keyDecrementRefcountRequest = 1;
        inputs[START + 60].valve0.keyDecrementRefcountRequest = 1;

        // get notified it has died
        outputs[START + 120].keyStatus.tryConfirmingRevokeKey = 1;
        
        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct HandleIncBeforeDecOrRevokeTest : public RefCountPipe2ValveCycleTest<DUT> {
    virtual std::string name() override {
        return "Check increment events take priority over decrement and revocation events";
    }
    virtual std::pair<RefCountPipe_2Valve_Inputs, RefCountPipe_Outputs> stimuli() {
        RefCountPipe_2Valve_InputsMaker inputs;
        RefCountPipe_OutputsMaker outputs;

        // It should take 128 cycles to zero-init the BRAM.
        constexpr size_t START = 150 * 10;

        // Increment and Revoke-Check the same key in the same cycle - we should NOT get a tryConfirmingRevokeKey event.
        inputs[START + 00].valve0.keyIncrementRefcountRequest = 1;
        inputs[START + 00].keyStatus.keyToStartRevoking = 1;

        // Then, Decrement the key - we SHOULD get a tryConfirmRevokingKey
        inputs[START + 100].valve0.keyDecrementRefcountRequest = 1;
        // get notified it has died
        outputs[START + 130].keyStatus.tryConfirmingRevokeKey = 1;

        // Increment, Decrement, and Revoke-Check the same key in the same cycle - we should get TWO tryConfirmingRevokeKey events, one for the revoke-check and one for the decrement.
        inputs[START + 200].valve0.keyIncrementRefcountRequest = 1;
        inputs[START + 200].valve0.keyDecrementRefcountRequest = 1;
        inputs[START + 200].keyStatus.keyToStartRevoking = 1;
        // get notified it has died
        outputs[START + 240].keyStatus.tryConfirmingRevokeKey = 1;
        outputs[START + 250].keyStatus.tryConfirmingRevokeKey = 1;
        
        return {inputs.asVec(), outputs.asVec()};
    }
};

// Template for further test creation

// struct TODO : public RefCountPipe2ValveCycleTest {
//     virtual std::string name() override {
//         return "TODO";
//     }
//     virtual std::pair<KeyManagerInputs, KeyManagerOutputs> stimuli() {
//         KeyManagerInputsMaker inputs;
//         KeyManagerOutputsMaker outputs;
//
//         // It should take 128 cycles to zero-init the BRAM.
//         constexpr size_t START = 150 * 10;
//         // TODO
//
//         return {inputs.asVec(), outputs.asVec()};
//     }
// };

int main(int argc, char** argv) {
    return tb_main(
        {
            new IncDecRefcountTest<VmkIOCapAxi_KeyManager2_RefCountPipe_TwoValve_Tb>(),
            new HandleIncBeforeDecOrRevokeTest<VmkIOCapAxi_KeyManager2_RefCountPipe_TwoValve_Tb>(),
        },
        argc, argv
    );
}
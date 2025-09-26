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

        for (int i = 0; i < 1; i += 1) {
            // increment 5x
            inputs[100 + (i * 70) + 00].valve0.keyIncrementRefcountRequest = i * 2;
            inputs[100 + (i * 70) + 10].valve0.keyIncrementRefcountRequest = i * 2;
            inputs[100 + (i * 70) + 20].valve0.keyIncrementRefcountRequest = i * 2;
            inputs[100 + (i * 70) + 30].valve0.keyIncrementRefcountRequest = i * 2;
            inputs[100 + (i * 70) + 40].valve0.keyIncrementRefcountRequest = i * 2;
            
            // decrement 5x, overlapping with increment
            inputs[100 + (i * 70) + 20].valve0.keyDecrementRefcountRequest = i * 2;
            inputs[100 + (i * 70) + 30].valve0.keyIncrementRefcountRequest = i * 2;
            inputs[100 + (i * 70) + 40].valve0.keyIncrementRefcountRequest = i * 2;
            inputs[100 + (i * 70) + 50].valve0.keyIncrementRefcountRequest = i * 2;
            inputs[100 + (i * 70) + 60].valve0.keyIncrementRefcountRequest = i * 2;

            // get notified it has died
            outputs[100 + (i * 70) + 90].keyStatus.tryConfirmingRevokeKey = i * 2;
        }

        for (int i = 0; i < 1; i += 1) {
            // increment 5x
            inputs[100 + (i * 70) + 20 + 00].valve1.keyIncrementRefcountRequest = (i * 2) + 1;
            inputs[100 + (i * 70) + 20 + 10].valve1.keyIncrementRefcountRequest = (i * 2) + 1;
            inputs[100 + (i * 70) + 20 + 20].valve1.keyIncrementRefcountRequest = (i * 2) + 1;
            inputs[100 + (i * 70) + 20 + 30].valve1.keyIncrementRefcountRequest = (i * 2) + 1;
            inputs[100 + (i * 70) + 20 + 40].valve1.keyIncrementRefcountRequest = (i * 2) + 1;
            
            // decrement 5x, overlapping with increment
            inputs[100 + (i * 70) + 20 + 20].valve1.keyDecrementRefcountRequest = (i * 2) + 1;
            inputs[100 + (i * 70) + 20 + 30].valve1.keyIncrementRefcountRequest = (i * 2) + 1;
            inputs[100 + (i * 70) + 20 + 40].valve1.keyIncrementRefcountRequest = (i * 2) + 1;
            inputs[100 + (i * 70) + 20 + 50].valve1.keyIncrementRefcountRequest = (i * 2) + 1;
            inputs[100 + (i * 70) + 20 + 60].valve1.keyIncrementRefcountRequest = (i * 2) + 1;

            // get notified it has died
            outputs[100 + (i * 70) + 90].keyStatus.tryConfirmingRevokeKey = i * 2;
        }
        
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
//         // TODO
//
//         return {inputs.asVec(), outputs.asVec()};
//     }
// };

int main(int argc, char** argv) {
    return tb_main(
        {
            new IncDecRefcountTest<VmkIOCapAxi_KeyManager2_RefCountPipe_TwoValve_Tb>(),
            new IncDecRefcountTest<VmkIOCapAxi_KeyManager2_RefCountPipe_TwoValve_Tb>(),
        },
        argc, argv
    );
}
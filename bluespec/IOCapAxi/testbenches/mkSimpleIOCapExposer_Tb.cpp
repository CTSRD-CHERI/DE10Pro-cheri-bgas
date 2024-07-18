#include <verilated.h>
#include "VmkSimpleIOCapExposer_Tb.h"

#include "key_manager.h"
#include "exposer.h"

#include "tb.h"
#include "util.h"

#include <random>

using ExposerCycleTest = CycleTest<VmkSimpleIOCapExposer_Tb, ShimmedExposerInput, ShimmedExposerOutput>;

int main(int argc, char** argv) {
    ExposerCycleTest(TestParams {
        .testName = "blah",
        .argc = argc,
        .argv = argv,
        .endTime = 1000*10,
    }, {}, {}).run();
    return EXIT_SUCCESS;
}
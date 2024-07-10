#include <verilated.h>
#include "VmkSimpleIOCapKeyManager_Tb.h"

int main(int argc, char** argv) {
    Verilated::commandArgs (argc, argv);    // remember args
    
    {
        // Make a design-under-test
        VmkSimpleIOCapKeyManager_Tb dut{};

        uint64_t main_time = 0;
        // initial conditions in order to generate appropriate edges on
        // reset
        dut.RST_N = 1;
        dut.CLK = 0;

        while (!Verilated::gotFinish()) { // $finish executed
            if (main_time == 2) {
                dut.RST_N = 0;    // assert reset
            }
            else if (main_time == 7) {
                dut.RST_N = 1;    // Deassert reset
            }

            // Toggle clock
            if ((main_time % 10) == 5) {
                dut.CLK = 1;
            }
            else if ((main_time % 10) == 0) {
                dut.CLK = 0;
            }

            dut.eval();
            main_time++;
        }

        dut.final();    // Done simulating
        // end of DUT lifetime, gets destructed
    }
}
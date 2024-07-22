#ifndef TB_H
#define TB_H

#include <verilated.h>
#include <cstdint>
#include <vector>
#include <random>

#define FMT_HEADER_ONLY
#include "fmt/format.h"

#include "dtl/dtl.hpp"

#include "util.h"

struct TestBase {
    virtual ~TestBase() = default;

    virtual bool run(int argc, char** argv) = 0;
};

template<class T>
concept ValidTbStim = requires(T out) {
    {out.time} -> std::convertible_to<uint64_t>;
    {out == out} -> std::convertible_to<bool>;
} && fmt::formattable<T>;

template<class DUT, class TbInput, class TbOutput>
    requires ValidTbStim<TbInput> && ValidTbStim<TbOutput> && requires(DUT dut, TbInput in, TbOutput out) {
        {push_input(dut, in)} -> std::same_as<void>;
        {pull_output(dut, out)} -> std::same_as<void>;
    }
struct CycleTest : TestBase {
    std::mt19937 rng;

    CycleTest(int seed = 123908104) : rng(seed) {}
    virtual ~CycleTest() override = default;

    virtual std::string_view name() = 0;
    virtual std::pair<std::vector<TbInput>, std::vector<TbOutput>> stimuli() = 0;

    /**
     * Create and run a DUT using this test's parameters
     */
    std::vector<TbOutput> execute(std::vector<TbInput> inputs, uint64_t end_time, int argc, char** argv) {
        // Step through the input vector in order
        size_t input_idx = 0;

        auto outputs = std::vector<TbOutput>();

        {
            VerilatedContext ctx{};
            ctx.commandArgs(argc, argv);    // remember args
            // Make a design-under-test
            DUT dut{&ctx};

            uint64_t main_time = 0;
            // initial conditions in order to generate appropriate edges on
            // reset
            dut.RST_N = 1;
            dut.CLK = 0;

            while ((!ctx.gotFinish()) && (main_time <= end_time) ) { // $finish executed
                if (main_time == 2) {
                    dut.RST_N = 0;    // assert reset
                }
                else if (main_time == 7) {
                    dut.RST_N = 1;    // deassert reset
                }

                // Toggle clock - goes up at 5, 15, 25, 35...
                if ((main_time % 10) == 5) {
                    dut.CLK = 1;
                }
                // Goes down at 10, 20, 30, 40...
                else if ((main_time % 10) == 0) {
                    dut.CLK = 0;
                    // ... and we set the inputs and pull outputs at this time too.

                    // Gather input. By default apply a null input.
                    TbInput input{};

                    // If the next input in the queue has a .time value set to the current time,
                    // use that instead.
                    if (input_idx >= inputs.size()) {
                        // Don't pull any more inputs
                    } else if (inputs[input_idx].time == main_time) {
                        input = inputs[input_idx];
                        input_idx++;
                    } else if (inputs[input_idx].time < main_time) {
                        throw std::runtime_error("Input had out-of-order indices");
                    }

                    // Actually apply the input to the DUT.
                    push_input(dut, input);
                    
                    // Now pull out the outputs.
                    TbOutput output{};
                    output.time = main_time;
                    pull_output(dut, output);

                    // Only remember non-zero outputs
                    if (output.is_notable()) {
                        outputs.push_back(std::move(output));
                    }
                }

                dut.eval();
                main_time++;
            }

            dut.final();    // Done simulating
            // end of DUT lifetime, gets destructed
            // end of ctx lifetime, gets destructed
        }

        return outputs;
    }

    /**
     * Check a DUT produces the given outputs when stimulated with the given inputs.
     * Returns true if it did, false if it didn't.
     * Prints a diff of the outputs if it didn't match.
     */
    virtual bool run(int argc, char** argv) override {
        fmt::println(stderr, "\033[1;33mTest: {}\033[0m", name());

        auto [inputs, expectedOutputs] = stimuli();
        uint64_t end_time = 0;
        if (inputs.size() > 0) {
            end_time = inputs[inputs.size() - 1].time;
        }
        if (expectedOutputs.size() > 0) {
            end_time = std::max(end_time, expectedOutputs[expectedOutputs.size() - 1].time);
        }
        auto outputs = execute(inputs, end_time, argc, argv);

        if (expectedOutputs == outputs) {
            fmt::println(stderr, "\033[1;32mTest-Success\033[0m");
            return true;
        }

        fmt::println(stderr, "\033[1;31mTest-Failure: Output Diff\033[0m");

        dtl::Diff<TbOutput, std::vector<TbOutput>> diff(expectedOutputs, outputs);
        diff.compose();
        
        for (std::pair<TbOutput, dtl::elemInfo> sesElem : diff.getSes().getSequence()) {
            switch (sesElem.second.type) {
                case dtl::SES_ADD:
                    fmt::print(stderr, "\033[32m++++++++++\n{}\n++++++++++\033[0m\n", sesElem.first);
                    break;
                case dtl::SES_DELETE:
                    fmt::print(stderr, "\033[91m----------\n{}\n----------\033[0m\n", sesElem.first);
                    break;
                case dtl::SES_COMMON:
                    fmt::print(stderr, "{}\n", sesElem.first);
                    break;
            }
        }

        return false;
    }
};

/**
 * Container for items of type T where (T::time) is a uint64_t, allowing Python defaultdict-style creation.
 * e.g. from an empty Maker, `maker[100].blah = "blah";` will construct a T, map it to time 100 and set `t.time = 100`, then return a reference for the user to modify.
 * The asVec() function converts it to a vector of T ordered by time.
 */
template<class T>
    // Don't use the full ValidTbStim concept here - that requires fmt::is_formattable,
    // which is only true once the compiler has seen the template specialization of fmt::formatter,
    // which has to be defined at the top-level namespace,
    // which means if you define T and use TimeSeriesMaker<T> together in a namespace, you won't have defined fmt::formatter yet.
    // All the TimeSeriesMaker cares about is that t.time = uint64_t
    // This should be possible with same_as, but we need to use convertible_to otherwise we get errors for reasons I can't explain.
    requires requires(T t) {
        {t.time} -> std::convertible_to<uint64_t>;
    }
class TimeSeriesMaker {
    std::map<uint64_t, T> elems;

public:

    std::vector<T> asVec() {
        std::vector<T> v;
        for (const auto& [time, elem] : elems) {
            v.push_back(elem);
        }
        return v;
    }

    T& operator[](const uint64_t& key) {
        auto iter = elems.find(key);
        if (iter == elems.end()) {
            T elem{};
            elem.time = key;
            elems[key] = elem;
            return elems[key];
        }
        return (*iter).second;
    }
};

#endif // TB_H
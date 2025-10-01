#ifndef KEY_MANAGER2_REFCOUNTPIPE_UVM_H
#define KEY_MANAGER2_REFCOUNTPIPE_UVM_H

#include "key_manager2.h"

#include "tb.h"
#include "capabilities.h"
#include "util.h"

#include <fmt/ranges.h>

#include <deque>
#include <memory>
#include <random>

// UVM testing strategy

// Simulation model for each valve queues up a bunch of (random?) incoming transactions using a small pool of keys.
// each of the keys from that pool can be used a max number of times, and when the max is reached try to send a revoke request.

struct ValveShimKeyPoolEntry {
    key_manager2::KeyId key;
    int32_t max_usages;
};

class ValveShimStimulus {
    // Inputs
    std::deque<ValveShimKeyPoolEntry> keyPoolQueue;
    uint8_t maxKeyPoolSize;

    // State
    std::vector<ValveShimKeyPoolEntry> keyPool;

    ValveShimStimulus(std::deque<ValveShimKeyPoolEntry> keyPoolQueue, uint8_t maxKeyPoolSize) : keyPoolQueue(keyPoolQueue), maxKeyPoolSize(maxKeyPoolSize), keyPool() {
        for (auto i = 0; i < maxKeyPoolSize && !this->keyPoolQueue.empty(); i++) {
            keyPool.emplace_back(keyPoolQueue.front());
            keyPoolQueue.pop_front();
        }
    }

    std::optional<key_manager2::KeyId> selectNextKeyIdToUse(std::mt19937& rng) {
        if (keyPool.empty()) {
            return std::nullopt;
        }
        auto idx = std::uniform_int_distribution<uint64_t>(0, keyPool.size() - 1)(rng);
        auto key = keyPool[idx].key;
        keyPool[idx].max_usages -= 1;
        if (keyPool[idx].max_usages <= 0) {
            keyPool.erase(keyPool.begin() + idx);
            if (!keyPoolQueue.empty()) {
                keyPool.emplace_back(keyPoolQueue.front());
                keyPoolQueue.pop_front();
            }
        }
        return key;
    }
};

/**
 * Base class for all ShimmedExposer stimulus generators
 */
template<class DUT>
class RefCountPipe_TwoValve_GeneralStimulus : public StimulusGenerator<DUT> {

};

template<class DUT>
class RefCountPipe_TwoValve_RevocationStimulus : public StimulusGenerator<DUT> {

};

// THIS IS INCOMPLETE

#endif // KEY_MANAGER2_REFCOUNTPIPE_UVM_H
#ifndef CAPABILITIES_H
#define CAPABILITIES_H

#include <random>
#include "util.h"

extern "C" {
    #include "librust_caps_c.h"
}

CCap2024_02 initial_resource_cap(const U128& key, uint64_t base, uint64_t length, bool is_almighty, uint32_t secret_id, CCapPerms perms) {
    CCapU128 cap_key;
    key.to_le(cap_key);
    CCap2024_02 cap;
    if (is_almighty) {
        if (ccap_init_almighty(&cap, &cap_key, secret_id, perms) != Success) {
            throw std::runtime_error("Failed to ccap_init_almighty");
        }
    } else {
        if (ccap_init_inexact(&cap, &cap_key, base, length, secret_id, perms) != Success) {
            throw std::runtime_error("Failed to ccap_init_inexact");
        }
    }

    return cap;
}

CCap2024_02 initial_resource_cap_exact(const U128& key, uint64_t base, uint64_t length, bool is_almighty, uint32_t secret_id, CCapPerms perms) {
    CCapU128 cap_key;
    key.to_le(cap_key);
    CCap2024_02 cap;
    if (is_almighty) {
        if (ccap_init_almighty(&cap, &cap_key, secret_id, perms) != Success) {
            throw std::runtime_error("Failed to ccap_init_almighty");
        }
    } else {
        if (ccap_init_exact(&cap, &cap_key, base, length, secret_id, perms) != Success) {
            throw std::runtime_error("Failed to ccap_init_inexact");
        }
    }

    return cap;
}

/**
 * Generate a random Cap2024_02 with a uniformly distributed log(length).
 */
template<class Generator>
CCap2024_02 random_initial_resource_cap(Generator& g, const U128& key, uint32_t secret_id, CCapPerms perms) {
    // Generate a random length with uniform log(length)
    // Generate log(length)
    uint8_t log_length = std::uniform_int_distribution<uint8_t>(0, 64)(g);
    // Generate 64 random bits, where some of them will be used based on log_length.
    uint64_t length = std::uniform_int_distribution<uint64_t>()(g);
    // If log_length = 20, we want bits[20] to be 1 and bits[18:0] to be uniformly distributed random.
    // If log_length = 0, we want bits[0] to be 1.
    // If log_length = 64, this work is discarded and base = 0, length = (1 << 64) i.e. the almighty capability is chosen
    // Always set the top bit of length to 1, so it's always in that log(length) bucket.
    length = length >> (64 - log_length);
    length = length | (1 << log_length);
    // Generate a base that fits inside [0, (1 << 64) - length]
    uint64_t base = std::uniform_int_distribution<uint64_t>(0, std::numeric_limits<uint64_t>::max() - length + 1)(g);

    return initial_resource_cap(key, base, length, (log_length == 64), secret_id, perms);
}

#endif // CAPABILITIES_H
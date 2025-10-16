#ifndef CAPABILITIES_H
#define CAPABILITIES_H

#include <random>
#include "util.h"

#define LIBRUST_CAPS_C_HOSTED 1
#include "librust_caps_c.h"

CCap2024_02 initial_resource_cap_02(const U128& key, uint64_t base, uint64_t length, bool is_almighty, uint32_t secret_id, CCapPerms perms) {
    CCapU128 cap_key;
    key.to_le(cap_key);
    CCap2024_02 cap;
    if (is_almighty) {
        if (ccap2024_02_init_almighty(&cap, &cap_key, secret_id, perms) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap2024_02_init_almighty");
        }
    } else {
        if (ccap2024_02_init_inexact(&cap, &cap_key, base, length, secret_id, perms) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap2024_02_init_inexact");
        }
    }

    return cap;
}

CCap2024_02 initial_resource_cap_exact_02(const U128& key, uint64_t base, uint64_t length, bool is_almighty, uint32_t secret_id, CCapPerms perms) {
    CCapU128 cap_key;
    key.to_le(cap_key);
    CCap2024_02 cap;
    if (is_almighty) {
        if (ccap2024_02_init_almighty(&cap, &cap_key, secret_id, perms) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap2024_02_init_almighty");
        }
    } else {
        if (ccap2024_02_init_exact(&cap, &cap_key, base, length, secret_id, perms) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap2024_02_init_inexact");
        }
    }

    return cap;
}

/**
 * Generate a random Cap2024_02 with a uniformly distributed log(length).
 */
template<class Generator> requires std::uniform_random_bit_generator<Generator>
CCap2024_02 random_initial_resource_cap_02(Generator& g, const U128& key, uint32_t secret_id, CCapPerms perms) {
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

    return initial_resource_cap_02(key, base, length, (log_length == 64), secret_id, perms);
}


CCap2024_11 initial_resource_cap_11(const U128& key, uint64_t base, uint64_t length, uint32_t secret_id, CCapPerms perms) {
    CCapU128 cap_key;
    key.to_le(cap_key);
    CCap2024_11 cap;

    CCapResult res = ccap2024_11_init_inexact(&cap, &cap_key, base, length, secret_id, perms);
    if (res != CCapResult_Success) {
        throw std::runtime_error(fmt::format("Failed to ccap2024_11_init_inexact: res {} base {:x} length {:x}", (uint8_t)res, base, length));
    }

    return cap;
}

CCap2024_11 initial_resource_cap_exact_11(const U128& key, uint64_t base, uint64_t length, uint32_t secret_id, CCapPerms perms) {
    CCapU128 cap_key;
    key.to_le(cap_key);
    CCap2024_11 cap;
    if (ccap2024_11_init_exact(&cap, &cap_key, base, length, secret_id, perms) != CCapResult_Success) {
        throw std::runtime_error("Failed to ccap2024_02_init_inexact");
    }

    return cap;
}

/**
 * Generate a random Cap2024_11 with a uniformly distributed log(length).
 */
template<class Generator> requires std::uniform_random_bit_generator<Generator>
CCap2024_11 random_initial_resource_cap_11(Generator& g, const U128& key, uint32_t secret_id, CCapPerms perms) {
    // Generate a random length with uniform log(length)
    // Generate log(length)
    uint8_t log_length = std::uniform_int_distribution<uint8_t>(0, 54)(g);
    // Generate 64 random bits, where some of them will be used based on log_length.
    uint64_t length = std::uniform_int_distribution<uint64_t>(0, (1ull << 54))(g);
    // If log_length = 20, we want bits[20] to be 1 and bits[18:0] to be uniformly distributed random.
    // If log_length = 0, we want bits[0] to be 1.
    // Always set the top bit of length to 1, so it's always in that log(length) bucket.
    length = length >> (64 - log_length);
    length = length | (1 << log_length);
    // Generate a base that fits inside [0, (1 << 54) - length]
    uint64_t base = std::uniform_int_distribution<uint64_t>(0, (1ull << 54) - length + 1)(g);

    return initial_resource_cap_11(key, base, length, secret_id, perms);
}

enum class CapType : uint8_t {
    Cap2024_02,
    Cap2024_11
};

template <> class fmt::formatter<CapType> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (CapType const& c, Context& ctx) const {
        if (c == CapType::Cap2024_02) {
		    return format_to(ctx.out(), "Cap2024_02");
        } else {
		    return format_to(ctx.out(), "Cap2024_11");
        }
	}
};

// Redefine API for CapStruct through a template so we can switch testbenches out more easily
template<CapType ctype>
struct CapStruct {};
template<>
struct CapStruct<CapType::Cap2024_02> : public CCap2024_02 {
    CCapResult read_perms(CCapPerms* perms) {
        return ccap2024_02_read_perms(this, perms);
    }
    CCapResult read_range(uint64_t* base, uint64_t* len, bool* len_64) {
        return ccap2024_02_read_range(this, base, len, len_64);
    }
    CCapResult read_secret_id(uint32_t* secret_key_id) {
        return ccap2024_02_read_secret_id(this, secret_key_id);
    }
    CCapResult check_signature(const CCapU128* secret) {
        return ccap2024_02_check_signature(this, secret);
    }
    template<class Generator> requires std::uniform_random_bit_generator<Generator>
    static CapStruct<CapType::Cap2024_02> legacy_random_initial_resource_cap(Generator& g, const U128& key, uint32_t secret_id, CCapPerms perms) {
        return CapStruct(random_initial_resource_cap_02(g, key, secret_id, perms));
    }
    static uintptr_t librust_rand_edge_case_num() {
        return ccap2024_02_rand_edge_case_num();
    }
    static const char* librust_rand_edge_case_str(uintptr_t edge_case) {
        return ccap2024_02_rand_edge_case_str(edge_case);
    }
    template<class Generator> requires std::uniform_random_bit_generator<Generator>
    static CapStruct<CapType::Cap2024_02> librust_rand_edge_case_cap(
        Generator& rng,
        const CCapU128 *secret_key,
        const uint32_t *secret_key_id,
        uintptr_t edge_case
    ) {
        CCapU128 seed;
        U128::random(rng).to_le(seed);
        CapStruct<CapType::Cap2024_02> cap;
        CCapResult res = ccap2024_02_rand_edge_case_cap(&cap, &seed, secret_key, secret_key_id, edge_case);
        if (res != CCapResult_Success) {
            throw std::runtime_error(
                fmt::format(
                    "Failed to generate librust_rand_edge_case_cap for {} {}: {}",
                    CapType::Cap2024_02,
                    ccap2024_02_rand_edge_case_str(edge_case),
                    ccap_result_str(res)
                )
            );
        }
        return cap;
    }
    template<class Generator> requires std::uniform_random_bit_generator<Generator>
    static CapStruct<CapType::Cap2024_02> librust_rand_valid_cap(
        Generator& rng,
        const CCapU128 *secret_key,
        const uint32_t *secret_key_id,
        const CCapPerms *perms=nullptr,
        int n_cavs=-1
    ) {
        CCapU128 seed;
        U128::random(rng).to_le(seed);
        CapStruct<CapType::Cap2024_02> cap;
        CCapResult res;
        switch (n_cavs) {
            case -1:
                ccap2024_02_rand_valid_cap(&cap, &seed, secret_key, secret_key_id, perms);
                break;
            case 0:
                ccap2024_02_rand_valid_cap_0cav(&cap, &seed, secret_key, secret_key_id, perms);
                break;
            case 1:
                ccap2024_02_rand_valid_cap_1cav(&cap, &seed, secret_key, secret_key_id, perms);
                break;
            case 2:
                ccap2024_02_rand_valid_cap_2cav(&cap, &seed, secret_key, secret_key_id, perms);
                break;
            default:
                throw std::runtime_error(fmt::format("Invalid n_cavs value {}", n_cavs));
        }
        if (res != CCapResult_Success) {
            throw std::runtime_error(
                fmt::format(
                    "Failed to generate librust_rand_valid_cap for {}: {}",
                    CapType::Cap2024_02,
                    ccap_result_str(res)
                )
            );
        }
        return cap;
    }
};
template<>
struct CapStruct<CapType::Cap2024_11> : public CCap2024_11 {
    CCapResult read_perms(CCapPerms* perms) {
        return ccap2024_11_read_perms(this, perms);
    }
    CCapResult read_range(uint64_t* base, uint64_t* len, bool* len_64) {
        return ccap2024_11_read_range(this, base, len, len_64);
    }
    CCapResult read_secret_id(uint32_t* secret_key_id) {
        return ccap2024_11_read_secret_id(this, secret_key_id);
    }
    CCapResult check_signature(const CCapU128* secret) {
        return ccap2024_11_check_signature(this, secret);
    }
    template<class Generator> requires std::uniform_random_bit_generator<Generator>
    static CapStruct<CapType::Cap2024_11> legacy_random_initial_resource_cap(Generator& g, const U128& key, uint32_t secret_id, CCapPerms perms) {
        return CapStruct(random_initial_resource_cap_11(g, key, secret_id, perms));
    }
    static uintptr_t librust_rand_edge_case_num() {
        return ccap2024_11_rand_edge_case_num();
    }
    static const char* librust_rand_edge_case_str(uintptr_t edge_case) {
        return ccap2024_11_rand_edge_case_str(edge_case);
    }
    template<class Generator> requires std::uniform_random_bit_generator<Generator>
    static CapStruct<CapType::Cap2024_11> librust_rand_edge_case_cap(
        Generator& rng,
        const CCapU128 *secret_key,
        const uint32_t *secret_key_id,
        uintptr_t edge_case
    ) {
        CCapU128 seed;
        U128::random(rng).to_le(seed);
        CapStruct<CapType::Cap2024_11> cap;
        CCapResult res = ccap2024_11_rand_edge_case_cap(&cap, &seed, secret_key, secret_key_id, edge_case);
        if (res != CCapResult_Success) {
            throw std::runtime_error(
                fmt::format(
                    "Failed to generate librust_rand_edge_case_cap for {} {}: {}",
                    CapType::Cap2024_11,
                    ccap2024_11_rand_edge_case_str(edge_case),
                    ccap_result_str(res)
                )
            );
        }
        return cap;
    }
    template<class Generator> requires std::uniform_random_bit_generator<Generator>
    static CapStruct<CapType::Cap2024_11> librust_rand_valid_cap(
        Generator& rng,
        const CCapU128 *secret_key,
        const uint32_t *secret_key_id,
        const CCapPerms *perms=nullptr,
        int n_cavs=-1
    ) {
        CCapU128 seed;
        U128::random(rng).to_le(seed);
        CapStruct<CapType::Cap2024_11> cap;
        CCapResult res;
        switch (n_cavs) {
            case -1:
                ccap2024_11_rand_valid_cap(&cap, &seed, secret_key, secret_key_id, perms);
                break;
            case 0:
                ccap2024_11_rand_valid_cap_0cav(&cap, &seed, secret_key, secret_key_id, perms);
                break;
            case 1:
                ccap2024_11_rand_valid_cap_1cav(&cap, &seed, secret_key, secret_key_id, perms);
                break;
            case 2:
                ccap2024_11_rand_valid_cap_2cav(&cap, &seed, secret_key, secret_key_id, perms);
                break;
            default:
                throw std::runtime_error(fmt::format("Invalid n_cavs value {}", n_cavs));
        }
        if (res != CCapResult_Success) {
            throw std::runtime_error(
                fmt::format(
                    "Failed to generate librust_rand_valid_cap for {}: {}",
                    CapType::Cap2024_11,
                    ccap_result_str(res)
                )
            );
        }
        return cap;
    }
};


#endif // CAPABILITIES_H
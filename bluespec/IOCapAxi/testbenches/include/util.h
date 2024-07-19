#ifndef UTIL_H
#define UTIL_H

#include <optional>

#define FMT_HEADER_ONLY
#include "fmt/format.h"

template<class T>
std::optional<T> some(T t) {
    return std::optional(t);
}

template <class T> class fmt::formatter<std::optional<T>> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (std::optional<T> const& x, Context& ctx) const {
        if (x.has_value()) {
            return format_to(ctx.out(), "Some({})", x.value());
        } else {
            return format_to(ctx.out(), "None");
        }
    }
};

/**
 * 128-bits of data stored in two uint64_t.
 * `top` holds data[127:64], `bottom` holds data[63:0].
 */
struct U128 {
    uint64_t top;
    uint64_t bottom;

    bool operator==(const U128& other) const = default;
    template<class Generator>
    static U128 random(Generator& g) {
        auto dist = std::uniform_int_distribution<uint64_t>();
        return U128 {
            .top = dist(g),
            .bottom = dist(g),
        };
    }
    static U128 from_le(const uint8_t data[16]) {
        return U128 {
            .top = (uint64_t(data[15]) << 56) |
                   (uint64_t(data[14]) << 48) |
                   (uint64_t(data[13]) << 40) |
                   (uint64_t(data[12]) << 32) |
                   (uint64_t(data[11]) << 24) |
                   (uint64_t(data[10]) << 16) |
                   (uint64_t(data[9]) << 8) |
                   (uint64_t(data[8]) << 0),
            .bottom = (uint64_t(data[7]) << 56) |
                    (uint64_t(data[6]) << 48) |
                    (uint64_t(data[5]) << 40) |
                    (uint64_t(data[4]) << 32) |
                    (uint64_t(data[3]) << 24) |
                    (uint64_t(data[2]) << 16) |
                    (uint64_t(data[1]) << 8) |
                    (uint64_t(data[0]) << 0),
        };
    }
    void to_le(uint8_t data[16]) const {
        data[0] = uint8_t(bottom >> 0);
        data[1] = uint8_t(bottom >> 8);
        data[2] = uint8_t(bottom >> 16);
        data[3] = uint8_t(bottom >> 24);
        data[4] = uint8_t(bottom >> 32);
        data[5] = uint8_t(bottom >> 40);
        data[6] = uint8_t(bottom >> 48);
        data[7] = uint8_t(bottom >> 56);
        data[8] = uint8_t(top >> 0);
        data[9] = uint8_t(top >> 8);
        data[10] = uint8_t(top >> 16);
        data[11] = uint8_t(top >> 24);
        data[12] = uint8_t(top >> 32);
        data[13] = uint8_t(top >> 40);
        data[14] = uint8_t(top >> 48);
        data[15] = uint8_t(top >> 56);
    }
};

template <> class fmt::formatter<U128> {
    public:
    constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
    template <typename Context>
    constexpr auto format (U128 const& key, Context& ctx) const {
        return format_to(ctx.out(), "0x{:016x}{:016x}", key.top, key.bottom);
    }
};

#endif // UTIL_H
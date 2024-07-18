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

#endif // UTIL_H
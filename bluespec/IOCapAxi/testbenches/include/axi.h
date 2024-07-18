#ifndef AXI_H
#define AXI_H

namespace axi {
    enum class AXI4_Resp: uint8_t {
        Okay = 0b00,
        ExOkay = 0b01,
        SlvErr = 0b10,
        DecErr = 0b11,
    };

    enum class AXI4_Lock: uint8_t {
        Normal = 0b0,
        Exclusive = 0b1,
    };

    enum class AXI4_Burst: uint8_t {
        Fixed = 0b00,
        Incr = 0b01,
        Wrap = 0b10,
        reserved = 0b11
    };

    // IOCapAxi and SanitizedAxi namespaces defined in tb_bitfields.h
}

#include "tb_bitfields.h"

#endif // AXI_H
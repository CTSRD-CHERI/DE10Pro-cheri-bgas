#ifndef AXI_H
#define AXI_H

#include "util.h"

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

    uint8_t n_transfers_to_len(uint16_t n_transfers){
        assert(n_transfers <= 0x100);
        return n_transfers - 1;
    }

    uint16_t len_to_n_transfers(uint8_t len){
        return len + 1;
    }

    /**
     * Given a transfer_width in bytes, return a 3-bit value
     * which can be inserted into .awsize or .arsize to represent that width.
     */
    uint8_t transfer_width_to_size(uint8_t transfer_width) {
        switch(transfer_width) {
            case 1:
                return 0b000;
            case 2:
                return 0b001;
            case 4:
                return 0b010;
            case 8:
                return 0b011;
            case 16:
                return 0b100;
            case 32:
                return 0b101;
            case 64:
                return 0b110;
            case 128:
                return 0b111;
            default:
                throw std::runtime_error("Invalid transfer width");
        }
    }

    uint16_t burst_byte_length(AXI4_Burst mode, uint8_t asize, uint8_t alen) {
        switch (mode) {
            case AXI4_Burst::Fixed: {
                // Just access the same region repeatedly
                // number of bytes per beat = 1 << burstSize, up to 128 => length = 7
                return (1 << asize);
            }
            case AXI4_Burst::Incr: {
                // Access an incrementing address => range = (1 transfer size) * (n transfers in burst)
                // max address = min address + (beats/burst) * (bytes/beat)
                // beats/burst = burstLen + 1, [1, 256]
                // bytes/beat  = 1 << burstSize, up to (i.e. overall 128)
                // multiplied together the max is 32768, up to 15 bits
                return ((uint16_t(alen) + 1) << asize);
            }
            case AXI4_Burst::Wrap:
            default: // WRAP not supported, others are reserved
                throw std::runtime_error(fmt::format("Can't decode AXI4_Burst {}", uint8_t(mode)));
        }
    }

    // IOCapAxi and SanitizedAxi namespaces defined in tb_bitfields.h
}

#include "tb_bitfields.h"

namespace axi::IOCapAxi {
    /**
     * Three-bit enum.
     * top bit = is flit the start of an authenticated group
     * bottom two bits = sequence number in authenticated group
     * */
    enum class IOCapAxi_User : uint8_t {
        Unauthed = 0b000,
        Start = 0b100,
        Cap1 = 0b001,
        Cap2 = 0b010,
        Cap3 = 0b011,
    };

    /*
    The IOCapAxi convention for attaching 256-bit cryptographically authenticated capabilities
    is to treat the 128-bit capability text as the bottom 128-bits of the metadata,
    and the 128-bit capability signature as the top 128-bits.
    */

    void unpackCap1_ar(U128& cap, U128& sig, ARFlit_id4_addr64_user3& flit) {
        cap.top |= uint64_t(flit.arqos & 0x1)   << 21;
        cap.top |= uint64_t(flit.arprot & 0x7)  << 18;
        cap.top |= uint64_t(flit.arcache & 0xf) << 14;
        cap.top |= uint64_t(flit.arlock & 0x1)  << 13;
        cap.top |= uint64_t(flit.arburst & 0x3) << 11;
        cap.top |= uint64_t(flit.arsize & 0x7)  << 8;
        cap.top |= uint64_t(flit.arlen & 0xff)  << 0;
        cap.bottom = flit.araddr;
    }
    void unpackCap2_ar(U128& cap, U128& sig, ARFlit_id4_addr64_user3& flit) {
        sig.bottom |= uint64_t(flit.arqos & 0x1)   << 43;
        sig.bottom |= uint64_t(flit.arprot & 0x7)  << 40;
        sig.bottom |= uint64_t(flit.arcache & 0xf) << 36;
        sig.bottom |= uint64_t(flit.arlock & 0x1)  << 35;
        sig.bottom |= uint64_t(flit.arburst & 0x3) << 33;
        sig.bottom |= uint64_t(flit.arsize & 0x7)  << 30;
        sig.bottom |= uint64_t(flit.arlen & 0xff)  << 22;
        sig.bottom |= uint64_t(flit.araddr >> 42);
        cap.top    |= uint64_t(flit.araddr << 22);
    }
    void unpackCap3_ar(U128& cap, U128& sig, ARFlit_id4_addr64_user3& flit) {
        sig.top |= uint64_t(flit.arprot & 0x3)  << 62;
        sig.top |= uint64_t(flit.arcache & 0xf) << 58;
        sig.top |= uint64_t(flit.arlock & 0x1)  << 57;
        sig.top |= uint64_t(flit.arburst & 0x3) << 55;
        sig.top |= uint64_t(flit.arsize & 0x7)  << 52;
        sig.top |= uint64_t(flit.arlen & 0xff)  << 44;
        sig.top |= uint64_t(flit.araddr >> 20);
        sig.bottom |= uint64_t(flit.araddr << 44);
    }

    ARFlit_id4_addr64_user3 packCap1_ar(U128 cap, U128 sig) {
        // READ THIS UPSIDE DOWN
        return ARFlit_id4_addr64_user3 {
            .aruser   = uint8_t(IOCapAxi_User::Cap1),
            .arregion = 0xac, // = not-set
            // have covered |-- cap.top[21:0] --|-- cap.bottom --|
            .arqos    = uint8_t((cap.top >> 21) & 0x1),
            .arprot   = uint8_t((cap.top >> 18) & 0x7),
            .arcache  = uint8_t((cap.top >> 14) & 0xf),
            .arlock   = uint8_t((cap.top >> 13) & 0x1),
            .arburst  = uint8_t((cap.top >> 11) & 0x3),
            .arsize   = uint8_t((cap.top >>  8) & 0x7),
            .arlen    = uint8_t((cap.top >>  0) & 0xff),
            .araddr   = cap.bottom, // bottom 64-bits
            .arid = 0xac, // = not-set
        };
    }
    ARFlit_id4_addr64_user3 packCap2_ar(U128 cap, U128 sig) {
        // READ THIS UPSIDE DOWN
        return ARFlit_id4_addr64_user3 {
            .aruser   = uint8_t(IOCapAxi_User::Cap2),
            // have covered |-- sig.bottom[43:0] --|-- cap.top --|-- cap.bottom --|
            .arregion = 0xac, // = not-set
            .arqos    = uint8_t((sig.bottom >> 43) & 0x1),
            .arprot   = uint8_t((sig.bottom >> 40) & 0x7),
            .arcache  = uint8_t((sig.bottom >> 36) & 0xf),
            .arlock   = uint8_t((sig.bottom >> 35) & 0x1),
            .arburst  = uint8_t((sig.bottom >> 33) & 0x3),
            .arsize   = uint8_t((sig.bottom >> 30) & 0x7),
            .arlen    = uint8_t((sig.bottom >> 22) & 0xff),
            // The bottom bits of araddr are the (64-22=)42-bits of cap.top that we didn't attach in packCap1
            // |-- sig.bottom[21:0] --|-- cap.top[63:22] --|
            .araddr   = uint64_t(cap.top >> 22) | uint64_t(sig.bottom << 42),
            .arid = 0xac, // = not-set
        };
    }
    ARFlit_id4_addr64_user3 packCap3_ar(U128 cap, U128 sig) {
        // READ THIS UPSIDE DOWN
        return ARFlit_id4_addr64_user3 {
            .aruser   = uint8_t(IOCapAxi_User::Cap3),
            // have covered |-- sig.top --|-- sig.bottom --|-- cap.top --|-- cap.bottom --|
            .arregion = 0xac,
            .arqos    = 0xac,
            .arprot   = uint8_t((sig.top >> 62) & 0x3),
            .arcache  = uint8_t((sig.top >> 58) & 0xf),
            .arlock   = uint8_t((sig.top >> 57) & 0x1),
            .arburst  = uint8_t((sig.top >> 55) & 0x3),
            .arsize   = uint8_t((sig.top >> 52) & 0x7),
            .arlen    = uint8_t((sig.top >> 44) & 0xff),
            // The bottom bits of araddr are the (64-44=)20-bits of sig.bottom that we didn't attach in packCap2
            // |-- sig.top[43:0] --|-- sig.bottom[63:44] --|
            .araddr   = uint64_t(sig.bottom >> 44) | uint64_t(sig.top << 20),
            .arid = 0xac, // = not-set
        };
    }

    void unpackCap1_aw(U128& cap, U128& sig, AWFlit_id4_addr64_user3& flit) {
        cap.top |= uint64_t(flit.awqos & 0x1)   << 21;
        cap.top |= uint64_t(flit.awprot & 0x7)  << 18;
        cap.top |= uint64_t(flit.awcache & 0xf) << 14;
        cap.top |= uint64_t(flit.awlock & 0x1)  << 13;
        cap.top |= uint64_t(flit.awburst & 0x3) << 11;
        cap.top |= uint64_t(flit.awsize & 0x7)  << 8;
        cap.top |= uint64_t(flit.awlen & 0xff)  << 0;
        cap.bottom = flit.awaddr;
    }
    void unpackCap2_aw(U128& cap, U128& sig, AWFlit_id4_addr64_user3& flit) {
        sig.bottom |= uint64_t(flit.awqos & 0x1)   << 43;
        sig.bottom |= uint64_t(flit.awprot & 0x7)  << 40;
        sig.bottom |= uint64_t(flit.awcache & 0xf) << 36;
        sig.bottom |= uint64_t(flit.awlock & 0x1)  << 35;
        sig.bottom |= uint64_t(flit.awburst & 0x3) << 33;
        sig.bottom |= uint64_t(flit.awsize & 0x7)  << 30;
        sig.bottom |= uint64_t(flit.awlen & 0xff)  << 22;
        sig.bottom |= uint64_t(flit.awaddr >> 42);
        cap.top    |= uint64_t(flit.awaddr << 22);
    }
    void unpackCap3_aw(U128& cap, U128& sig, AWFlit_id4_addr64_user3& flit) {
        sig.top |= uint64_t(flit.awprot & 0x3)  << 62;
        sig.top |= uint64_t(flit.awcache & 0xf) << 58;
        sig.top |= uint64_t(flit.awlock & 0x1)  << 57;
        sig.top |= uint64_t(flit.awburst & 0x3) << 55;
        sig.top |= uint64_t(flit.awsize & 0x7)  << 52;
        sig.top |= uint64_t(flit.awlen & 0xff)  << 44;
        sig.top |= uint64_t(flit.awaddr >> 20);
        sig.bottom |= uint64_t(flit.awaddr << 44);
    }

    AWFlit_id4_addr64_user3 packCap1_aw(U128 cap, U128 sig) {
        // READ THIS UPSIDE DOWN
        return AWFlit_id4_addr64_user3 {
            .awuser   = uint8_t(IOCapAxi_User::Cap1),
            .awregion = 0xac, // = not-set
            // have covered |-- cap.top[21:0] --|-- cap.bottom --|
            .awqos    = uint8_t((cap.top >> 21) & 0x1),
            .awprot   = uint8_t((cap.top >> 18) & 0x7),
            .awcache  = uint8_t((cap.top >> 14) & 0xf),
            .awlock   = uint8_t((cap.top >> 13) & 0x1),
            .awburst  = uint8_t((cap.top >> 11) & 0x3),
            .awsize   = uint8_t((cap.top >>  8) & 0x7),
            .awlen    = uint8_t((cap.top >>  0) & 0xff),
            .awaddr   = cap.bottom, // bottom 64-bits
            .awid = 0xac, // = not-set
        };
    }
    AWFlit_id4_addr64_user3 packCap2_aw(U128 cap, U128 sig) {
        // READ THIS UPSIDE DOWN
        return AWFlit_id4_addr64_user3 {
            .awuser   = uint8_t(IOCapAxi_User::Cap2),
            // have covered |-- sig.bottom[43:0] --|-- cap.top --|-- cap.bottom --|
            .awregion = 0xac, // = not-set
            .awqos    = uint8_t((sig.bottom >> 43) & 0x1),
            .awprot   = uint8_t((sig.bottom >> 40) & 0x7),
            .awcache  = uint8_t((sig.bottom >> 36) & 0xf),
            .awlock   = uint8_t((sig.bottom >> 35) & 0x1),
            .awburst  = uint8_t((sig.bottom >> 33) & 0x3),
            .awsize   = uint8_t((sig.bottom >> 30) & 0x7),
            .awlen    = uint8_t((sig.bottom >> 22) & 0xff),
            // The bottom bits of awaddr are the (64-22=)42-bits of cap.top that we didn't attach in packCap1
            // |-- sig.bottom[21:0] --|-- cap.top[63:22] --|
            .awaddr   = uint64_t(cap.top >> 22) | uint64_t(sig.bottom << 42),
            .awid = 0xac, // = not-set
        };
    }
    AWFlit_id4_addr64_user3 packCap3_aw(U128 cap, U128 sig) {
        // READ THIS UPSIDE DOWN
        return AWFlit_id4_addr64_user3 {
            .awuser   = uint8_t(IOCapAxi_User::Cap3),
            // have covered |-- sig.top --|-- sig.bottom --|-- cap.top --|-- cap.bottom --|
            .awregion = 0xac,
            .awqos    = 0xac,
            .awprot   = uint8_t((sig.top >> 62) & 0x3),
            .awcache  = uint8_t((sig.top >> 58) & 0xf),
            .awlock   = uint8_t((sig.top >> 57) & 0x1),
            .awburst  = uint8_t((sig.top >> 55) & 0x3),
            .awsize   = uint8_t((sig.top >> 52) & 0x7),
            .awlen    = uint8_t((sig.top >> 44) & 0xff),
            // The bottom bits of awaddr are the (64-44=)20-bits of sig.bottom that we didn't attach in packCap2
            // |-- sig.top[43:0] --|-- sig.bottom[63:44] --|
            .awaddr   = uint64_t(sig.bottom >> 44) | uint64_t(sig.top << 20),
            .awid = 0xac, // = not-set
        };
    }
}

#endif // AXI_H
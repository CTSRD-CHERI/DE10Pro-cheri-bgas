#include <verilated.h>
#include "util.h"
#include "axi.h"

int main() {
    U128 data{.top = 0x0123456789ABCDEF, .bottom = 0xFEDCBA9876543210};
    U128 sig{.top = 0xFEDCBA9876543210, .bottom = 0x0123456789ABCDEF};

    {
        auto cap1 = axi::IOCapAxi::packCap1_ar(data, sig);
        auto cap2 = axi::IOCapAxi::packCap2_ar(data, sig);
        auto cap3 = axi::IOCapAxi::packCap3_ar(data, sig);

        U128 otherData{.top=0,.bottom=0};
        U128 otherSig{.top=0,.bottom=0};
        axi::IOCapAxi::unpackCap1_ar(otherData, otherSig, cap1);
        axi::IOCapAxi::unpackCap2_ar(otherData, otherSig, cap2);
        axi::IOCapAxi::unpackCap3_ar(otherData, otherSig, cap3);

        if (otherData != data) {
            throw std::runtime_error(fmt::format("AR data 0x{} != otherData 0x{}, ", data, otherData));
        }
        if (otherSig != sig) {
            throw std::runtime_error(fmt::format("AR sig 0x{} != otherSig 0x{}", sig, otherSig));
        }
    }

    {
        auto cap1 = axi::IOCapAxi::packCap1_aw(data, sig);
        auto cap2 = axi::IOCapAxi::packCap2_aw(data, sig);
        auto cap3 = axi::IOCapAxi::packCap3_aw(data, sig);

        U128 otherData{.top=0,.bottom=0};
        U128 otherSig{.top=0,.bottom=0};
        axi::IOCapAxi::unpackCap1_aw(otherData, otherSig, cap1);
        axi::IOCapAxi::unpackCap2_aw(otherData, otherSig, cap2);
        axi::IOCapAxi::unpackCap3_aw(otherData, otherSig, cap3);

        if (otherData != data) {
            throw std::runtime_error(fmt::format("AW data 0x{:16x}{:16x} != otherData 0x{:16x}{:16x}", data.top, data.bottom, otherData.top, otherData.bottom));
        }
        if (otherSig != sig) {
            throw std::runtime_error(fmt::format("AW sig 0x{:16x}{:16x} != otherSig 0x{:16x}{:16x}", sig.top, sig.bottom, otherSig.top, otherSig.bottom));
        }
    }
    return 0;
}
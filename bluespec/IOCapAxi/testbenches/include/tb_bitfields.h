#ifndef TB_BITFIELDS_H
#define TB_BITFIELDS_H
// Autogenerated by generate_bitfield.py

#include <cstdint>
#include <array>
#define FMT_HEADER_ONLY
#include "fmt/format.h"

namespace axi::IOCapAxi {
	struct AWFlit_id4_addr64_user3 {
		/** 4-bit field */
		uint8_t awid;
		/** 64-bit field */
		uint64_t awaddr;
		/** 8-bit field */
		uint8_t awlen;
		/** 3-bit field */
		uint8_t awsize;
		/** 2-bit field */
		uint8_t awburst;
		/** 1-bit field */
		uint8_t awlock;
		/** 4-bit field */
		uint8_t awcache;
		/** 3-bit field */
		uint8_t awprot;
		/** 4-bit field */
		uint8_t awqos;
		/** 8-bit field */
		uint8_t awregion;
		/** 3-bit field */
		uint8_t awuser;
	
		static AWFlit_id4_addr64_user3 unpack(const std::array<uint32_t, 4>& backing) {
			AWFlit_id4_addr64_user3 value{};
			value.awid = (
				uint8_t((backing[0] >> 0u) & 0xfu)
			);
			value.awaddr = (
				(uint64_t((backing[0] >> 4u) & 0xfffffffu) << 0) | 
				(uint64_t((backing[1] >> 0u) & 0xffffffffu) << 28) | 
				(uint64_t((backing[2] >> 0u) & 0xfu) << 60)
			);
			value.awlen = (
				uint8_t((backing[2] >> 4u) & 0xffu)
			);
			value.awsize = (
				uint8_t((backing[2] >> 12u) & 0x7u)
			);
			value.awburst = (
				uint8_t((backing[2] >> 15u) & 0x3u)
			);
			value.awlock = (
				uint8_t((backing[2] >> 17u) & 0x1u)
			);
			value.awcache = (
				uint8_t((backing[2] >> 18u) & 0xfu)
			);
			value.awprot = (
				uint8_t((backing[2] >> 22u) & 0x7u)
			);
			value.awqos = (
				uint8_t((backing[2] >> 25u) & 0xfu)
			);
			value.awregion = (
				(uint8_t((backing[2] >> 29u) & 0x7u) << 0) | 
				(uint8_t((backing[3] >> 0u) & 0x1fu) << 3)
			);
			value.awuser = (
				uint8_t((backing[3] >> 5u) & 0x7u)
			);
			return value;
		}
		std::array<uint32_t, 4> pack() const {
			std::array<uint32_t, 4> backing{};
			backing[0] = (
				(uint32_t((awid >> 0u) & uint8_t(0xful)) << 0) | 
				(uint32_t((awaddr >> 0u) & 0xffffffful) << 4)
			);
			backing[1] = (
				uint32_t((awaddr >> 28u) & 0xfffffffful)
			);
			backing[2] = (
				(uint32_t((awaddr >> 60u) & 0xful) << 0) | 
				(uint32_t((awlen >> 0u) & uint8_t(0xfful)) << 4) | 
				(uint32_t((awsize >> 0u) & uint8_t(0x7ul)) << 12) | 
				(uint32_t((awburst >> 0u) & uint8_t(0x3ul)) << 15) | 
				(uint32_t((awlock >> 0u) & uint8_t(0x1ul)) << 17) | 
				(uint32_t((awcache >> 0u) & uint8_t(0xful)) << 18) | 
				(uint32_t((awprot >> 0u) & uint8_t(0x7ul)) << 22) | 
				(uint32_t((awqos >> 0u) & uint8_t(0xful)) << 25) | 
				(uint32_t((awregion >> 0u) & uint8_t(0x7ul)) << 29)
			);
			backing[3] = (
				(uint32_t((awregion >> 3u) & uint8_t(0x1ful)) << 0) | 
				(uint32_t((awuser >> 0u) & uint8_t(0x7ul)) << 5)
			);
			return backing;
		}
		bool operator==(const AWFlit_id4_addr64_user3&) const = default;
	};
	struct WFlit_data32 {
		/** 32-bit field */
		uint32_t wdata;
		/** 4-bit field */
		uint8_t wstrb;
		/** 1-bit field */
		uint8_t wlast;
	
		static WFlit_data32 unpack(const uint64_t& backing) {
			WFlit_data32 value{};
			value.wdata = (
				uint32_t((backing >> 0u) & 0xfffffffful)
			);
			value.wstrb = (
				uint8_t((backing >> 32u) & 0xful)
			);
			value.wlast = (
				uint8_t((backing >> 36u) & 0x1ul)
			);
			return value;
		}
		uint64_t pack() const {
			uint64_t backing{};
			backing = (
				(uint64_t((wdata >> 0u) & 0xffffffffu) << 0) | 
				(uint64_t((wstrb >> 0u) & uint8_t(0xful)) << 32) | 
				(uint64_t((wlast >> 0u) & uint8_t(0x1ul)) << 36)
			);
			return backing;
		}
		bool operator==(const WFlit_data32&) const = default;
	};
	struct BFlit_id4 {
		/** 4-bit field */
		uint8_t bid;
		/** 2-bit field */
		uint8_t bresp;
	
		static BFlit_id4 unpack(const uint8_t& backing) {
			BFlit_id4 value{};
			value.bid = (
				((backing >> 0u) & uint8_t(0xful))
			);
			value.bresp = (
				((backing >> 4u) & uint8_t(0x3ul))
			);
			return value;
		}
		uint8_t pack() const {
			uint8_t backing{};
			backing = (
				(((bid >> 0u) & uint8_t(0xful)) << 0) | 
				(((bresp >> 0u) & uint8_t(0x3ul)) << 4)
			);
			return backing;
		}
		bool operator==(const BFlit_id4&) const = default;
	};
	struct ARFlit_id4_addr64_user3 {
		/** 4-bit field */
		uint8_t arid;
		/** 64-bit field */
		uint64_t araddr;
		/** 8-bit field */
		uint8_t arlen;
		/** 3-bit field */
		uint8_t arsize;
		/** 2-bit field */
		uint8_t arburst;
		/** 1-bit field */
		uint8_t arlock;
		/** 4-bit field */
		uint8_t arcache;
		/** 3-bit field */
		uint8_t arprot;
		/** 4-bit field */
		uint8_t arqos;
		/** 8-bit field */
		uint8_t arregion;
		/** 3-bit field */
		uint8_t aruser;
	
		static ARFlit_id4_addr64_user3 unpack(const std::array<uint32_t, 4>& backing) {
			ARFlit_id4_addr64_user3 value{};
			value.arid = (
				uint8_t((backing[0] >> 0u) & 0xfu)
			);
			value.araddr = (
				(uint64_t((backing[0] >> 4u) & 0xfffffffu) << 0) | 
				(uint64_t((backing[1] >> 0u) & 0xffffffffu) << 28) | 
				(uint64_t((backing[2] >> 0u) & 0xfu) << 60)
			);
			value.arlen = (
				uint8_t((backing[2] >> 4u) & 0xffu)
			);
			value.arsize = (
				uint8_t((backing[2] >> 12u) & 0x7u)
			);
			value.arburst = (
				uint8_t((backing[2] >> 15u) & 0x3u)
			);
			value.arlock = (
				uint8_t((backing[2] >> 17u) & 0x1u)
			);
			value.arcache = (
				uint8_t((backing[2] >> 18u) & 0xfu)
			);
			value.arprot = (
				uint8_t((backing[2] >> 22u) & 0x7u)
			);
			value.arqos = (
				uint8_t((backing[2] >> 25u) & 0xfu)
			);
			value.arregion = (
				(uint8_t((backing[2] >> 29u) & 0x7u) << 0) | 
				(uint8_t((backing[3] >> 0u) & 0x1fu) << 3)
			);
			value.aruser = (
				uint8_t((backing[3] >> 5u) & 0x7u)
			);
			return value;
		}
		std::array<uint32_t, 4> pack() const {
			std::array<uint32_t, 4> backing{};
			backing[0] = (
				(uint32_t((arid >> 0u) & uint8_t(0xful)) << 0) | 
				(uint32_t((araddr >> 0u) & 0xffffffful) << 4)
			);
			backing[1] = (
				uint32_t((araddr >> 28u) & 0xfffffffful)
			);
			backing[2] = (
				(uint32_t((araddr >> 60u) & 0xful) << 0) | 
				(uint32_t((arlen >> 0u) & uint8_t(0xfful)) << 4) | 
				(uint32_t((arsize >> 0u) & uint8_t(0x7ul)) << 12) | 
				(uint32_t((arburst >> 0u) & uint8_t(0x3ul)) << 15) | 
				(uint32_t((arlock >> 0u) & uint8_t(0x1ul)) << 17) | 
				(uint32_t((arcache >> 0u) & uint8_t(0xful)) << 18) | 
				(uint32_t((arprot >> 0u) & uint8_t(0x7ul)) << 22) | 
				(uint32_t((arqos >> 0u) & uint8_t(0xful)) << 25) | 
				(uint32_t((arregion >> 0u) & uint8_t(0x7ul)) << 29)
			);
			backing[3] = (
				(uint32_t((arregion >> 3u) & uint8_t(0x1ful)) << 0) | 
				(uint32_t((aruser >> 0u) & uint8_t(0x7ul)) << 5)
			);
			return backing;
		}
		bool operator==(const ARFlit_id4_addr64_user3&) const = default;
	};
	struct RFlit_id4_data32 {
		/** 4-bit field */
		uint8_t rid;
		/** 32-bit field */
		uint32_t rdata;
		/** 2-bit field */
		uint8_t rresp;
		/** 1-bit field */
		uint8_t rlast;
	
		static RFlit_id4_data32 unpack(const uint64_t& backing) {
			RFlit_id4_data32 value{};
			value.rid = (
				uint8_t((backing >> 0u) & 0xful)
			);
			value.rdata = (
				uint32_t((backing >> 4u) & 0xfffffffful)
			);
			value.rresp = (
				uint8_t((backing >> 36u) & 0x3ul)
			);
			value.rlast = (
				uint8_t((backing >> 38u) & 0x1ul)
			);
			return value;
		}
		uint64_t pack() const {
			uint64_t backing{};
			backing = (
				(uint64_t((rid >> 0u) & uint8_t(0xful)) << 0) | 
				(uint64_t((rdata >> 0u) & 0xffffffffu) << 4) | 
				(uint64_t((rresp >> 0u) & uint8_t(0x3ul)) << 36) | 
				(uint64_t((rlast >> 0u) & uint8_t(0x1ul)) << 38)
			);
			return backing;
		}
		bool operator==(const RFlit_id4_data32&) const = default;
	};
	
}

namespace axi::SanitizedAxi {
	struct AWFlit_id4_addr64_user0 {
		/** 4-bit field */
		uint8_t awid;
		/** 64-bit field */
		uint64_t awaddr;
		/** 8-bit field */
		uint8_t awlen;
		/** 3-bit field */
		uint8_t awsize;
		/** 2-bit field */
		uint8_t awburst;
		/** 1-bit field */
		uint8_t awlock;
		/** 4-bit field */
		uint8_t awcache;
		/** 3-bit field */
		uint8_t awprot;
		/** 4-bit field */
		uint8_t awqos;
		/** 8-bit field */
		uint8_t awregion;
		/** 3-bit field */
		uint8_t awuser;
	
		static AWFlit_id4_addr64_user0 unpack(const std::array<uint32_t, 4>& backing) {
			AWFlit_id4_addr64_user0 value{};
			value.awid = (
				uint8_t((backing[0] >> 0u) & 0xfu)
			);
			value.awaddr = (
				(uint64_t((backing[0] >> 4u) & 0xfffffffu) << 0) | 
				(uint64_t((backing[1] >> 0u) & 0xffffffffu) << 28) | 
				(uint64_t((backing[2] >> 0u) & 0xfu) << 60)
			);
			value.awlen = (
				uint8_t((backing[2] >> 4u) & 0xffu)
			);
			value.awsize = (
				uint8_t((backing[2] >> 12u) & 0x7u)
			);
			value.awburst = (
				uint8_t((backing[2] >> 15u) & 0x3u)
			);
			value.awlock = (
				uint8_t((backing[2] >> 17u) & 0x1u)
			);
			value.awcache = (
				uint8_t((backing[2] >> 18u) & 0xfu)
			);
			value.awprot = (
				uint8_t((backing[2] >> 22u) & 0x7u)
			);
			value.awqos = (
				uint8_t((backing[2] >> 25u) & 0xfu)
			);
			value.awregion = (
				(uint8_t((backing[2] >> 29u) & 0x7u) << 0) | 
				(uint8_t((backing[3] >> 0u) & 0x1fu) << 3)
			);
			value.awuser = (
				uint8_t((backing[3] >> 5u) & 0x7u)
			);
			return value;
		}
		std::array<uint32_t, 4> pack() const {
			std::array<uint32_t, 4> backing{};
			backing[0] = (
				(uint32_t((awid >> 0u) & uint8_t(0xful)) << 0) | 
				(uint32_t((awaddr >> 0u) & 0xffffffful) << 4)
			);
			backing[1] = (
				uint32_t((awaddr >> 28u) & 0xfffffffful)
			);
			backing[2] = (
				(uint32_t((awaddr >> 60u) & 0xful) << 0) | 
				(uint32_t((awlen >> 0u) & uint8_t(0xfful)) << 4) | 
				(uint32_t((awsize >> 0u) & uint8_t(0x7ul)) << 12) | 
				(uint32_t((awburst >> 0u) & uint8_t(0x3ul)) << 15) | 
				(uint32_t((awlock >> 0u) & uint8_t(0x1ul)) << 17) | 
				(uint32_t((awcache >> 0u) & uint8_t(0xful)) << 18) | 
				(uint32_t((awprot >> 0u) & uint8_t(0x7ul)) << 22) | 
				(uint32_t((awqos >> 0u) & uint8_t(0xful)) << 25) | 
				(uint32_t((awregion >> 0u) & uint8_t(0x7ul)) << 29)
			);
			backing[3] = (
				(uint32_t((awregion >> 3u) & uint8_t(0x1ful)) << 0) | 
				(uint32_t((awuser >> 0u) & uint8_t(0x7ul)) << 5)
			);
			return backing;
		}
		bool operator==(const AWFlit_id4_addr64_user0&) const = default;
	};
	struct WFlit_data32 {
		/** 32-bit field */
		uint32_t wdata;
		/** 4-bit field */
		uint8_t wstrb;
		/** 1-bit field */
		uint8_t wlast;
	
		static WFlit_data32 unpack(const uint64_t& backing) {
			WFlit_data32 value{};
			value.wdata = (
				uint32_t((backing >> 0u) & 0xfffffffful)
			);
			value.wstrb = (
				uint8_t((backing >> 32u) & 0xful)
			);
			value.wlast = (
				uint8_t((backing >> 36u) & 0x1ul)
			);
			return value;
		}
		uint64_t pack() const {
			uint64_t backing{};
			backing = (
				(uint64_t((wdata >> 0u) & 0xffffffffu) << 0) | 
				(uint64_t((wstrb >> 0u) & uint8_t(0xful)) << 32) | 
				(uint64_t((wlast >> 0u) & uint8_t(0x1ul)) << 36)
			);
			return backing;
		}
		bool operator==(const WFlit_data32&) const = default;
	};
	struct BFlit_id4 {
		/** 4-bit field */
		uint8_t bid;
		/** 2-bit field */
		uint8_t bresp;
	
		static BFlit_id4 unpack(const uint8_t& backing) {
			BFlit_id4 value{};
			value.bid = (
				((backing >> 0u) & uint8_t(0xful))
			);
			value.bresp = (
				((backing >> 4u) & uint8_t(0x3ul))
			);
			return value;
		}
		uint8_t pack() const {
			uint8_t backing{};
			backing = (
				(((bid >> 0u) & uint8_t(0xful)) << 0) | 
				(((bresp >> 0u) & uint8_t(0x3ul)) << 4)
			);
			return backing;
		}
		bool operator==(const BFlit_id4&) const = default;
	};
	struct ARFlit_id4_addr64_user0 {
		/** 4-bit field */
		uint8_t arid;
		/** 64-bit field */
		uint64_t araddr;
		/** 8-bit field */
		uint8_t arlen;
		/** 3-bit field */
		uint8_t arsize;
		/** 2-bit field */
		uint8_t arburst;
		/** 1-bit field */
		uint8_t arlock;
		/** 4-bit field */
		uint8_t arcache;
		/** 3-bit field */
		uint8_t arprot;
		/** 4-bit field */
		uint8_t arqos;
		/** 8-bit field */
		uint8_t arregion;
	
		static ARFlit_id4_addr64_user0 unpack(const std::array<uint32_t, 4>& backing) {
			ARFlit_id4_addr64_user0 value{};
			value.arid = (
				uint8_t((backing[0] >> 0u) & 0xfu)
			);
			value.araddr = (
				(uint64_t((backing[0] >> 4u) & 0xfffffffu) << 0) | 
				(uint64_t((backing[1] >> 0u) & 0xffffffffu) << 28) | 
				(uint64_t((backing[2] >> 0u) & 0xfu) << 60)
			);
			value.arlen = (
				uint8_t((backing[2] >> 4u) & 0xffu)
			);
			value.arsize = (
				uint8_t((backing[2] >> 12u) & 0x7u)
			);
			value.arburst = (
				uint8_t((backing[2] >> 15u) & 0x3u)
			);
			value.arlock = (
				uint8_t((backing[2] >> 17u) & 0x1u)
			);
			value.arcache = (
				uint8_t((backing[2] >> 18u) & 0xfu)
			);
			value.arprot = (
				uint8_t((backing[2] >> 22u) & 0x7u)
			);
			value.arqos = (
				uint8_t((backing[2] >> 25u) & 0xfu)
			);
			value.arregion = (
				(uint8_t((backing[2] >> 29u) & 0x7u) << 0) | 
				(uint8_t((backing[3] >> 0u) & 0x1fu) << 3)
			);
			return value;
		}
		std::array<uint32_t, 4> pack() const {
			std::array<uint32_t, 4> backing{};
			backing[0] = (
				(uint32_t((arid >> 0u) & uint8_t(0xful)) << 0) | 
				(uint32_t((araddr >> 0u) & 0xffffffful) << 4)
			);
			backing[1] = (
				uint32_t((araddr >> 28u) & 0xfffffffful)
			);
			backing[2] = (
				(uint32_t((araddr >> 60u) & 0xful) << 0) | 
				(uint32_t((arlen >> 0u) & uint8_t(0xfful)) << 4) | 
				(uint32_t((arsize >> 0u) & uint8_t(0x7ul)) << 12) | 
				(uint32_t((arburst >> 0u) & uint8_t(0x3ul)) << 15) | 
				(uint32_t((arlock >> 0u) & uint8_t(0x1ul)) << 17) | 
				(uint32_t((arcache >> 0u) & uint8_t(0xful)) << 18) | 
				(uint32_t((arprot >> 0u) & uint8_t(0x7ul)) << 22) | 
				(uint32_t((arqos >> 0u) & uint8_t(0xful)) << 25) | 
				(uint32_t((arregion >> 0u) & uint8_t(0x7ul)) << 29)
			);
			backing[3] = (
				uint32_t((arregion >> 3u) & uint8_t(0x1ful))
			);
			return backing;
		}
		bool operator==(const ARFlit_id4_addr64_user0&) const = default;
	};
	struct RFlit_id4_data32 {
		/** 4-bit field */
		uint8_t rid;
		/** 32-bit field */
		uint32_t rdata;
		/** 2-bit field */
		uint8_t rresp;
		/** 1-bit field */
		uint8_t rlast;
	
		static RFlit_id4_data32 unpack(const uint64_t& backing) {
			RFlit_id4_data32 value{};
			value.rid = (
				uint8_t((backing >> 0u) & 0xful)
			);
			value.rdata = (
				uint32_t((backing >> 4u) & 0xfffffffful)
			);
			value.rresp = (
				uint8_t((backing >> 36u) & 0x3ul)
			);
			value.rlast = (
				uint8_t((backing >> 38u) & 0x1ul)
			);
			return value;
		}
		uint64_t pack() const {
			uint64_t backing{};
			backing = (
				(uint64_t((rid >> 0u) & uint8_t(0xful)) << 0) | 
				(uint64_t((rdata >> 0u) & 0xffffffffu) << 4) | 
				(uint64_t((rresp >> 0u) & uint8_t(0x3ul)) << 36) | 
				(uint64_t((rlast >> 0u) & uint8_t(0x1ul)) << 38)
			);
			return backing;
		}
		bool operator==(const RFlit_id4_data32&) const = default;
	};
	
}

namespace key_manager {
	struct Tuple2_KeyId_MaybeKey {
		/** 8-bit field */
		uint8_t keyId;
		/** 1-bit field */
		uint8_t keyValid;
		/** 64-bit field */
		uint64_t keyTop;
		/** 64-bit field */
		uint64_t keyBot;
	
		static Tuple2_KeyId_MaybeKey unpack(const std::array<uint32_t, 5>& backing) {
			Tuple2_KeyId_MaybeKey value{};
			value.keyId = (
				uint8_t((backing[0] >> 0u) & 0xffu)
			);
			value.keyValid = (
				uint8_t((backing[0] >> 8u) & 0x1u)
			);
			value.keyTop = (
				(uint64_t((backing[0] >> 9u) & 0x7fffffu) << 0) | 
				(uint64_t((backing[1] >> 0u) & 0xffffffffu) << 23) | 
				(uint64_t((backing[2] >> 0u) & 0x1ffu) << 55)
			);
			value.keyBot = (
				(uint64_t((backing[2] >> 9u) & 0x7fffffu) << 0) | 
				(uint64_t((backing[3] >> 0u) & 0xffffffffu) << 23) | 
				(uint64_t((backing[4] >> 0u) & 0x1ffu) << 55)
			);
			return value;
		}
		std::array<uint32_t, 5> pack() const {
			std::array<uint32_t, 5> backing{};
			backing[0] = (
				(uint32_t((keyId >> 0u) & uint8_t(0xfful)) << 0) | 
				(uint32_t((keyValid >> 0u) & uint8_t(0x1ul)) << 8) | 
				(uint32_t((keyTop >> 0u) & 0x7ffffful) << 9)
			);
			backing[1] = (
				uint32_t((keyTop >> 23u) & 0xfffffffful)
			);
			backing[2] = (
				(uint32_t((keyTop >> 55u) & 0x1fful) << 0) | 
				(uint32_t((keyBot >> 0u) & 0x7ffffful) << 9)
			);
			backing[3] = (
				uint32_t((keyBot >> 23u) & 0xfffffffful)
			);
			backing[4] = (
				uint32_t((keyBot >> 55u) & 0x1fful)
			);
			return backing;
		}
		bool operator==(const Tuple2_KeyId_MaybeKey&) const = default;
	};
	
}

template <> class fmt::formatter<axi::IOCapAxi::AWFlit_id4_addr64_user3> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (axi::IOCapAxi::AWFlit_id4_addr64_user3 const& s, Context& ctx) const {
		return format_to(ctx.out(), "AWFlit_id4_addr64_user3 {{ .awid = 0x{:01x}, .awaddr = 0x{:016x}, .awlen = 0x{:02x}, .awsize = 0x{:01x}, .awburst = 0x{:01x}, .awlock = 0x{:01x}, .awcache = 0x{:01x}, .awprot = 0x{:01x}, .awqos = 0x{:01x}, .awregion = 0x{:02x}, .awuser = 0x{:01x} }}", s.awid, s.awaddr, s.awlen, s.awsize, s.awburst, s.awlock, s.awcache, s.awprot, s.awqos, s.awregion, s.awuser);
	}
};
template <> class fmt::formatter<axi::IOCapAxi::WFlit_data32> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (axi::IOCapAxi::WFlit_data32 const& s, Context& ctx) const {
		return format_to(ctx.out(), "WFlit_data32 {{ .wdata = 0x{:08x}, .wstrb = 0x{:01x}, .wlast = 0x{:01x} }}", s.wdata, s.wstrb, s.wlast);
	}
};
template <> class fmt::formatter<axi::IOCapAxi::BFlit_id4> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (axi::IOCapAxi::BFlit_id4 const& s, Context& ctx) const {
		return format_to(ctx.out(), "BFlit_id4 {{ .bid = 0x{:01x}, .bresp = 0x{:01x} }}", s.bid, s.bresp);
	}
};
template <> class fmt::formatter<axi::IOCapAxi::ARFlit_id4_addr64_user3> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (axi::IOCapAxi::ARFlit_id4_addr64_user3 const& s, Context& ctx) const {
		return format_to(ctx.out(), "ARFlit_id4_addr64_user3 {{ .arid = 0x{:01x}, .araddr = 0x{:016x}, .arlen = 0x{:02x}, .arsize = 0x{:01x}, .arburst = 0x{:01x}, .arlock = 0x{:01x}, .arcache = 0x{:01x}, .arprot = 0x{:01x}, .arqos = 0x{:01x}, .arregion = 0x{:02x}, .aruser = 0x{:01x} }}", s.arid, s.araddr, s.arlen, s.arsize, s.arburst, s.arlock, s.arcache, s.arprot, s.arqos, s.arregion, s.aruser);
	}
};
template <> class fmt::formatter<axi::IOCapAxi::RFlit_id4_data32> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (axi::IOCapAxi::RFlit_id4_data32 const& s, Context& ctx) const {
		return format_to(ctx.out(), "RFlit_id4_data32 {{ .rid = 0x{:01x}, .rdata = 0x{:08x}, .rresp = 0x{:01x}, .rlast = 0x{:01x} }}", s.rid, s.rdata, s.rresp, s.rlast);
	}
};
template <> class fmt::formatter<axi::SanitizedAxi::AWFlit_id4_addr64_user0> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (axi::SanitizedAxi::AWFlit_id4_addr64_user0 const& s, Context& ctx) const {
		return format_to(ctx.out(), "AWFlit_id4_addr64_user0 {{ .awid = 0x{:01x}, .awaddr = 0x{:016x}, .awlen = 0x{:02x}, .awsize = 0x{:01x}, .awburst = 0x{:01x}, .awlock = 0x{:01x}, .awcache = 0x{:01x}, .awprot = 0x{:01x}, .awqos = 0x{:01x}, .awregion = 0x{:02x}, .awuser = 0x{:01x} }}", s.awid, s.awaddr, s.awlen, s.awsize, s.awburst, s.awlock, s.awcache, s.awprot, s.awqos, s.awregion, s.awuser);
	}
};
template <> class fmt::formatter<axi::SanitizedAxi::WFlit_data32> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (axi::SanitizedAxi::WFlit_data32 const& s, Context& ctx) const {
		return format_to(ctx.out(), "WFlit_data32 {{ .wdata = 0x{:08x}, .wstrb = 0x{:01x}, .wlast = 0x{:01x} }}", s.wdata, s.wstrb, s.wlast);
	}
};
template <> class fmt::formatter<axi::SanitizedAxi::BFlit_id4> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (axi::SanitizedAxi::BFlit_id4 const& s, Context& ctx) const {
		return format_to(ctx.out(), "BFlit_id4 {{ .bid = 0x{:01x}, .bresp = 0x{:01x} }}", s.bid, s.bresp);
	}
};
template <> class fmt::formatter<axi::SanitizedAxi::ARFlit_id4_addr64_user0> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (axi::SanitizedAxi::ARFlit_id4_addr64_user0 const& s, Context& ctx) const {
		return format_to(ctx.out(), "ARFlit_id4_addr64_user0 {{ .arid = 0x{:01x}, .araddr = 0x{:016x}, .arlen = 0x{:02x}, .arsize = 0x{:01x}, .arburst = 0x{:01x}, .arlock = 0x{:01x}, .arcache = 0x{:01x}, .arprot = 0x{:01x}, .arqos = 0x{:01x}, .arregion = 0x{:02x} }}", s.arid, s.araddr, s.arlen, s.arsize, s.arburst, s.arlock, s.arcache, s.arprot, s.arqos, s.arregion);
	}
};
template <> class fmt::formatter<axi::SanitizedAxi::RFlit_id4_data32> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (axi::SanitizedAxi::RFlit_id4_data32 const& s, Context& ctx) const {
		return format_to(ctx.out(), "RFlit_id4_data32 {{ .rid = 0x{:01x}, .rdata = 0x{:08x}, .rresp = 0x{:01x}, .rlast = 0x{:01x} }}", s.rid, s.rdata, s.rresp, s.rlast);
	}
};
template <> class fmt::formatter<key_manager::Tuple2_KeyId_MaybeKey> {
	public:
	// Ignore parse formats - only {} is supported for this type
	constexpr auto parse (fmt::format_parse_context& ctx) { return ctx.begin(); }
	template <typename Context>
	constexpr auto format (key_manager::Tuple2_KeyId_MaybeKey const& s, Context& ctx) const {
		return format_to(ctx.out(), "Tuple2_KeyId_MaybeKey {{ .keyId = 0x{:02x}, .keyValid = 0x{:01x}, .keyTop = 0x{:016x}, .keyBot = 0x{:016x} }}", s.keyId, s.keyValid, s.keyTop, s.keyBot);
	}
};


#endif // TB_BITFIELDS_H
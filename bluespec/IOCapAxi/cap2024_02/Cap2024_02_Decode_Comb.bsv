package Cap2024_02_Decode_Comb;

import Cap2024_02::*;
import GetPut::*;
import CheckedBits::*;
import LeftShift::*;

export mkCombCapDecode;

typedef struct {
    // The top 54 bits of the base address
    ChkUInt#(54) base_addr_shr_10;
    // Length = "element" width * "element" count
    // The number of "elements"
    ChkUInt#(28) elem_count;
    // log2(element width) - 10
    ChkUInt#(5) elem_width_log2_minus_10;
    // log2(max(elem_count)), which index-caveats are expressed relative to
    ChkUInt#(5) log_max_count;
} InitialResource deriving (Bits, FShow);

typedef struct {
    ChkUInt#(64) base_addr;
    // elem_count is implicitly 1
    // elem_width_log2 may be = 64
    ChkUInt#(7) elem_width_log2;
} PostCaveat1 deriving (Bits, FShow);

typedef struct {
    ChkUInt#(64) base_addr;
    ChkUInt#(10) elem_count;
    ChkUInt#(6) elem_width_log2;
} PostCaveat2 deriving (Bits, FShow);

typeclass EncodesRange#(type a);
    function ActionValue#(CapRange) rangeOf(a val);
endtypeclass

// TODO bounds assertions

instance EncodesRange#(InitialResource);
    function ActionValue#(CapRange) rangeOf(InitialResource init) = actionvalue
        // shift right by 10
        ChkUInt#(64) base = rightZeroExtend(init.base_addr_shr_10);
        // shift right by 10
        ChkUInt#(38) elem_count_shl_10 = rightZeroExtend(init.elem_count);
        // (elem_count << 10) << (elem_width_log2 - 10)
        // = (elem_count << elem_width_log2)
        // for all elem_width_log2 >= 10
        ChkUInt#(65) length <- shl(extend(elem_count_shl_10), init.elem_width_log2_minus_10, "InitialResource rangeOf");
        let top <- add(zeroExtend(base), length, "InitialResource rangeOf");
        return CapRange{base: pack(currVal(base)), top: pack(currVal(top))};
    endactionvalue;
endinstance

instance EncodesRange#(PostCaveat1);
    function ActionValue#(CapRange) rangeOf(PostCaveat1 cav1) = actionvalue
        // length = 65-bit value of 1 << elem_width_log2
        let length <- shl(65'd1, cav1.elem_width_log2, "PostCaveat1 rangeOf");
        let top <- add(zeroExtend(cav1.base_addr), length, "PostCaveat1 rangeOf");
        return CapRange{base: pack(currVal(cav1.base_addr)), top: pack(currVal(top))};
    endactionvalue;
endinstance

instance EncodesRange#(PostCaveat2);
    function ActionValue#(CapRange) rangeOf(PostCaveat2 cav2) = actionvalue
        ChkUInt#(65) length <- shl(zeroExtend(cav2.elem_count), cav2.elem_width_log2, "PostCaveat2 rangeOf");
        let top <- add(zeroExtend(cav2.base_addr), length, "PostCaveat2 rangeOf");
        return CapRange{base: pack(currVal(cav2.base_addr)), top: pack(currVal(top))};
    endactionvalue;
endinstance

function UInt#(5) decodeElemWidthLog2Minus10(Bit#(5) encodedElemWidth);
    case (encodedElemWidth[4:3])
        2'b00: return unpack(encodedElemWidth); 
        2'b01: return unpack(encodedElemWidth) - 1;
        2'b10: return unpack(encodedElemWidth) - 2;
        2'b11: if (encodedElemWidth == 31) begin
            return 0;
        end else begin
            return unpack(encodedElemWidth) - 3;
        end 
    endcase
endfunction

function UInt#(2) decodeQuadrant(Bit#(5) encodedElemWidth);
    if (encodedElemWidth[2:0] == 3'b111) begin
        return (unpack(encodedElemWidth[4:3]) + 1) & 2'b11;
    end else begin
        return unpack(encodedElemWidth[4:3]);
    end
endfunction

function UInt#(1) decodeImplicit1(Bit#(5) encodedElemWidth);
    return (encodedElemWidth[2:0] == 3'b111) ? 0 : 1;
endfunction

module mkCombCapDecode#(Get#(Cap2024_02) in, Put#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) out)(Empty);
    function ActionValue#(InitialResource) decodeInitialResource(Cap2024_02 in) = actionvalue
        let quadrant = decodeQuadrant(in.encoded_elem_width);
        let implicit_1 = decodeImplicit1(in.encoded_elem_width);

        ChkUInt#(54) base_addr_shr_10 = ?;
        ChkUInt#(27) elem_count_minus_one = ?;
        UInt#(5) log_max_count_no_implicit_one = ?;
        case (quadrant)
            2'b00: begin
                base_addr_shr_10 = withMaxBound(unpack(
                    {in.b_c[58:5]}
                ));
                elem_count_minus_one = withMaxBound(unpack(
                    {0, pack(implicit_1), in.b_c[4:0]}
                ));
                log_max_count_no_implicit_one = 5;
            end
            2'b01: begin
                base_addr_shr_10 = withMaxBound(unpack(
                    {in.b_c[58:12], 0}
                ));
                elem_count_minus_one = withMaxBound(unpack(
                    {0, pack(implicit_1), in.b_c[11:0]}
                ));
                log_max_count_no_implicit_one = 12;
            end
            2'b10: begin
                base_addr_shr_10 = withMaxBound(unpack(
                    {in.b_c[58:19], 0}
                ));
                elem_count_minus_one = withMaxBound(unpack(
                    {0, pack(implicit_1), in.b_c[18:0]}
                ));
                log_max_count_no_implicit_one = 19;
            end
            2'b11: begin
                base_addr_shr_10 = withMaxBound(unpack(
                    {in.b_c[58:26], 0}
                ));
                elem_count_minus_one = withMaxBound(unpack(
                    {pack(implicit_1), in.b_c[25:0]}
                ));
                log_max_count_no_implicit_one = 26;
            end
        endcase

        ChkUInt#(5) log_max_count <- withBound(
            log_max_count_no_implicit_one + extend(implicit_1),
            5, 27,
            "log_max_count has a statically known bound"
        );
        ChkUInt#(28) elem_count <- add(extend(elem_count_minus_one), 1, "elem_count = elem_count_minus_one + 1");

        let elem_width_log2_minus_10_unbounded = decodeElemWidthLog2Minus10(in.encoded_elem_width);
        let elem_width_log2_minus_10 <- withBound(elem_width_log2_minus_10_unbounded, 0, 27, "elem_width_log2_minus_10");

        return InitialResource {
            base_addr_shr_10: base_addr_shr_10,
            elem_count: elem_count,
            log_max_count: log_max_count,
            elem_width_log2_minus_10: elem_width_log2_minus_10
        };
    endactionvalue;

    function ActionValue#(Maybe#(PostCaveat1)) refineInitialWithCav1(Cap2024_02 in, InitialResource init) = actionvalue
        // TODO because log_max_count is pretty static, we could implement the shift by it and additions with it statically

        ChkUInt#(4) size_div = withMaxBound(in.index_size_div);
        ChkUInt#(15) index = withMaxBound(in.index);

        ChkUInt#(43) check_rhs <- shl(extend(init.elem_count), size_div, "check_rhs");
        ChkUInt#(16) index_plus_one <- add(extend(index), 1, "index plus 1");
        ChkUInt#(43) check_lhs <- shl(extend(index_plus_one), init.log_max_count, "check_lhs");

        if (isValid(sub_overflow(check_rhs, check_lhs))) begin
            // TODO we could sized_add init.log_max_count+10 statically?
            ChkUInt#(6) old_elem_width_log2 <- add(
                extend(init.elem_width_log2_minus_10), 10,
                "cav1 old_elem_width_log2"
            );
            // 7-bit add
            ChkUInt#(7) old_elem_width_log2_plus_log_max_count <- add(
                extend(old_elem_width_log2), extend(init.log_max_count),
                "cav1 old_elem_width_log2_plus_log_max_count"
            );
            ChkUInt#(7) elem_width_log2 <- sub(
                old_elem_width_log2_plus_log_max_count,
                // extend from 4 to 7-bit
                extend(size_div),
                "cav1 elem_width_log2"
            );

            // The check_rhs and check_lhs have ensured this won't go OOB
            ChkUInt#(64) start_offset = shl_unchecked(extend(index), elem_width_log2);
            ChkUInt#(64) base = rightZeroExtend(init.base_addr_shr_10);

            case (add_overflow(base, start_offset)) matches
                tagged Valid .new_base : return tagged Valid PostCaveat1 {
                    base_addr: new_base,
                    elem_width_log2: elem_width_log2
                };
                tagged Invalid : return tagged Invalid;
            endcase
        end else return tagged Invalid;
    endactionvalue;

    function ActionValue#(Maybe#(PostCaveat2)) refineCav1WithCav2(Cap2024_02 in, PostCaveat1 cav1) = actionvalue
        ChkUInt#(10) elem_count = ?;
        ChkUInt#(9) range_x = withMaxBound(in.range_x);
        ChkUInt#(9) range_y_minus_one = withMaxBound(in.range_y_minus_one);
        // 9-bit subtraction
        case (sub_overflow(range_y_minus_one, range_x)) matches
            tagged Valid .elem_count_minus_one : begin
                elem_count <- add(extend(elem_count_minus_one), 1, "cav2 elem_count");

                ChkUInt#(6) elem_width_log2 = ?;
                ChkUInt#(64) start_offset = ?;
                // 7-bit subtraction
                case (sub_overflow(cav1.elem_width_log2, 9)) matches
                    tagged Valid .elem_width_log2_toowide : begin
                        elem_width_log2 = truncate(elem_width_log2_toowide);
                        start_offset <- shl(extend(range_x), elem_width_log2_toowide, "cav2 big case start_offset");
                    end
                    tagged Invalid : begin
                        // cav1.elem_width_log_2 is too small to subdivide into 2^9 pieces.
                        // e.g. if the range is 256 bytes long we don't have enough room for 512 pieces.
                        // => We need to eliminate some bits of the range to fix the precision at 1-byte pieces - so if the range is 256, only use 8 bits of range_y and range_x.
                        // This is effectively truncation.
                        // Truncation usually requires a variable shift-right (i.e. extracting a bit range starting at a variable position), but we don't have a right-shifter - we only have a left-shifter.
                        // Fortunately we have an upper limit on the truncation - it can be at most 9 (range_x = 0, range_y = 1 << 9, cav1_elem_width_log_2 = 0 -> new_range_x = 0, new_length = 1)
                        // so we can take values range_x[8:0] and elem_count[9:0], treat them as fixed point numbers with 9 fractional bits (i.e. the integer portion is the maximum right-shift)
                        // and then shift those fixed-point numbers *left* by (cav2_elem_width_log_2 + 9) (assuming cav2_elem_width_log_2 = -1 for a 1-bit right shift)
                        // i.e. shift them left by cav1_elem_width_log_2(!)
                        // and finally take the integer portion post-shift, which is a static window and thus cheap in hardware.

                        elem_width_log2 = 0;
                        let cav1_elem_width_log2 <- withBound(currVal(cav1.elem_width_log2), 0, 8, "In this block elem_width_log2 must be 0..=8");

                        // NOTE: in a more complicated impl these shifts can be combined into one!

                        ChkUInt#(17) start_offset_shl_9 <- shl(extend(range_x), cav1_elem_width_log2, "cav2 small case start_offset");
                        // Manually set the width of start_offset to 8 to make sure we shift down by 9 bits
                        ChkUInt#(8) start_offset_shrunk = rightTruncate(start_offset_shl_9);
                        // Expand it out to 64 bits when we store it
                        start_offset = extend(start_offset_shrunk);

                        ChkUInt#(18) elem_count_shl_9 <- shl(extend(elem_count), cav1_elem_width_log2, "cav2 small case elem_count");
                        // Manually set the width of elem_count to 9 to make sure we shift down by 9 bits
                        ChkUInt#(9) elem_count_shrunk = rightTruncate(elem_count_shl_9);
                        // Expand it out to 10 bits when we store it
                        elem_count = extend(elem_count_shrunk);
                    end
                endcase

                case (add_overflow(cav1.base_addr, start_offset)) matches
                    tagged Invalid : return tagged Invalid;
                    tagged Valid .new_base : return tagged Valid PostCaveat2 {
                        base_addr: new_base,
                        elem_count: elem_count,
                        elem_width_log2: elem_width_log2
                    };
                endcase
            end
            tagged Invalid : return tagged Invalid;
        endcase
    endactionvalue;

    function ActionValue#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) decodeIn(Cap2024_02 in) = actionvalue
        if (in.chain matches tagged Valid .chain) begin
            let init <- decodeInitialResource(in);

            if (!isValid(atCav1(chain))) begin
                // There's only the initial resource
                let range <- rangeOf(init);
                return tagged Succ tuple2(permsOfChain(chain), range);
            end else begin
                // The first caveat is in use
                let postCav1 <- refineInitialWithCav1(in, init);
                
                if (postCav1 matches tagged Valid .cav1) begin
                    // Caveat 1 was applied and worked :)
                    if (!isValid(atCav2(chain))) begin
                        // There isn't a second caveat, => get the range for the first caveat
                        let range <- rangeOf(cav1);
                        return tagged Succ tuple2(permsOfChain(chain), range);
                    end else begin
                        // The second caveat is in use
                        let postCav2 <- refineCav1WithCav2(in, cav1);
                        if (postCav2 matches tagged Valid .cav2) begin
                            // Caveat 2 was applied and worked :)
                            let range <- rangeOf(cav2);
                            return tagged Succ tuple2(permsOfChain(chain), range);
                        end else begin
                            // Caveat 2 was applied and Invalid :(
                            return tagged Fail InvalidCaveat;
                        end
                    end
                end else begin
                    // Caveat 1 was applied and Invalid :(
                    return tagged Fail InvalidCaveat;
                end
            end
        end else begin
            // The permissions chain was invalid
            return tagged Fail InvalidCapPermsChain;
        end
    endactionvalue;

    rule r;
        let i <- in.get();
        let decoded <- decodeIn(i);
        out.put(decoded);
    endrule
endmodule

endpackage
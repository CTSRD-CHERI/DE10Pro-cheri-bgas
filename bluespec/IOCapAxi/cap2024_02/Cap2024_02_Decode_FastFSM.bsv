package Cap2024_02_Decode_FastFSM;

/// This package defines a fast Cap2024_02 decoder using a FSM.
/// It is limited to a frontend stage and a main decoder stage which is busy for 3, 6, or 7 cycles
/// decoding the capability based on the number of caveats involved

import Cap2024_02::*;
import GetPut::*;
import StmtFSM::*;
import LeftShift::*;
import MattUtil::*;

export mkFastFSMCapDecode;

// The internals of the decoder are structured as a single module which takes a single command each cycle
// encompassing four computations for it to do. Those inputs are clocked and at the start of each cycle
// the previous cycle's computations are performed.
// The available computations are
// - 65-bit left shift by up to 7 bits 0..=64
// - 65-bit addition or subtraction
// - 16-bit addition or subtraction
// - 9-bit addition or subtraction TODO can this be 7-bit? maybe...
// Commands for each addition/subtraction are represented as a struct AddOp, with a parameter AddOrSub.
// 

typedef enum {
    Add,
    Sub
} AddOrSub deriving (Bits, FShow, Eq);

typedef struct {
    AddOrSub op;
    Bit#(n) a;
    Bit#(n) b;
} AddOp#(numeric type n) deriving (Bits, FShow);

function Bit#(n) evalAddOp(AddOp#(n) op);
    if (op.op == Add)
        return op.a + op.b;
    else
        return op.a - op.b;
endfunction

// TODO this only works for i, j < n NOT i, j <= n? maybe?

function AddOp#(n) add(Bit#(i) a, Bit#(j) b) provisos (Add#(i, _a, n), Add#(j, _b, n));
    return AddOp { op: Add, a: zeroExtend(a), b: zeroExtend(b) };
endfunction

function AddOp#(n) sub(Bit#(i) a, Bit#(j) b) provisos (Add#(i, _a, n), Add#(j, _b, n));
    return AddOp { op: Sub, a: zeroExtend(a), b: zeroExtend(b) };
endfunction

// The overall command for InternalCalc
typedef struct {
    Bit#(65) shl_to_shift;
    Bit#(7) shl_amt;

    AddOp#(65) add_op_65;
    AddOp#(16) add_op_16;
    AddOp#(9) add_op_9;
} CalcInput deriving (Bits, FShow);

// InternalCalc allows retrieving values even on cycles where none has been calculated, ? is returned in that case
interface InternalCalc;
    // Get the output from shl_to_shift << shl_amt from the previous cycle
    method Bit#(65) shifted;
    // Get the output from add_op_65 from the previous cycle
    method Bit#(65) added_65;
    // Get the output from add_op_16 from the previous cycle
    method Bit#(16) added_16;
    // Get the output from add_op_9 from the previous cycle
    method Bit#(9) added_9;

    // Set the new input for the next cycle
    interface Put#(CalcInput) in;
endinterface

module mkInternalCalc(InternalCalc);
    // Inputs are clocked, outputs are not
    // Use vReg so we only do computations when requested
    Reg#(CalcInput) inReg <- mkVReg;

    Wire#(Bit#(65)) shifted_w <- mkDWire(?);
    Wire#(Bit#(65)) added_65_w <- mkDWire(?);
    Wire#(Bit#(16)) added_16_w <- mkDWire(?);
    Wire#(Bit#(9)) added_9_w <- mkDWire(?);

    rule comp;
        let in = inReg;
        shifted_w <= left_shift_comb(in.shl_to_shift, unpack(in.shl_amt));
        added_65_w <= evalAddOp(in.add_op_65);
        added_16_w <= evalAddOp(in.add_op_16);
        added_9_w <= evalAddOp(in.add_op_9);
    endrule

    // Get the output from shl_to_shift << shl_amt from the previous cycle
    method Bit#(65) shifted;
        return shifted_w;
    endmethod
    // Get the output from add_op_65 from the previous cycle
    method Bit#(65) added_65;
        return added_65_w;
    endmethod
    // Get the output from add_op_16 from the previous cycle
    method Bit#(16) added_16;
        return added_16_w;
    endmethod
    // Get the output from add_op_9 from the previous cycle
    method Bit#(9) added_9;
        return added_9_w;
    endmethod

    interface in = toPut(asIfc(inReg));
endmodule

// The frontend/first stage of the decoder decodes the top-level B|C encoding into separate base_addr, elem_count, elem_width_log2, log_max_count in one cycle.
typedef struct {
    Bool zero_cav;
    Bool one_cav;
    Bool two_cav;

    CapPerms perms;

    Bit#(64) base_addr;
    Bit#(28) elem_count;
    Bit#(7) elem_width_log2;
    Bit#(5) log_max_count;

    Bit#(4) index_size_div;
    Bit#(15) index;
    Bit#(9) range_x;
    Bit#(9) range_y_minus_one;

    Maybe#(CapFailReason) fail;
} FrontendMatter deriving (Bits, FShow);

// There are some helper functions for different parts
// elem_width_log2 is decoded from a 5-bit value to an integer 10..=37, so needs 6 bits.
function Bit#(6) decodeElemWidthLog2(Bit#(5) encodedElemWidth);
    case (encodedElemWidth[4:3])
        2'b00: return zeroExtend(encodedElemWidth) + 10; 
        2'b01: return zeroExtend(encodedElemWidth) + 9;
        2'b10: return zeroExtend(encodedElemWidth) + 8;
        2'b11: if (encodedElemWidth == 31) begin
            return 10;
        end else begin
            return zeroExtend(encodedElemWidth) + 7;
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

// The actual frontend decoder
function FrontendMatter frontend(Cap2024_02 in);
    case (in.chain) matches
        tagged Invalid: return FrontendMatter {
            fail: tagged Valid InvalidCapPermsChain,
            // the rest are undefined, getting InvalidCapPermsChain immediately bails
            zero_cav: ?,
            one_cav: ?,
            two_cav: ?,

            perms: ?,
            
            base_addr: ?,
            elem_count: ?,
            elem_width_log2: ?,
            log_max_count: ?,

            index_size_div: ?,
            index: ?,
            range_x: ?,
            range_y_minus_one: ?
        };
        tagged Valid .chain: begin
            let quadrant = decodeQuadrant(in.encoded_elem_width);
            let implicit_1 = decodeImplicit1(in.encoded_elem_width);
    
            Bit#(64) base_addr = ?;
            Bit#(27) elem_count_minus_one = ?;
            UInt#(5) log_max_count_no_implicit_one = ?;
            case (quadrant)
                2'b00: begin
                    base_addr = {in.b_c[58:5], 0};
                    elem_count_minus_one = {0, pack(implicit_1), in.b_c[4:0]};
                    log_max_count_no_implicit_one = 5;
                end
                2'b01: begin
                    base_addr = {in.b_c[58:12], 0};
                    elem_count_minus_one = {0, pack(implicit_1), in.b_c[11:0]};
                    log_max_count_no_implicit_one = 12;
                end
                2'b10: begin
                    base_addr = {in.b_c[58:19], 0};
                    elem_count_minus_one = {0, pack(implicit_1), in.b_c[18:0]};
                    log_max_count_no_implicit_one = 19;
                end
                2'b11: begin
                    base_addr = {in.b_c[58:26], 0};
                    elem_count_minus_one = {pack(implicit_1), in.b_c[25:0]};
                    log_max_count_no_implicit_one = 26;
                end
            endcase
    
            UInt#(5) log_max_count = log_max_count_no_implicit_one + extend(implicit_1);
            UInt#(28) elem_count = extend(unpack(elem_count_minus_one)) + 1;
    
            let elem_width_log2 = decodeElemWidthLog2(in.encoded_elem_width);
            
            return FrontendMatter {
                // no failure
                fail: tagged Invalid,
            
                zero_cav: !isValid(atCav1(chain)),
                one_cav: isValid(atCav1(chain)) && !isValid(atCav2(chain)),
                two_cav: isValid(atCav2(chain)),

                perms: permsOfChain(chain),
                
                base_addr: base_addr,
                elem_count: pack(elem_count),
                elem_width_log2: zeroExtend(elem_width_log2),
                log_max_count: pack(log_max_count),

                index_size_div: pack(in.index_size_div),
                index: pack(in.index),
                range_x: pack(in.range_x),
                range_y_minus_one: pack(in.range_y_minus_one)
            };
        end
    endcase
endfunction

// Helper function because Bluespec doesn't have an implicit conversion between Bit#(1) and Bool :(
function Bool msbSet(Bit#(n) x) = unpack(msb(x));

// TODO this is timing-independent on invalid caveats right now because that's the most convenient thing to do with an FSM.
// Maybe bad for real fastness.
module mkFastFSMCapDecode#(Get#(Cap2024_02) in, Put#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) out)(Empty);
    InternalCalc calc <- mkInternalCalc;

    // Working registers for the state machine.
    Reg#(Bool) working_zero_cav <- mkReg(?);
    Reg#(Bool) working_one_cav <- mkReg(?);
    Reg#(Bool) working_two_cav <- mkReg(?);

    Reg#(CapPerms) working_perms <- mkReg(?);

    Reg#(Bit#(64)) working_base_addr <- mkReg(?);
    Reg#(Bit#(28)) working_elem_count <- mkReg(?);
    Reg#(Bit#(7)) working_elem_width_log2 <- mkReg(?);
    Reg#(Bit#(5)) working_log_max_count <- mkReg(?);

    Reg#(Bit#(4)) working_index_size_div <- mkReg(?);
    Reg#(Bit#(15)) working_index <- mkReg(?);
    Reg#(Bit#(9)) working_range_x <- mkReg(?);
    Reg#(Bit#(9)) working_range_y_minus_one <- mkReg(?);

    Reg#(Bit#(43)) working_scratch <- mkReg(?);

    // This is set by the state machine if a failure condition is reached during processing.
    // The rest of the processing continues but will be thrown away at the final stage.
    // I don't think there's a convenient way to bail out in the middle of a state machine.
    Reg#(Maybe#(CapFailReason)) working_fail <- mkReg(?);

    Reg#(Bool) fsmWorking <- mkReg(False);

    Stmt backendStmt = seq
        if (working_zero_cav) action
            let length = calc.shifted(); // f.elem_count << f.elem_width_log2
            calc.in.put(CalcInput {
                shl_to_shift: ?,
                shl_amt: ?,
                // Compute top = base + length
                add_op_65: AddOp { op: Add, a: {0, working_base_addr}, b: length },
                add_op_16: ?,
                add_op_9: ?
            });
        endaction else seq
            // We have handled the zero_cav case.
            // one_cav and two_cav are mutually exclusive and exactly one of them is true.
            action
                let check_rhs = calc.shifted()[42:0]; // f.elem_count << f.index_size_div, 43 bits
                working_scratch <= check_rhs; // This value will be used in a later cycle
                let index_plus_one = calc.added_16(); // f.index + 1
                Bit#(7) elem_width_log2 = truncate(calc.added_9()); // f.elem_width_log2 + f.log_max_count
                working_elem_width_log2 <= elem_width_log2; // This value will be reused later

                calc.in.put(CalcInput {
                    // Compute check_lhs = index_plus_one << log_max_count
                    shl_to_shift: zeroExtend(index_plus_one),
                    shl_amt: zeroExtend(working_log_max_count),
                    // No large add
                    add_op_65: ?,
                    // Start finding elem_count for the range caveat
                    add_op_16: sub(working_range_y_minus_one, working_range_x),
                    // Finish the elem_width_log2 computation we started earlier
                    add_op_9: sub(elem_width_log2, working_index_size_div)
                });
            endaction

            action
                let check_lhs = calc.shifted()[42:0]; // index_plus_one << log_max_count
                let elem_width_log2 = calc.added_9()[6:0]; // elem_width_log2 - index_size_div, will always be less than 7 bits
                working_elem_width_log2 <= elem_width_log2; // This value will be reused later

                // in the two-cav case elem_count = range_y_minus_one - range_x (+1, which we'll add in a second)
                // = calc.added_16() + 1
                // If range_y_minus_one <= range_x, that's invalid
                // the MSB will only be set of range_y_minus_one <= range_x, because both quantities are 9-bit
                if (msbSet(calc.added_16())) begin
                    working_fail <= tagged Valid InvalidCaveat;
                end

                // in the one-cav case set elem_count to one, we'll compute length on the next cycle
                if (working_one_cav)
                    working_elem_count <= 1;

                calc.in.put(CalcInput {
                    // Compute the one_cav start offset = index << elem_width_log2
                    shl_to_shift: zeroExtend(working_index),
                    shl_amt: elem_width_log2,
                    // For one and two cav, check that the first caveat is valid
                    // scratch currently holds check_rhs from the previous cycle
                    add_op_65: sub(working_scratch, check_lhs),
                    // For two_cav
                    // (range_y_minus_one - range_x) + 1
                    add_op_16: add(calc.added_16(), 16'd1),
                    // For two_cav
                    // test if elem_width_log2 is < 9, which changes how some things are handled
                    add_op_9: sub(elem_width_log2, 9'd9)
                });
            endaction

            action
                let cav1_start_offset = calc.shifted()[63:0]; // index << elem_width_log2
                let check_cmp = calc.added_65();
                // Check if check_rhs < check_lhs i.e. if the subtraction check_rhs - check_lhs overflows.
                // They are both 43-bit quantities, so the top bit will only be true if check_rhs < check_lhs.
                if (msbSet(check_cmp)) begin
                    working_fail <= tagged Valid InvalidCaveat;
                end

                if (working_one_cav) begin
                    calc.in.put(CalcInput {
                        shl_to_shift: zeroExtend(working_elem_count),
                        shl_amt: working_elem_width_log2, // initial elem_width_log2 + log_max_count - index_size_div

                        // Compute base + cav1_start_offset
                        add_op_65: add(working_base_addr, cav1_start_offset),
                        add_op_16: ?,
                        add_op_9: ?
                    });
                end else begin // i.e. two_cav
                    let cav2_elem_count = calc.added_16();
                    working_elem_count <= zeroExtend(cav2_elem_count); // (range_y_minus_one - range_x) + 1
                    let elem_width_log2_sub9 = calc.added_9(); // (cav1.elem_width_log2 - 9) TODO think more about if this can be 7 bits
                    working_scratch <= {?, msb(elem_width_log2_sub9)}; // This value will be used later

                    if (msbSet(elem_width_log2_sub9)) begin
                        // cav1.elem_width_log2 is smaller than 9, saturate cav2_elem_width_log2 to 0
                        working_elem_width_log2 <= 0;
                        // We now need to shift down elem_count and range_x to keep them inside the range.

                        calc.in.put(CalcInput {
                            // Compute (cav2_elem_count << cav1_elem_width_log2) and (range_x << cav1_elem_width_log2)
                            // together in one shift. Taking specific windows in the next cycle turns this into
                            // a dual *right*-shift of (9-cav1_elem_width_log2)
                            shl_to_shift: {0, cav2_elem_count[9:0], 8'd0, working_range_x[8:0]},
                            shl_amt: working_elem_width_log2,

                            // Compute base + cav1_start_offset
                            add_op_65: add(working_base_addr, cav1_start_offset),
                            add_op_16: ?,
                            add_op_9: ?
                        });
                    end else begin
                        // cav1.elem_width_log2 is greater than or equal to 9,
                        // cav2_elem_width_log2 is just cav1.elem_width_log2 - 9.
                        // No adjustments to range_x or elem_count necessary.
                        working_elem_width_log2 <= elem_width_log2_sub9[6:0]; // this is known to be 7-bits (actually 5-bits) when its positive

                        calc.in.put(CalcInput {
                            // Compute cav2_start_offset = range_x << cav2_elem_width_log2
                            shl_to_shift: zeroExtend(working_range_x),
                            shl_amt: elem_width_log2_sub9[6:0],

                            // Compute base + cav1_start_offset
                            add_op_65: add(working_base_addr, cav1_start_offset),
                            add_op_16: ?,
                            add_op_9: ?
                        });
                    end
                end
            endaction

            action
                let base = calc.added_65(); // cav0_base + cav1_start_offset
                if (msbSet(base)) begin
                    working_fail <= tagged Valid InvalidCaveat;
                end
                working_base_addr <= base[63:0];

                if (working_one_cav) begin
                    let length = calc.shifted(); // elem_count (i.e. 1) << cav1_elem_width_log2
                    calc.in.put(CalcInput {
                        shl_to_shift: ?,
                        shl_amt: ?,
                        // Compute top = base + length
                        add_op_65: add(base, length),
                        add_op_16: ?,
                        add_op_9: ?
                    });
                end else begin
                    // In the normal case, we just shifted range_x 
                    Bit#(65) start_offset = calc.shifted();
                    Bit#(28) elem_count = working_elem_count;
                    // working_scratch[0] is set if elem_width_log2_sub9 was negative in the last cycle.
                    if (unpack(working_scratch[0])) begin
                        // In that case we need to unpack the pair of elements we shifted in before
                        let shifted = calc.shifted();
                        start_offset = {57'b0, shifted[16:9]};
                        elem_count = {19'b0, shifted[34:26]};
                    end

                    calc.in.put(CalcInput {
                        shl_to_shift: {0, elem_count},
                        shl_amt: working_elem_width_log2,
                        // Compute cav1_base + cav2_start_offset
                        add_op_65: add(base, start_offset),
                        add_op_16: ?,
                        add_op_9: ?
                    });
                end
            endaction

            if (working_two_cav) seq
                action
                    let base = calc.added_65(); // cav1_base + cav2_start_offset
                    if (msbSet(base)) begin
                        working_fail <= tagged Valid InvalidCaveat;
                    end
                    working_base_addr <= base[63:0];

                    let length = calc.shifted(); // cav2 elem_count << cav2 elem_width_log2

                    calc.in.put(CalcInput {
                        shl_to_shift: ?,
                        shl_amt: ?,
                        // Compute top = base + length
                        add_op_65: add(base, length),
                        add_op_16: ?,
                        add_op_9: ?
                    });
                endaction
            endseq
        endseq

        action
            let top = calc.added_65(); // base + length
            case (working_fail) matches
                tagged Valid .failReason : out.put(tagged Fail failReason);
                tagged Invalid : out.put(tagged Succ tuple2(working_perms, CapRange { base: working_base_addr, top: top }));
            endcase
            // TODO this is inefficient - could be doing fetch on this cycle...
            fsmWorking <= False;
        endaction
    endseq;

    FSM backendFSM <- mkFSMWithPred(backendStmt, fsmWorking);

    rule fetch if (!fsmWorking);
        let i <- in.get();
        let f = frontend(i);

        // Setup the working state
        working_zero_cav <= f.zero_cav;
        working_one_cav <= f.one_cav;
        working_two_cav <= f.two_cav;

        working_perms <= f.perms;

        working_base_addr <= f.base_addr;
        working_elem_count <= f.elem_count;
        working_elem_width_log2 <= f.elem_width_log2;
        working_log_max_count <= f.log_max_count;

        working_index_size_div <= f.index_size_div;
        working_index <= f.index;
        working_range_x <= f.range_x;
        working_range_y_minus_one <= f.range_y_minus_one;

        working_fail <= f.fail;

        // Start the first cycle of calculations
        Bit#(7) shl_amt = {3'b0, f.index_size_div};
        if (f.zero_cav)
            shl_amt = f.elem_width_log2;

        calc.in.put(CalcInput {
            // computing (length = elem_count << elem_width_log2) for zerocav, (check_rhs = elem_count << index_size_div) otherwise
            shl_to_shift: zeroExtend(f.elem_count),
            shl_amt: shl_amt,
            // No large add
            add_op_65: ?,
            // For onecav and twocav, index + 1
            add_op_16: add(f.index, 16'd1),
            // For onecav and twocav, elem_width_log2 = (elem_width_log2 + log_max_count) (and then we subtract size_div)
            add_op_9: add(f.elem_width_log2, f.log_max_count)
        });
        
        case (f.fail) matches
            tagged Invalid: begin
                backendFSM.start();
                fsmWorking <= True;
            end
            tagged Valid .fail: out.put(tagged Fail fail);
        endcase
    endrule
endmodule

endpackage
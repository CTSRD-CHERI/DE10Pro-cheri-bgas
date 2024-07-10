package CheckedBits;

import LeftShift::*;

typeclass Checkable#(type num, numeric type sz) provisos (Literal#(num#(sz)), SizedLiteral#(num#(sz), sz));
    function num#(sz) withMaxBound(UInt#(sz) val);
    // Optionally check if val is within [min, max], and if not then $display an error and $finish
    function ActionValue#(num#(sz)) withBound(UInt#(sz) val, UInt#(sz) min, UInt#(sz) max, String msg);
    function UInt#(sz) currVal(num#(sz) val);

    // add two numbers together, checking if they overflowed and returning Invalid if they did
    function Maybe#(num#(sz)) add_overflow(num#(sz) a, num#(sz) b);
    // subtract two numbers, checking if they overflowed and returning Invalid if they did
    function Maybe#(num#(sz)) sub_overflow(num#(sz) a, num#(sz) b);

    function num#(sz) shl_unchecked(num#(sz) a, num#(szShift) shift);

    // add two numbers together. Optionally check if overflow is ever possible,
    // at which point $display a message and $finish immediately.
    function ActionValue#(num#(sz)) add(num#(sz) a, num#(sz) b, String failMsg);
    // subtract two numbers. Optionally check if overflow is ever possible,
    // at which point $display a message and $finish immediately.
    function ActionValue#(num#(sz)) sub(num#(sz) a, num#(sz) b, String failMsg);
endtypeclass

// Split shift-left out into a separate typeclass because provisos-directly-on-functions-returning-ActionValue seem to be buggy
typeclass CheckableShift#(type num, numeric type sz, numeric type szShift);
    function ActionValue#(num#(sz)) shl(num#(sz) a, num#(szShift) shift, String failMsg);
endtypeclass

// truncate() and zeroExtend() implemented where the low bits are truncated/extended with zeros.
typeclass BitRightOps#(numeric type m, numeric type n, type x);
    function x#(m) rightTruncate(x#(n) d);
    function x#(n) rightZeroExtend(x#(m) d);
endtypeclass

/*
Checkable, CheckableShift, and BitRightOps for UInt, used when synthesizing

ActionValue functions do not do any unnecessary checks
*/
instance Checkable#(UInt, sz);
    function UInt#(sz) withMaxBound(UInt#(sz) val);
        return val;
    endfunction
    function ActionValue#(UInt#(sz)) withBound(UInt#(sz) val, UInt#(sz) min, UInt#(sz) max, String msg) = actionvalue
        return val;
    endactionvalue;
    function UInt#(sz) currVal(UInt#(sz) val);
        return val;
    endfunction

    function Maybe#(UInt#(sz)) add_overflow(UInt#(sz) a, UInt#(sz) b);
        UInt#(TAdd#(sz, 1)) aWide = zeroExtend(a);
        UInt#(TAdd#(sz, 1)) bWide = zeroExtend(b);
        let resWide = aWide + bWide;
        if (msb(resWide) == 1'b1) begin
            return tagged Invalid;
        end else begin
            return tagged Valid truncate(resWide);
        end
    endfunction

    function Maybe#(UInt#(sz)) sub_overflow(UInt#(sz) a, UInt#(sz) b);
        UInt#(TAdd#(sz, 1)) aWide = zeroExtend(a);
        UInt#(TAdd#(sz, 1)) bWide = zeroExtend(b);
        let resWide = aWide - bWide;
        if (msb(resWide) == 1'b1) begin
            return tagged Invalid;
        end else begin
            return tagged Valid truncate(resWide);
        end
    endfunction

    function UInt#(sz) shl_unchecked(UInt#(sz) a, UInt#(szShift) shift);
        return left_shift_comb(a, shift);
    endfunction

    function ActionValue#(UInt#(sz)) add(UInt#(sz) a, UInt#(sz) b, String msg) = actionvalue
        return a + b;
    endactionvalue;
    function ActionValue#(UInt#(sz)) sub(UInt#(sz) a, UInt#(sz) b, String msg) = actionvalue
        return a - b;
    endactionvalue;
endinstance

instance CheckableShift#(UInt, sz, szShift);
    function ActionValue#(UInt#(sz)) shl(UInt#(sz) a, UInt#(szShift) shift, String msg) = actionvalue
        return left_shift_comb(a, shift);
    endactionvalue;
endinstance

instance BitRightOps#(m, n, UInt) provisos (Add#(k, m, n));
    function UInt#(m) rightTruncate(UInt#(n) d);
        Bit#(n) bd = pack(d);
        return unpack(bd[valueOf(n)-1:valueOf(n)-valueOf(m)]);
    endfunction
    function UInt#(n) rightZeroExtend(UInt#(m) d);
        return unpack({pack(d), 0});
    endfunction
endinstance

/*
Checkable, CheckableShift, BitExtend, and BitRightOps for (current value, min inclusive, max inclusive) triple, used in simulation.

Implements Literal and SizedLiteral to create values where min = max = val = literal value.

ActionValue functions do perform the necessary checks for overflow and call $display $finish when overflow is possible.
*/

typedef struct {
    UInt#(sz) val;
    UInt#(sz) min;
    UInt#(sz) max;
} CheckedBits#(numeric type sz) deriving (Bits, FShow);

instance Checkable#(CheckedBits, sz);
    function CheckedBits#(sz) withMaxBound(UInt#(sz) val);
        return CheckedBits {
            val: val,
            min: minBound,
            max: maxBound
        };
    endfunction
    function ActionValue#(CheckedBits#(sz)) withBound(UInt#(sz) val, UInt#(sz) min, UInt#(sz) max, String msg) = actionvalue
        if (min > max) begin
            $display("Error creating CheckedBits min 0x%x val 0x%x max 0x%x - min > max: ", min, val, max, msg);
            $finish;
        end
        if (val < min) begin
            $display("Error creating CheckedBits min 0x%x val 0x%x max 0x%x - val < min: ", min, val, max, msg);
            $finish;
        end
        if (val > max) begin
            $display("Error creating CheckedBits min 0x%x val 0x%x max 0x%x - val > max: ", min, val, max, msg);
            $finish;
        end
        return CheckedBits {
            val: val,
            min: min,
            max: max
        };
    endactionvalue;
    function UInt#(sz) currVal(CheckedBits#(sz) val);
        return val.val;
    endfunction

    // add two numbers together, checking if they overflowed and returning Invalid.
    // If the bounds overflowed Invalid, but the current value didn't, return bounds as if the value cannot overflow
    function Maybe#(CheckedBits#(sz)) add_overflow(CheckedBits#(sz) a, CheckedBits#(sz) b);
        // Try val + val
        case (add_overflow(a.val, b.val)) matches
            // If that overflowed, return Invalid
            tagged Invalid : return tagged Invalid;
            // If it didn't overflow, return Valid from a universe where overflow is impossible
            // We already know a.min + b.min will never overflow, because they're both smaller than a.val, b.val
            // If a.max+b.max overflows, pretend it stops at the maximum bound of UInt#(sz) 
            tagged Valid .val : begin
                return tagged Valid CheckedBits {
                    val: val,
                    min: a.min + b.min,
                    max: fromMaybe(maxBound, add_overflow(a.max, b.max))
                };
            end
        endcase
    endfunction
    // subtract two numbers, checking if they overflowed and returning Invalid.
    // If the bounds overflowed Invalid, but the current value didn't, return bounds as if the value cannot overflow
    function Maybe#(CheckedBits#(sz)) sub_overflow(CheckedBits#(sz) a, CheckedBits#(sz) b);
        // Try val - val
        case (sub_overflow(a.val, b.val)) matches
            // If that overflowed, return Invalid
            // a.val < b.val
            tagged Invalid : return tagged Invalid;
            // If it didn't overflow, return Valid from a universe where overflow is impossible
            // We already know a.max - b.min will never overflow, because a.val >= b.val, a.max >= a.val, b.val >= b.min => a.max >= b.min
            // If a.min-b.max overflows, pretend it stops at the minimum bound of UInt#(sz) = 0
            tagged Valid .val : begin
                return tagged Valid CheckedBits {
                    val: val,
                    min: fromMaybe(minBound, sub_overflow(a.min, b.max)),
                    max: a.max - b.min
                };
            end
        endcase
    endfunction

    function CheckedBits#(sz) shl_unchecked(CheckedBits#(sz) a, CheckedBits#(szShift) b);
        return withMaxBound(left_shift_comb(a.val, b.val));
    endfunction

    function ActionValue#(CheckedBits#(sz)) add(CheckedBits#(sz) a, CheckedBits#(sz) b, String msg) = actionvalue
        case (add_overflow(a.max, b.max)) matches
            tagged Invalid: begin
                $display("Adding ", fshow(a), " + ", fshow(b), " overflows ", valueOf(sz), " bits. ", msg);
                $finish;
                return ?;
            end
            tagged Valid .max: begin
                // If max doesn't overflow, we know min and val can't overflow either
                return CheckedBits {
                    val: a.val + b.val,
                    min: a.min + b.min,
                    max: max
                };
            end
        endcase
    endactionvalue;

    function ActionValue#(CheckedBits#(sz)) sub(CheckedBits#(sz) a, CheckedBits#(sz) b, String msg) = actionvalue
        case (sub_overflow(a.min, b.max)) matches
            tagged Invalid: begin
                $display("Subtracting ", fshow(a), " - ", fshow(b), " overflows ", valueOf(sz), " bits. ", msg);
                $finish;
                return ?;
            end
            tagged Valid .min: begin
                // If min-max doesn't overflow, we know max-min and val-val can't overflow either
                return CheckedBits {
                    val: a.val - b.val,
                    min: min,
                    max: a.max - b.min
                };
            end
        endcase
    endactionvalue;

    // Bluespec error
    /*
    Error: "CheckedBits.bsv", line 215, column 45: (T0030)
  The provisos for this expression are too general.
  Given type:
    function ActionValue#(CheckedBits::CheckedBits#(sz))
    f(CheckedBits::CheckedBits#(sz) x1, CheckedBits::CheckedBits#(b__) x2,
    String x3)
  The following provisos are needed:
    Add#(a__, b__, sz)
      Introduced at the following locations:
        "CheckedBits.bsv", line 215, column 45
  The type variables are from the following positions:
    "a__" at "CheckedBits.bsv", line 215, column 45
    "b__" at "CheckedBits.bsv", line 215, column 45
    */
    // As far as I can tell this is a nonsense error.
    // The proviso Add#(a__, szShift, sz) proves everything we need but it doesn't use the same name.
    // => use a different approach
    /*
    function ActionValue#(CheckedBits#(sz)) shl(CheckedBits#(sz) a, CheckedBits#(szShift) shift, String msg) provisos (Add#(a__, szShift, sz));
        return actionvalue       
            UInt#(sz) shiftMax = extend(shift.max);
            UInt#(sz) zeros = extend(countZerosMSB(pack(a.max)));
            if (shiftMax > zeros) begin
                $display("left shift of (a value with %d leading zeros) by (up to %d) into (a container of %d bits) does not fit ", zeros, shift.max, valueOf(sz), msg);
                $finish;
            end

            return CheckedBits {
                val: left_shift_comb(a.val, shift.val),
                min: left_shift_comb(a.min, shift.min),
                max: left_shift_comb(a.max, shift.max)
            };
        endactionvalue;
    endfunction
    */
endinstance

instance CheckableShift#(CheckedBits, sz, szShift) provisos (Add#(_, szShift, sz), Add#(a__, TLog#(TAdd#(1, sz)), sz));
    function ActionValue#(CheckedBits#(sz)) shl(CheckedBits#(sz) a, CheckedBits#(szShift) shift, String msg);
        return actionvalue       
            UInt#(sz) shiftMax = extend(shift.max);
            UInt#(sz) zeros = extend(countZerosMSB(pack(a.max)));
            if (shiftMax > zeros) begin
                $display("left shift of (a value with %d leading zeros) by (up to %d) into (a container of %d bits) does not fit ", zeros, shift.max, valueOf(sz), msg);
                $finish;
            end

            return CheckedBits {
                val: left_shift_comb(a.val, shift.val),
                min: left_shift_comb(a.min, shift.min),
                max: left_shift_comb(a.max, shift.max)
            };
        endactionvalue;
    endfunction
endinstance

instance Literal#(CheckedBits#(sz));
    function CheckedBits#(sz) fromInteger(Integer x);
        return CheckedBits {
            // value, min, max are all the same
            val: fromInteger(x),
            min: fromInteger(x),
            max: fromInteger(x)
        };
    endfunction
    function Bool inLiteralRange(CheckedBits#(sz) target, Integer x);
        UInt#(sz) targetUint = ?;
        return inLiteralRange(targetUint, x);
    endfunction
endinstance

instance SizedLiteral#(CheckedBits#(sz), sz);
    function CheckedBits#(sz) fromSizedInteger(Bit#(sz) x);
        return CheckedBits {
            // value, min, max are all the same
            val: unpack(x),
            min: unpack(x),
            max: unpack(x)
        };
    endfunction
endinstance

instance BitExtend#(m, n, CheckedBits) provisos (Add#(_, m, n));
    // These numbers are unsigned => zero extend by default
    function CheckedBits#(n) extend(CheckedBits#(m) d);
        return zeroExtend(d);
    endfunction
    // Zero extend is just zero extending all the values together
    function CheckedBits#(n) zeroExtend(CheckedBits#(m) d);
        return CheckedBits {
            val: zeroExtend(d.val),
            min: zeroExtend(d.min),
            max: zeroExtend(d.max)
        };
    endfunction
    function CheckedBits#(n) signExtend(CheckedBits#(m) d);
        // Sign-extension on val, min, max independently is weird, but fine.
        // We know they're all the same width => if msb(min) then msb(val) and msb(max) must also be set
        // if not msb(min) but msb(val) then msb(max) must also be set
        // if not msb(min) and not msb(val) then msb(max) may also be set
        // signExtending all of them at once will never violate the min <= val <= max constraint
        return CheckedBits {
            val: signExtend(d.val),
            min: signExtend(d.min),
            max: signExtend(d.max)
        };
    endfunction
    // This truncates from the top, and we can't make guarantees about the min and max value anymore
    // unless the min and max all fit into m bits.
    function CheckedBits#(m) truncate(CheckedBits#(n) d);
        CheckedBits#(m) truncated = CheckedBits {
            val: truncate(d.val),
            min: truncate(d.min),
            max: truncate(d.max)
        };
        if (extend(truncated.min) == d.min && extend(truncated.max) == d.max) begin
            return truncated;
        end else begin
            return withMaxBound(truncate(d.val));
        end
    endfunction
endinstance

instance BitRightOps#(m, n, CheckedBits) provisos (Add#(k, m, n));
    // Truncating by removing the right bits = shifting down, cannot make val > max nor can it make min > val
    function CheckedBits#(m) rightTruncate(CheckedBits#(n) d);
        return CheckedBits {
            val: rightTruncate(d.val),
            min: rightTruncate(d.min),
            max: rightTruncate(d.max)
        };
    endfunction
    // Push in zeroes in the right, has no impact on the relative values of val, min, max
    function CheckedBits#(n) rightZeroExtend(CheckedBits#(m) d);
        return CheckedBits {
            val: rightZeroExtend(d.val),
            min: rightZeroExtend(d.min),
            max: rightZeroExtend(d.max)
        };
    endfunction
endinstance

/*
ChkUInt#(n) is a type guaranteed to implement
- Checkable
- CheckableShift
- BitRightOps
- BitExtend
- Literal
- SizedLiteral
*/

`ifdef SYNTHESIZE
typedef UInt#(n) ChkUInt#(numeric type n);
`else
typedef CheckedBits#(n) ChkUInt#(numeric type n);
`endif

endpackage
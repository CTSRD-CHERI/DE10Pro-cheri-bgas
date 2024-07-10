/// This package provides a struct RijndaelGf8 implementing addition and multiplication in the Galois Field 2^8
/// using the Rijndael reducing polynomial
/// as described in the NIST AES specifiation https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf,
/// used throughout for documentation.
///
/// The functions gf_add and gf_mul are combinational implementations of addition and multiplication.
///
/// From FIPS-197 Section 4:
/// In order to defne addition and multiplication in GF(2^8),
/// each byte {b7 b6 b5 b4 b3 b2 b1 b0} is interpreted as a polynomial,
/// denoted by b(x), as follows:
/// b(x) = b7*x^7 + b6*x^6 + b5*x^5 + b4*x^4 + b3*x^3 + b2*x^2 + b1*x + b0. (4.1)
/// For example, {01100011} is represented by the polynomial x6 + x5 + x + 1.
///
/// Addition is defined by adding the coeffienents modulo-2 (i.e. with XOR).
/// This means addition is the inverse of itself.
/// Examples from FIPS-197 Section 4.1:
/// (x^6 + x^4 + x^2 + x + 1) + (x^7 + x + 1) = x^7 + x^6 + x^4 + x^2 (polynomial)
/// {01010111} ^ {10000011} = {11010100} (binary)
/// {57} ^ {83} = {d4} (hexadecimal)
/// 
/// Multiplication is defined modulo a "reducing polynomial".
/// For Rijndael this polynomial is x^8 + x^4 + x^3 + x + 1.
/// Otherwise it is equivalent to normal polynomial multiplication.

package GaloisField;
export RijndaelGf8;
export gf_add;
export gf_mul;

typedef struct {
    Bit#(8) bits;
} RijndaelGf8 deriving (Bits, Eq);

instance FShow#(RijndaelGf8);
    function Fmt fshow(RijndaelGf8 value) = fshow(value.bits);
endinstance

instance Arith#(RijndaelGf8);
    function RijndaelGf8 \+ (RijndaelGf8 x, RijndaelGf8 y);
        return gf_add(x, y);
    endfunction
    function RijndaelGf8 \- (RijndaelGf8 x, RijndaelGf8 y);
        return gf_add(x, y);
    endfunction
    function RijndaelGf8 negate(RijndaelGf8 x) = x;
    function RijndaelGf8 \* (RijndaelGf8 x, RijndaelGf8 y);
        return gf_mul(x, y);
    endfunction

    /*
    function RijndaelGf8 \/ = ?;
    function RijndaelGf8 \% = ?;
    function RijndaelGf8 abs = ?;
    function RijndaelGf8 signum = ?;
    function RijndaelGf8 \** = ?;
    function RijndaelGf8 exp_e = ?;
    function RijndaelGf8 log = ?;
    function RijndaelGf8 logb = ?;
    function RijndaelGf8 log2 = ?;
    function RijndaelGf8 log10 = ?;
    */
endinstance

// Create RijndaelGf8 from unsized literals e.g. 'h25
instance Literal#(RijndaelGf8);
    function RijndaelGf8 fromInteger(Integer x);
        return RijndaelGf8 {
            bits: fromInteger(x)
        };
    endfunction
    function Bool inLiteralRange(RijndaelGf8 f, Integer x);
        return 0 <= x && x <= 255;
    endfunction
endinstance

// Create RijndaelGf8 from sized literals e.g. 8'h25
instance SizedLiteral#(RijndaelGf8, 8);
    function RijndaelGf8 fromSizedInteger(Bit#(8) bits);
        return RijndaelGf8 {
            bits: bits
        };
    endfunction
endinstance

// Addition is equivalent to XOR
function RijndaelGf8 gf_add(RijndaelGf8 a, RijndaelGf8 b);
    return RijndaelGf8 {
        bits: a.bits ^ b.bits
    };
endfunction

function RijndaelGf8 gf_mul(RijndaelGf8 a, RijndaelGf8 b);
    // Using a simple algorithm: if b[0] is set `accum += a`, if b[1] (x) is set `accum += a * x`,
    // if b[2] i.e. x^2 is set `accum += a * x^2` etc.
    RijndaelGf8 accum = 0;
    RijndaelGf8 a_times_x_to_i = a; // i = 0, a * x^i = a * 1 = a
    for (Integer i = 0; i < 8; i = i + 1) begin
        if (b.bits[i] == 1)
            accum = gf_add(accum, a_times_x_to_i);
        // as i += 1, `a * x^i * x` = `a * x^(i+1)`
        // multiplication is done in mod_m space so the state stays in 8 bits.
        a_times_x_to_i = gf_times_x_mod_m(a_times_x_to_i);
    end
    return accum;
endfunction

// Multiply a Rijndael polynomial by X modulo the reducing polynomial m.
// Defined as in FIPS-197 equation 4.5.
function RijndaelGf8 gf_times_x_mod_m(RijndaelGf8 gf);
    if (gf.bits[7] == 0) begin
        return unpack({gf.bits[6:0], 0});
    end else begin
        return unpack({gf.bits[6:0], 0} ^ 8'b00011011);
    end
endfunction

endpackage
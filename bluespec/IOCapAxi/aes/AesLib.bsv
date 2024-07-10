/// This package defines a set of vaguely optimized "building blocks" for AES-128.
/// This will be used by the Cap2024_02_Aes package to create a vaguely optimized AES encryptor/decryptor.
///
/// This package exposes the following building-block functions:
/// - add_round_key i.e. XOR
/// - shift_rows
/// - inv_shift_rows
/// - sbox and inv_sbox for the default AES Sbox
/// - mix_columns
/// - inv_mix_column
/// - next_key - a step forward in key_expansion
/// - prev_key - a step backward in key_expansion

package AesLib;

import GaloisField::*;
// Unfortunately to define arrays with type-safe length we have to use the Vector package,
// because Bluespec is a bad language.
import Vector::*;

// AES word size = 32 bits.
// If this were interpreted directly as 32-bits it would be little-endian.
typedef Vector#(4, RijndaelGf8) AesWord;
function AesWord gf_add_word(AesWord a, AesWord b);
    // Add a[0]+b[0], a[1]+b[1]... etc
    return zipWith(gf_add, a, b);
endfunction

// AES block size is always 128 bits = 16 bytes.
// This is column-major i.e. each AesWord is a column, as defined in FIPS-197 Section 3.4.
// Column-major encoding implies the layout of the state is always identical to input and output arrays.
// FIPS-197 defines input and output arrays to map to AesState as
// state[row, column] = in[row + column*4]
// A column-major encoding of State places state[row=0, column=0] as byte 0, state[row=1, column=0] as byte 1,
// and state[row, column] as byte row+column * 4.
// Therefore pack() and unpack() work natively on AesState.
typedef Vector#(4, AesWord) AesState;
AesState initialState = unpack(0);


// addition in the galois field is XOR, which is its own inverse
function AesState addRoundKey(AesState state, AesState key);
    return zipWith(gf_add_word, state, key);
endfunction

/// "The transformation of the state in which the last three rows are cyclically shifted by different offsets"
///
/// Section 5.1.2
function AesState shiftRows(AesState s);
    // Transposed from diagram in figure 3, because we fill out the elements column major,
    // and the indices are the other way around - s is indexed [c][r] instead of r,c

    // If this language weren't dogshit the code would look something like this.
    // return unpack({
    //     {s[0][0], s[1][1], s[2][2], s[3][3]},
    //     {s[1][0], s[2][1], s[3][2], s[0][3]},
    //     {s[2][0], s[3][1], s[0][2], s[1][3]},
    //     {s[3][0], s[0][1], s[1][2], s[2][3]}
    // });

    // https://github.com/bluespec/Accel_AES uses code that calls genWith(rotate(something)) but that only works
    // in a row-major context.

    AesState res = initialState;
    
    for (Integer r = 0; r < 4; r = r + 1) begin
        Integer shiftAmount = r;

        for (Integer c = 0; c < 4; c = c + 1) begin
            res[c][r] = s[(c + shiftAmount) % 4][r];
        end
    end

    return res;
endfunction

/// "The inverse of shift_rows"
///
/// Section 5.3.1
function AesState invShiftRows(AesState s);
    // Transposed from diagram in figure 9, because we fill out the elements column major,
    // and the indices are the other way around - s is indexed [c][r] instead of r,c

    // Transposed with row-major indexing:
    // s00 s13 s22 s31
    // s01 s10 s23 s32
    // s02 s11 s20 s33
    // s03 s12 s21 s30

    // Transposed with col-major indexing:
    // s00 s31 s22 s13
    // s10 s01 s32 s23
    // s20 s11 s02 s33
    // s30 s21 s12 s03

    // If this language weren't dogshit the code would look something like this.
    // return {
    //     {s[0][0], s[3][1], s[2][2], s[1][3]},
    //     {s[1][0], s[0][1], s[3][2], s[2][3]},
    //     {s[2][0], s[1][1], s[0][2], s[3][3]},
    //     {s[3][0], s[2][1], s[1][2], s[0][3]}
    // };

    AesState res = initialState;
    
    for (Integer r = 0; r < 4; r = r + 1) begin
        Integer shiftAmount = 4 - r;

        for (Integer c = 0; c < 4; c = c + 1) begin
            res[c][r] = s[(c + shiftAmount) % 4][r];
        end
    end

    return res;
endfunction


/// "The transformation of words in which the four bytes of the word are permuted cyclically"
///
/// i.e. {0, 1, 2, 3} -> {1, 2, 3, 0}
function AesWord rotWord(AesWord w) = rotate(w);

RijndaelGf8 fwdSbox[256] = {
    8'h63,8'h7c,8'h77,8'h7b,8'hf2,8'h6b,8'h6f,8'hc5,8'h30,8'h01,8'h67,8'h2b,8'hfe,8'hd7,8'hab,8'h76,
    8'hca,8'h82,8'hc9,8'h7d,8'hfa,8'h59,8'h47,8'hf0,8'had,8'hd4,8'ha2,8'haf,8'h9c,8'ha4,8'h72,8'hc0,
    8'hb7,8'hfd,8'h93,8'h26,8'h36,8'h3f,8'hf7,8'hcc,8'h34,8'ha5,8'he5,8'hf1,8'h71,8'hd8,8'h31,8'h15,
    8'h04,8'hc7,8'h23,8'hc3,8'h18,8'h96,8'h05,8'h9a,8'h07,8'h12,8'h80,8'he2,8'heb,8'h27,8'hb2,8'h75,
    8'h09,8'h83,8'h2c,8'h1a,8'h1b,8'h6e,8'h5a,8'ha0,8'h52,8'h3b,8'hd6,8'hb3,8'h29,8'he3,8'h2f,8'h84,
    8'h53,8'hd1,8'h00,8'hed,8'h20,8'hfc,8'hb1,8'h5b,8'h6a,8'hcb,8'hbe,8'h39,8'h4a,8'h4c,8'h58,8'hcf,
    8'hd0,8'hef,8'haa,8'hfb,8'h43,8'h4d,8'h33,8'h85,8'h45,8'hf9,8'h02,8'h7f,8'h50,8'h3c,8'h9f,8'ha8,
    8'h51,8'ha3,8'h40,8'h8f,8'h92,8'h9d,8'h38,8'hf5,8'hbc,8'hb6,8'hda,8'h21,8'h10,8'hff,8'hf3,8'hd2,
    8'hcd,8'h0c,8'h13,8'hec,8'h5f,8'h97,8'h44,8'h17,8'hc4,8'ha7,8'h7e,8'h3d,8'h64,8'h5d,8'h19,8'h73,
    8'h60,8'h81,8'h4f,8'hdc,8'h22,8'h2a,8'h90,8'h88,8'h46,8'hee,8'hb8,8'h14,8'hde,8'h5e,8'h0b,8'hdb,
    8'he0,8'h32,8'h3a,8'h0a,8'h49,8'h06,8'h24,8'h5c,8'hc2,8'hd3,8'hac,8'h62,8'h91,8'h95,8'he4,8'h79,
    8'he7,8'hc8,8'h37,8'h6d,8'h8d,8'hd5,8'h4e,8'ha9,8'h6c,8'h56,8'hf4,8'hea,8'h65,8'h7a,8'hae,8'h08,
    8'hba,8'h78,8'h25,8'h2e,8'h1c,8'ha6,8'hb4,8'hc6,8'he8,8'hdd,8'h74,8'h1f,8'h4b,8'hbd,8'h8b,8'h8a,
    8'h70,8'h3e,8'hb5,8'h66,8'h48,8'h03,8'hf6,8'h0e,8'h61,8'h35,8'h57,8'hb9,8'h86,8'hc1,8'h1d,8'h9e,
    8'he1,8'hf8,8'h98,8'h11,8'h69,8'hd9,8'h8e,8'h94,8'h9b,8'h1e,8'h87,8'he9,8'hce,8'h55,8'h28,8'hdf,
    8'h8c,8'ha1,8'h89,8'h0d,8'hbf,8'he6,8'h42,8'h68,8'h41,8'h99,8'h2d,8'h0f,8'hb0,8'h54,8'hbb,8'h16
};

function RijndaelGf8 subByte(RijndaelGf8 x) = fwdSbox[pack(x)];

function AesWord subWord(AesWord w) = map(subByte, w);

function AesState subBytes(AesState s) = map(subWord, s);

RijndaelGf8 invSbox[256] = {
    8'h52,8'h09,8'h6a,8'hd5,8'h30,8'h36,8'ha5,8'h38,8'hbf,8'h40,8'ha3,8'h9e,8'h81,8'hf3,8'hd7,8'hfb,
    8'h7c,8'he3,8'h39,8'h82,8'h9b,8'h2f,8'hff,8'h87,8'h34,8'h8e,8'h43,8'h44,8'hc4,8'hde,8'he9,8'hcb,
    8'h54,8'h7b,8'h94,8'h32,8'ha6,8'hc2,8'h23,8'h3d,8'hee,8'h4c,8'h95,8'h0b,8'h42,8'hfa,8'hc3,8'h4e,
    8'h08,8'h2e,8'ha1,8'h66,8'h28,8'hd9,8'h24,8'hb2,8'h76,8'h5b,8'ha2,8'h49,8'h6d,8'h8b,8'hd1,8'h25,
    8'h72,8'hf8,8'hf6,8'h64,8'h86,8'h68,8'h98,8'h16,8'hd4,8'ha4,8'h5c,8'hcc,8'h5d,8'h65,8'hb6,8'h92,
    8'h6c,8'h70,8'h48,8'h50,8'hfd,8'hed,8'hb9,8'hda,8'h5e,8'h15,8'h46,8'h57,8'ha7,8'h8d,8'h9d,8'h84,
    8'h90,8'hd8,8'hab,8'h00,8'h8c,8'hbc,8'hd3,8'h0a,8'hf7,8'he4,8'h58,8'h05,8'hb8,8'hb3,8'h45,8'h06,
    8'hd0,8'h2c,8'h1e,8'h8f,8'hca,8'h3f,8'h0f,8'h02,8'hc1,8'haf,8'hbd,8'h03,8'h01,8'h13,8'h8a,8'h6b,
    8'h3a,8'h91,8'h11,8'h41,8'h4f,8'h67,8'hdc,8'hea,8'h97,8'hf2,8'hcf,8'hce,8'hf0,8'hb4,8'he6,8'h73,
    8'h96,8'hac,8'h74,8'h22,8'he7,8'had,8'h35,8'h85,8'he2,8'hf9,8'h37,8'he8,8'h1c,8'h75,8'hdf,8'h6e,
    8'h47,8'hf1,8'h1a,8'h71,8'h1d,8'h29,8'hc5,8'h89,8'h6f,8'hb7,8'h62,8'h0e,8'haa,8'h18,8'hbe,8'h1b,
    8'hfc,8'h56,8'h3e,8'h4b,8'hc6,8'hd2,8'h79,8'h20,8'h9a,8'hdb,8'hc0,8'hfe,8'h78,8'hcd,8'h5a,8'hf4,
    8'h1f,8'hdd,8'ha8,8'h33,8'h88,8'h07,8'hc7,8'h31,8'hb1,8'h12,8'h10,8'h59,8'h27,8'h80,8'hec,8'h5f,
    8'h60,8'h51,8'h7f,8'ha9,8'h19,8'hb5,8'h4a,8'h0d,8'h2d,8'he5,8'h7a,8'h9f,8'h93,8'hc9,8'h9c,8'hef,
    8'ha0,8'he0,8'h3b,8'h4d,8'hae,8'h2a,8'hf5,8'hb0,8'hc8,8'heb,8'hbb,8'h3c,8'h83,8'h53,8'h99,8'h61,
    8'h17,8'h2b,8'h04,8'h7e,8'hba,8'h77,8'hd6,8'h26,8'he1,8'h69,8'h14,8'h63,8'h55,8'h21,8'h0c,8'h7d
};

function RijndaelGf8 invSubByte(RijndaelGf8 x) = invSbox[pack(x)];

function AesWord invSubWord(AesWord w) = map(invSubByte, w);

function AesState invSubBytes(AesState s) = map(invSubWord, s);

/// mixColumn and invMixColumn are both implemented as matrix multiplications on individual columns
// where the matrix is defined based on one row.
//
// FIPS-197 notes this in equation 5.6 and 5.13 but specifies the elements in the "generator" word
// in a different order than they appear in the matrix. This function takes the elements as they appear *in the matrix*.
function AesWord colMixer(AesWord matrixGen, AesWord col);
    AesWord res = newVector();
    res[0] = fold(gf_add, zipWith(gf_mul, col, matrixGen));
    res[1] = fold(gf_add, zipWith(gf_mul, col, rotateBy(matrixGen, 1)));
    res[2] = fold(gf_add, zipWith(gf_mul, col, rotateBy(matrixGen, 2)));
    res[3] = fold(gf_add, zipWith(gf_mul, col, rotateBy(matrixGen, 3)));
    return res;
endfunction

// mixColumns mixes the data of each column *independently*
// by multiplying that column by a matrix.
//
// See FIPS-197 equation 5.7. 5.6 specifies the elements in a different order.
RijndaelGf8 mixColumnGen[4] = {8'h02, 8'h03, 8'h01, 8'h01};
function AesWord mixColumn(AesWord col) = colMixer(arrayToVector(mixColumnGen), col);

/// "The transformation of the state that takes all of the columns of the state
/// and mixes their data (independently of one another) to produce new columns"
///
/// See section 4.3, 5.1.3
function AesState mixColumns(AesState state) = map(mixColumn, state);

// invMixColumns mixes the data of each column *independently*
// by multiplying that column by a matrix.
//
// See FIPS-197 equation 5.14. 5.13 specifies the elements in a different order.
RijndaelGf8 invMixColumnGen[4] = {8'h0e, 8'h0b, 8'h0d, 8'h09};
function AesWord invMixColumn(AesWord col) = colMixer(arrayToVector(invMixColumnGen), col);

/// "The inverse of mix_columns"
///
/// See section 5.3.3
function AesState invMixColumns(AesState state) = map(invMixColumn, state);

// Note that this is zero indexed unlike FIPS-197 which was one indexed but always indexed with n>=1
function AesWord getRoundConstant(Integer i);
   RijndaelGf8 roundConstantFirstBytes[10] = {
        8'h01,
        8'h02,
        8'h04,
        8'h08,
        8'h10,
        8'h20,
        8'h40,
        8'h80,
        8'h1b,
        8'h36
    };

    AesWord res = replicate(0);
    res[0] = roundConstantFirstBytes[i];
    return res;
endfunction

function AesWord getDynRoundConstant(Bit#(4) i);
    RijndaelGf8 roundConstantFirstBytes[16] = {
        8'h01, // [0]
        8'h02,
        8'h04,
        8'h08,
        8'h10,
        8'h20,
        8'h40,
        8'h80,
        8'h1b,
        8'h36, // [9]
        ?, // [10]
        ?, // [11]
        ?, // [12]
        ?, // [13]
        ?, // [14]
        ? // [15]
    };

    AesWord res = replicate(0);
    res[0] = roundConstantFirstBytes[i];
    return res;
endfunction

function AesState getNextRoundKey(AesState prevRoundKey, AesWord roundConstant);
    AesState nextRoundKey = newVector();

    nextRoundKey[0] = gf_add_word(prevRoundKey[0], gf_add_word(subWord(rotWord(prevRoundKey[3])), roundConstant));
    nextRoundKey[1] = gf_add_word(prevRoundKey[1], nextRoundKey[0]);
    nextRoundKey[2] = gf_add_word(prevRoundKey[2], nextRoundKey[1]);
    nextRoundKey[3] = gf_add_word(prevRoundKey[3], nextRoundKey[2]);

    return nextRoundKey;
endfunction

// Requires the roundConstant that would have been used to generate nextRoundKey from prevRoundKey
function AesState getPrevRoundKey(AesState nextRoundKey, AesWord roundConstant);
    AesState prevRoundKey = newVector();
    
    // nextRoundKey[3] = prevRoundKey[3] ^ nextRoundKey[2]
    // => prevRoundKey[3] = nextRoundKey[3] ^ nextRoundKey[2]
    prevRoundKey[3] = gf_add_word(nextRoundKey[3], nextRoundKey[2]);
    prevRoundKey[2] = gf_add_word(nextRoundKey[2], nextRoundKey[1]);
    prevRoundKey[1] = gf_add_word(nextRoundKey[1], nextRoundKey[0]);
    // nextRoundKey[0] = prevRoundKey[0] ^ subWord(rotWord(prevRoundKey[3])) ^ roundConstant
    // => prevRoundKey[0] = nextRoundKey[0] ^ subWord(rotWord(prevRoundKey[3])) ^ roundConstant
    prevRoundKey[0] = gf_add_word(nextRoundKey[0], gf_add_word(subWord(rotWord(prevRoundKey[3])), roundConstant));

    return prevRoundKey;
endfunction

endpackage
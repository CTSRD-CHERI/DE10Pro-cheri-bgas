import Aes::*;
import AesLib::*;
import GaloisField::*;
import TbUtils::*;
import StmtFSM::*;

import Vector::*;
import SamUtil::*;

// FIPS-197 test vectors describe their AES words in little endian (byte 0 first),
// but Bluespec (and humans!) read literals as big endian (most signficant byte first).
// fipsWord and fipsState interpret "little-endian" literals from FIPS to the correct byte order
// before converting them to AesWord/State.
function AesWord fipsWord(Bit#(32) b);
    Vector#(4, Bit#(8)) bytes = reverseBytes(b);
    return map(fromSizedInteger, bytes);
endfunction

function AesState fipsState(Bit#(128) b);
    Vector#(16, Bit#(8)) bytes = reverseBytes(b);
    return unpack(pack(bytes));
endfunction

function Tuple2#(b, a) flipTuple2(Tuple2#(a, b) t) = tuple2(tpl_2(t), tpl_1(t));
function Tuple3#(a, b, a) flipTuple3(Tuple3#(a, b, a) t) = tuple3(tpl_3(t), tpl_2(t), tpl_1(t));


Tuple3#(RijndaelGf8, RijndaelGf8, RijndaelGf8) gfMulTestVectorsArray[9] = {
    tuple3(8'h57, 8'h01, 8'h57), // = 0x57 * 1
    tuple3(8'h57, 8'h02, 8'hae), // = 0x57 * x 
    tuple3(8'h57, 8'h04, 8'h47), // = 0x57 * x^2
    tuple3(8'h57, 8'h08, 8'h8e), // = 0x57 * x^3
    tuple3(8'h57, 8'h10, 8'h07), // = 0x57 * x^4
    tuple3(8'h57, 8'h20, 8'h0e), // = 0x57 * x^5
    tuple3(8'h57, 8'h40, 8'h1c), // = 0x57 * x^6
    tuple3(8'h57, 8'h80, 8'h38), // = 0x57 * x^7

    // From FIPS-197 Sec 4.2
    tuple3(8'h57, 8'h13, 8'hfe)
};
Vector#(9, Tuple3#(RijndaelGf8, RijndaelGf8, RijndaelGf8)) gfMulTestVectors = arrayToVector(gfMulTestVectorsArray);

// From FIPS-197 A.1
Tuple2#(AesWord, AesWord) rotWordArray[10] = {
    tuple2(fipsWord(32'h09cf4f3c), fipsWord(32'hcf4f3c09)),
    tuple2(fipsWord(32'h2a6c7605), fipsWord(32'h6c76052a)),
    tuple2(fipsWord(32'h7359f67f), fipsWord(32'h59f67f73)),
    tuple2(fipsWord(32'h6d7a883b), fipsWord(32'h7a883b6d)),
    tuple2(fipsWord(32'hdb0bad00), fipsWord(32'h0bad00db)),
    tuple2(fipsWord(32'h11f915bc), fipsWord(32'hf915bc11)),
    tuple2(fipsWord(32'hca0093fd), fipsWord(32'h0093fdca)),
    tuple2(fipsWord(32'h4ea6dc4f), fipsWord(32'ha6dc4f4e)),
    tuple2(fipsWord(32'h7f8d292f), fipsWord(32'h8d292f7f)),
    tuple2(fipsWord(32'h575c006e), fipsWord(32'h5c006e57))
};
Tuple2#(AesWord, AesWord) subWordArray[10] = {
    tuple2(fipsWord(32'hcf4f3c09), fipsWord(32'h8a84eb01)),
    tuple2(fipsWord(32'h6c76052a), fipsWord(32'h50386be5)),
    tuple2(fipsWord(32'h59f67f73), fipsWord(32'hcb42d28f)),
    tuple2(fipsWord(32'h7a883b6d), fipsWord(32'hdac4e23c)),
    tuple2(fipsWord(32'h0bad00db), fipsWord(32'h2b9563b9)),
    tuple2(fipsWord(32'hf915bc11), fipsWord(32'h99596582)),
    tuple2(fipsWord(32'h0093fdca), fipsWord(32'h63dc5474)),
    tuple2(fipsWord(32'ha6dc4f4e), fipsWord(32'h2486842f)),
    tuple2(fipsWord(32'h8d292f7f), fipsWord(32'h5da515d2)),
    tuple2(fipsWord(32'h5c006e57), fipsWord(32'h4a639f5b))
};
Vector#(10, Tuple2#(AesWord, AesWord)) rotWordVector = arrayToVector(rotWordArray);
Vector#(10, Tuple2#(AesWord, AesWord)) subWordVector = arrayToVector(subWordArray);
Vector#(10, Tuple2#(AesWord, AesWord)) invSubWordVector = map(flipTuple2, subWordVector);

Tuple3#(AesState, AesWord, AesState) getNextRoundKeyArray[10] = {
    tuple3(
        fipsState(128'h2b7e151628aed2a6abf7158809cf4f3c),
        fipsWord(32'h01000000),
        fipsState(128'ha0fafe1788542cb123a339392a6c7605)
    ),
    tuple3(
        fipsState(128'ha0fafe1788542cb123a339392a6c7605),
        fipsWord(32'h02000000),
        fipsState(128'hf2c295f27a96b9435935807a7359f67f)
    ),
    tuple3(
        fipsState(128'hf2c295f27a96b9435935807a7359f67f),
        fipsWord(32'h04000000),
        fipsState(128'h3d80477d4716fe3e1e237e446d7a883b)
    ),
    tuple3(
        fipsState(128'h3d80477d4716fe3e1e237e446d7a883b),
        fipsWord(32'h08000000),
        fipsState(128'hef44a541a8525b7fb671253bdb0bad00)
    ),
    tuple3(
        fipsState(128'hef44a541a8525b7fb671253bdb0bad00),
        fipsWord(32'h10000000),
        fipsState(128'hd4d1c6f87c839d87caf2b8bc11f915bc)
    ),
    tuple3(
        fipsState(128'hd4d1c6f87c839d87caf2b8bc11f915bc),
        fipsWord(32'h20000000),
        fipsState(128'h6d88a37a110b3efddbf98641ca0093fd)
    ),
    tuple3(
        fipsState(128'h6d88a37a110b3efddbf98641ca0093fd),
        fipsWord(32'h40000000),
        fipsState(128'h4e54f70e5f5fc9f384a64fb24ea6dc4f)
    ),
    tuple3(
        fipsState(128'h4e54f70e5f5fc9f384a64fb24ea6dc4f),
        fipsWord(32'h80000000),
        fipsState(128'head27321b58dbad2312bf5607f8d292f)
    ),
    tuple3(
        fipsState(128'head27321b58dbad2312bf5607f8d292f),
        fipsWord(32'h1b000000),
        fipsState(128'hac7766f319fadc2128d12941575c006e)
    ),
    tuple3(
        fipsState(128'hac7766f319fadc2128d12941575c006e),
        fipsWord(32'h36000000),
        fipsState(128'hd014f9a8c9ee2589e13f0cc8b6630ca6)
    )
};
Vector#(10, Tuple3#(AesState, AesWord, AesState)) getNextRoundKeyVector = arrayToVector(getNextRoundKeyArray);
// getPrevRoundKey is the same but backwards
Vector#(10, Tuple3#(AesState, AesWord, AesState)) getPrevRoundKeyVector = map(flipTuple3, getNextRoundKeyVector);

// From FIPS-197 Appendix B, rounds 1,2,3
Tuple2#(AesState, AesState) subBytesArray[3] = {
    tuple2(
        fipsState(128'h193de3bea0f4e22b9ac68d2ae9f84808),
        fipsState(128'hd42711aee0bf98f1b8b45de51e415230)
    ),
    tuple2(
        fipsState(128'ha49c7ff2689f352b6b5bea43026a5049),
        fipsState(128'h49ded28945db96f17f39871a7702533b)
    ),
    tuple2(
        fipsState(128'haa8f5f0361dde3ef82d24ad26832469a),
        fipsState(128'hac73cf7befc111df13b5d6b545235ab8)
    )
};
Tuple2#(AesState, AesState) shiftRowsArray[3] = {
    tuple2(
        fipsState(128'hd42711aee0bf98f1b8b45de51e415230),
        fipsState(128'hd4bf5d30e0b452aeb84111f11e2798e5)
    ),
    tuple2(
        fipsState(128'h49ded28945db96f17f39871a7702533b),
        fipsState(128'h49db873b453953897f02d2f177de961a)
    ),
    tuple2(
        fipsState(128'hac73cf7befc111df13b5d6b545235ab8),
        fipsState(128'hacc1d6b8efb55a7b1323cfdf457311b5)
    )
};
Tuple2#(AesState, AesState) mixColumnsArray[3] = {
    tuple2(
        fipsState(128'hd4bf5d30e0b452aeb84111f11e2798e5),
        fipsState(128'h046681e5e0cb199a48f8d37a2806264c)
    ),
    tuple2(
        fipsState(128'h49db873b453953897f02d2f177de961a),
        fipsState(128'h584dcaf11b4b5aacdbe7caa81b6bb0e5)
    ),
    tuple2(
        fipsState(128'hacc1d6b8efb55a7b1323cfdf457311b5),
        fipsState(128'h75ec0993200b633353c0cf7cbb25d0dc)
    )
};
Vector#(3, Tuple2#(AesState, AesState)) subBytesVector = arrayToVector(subBytesArray);
Vector#(3, Tuple2#(AesState, AesState)) shiftRowsVector = arrayToVector(shiftRowsArray);
Vector#(3, Tuple2#(AesState, AesState)) invShiftRowsVector = map(flipTuple2, shiftRowsVector);
Vector#(3, Tuple2#(AesState, AesState)) mixColumnsVector = arrayToVector(mixColumnsArray);
Vector#(3, Tuple2#(AesState, AesState)) invMixColumnsVector = map(flipTuple2, mixColumnsVector);

Tuple3#(AesState, AesState, AesState) addRoundKeyArray[3] = {
    tuple3(
        fipsState(128'h046681e5e0cb199a48f8d37a2806264c),
        fipsState(128'ha0fafe1788542cb123a339392a6c7605),
        fipsState(128'ha49c7ff2689f352b6b5bea43026a5049)
    ),
    tuple3(
        fipsState(128'h584dcaf11b4b5aacdbe7caa81b6bb0e5),
        fipsState(128'hf2c295f27a96b9435935807a7359f67f),
        fipsState(128'haa8f5f0361dde3ef82d24ad26832469a)
    ),
    tuple3(
        fipsState(128'h75ec0993200b633353c0cf7cbb25d0dc),
        fipsState(128'h3d80477d4716fe3e1e237e446d7a883b),
        fipsState(128'h486c4eee671d9d0d4de3b138d65f58e7)
    )
};
Tuple3#(AesState, AesState, AesState) encryptionArray[1] = {
    tuple3(
        fipsState(128'h3243f6a8885a308d313198a2e0370734),
        fipsState(128'h2b7e151628aed2a6abf7158809cf4f3c),
        fipsState(128'h3925841d02dc09fbdc118597196a0b32)        
    )
};
// From FIPS-197 Appendix B, first arg = cipherText, second arg = final round key value, third arg = plaintext
Tuple3#(AesState, AesState, AesState) decryptionKeySched10Array[1] = {
    tuple3(
        fipsState(128'h3925841d02dc09fbdc118597196a0b32),
        fipsState(128'hd014f9a8c9ee2589e13f0cc8b6630ca6), // the final generated round key
        fipsState(128'h3243f6a8885a308d313198a2e0370734)
    )
};
Vector#(3, Tuple3#(AesState, AesState, AesState)) addRoundKeyVector = arrayToVector(addRoundKeyArray);

Vector#(1, Tuple3#(AesState, AesState, AesState)) encryptionVector = arrayToVector(encryptionArray);
Vector#(1, Tuple3#(AesState, AesState, AesState)) decryptionVector = map(flipTuple3, encryptionVector);
Vector#(1, Tuple3#(AesState, AesState, AesState)) decryptionKeySched10Vector = arrayToVector(decryptionKeySched10Array);

(* synthesize *)
module mkTb(Empty);
    Stmt tbStmt = seq
        displayHeader("GF(2^8) Multiplication");
        assertFunc2GivesResults(gf_mul, gfMulTestVectors);

        displayHeader("addRoundKey");
        assertFunc2GivesResults(addRoundKey, addRoundKeyVector);

        displayHeader("shiftRows");
        assertFuncGivesResults(shiftRows, shiftRowsVector);

        displayHeader("invShiftRows");
        assertFuncGivesResults(invShiftRows, invShiftRowsVector);

        displayHeader("rotWord");
        assertFuncGivesResults(rotWord, rotWordVector);

        displayHeader("subWord");
        assertFuncGivesResults(subWord, subWordVector);

        displayHeader("invSubWord");
        assertFuncGivesResults(invSubWord, invSubWordVector);

        displayHeader("mixColumns");
        assertFuncGivesResults(mixColumns, mixColumnsVector);

        displayHeader("invMixColumns");
        assertFuncGivesResults(invMixColumns, invMixColumnsVector);

        displayHeader("getNextRoundKey");
        assertFunc2GivesResults(getNextRoundKey, getNextRoundKeyVector);

        displayHeader("getPrevRoundKey");
        assertFunc2GivesResults(getPrevRoundKey, getPrevRoundKeyVector);

        displayHeader("cipher");
        assertFunc2GivesResults(cipherOnState, encryptionVector);

        displayHeader("decipher - ahead-of-time key schedule");
        assertFunc2GivesResults(decipherOnStateFwdKey, decryptionVector);

        displayHeader("decipher - on-the-fly key schedule");
        assertFunc2GivesResults(decipherOnStateBkwKey, decryptionKeySched10Vector);
    endseq;

    mkAutoFSM(tbStmt);
endmodule
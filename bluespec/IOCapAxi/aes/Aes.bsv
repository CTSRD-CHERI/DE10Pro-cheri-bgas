import AesLib::*;
import Vector::*;

// perform the forward cipher, deriving the key schedule on the fly based on the initial round key
function Bit#(128) cipher(Bit#(128) text, Bit#(128) key) = pack(cipherOnState(unpack(text), unpack(key)));

function AesState cipherOnState(AesState state, AesState key);
    AesState roundKey = key;

    for (Integer round = 0; round <= 10; round = round + 1) begin
        state = addRoundKey(state, roundKey);
        if (round != 10) begin
            roundKey = getNextRoundKey(roundKey, getRoundConstant(round));
            state = subBytes(state);
            state = shiftRows(state);
            if (round != 9)
                state = mixColumns(state);
        end
    end

    return state;
endfunction

// perform the inverse cipher, taking time initially to derive the key schedule with getNextRoundKey()
function Bit#(128) decipherFwdKey(Bit#(128) cipherText, Bit#(128) key) = pack(decipherOnStateFwdKey(unpack(cipherText), unpack(key)));

function AesState decipherOnStateFwdKey(AesState cipherText, AesState key);
    Vector#(11, AesState) keySchedule = newVector();
    keySchedule[0] = key;
    for (Integer round = 1; round < 11; round = round + 1) begin
        // keySchedule[10] = getNextRoundKey(keySchedule[9], getRoundConstant(9));
        keySchedule[round] = getNextRoundKey(keySchedule[round - 1], getRoundConstant(round - 1));
    end

    AesState state = addRoundKey(cipherText, keySchedule[10]);

    for (Integer round = 9; round >= 0; round = round - 1) begin
        state = invShiftRows(state);
        state = invSubBytes(state);
        state = addRoundKey(state, keySchedule[round]);
        if (round != 0)
            state = invMixColumns(state);
    end

    return state;
endfunction

// perform the inverse cipher, deriving the key schedule on the fly based on the final round key
function Bit#(128) decipherBkwKey(Bit#(128) cipherText, Bit#(128) keySchedule10) = pack(decipherOnStateBkwKey(unpack(cipherText), unpack(keySchedule10)));

function AesState decipherOnStateBkwKey(AesState cipherText, AesState keySchedule10);
    AesState roundKey = keySchedule10;

    AesState state = addRoundKey(cipherText, keySchedule10);

    for (Integer round = 9; round >= 0; round = round - 1) begin
        state = invShiftRows(state);
        state = invSubBytes(state);

        // keySchedule[10] = getNextRoundKey(keySchedule[9], getRoundConstant(9));
        // => keySchedule[9] = getPrevRoundKey(keySchedule[10], getRoundConstant(9));
        roundKey = getPrevRoundKey(roundKey, getRoundConstant(round));
        state = addRoundKey(state, roundKey);

        if (round != 0)
            state = invMixColumns(state);
    end

    return state;
endfunction
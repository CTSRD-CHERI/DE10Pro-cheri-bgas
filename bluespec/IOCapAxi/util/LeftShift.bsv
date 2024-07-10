package LeftShift;

/*
This package defines

- left_shift_comb - a combinational function that uses a (shift bits) * (amount bits) two-way mux algorithm to combinatorially shift a number by a variable amount
- LeftShift - an interface for a server that provides left-shifts, potentially with buffering
- mkLeftShift - a module that takes GetPut interfaces for the input and output of a left shift and implements LeftShift. Not intended for direct use, instead use one of 
- mkLeftShiftInFIFO - a module that buffers the input through a 1-stage FIFO and produces a Wire holding the shifted output
- mkLeftShiftOutFIFO - a module that takes a Wire holding the input and buffers the output through a 1-stage FIFO
- mkLeftShiftComb - a module that uses a Wire for both input and output, providing a server-like interface to a combinational shifter
*/

function tshift left_shift_comb(tshift shift, UInt#(szAmt) amt) provisos (Bits#(tshift, szTShift));
    Bit#(szTShift) shiftInProgress = pack(shift);
    Bit#(szAmt) amtBits = pack(amt);

    // log2 algorithm for shifting: check the top bit of the amt, have a two-way mux for (shifted by this bit, not shifted).
    // iterate through all bits of amt until you reach the end.
    // e.g. for a 64-bit shift and 5-bit amt, first have a two-way mux for (amt[4] ? toShift << 16 : toShift)
    // Generates szTShift * szAmt two-way muxes.
    for (Integer i = valueOf(szAmt) - 1; i >= 0; i = i - 1) begin
        // if bit #4 of amtBits is set we want to shift by 16
        Integer amount = 2 ** i;
        // (shiftInProgress << amount) can compile down to a bit-extraction because amount is known at compile-time
        shiftInProgress = (amtBits[i] == 1) ? (shiftInProgress << amount) : shiftInProgress;
    end

    return unpack(shiftInProgress);
endfunction

endpackage
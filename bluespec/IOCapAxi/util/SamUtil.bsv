package SamUtil;

import Vector::*;

function Vector#(n_bytes, Bit#(8)) reverseBytes(Bit#(n_bits) b) provisos (Div#(n_bits, 8, n_bytes), Mul#(n_bytes, 8, n_bits));
    Vector#(n_bytes, Bit#(8)) bytes = unpack(b);
    return reverse(bytes);
endfunction

endpackage
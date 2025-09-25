package SamUtil;

import Vector::*;

function Vector#(n_bytes, Bit#(8)) reverseBytes(Bit#(n_bits) b) provisos (Div#(n_bits, 8, n_bytes), Mul#(n_bytes, 8, n_bits));
    Vector#(n_bytes, Bit#(8)) bytes = unpack(b);
    return reverse(bytes);
endfunction

function WriteOnly#(t) rwireToWriteOnly(RWire#(t) rwire) = interface WriteOnly;
    method Action _write(t v);
        rwire.wset(v);
    endmethod
endinterface;

function WriteOnly#(t) regToWriteOnly(Reg#(t) r) = interface WriteOnly;
    method _write = r._write;
endinterface;

module mkRwireToReadOnlyViaReg#(RWire#(t) rwire)(ReadOnly#(Maybe#(t))) provisos (
    Bits#(Maybe#(t), __a)
);
    Reg#(Maybe#(t)) val <- mkReg(tagged Invalid);

    rule tick;
        val <= rwire.wget();
    endrule

    method _read = val._read;
endmodule

endpackage
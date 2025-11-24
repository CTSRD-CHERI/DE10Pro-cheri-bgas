// A module implementing the SingleChecker3 interface that doesn't do anything with it,
// it simply passes it out.
// This is used as a baseline to determine the number of LUTs for the input/output harnesses.
module null_SingleChecker3(
    input  [553 : 0] checker_in_put_val,
    input  [8 : 0] checker_keyToKill__write_x,
    input  CLK,
    input  EN_checker_checkResponse_drop,
    input  EN_checker_in_put,
    input  EN_checker_keyToKill__write,
    input  RST_N,

    output [63 : 0] checker_checkResponse_peek_snd_fst,
    output [7 : 0] checker_checkResponse_peek_snd_snd_fst,
    output [96 : 0] checker_checkResponse_peek_fst,
    output checker_checkResponse_canPeek,
    output checker_checkResponse_peek_snd_snd_snd,
    output checker_in_canPut,
    output RDY_checker_checkResponse_drop,
    output RDY_checker_checkResponse_peek_fst,
    output RDY_checker_checkResponse_peek_snd_fst,
    output RDY_checker_checkResponse_peek_snd_snd_fst,
    output RDY_checker_checkResponse_peek_snd_snd_snd,
    output RDY_checker_in_put,
    output RDY_checker_keyToKill__write
);
    wire [565:0] i;
    assign i = {
        EN_checker_keyToKill__write,
        EN_checker_in_put,
        EN_checker_checkResponse_drop,
        checker_keyToKill__write_x,
        checker_in_put_val
    };
    
    // input_harness is a shift register, shifting a single input bit into the top of the input signals.
    // i is a reconstruction of that input signal.
    // taking the low bits of i ensures that all of the machinery of the input_harness is retained.

    wire [178:0] o;
    assign o = i[178:0];

    // spit the bits out in the same order that the test harness expects
    assign checker_checkResponse_peek_snd_fst = o[63:0];
    assign checker_checkResponse_peek_snd_snd_fst = o[71:64];
    assign checker_checkResponse_peek_fst = o[168:72];
    assign checker_checkResponse_canPeek = o[169];
    assign checker_checkResponse_peek_snd_snd_snd = o[170];
    assign checker_in_canPut = o[171];
    assign RDY_checker_checkResponse_drop = o[172];
    assign RDY_checker_checkResponse_peek_fst = o[173];
    assign RDY_checker_checkResponse_peek_snd_fst = o[174];
    assign RDY_checker_checkResponse_peek_snd_snd_fst = o[175];
    assign RDY_checker_checkResponse_peek_snd_snd_snd = o[176];
    assign RDY_checker_in_put = o[177];
    assign RDY_checker_keyToKill__write = o[178];
endmodule
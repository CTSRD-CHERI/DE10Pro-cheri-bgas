// A module implementing the UnifiedSingleExposerKeyMngrTb interface that doesn't do anything with it,
// it simply passes it out.
// This is used as a baseline to determine the number of LUTs for the input/output harnesses.
module null_UnifiedSingleExposerKeyMngrTb(
    input  [15 : 0] keyStore_ar_put_val,
    input  [15 : 0] keyStore_aw_put_val,
    input  [35 : 0] keyStore_w_put_val,
    input  [36 : 0] exposer4x32_iocapsIn_axiSignals_w_put_val,
    input  [38 : 0] exposer4x32_sanitizedOut_r_put_val,
    input  [5 : 0] exposer4x32_sanitizedOut_b_put_val,
    input  [99 : 0] exposer4x32_iocapsIn_axiSignals_ar_put_val,
    input  [99 : 0] exposer4x32_iocapsIn_axiSignals_aw_put_val,
    input  CLK,
    input  EN_exposer4x32_iocapsIn_axiSignals_ar_put,
    input  EN_exposer4x32_iocapsIn_axiSignals_aw_put,
    input  EN_exposer4x32_iocapsIn_axiSignals_b_drop,
    input  EN_exposer4x32_iocapsIn_axiSignals_r_drop,
    input  EN_exposer4x32_iocapsIn_axiSignals_w_put,
    input  EN_exposer4x32_sanitizedOut_ar_drop,
    input  EN_exposer4x32_sanitizedOut_aw_drop,
    input  EN_exposer4x32_sanitizedOut_b_put,
    input  EN_exposer4x32_sanitizedOut_r_put,
    input  EN_exposer4x32_sanitizedOut_w_drop,
    input  EN_keyStore_ar_put,
    input  EN_keyStore_aw_put,
    input  EN_keyStore_b_drop,
    input  EN_keyStore_r_drop,
    input  EN_keyStore_w_put,
    input  RST_N,
    output [1 : 0] keyStore_b_peek,
    output [33 : 0] keyStore_r_peek,
    output [36 : 0] exposer4x32_sanitizedOut_w_peek,
    output [38 : 0] exposer4x32_iocapsIn_axiSignals_r_peek,
    output [5 : 0] exposer4x32_iocapsIn_axiSignals_b_peek,
    output [511 : 0] debugKeyState__read,
    output [63 : 0] debugBadRead__read,
    output [63 : 0] debugBadWrite__read,
    output [63 : 0] debugGoodRead__read,
    output [63 : 0] debugGoodWrite__read,
    output [96 : 0] exposer4x32_sanitizedOut_ar_peek,
    output [96 : 0] exposer4x32_sanitizedOut_aw_peek,
    output exposer4x32_iocapsIn_axiSignals_ar_canPut,
    output exposer4x32_iocapsIn_axiSignals_aw_canPut,
    output exposer4x32_iocapsIn_axiSignals_b_canPeek,
    output exposer4x32_iocapsIn_axiSignals_r_canPeek,
    output exposer4x32_iocapsIn_axiSignals_w_canPut,
    output exposer4x32_sanitizedOut_ar_canPeek,
    output exposer4x32_sanitizedOut_aw_canPeek,
    output exposer4x32_sanitizedOut_b_canPut,
    output exposer4x32_sanitizedOut_r_canPut,
    output exposer4x32_sanitizedOut_w_canPeek,
    output keyStore_ar_canPut,
    output keyStore_aw_canPut,
    output keyStore_b_canPeek,
    output keyStore_r_canPeek,
    output keyStore_w_canPut,
    output RDY_debugBadRead__read,
    output RDY_debugBadWrite__read,
    output RDY_debugGoodRead__read,
    output RDY_debugGoodWrite__read,
    output RDY_debugKeyState__read,
    output RDY_exposer4x32_iocapsIn_axiSignals_ar_put,
    output RDY_exposer4x32_iocapsIn_axiSignals_aw_put,
    output RDY_exposer4x32_iocapsIn_axiSignals_b_drop,
    output RDY_exposer4x32_iocapsIn_axiSignals_b_peek,
    output RDY_exposer4x32_iocapsIn_axiSignals_r_drop,
    output RDY_exposer4x32_iocapsIn_axiSignals_r_peek,
    output RDY_exposer4x32_iocapsIn_axiSignals_w_put,
    output RDY_exposer4x32_sanitizedOut_ar_drop,
    output RDY_exposer4x32_sanitizedOut_ar_peek,
    output RDY_exposer4x32_sanitizedOut_aw_drop,
    output RDY_exposer4x32_sanitizedOut_aw_peek,
    output RDY_exposer4x32_sanitizedOut_b_put,
    output RDY_exposer4x32_sanitizedOut_r_put,
    output RDY_exposer4x32_sanitizedOut_w_drop,
    output RDY_exposer4x32_sanitizedOut_w_peek,
    output RDY_keyStore_ar_put,
    output RDY_keyStore_aw_put,
    output RDY_keyStore_b_drop,
    output RDY_keyStore_b_peek,
    output RDY_keyStore_r_drop,
    output RDY_keyStore_r_peek,
    output RDY_keyStore_w_put
);
    wire [364:0] i;
    assign i = {
        EN_keyStore_w_put,
        EN_keyStore_r_drop,
        EN_keyStore_b_drop,
        EN_keyStore_aw_put,
        EN_keyStore_ar_put,
        EN_exposer4x32_sanitizedOut_w_drop,
        EN_exposer4x32_sanitizedOut_r_put,
        EN_exposer4x32_sanitizedOut_b_put,
        EN_exposer4x32_sanitizedOut_aw_drop,
        EN_exposer4x32_sanitizedOut_ar_drop,
        EN_exposer4x32_iocapsIn_axiSignals_w_put,
        EN_exposer4x32_iocapsIn_axiSignals_r_drop,
        EN_exposer4x32_iocapsIn_axiSignals_b_drop,
        EN_exposer4x32_iocapsIn_axiSignals_aw_put,
        EN_exposer4x32_iocapsIn_axiSignals_ar_put,
        exposer4x32_iocapsIn_axiSignals_aw_put_val,
        exposer4x32_iocapsIn_axiSignals_ar_put_val,
        exposer4x32_sanitizedOut_b_put_val,
        exposer4x32_sanitizedOut_r_put_val,
        exposer4x32_iocapsIn_axiSignals_w_put_val,
        keyStore_w_put_val,
        keyStore_aw_put_val,
        keyStore_ar_put_val
    };
    
    // input_harness is a shift register, shifting a single input bit into the top of the input signals.
    // i is a reconstruction of that input signal.
    // taking the low bits of i ensures that all of the machinery of the input_harness is retained.

    wire [353:0] o;
    assign o = i[353:0];

    // spit the bits out in the same order that the test harness expects
    assign keyStore_b_peek = o[1:0];
    assign keyStore_r_peek = o[35:2];
    assign exposer4x32_sanitizedOut_w_peek = o[72:36];
    assign exposer4x32_iocapsIn_axiSignals_r_peek = o[111:73];
    assign exposer4x32_iocapsIn_axiSignals_b_peek = o[117:112];
    assign exposer4x32_sanitizedOut_ar_peek = o[214:118];
    assign exposer4x32_sanitizedOut_aw_peek = o[311:215];
    assign exposer4x32_iocapsIn_axiSignals_ar_canPut = o[312];
    assign exposer4x32_iocapsIn_axiSignals_aw_canPut = o[313];
    assign exposer4x32_iocapsIn_axiSignals_b_canPeek = o[314];
    assign exposer4x32_iocapsIn_axiSignals_r_canPeek = o[315];
    assign exposer4x32_iocapsIn_axiSignals_w_canPut = o[316];
    assign exposer4x32_sanitizedOut_ar_canPeek = o[317];
    assign exposer4x32_sanitizedOut_aw_canPeek = o[318];
    assign exposer4x32_sanitizedOut_b_canPut = o[319];
    assign exposer4x32_sanitizedOut_r_canPut = o[320];
    assign exposer4x32_sanitizedOut_w_canPeek = o[321];
    assign keyStore_ar_canPut = o[322];
    assign keyStore_aw_canPut = o[323];
    assign keyStore_b_canPeek = o[324];
    assign keyStore_r_canPeek = o[325];
    assign keyStore_w_canPut = o[326];
    assign RDY_debugBadRead__read = o[327];
    assign RDY_debugBadWrite__read = o[328];
    assign RDY_debugGoodRead__read = o[329];
    assign RDY_debugGoodWrite__read = o[330];
    assign RDY_debugKeyState__read = o[331];
    assign RDY_exposer4x32_iocapsIn_axiSignals_ar_put = o[332];
    assign RDY_exposer4x32_iocapsIn_axiSignals_aw_put = o[333];
    assign RDY_exposer4x32_iocapsIn_axiSignals_b_drop = o[334];
    assign RDY_exposer4x32_iocapsIn_axiSignals_b_peek = o[335];
    assign RDY_exposer4x32_iocapsIn_axiSignals_r_drop = o[336];
    assign RDY_exposer4x32_iocapsIn_axiSignals_r_peek = o[337];
    assign RDY_exposer4x32_iocapsIn_axiSignals_w_put = o[338];
    assign RDY_exposer4x32_sanitizedOut_ar_drop = o[339];
    assign RDY_exposer4x32_sanitizedOut_ar_peek = o[340];
    assign RDY_exposer4x32_sanitizedOut_aw_drop = o[341];
    assign RDY_exposer4x32_sanitizedOut_aw_peek = o[342];
    assign RDY_exposer4x32_sanitizedOut_b_put = o[343];
    assign RDY_exposer4x32_sanitizedOut_r_put = o[344];
    assign RDY_exposer4x32_sanitizedOut_w_drop = o[345];
    assign RDY_exposer4x32_sanitizedOut_w_peek = o[346];
    assign RDY_keyStore_ar_put = o[347];
    assign RDY_keyStore_aw_put = o[348];
    assign RDY_keyStore_b_drop = o[349];
    assign RDY_keyStore_b_peek = o[350];
    assign RDY_keyStore_r_drop = o[351];
    assign RDY_keyStore_r_peek = o[352];
    assign RDY_keyStore_w_put = o[353];
endmodule
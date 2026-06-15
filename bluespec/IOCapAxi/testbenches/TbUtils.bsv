package TbUtils;

import Vector::*;

function Action displayHeader(String head) = action
    $display("===== ", head, " =====");
endaction;

function Action assertFuncGivesResults(function t_out f(t_in x), Vector#(n, Tuple2#(t_in, t_out)) expected) provisos (Eq#(t_out), FShow#(t_in), FShow#(t_out)) = action
    for (Integer i = 0; i < valueOf(n); i = i + 1) begin
        let in = tpl_1(expected[i]);
        let expected_out = tpl_2(expected[i]);
        let actual_out = f(in);

        if (expected_out != actual_out) begin
            $display("assertEq failure: f(in) != expected_out",
            "\nin:       ", fshow(in),
            "\nexpected: ", fshow(expected_out),
            "\nactual:   ", fshow(actual_out));
            $finish();
        end
    end
    $display("Success!");
endaction;

function Action assertFunc2GivesResults(function t_out f(t_in1 x, t_in2 y), Vector#(n, Tuple3#(t_in1, t_in2, t_out)) expected) provisos (Eq#(t_out), FShow#(t_in1), FShow#(t_in2), FShow#(t_out)) = action
    for (Integer i = 0; i < valueOf(n); i = i + 1) begin
        let in1 = tpl_1(expected[i]);
        let in2 = tpl_2(expected[i]);
        let expected_out = tpl_3(expected[i]);
        let actual_out = f(in1, in2);

        if (expected_out != actual_out) begin
            $display("assertEq failure: f(in1, in2) != expected_out",
            "\nin1:      ", fshow(in1),
            "\nin2:      ", fshow(in2),
            "\nexpected: ", fshow(expected_out),
            "\nactual:   ", fshow(actual_out));
            $finish();
        end
    end
    $display("Success!");
endaction;

function Action assertEq(t x, t y, String message) provisos (Eq#(t), FShow#(t)) = action
    if (x != y) begin
        $display("assertEq failure: x != y, ", message, "\nx: ", fshow(x), "\ny: ", fshow(y));
        $finish();
    end
endaction;

endpackage
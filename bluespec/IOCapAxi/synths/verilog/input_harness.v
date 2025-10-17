// Expand a single input wire in a slow clock domain to 
// a shift register of width BITS in a fast clock domain.
// This retains the dependencies, so the data bits can be used without fear of getting them optimized out.
//
// Uses synchronous active-low reset, like Bluespec-generated verilog designs by default
module input_harness#(parameter BITS)(
    input slow_clk,
    input fast_clk,
    input rst_n,
    input pin_on_slow_clk,
    output [BITS-1:0] data_on_fast_clk
);
    // Clock domain crossing from slow_clk to fast_clk.
    // pin_crossing[2] is where the pin goes in from slow, most likely metastable
    // pin_crossing[1] and pin_crossing[0] are stable
    // pin_crossing[0] is used as the source for the data shift register
    reg[2:0] pin_crossing;
    // The actual shift register for the data. TODO consider more layers of registering
    reg[BITS-1:0] shift_reg;
    assign data_on_fast_clk = shift_reg;

    always @(posedge fast_clk) begin
        if (rst_n == 0) begin
            pin_crossing <= 0;
            shift_reg <= 0;
        end else begin
            pin_crossing <= { pin_on_slow_clk, pin_crossing[2:1] };
            shift_reg <= { pin_crossing[0], shift_reg[BITS-1:1] };
        end
    end
endmodule
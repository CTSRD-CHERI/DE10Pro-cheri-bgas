// Compress an input signal of width BITS in a fast clock domain to 
// a single bit in a slow clock domain.
// This retains the dependencies, so the data bits can be used without fear of getting them optimized out.
// BITS should be a pow2.
//
// Uses synchronous active-low reset, like Bluespec-generated verilog designs by default
module output_harness#(parameter BITS, parameter LG_BITS = $clog2(BITS))(
    input slow_clk,
    input fast_clk,
    input rst_n,
    input [BITS-1:0] data_on_fast_clk,
    output pin_on_slow_clk
);
    // Register the input on the fast clock
    reg[BITS-1:0] data_reg;
    // Parallel reduction of data_delta where each register stage halves the width by XORing adjacent signals together.
    // Half the regs in this should be optimized out
    // data_xord[0] <= data_xord[0] ^ data_reg
    reg[BITS-1:0] data_xord[LG_BITS-1:0];
    wire pin_in = data_xord[LG_BITS-1][0];

    // Clock domain crossing from fast_clk to slow_clk.
    // pin_crossing[2] is where pin_in goes in from fast, most likely metastable
    // pin_crossing[1] and pin_crossing[0] are stable
    // pin_crossing[0] is used as the source for the output
    reg[2:0] pin_crossing;
    assign pin_on_slow_clk = pin_crossing[0];

    always @(posedge fast_clk) begin
        if (rst_n == 0) begin
            data_reg <= 0;
            data_xord[0] <= 0;
        end else begin
            data_reg <= data_on_fast_clk;
            data_xord[0] <= data_xord[0] ^ data_reg;
        end
    end
	 
    generate
        genvar i;
        // Generate a reduction for data_xord[i+1] from the two halves of data_xord[i]
        for (i = 0; i < LG_BITS - 1; i = i + 1) begin
            `define WIDTH (1 << (LG_BITS - 1 - i))
            always @(posedge fast_clk) begin
                if (rst_n == 0) begin
                    data_xord[i + 1] <= 0;
                end else begin
                    data_xord[i + 1][`WIDTH - 1:0] <= data_xord[i][`WIDTH-1:0] ^ data_xord[i][2*`WIDTH - 1:`WIDTH];
                end
            end
        end
    endgenerate

    always @(posedge slow_clk) begin
        if (rst_n == 0) begin
            pin_crossing <= 0;
        end else begin
            pin_crossing <= { pin_in, pin_crossing[2:1] };
        end
    end
endmodule
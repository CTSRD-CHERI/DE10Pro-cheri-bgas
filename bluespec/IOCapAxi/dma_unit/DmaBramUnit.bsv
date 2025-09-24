package DmaBramUnit;

import BRAM::*;

interface DmaBramUnit#( numeric type word_width, numeric type planner_log_words, numeric type executor_log_words, numeric type executor_n_units);
    interface BRAMServerBE#(UInt#(planner_log_words), Bit#(word_width), TDiv#(word_width, 8)) planner_bram_out;
    interface BRAMServerBE#(UInt#(TAdd#(executor_log_words, TLog#(executor_n_units))), Bit#(word_width), TDiv#(word_width, 8)) executor_bram_out;
endinterface

// Typical values:
// - word width ~= 256bits (32 bytes) so each word fits an IOCap
// - number of planner-words ~= (TODO ENOUGH FOR THE VIRTIO SPLIT QUEUES + 3x descriptors + header) / (32 bytes) = (32 bytes * 3 + 16 bytes) / (32 bytes) = (112 / 32) bytes ~= 4 words
//   => planner_log_words = log2(4) = 2
// - number of executor-words ~= (2x block size (write into one while DMAing out another)) / (32 bytes) = (2x 512 bytes) / (32 bytes) = 32 words
//   => executor_log_words = log2(32) = 5
// - executor_n_units = 1 (for now)
module mkDmaBramUnit#(DmaBramUnit(word_width, planner_log_words, executor_log_words, executor_n_units));
    BRAM_Configure cfg = defaultValue;
    BRAM2PortBe#(UInt#(planner_log_words), Bit#(word_width), TDiv#(word_width, 8)) planner_bram <- mkBRAM2BEServer(cfg);
    BRAM2PortBe#(UInt#(TAdd#(executor_log_words, TLog#(executor_n_units))), Bit#(word_width), TDiv#(word_width, 8)) executor_bram <- mkBRAM2BEServer(cfg);

    // DMA data into planner BRAM via port A
    // DMA data into executor BRAM via port A
    
    // Expose port B of planner BRAM on interface
    interface planner_bram_out = planner_bram.portB;

    // Expose port B of planner BRAM on interface
    interface executor_bram_out = executor_bram.portB;
endmodule

module mkStandardDmaBramUnit#(DmaBramUnit(512, ))
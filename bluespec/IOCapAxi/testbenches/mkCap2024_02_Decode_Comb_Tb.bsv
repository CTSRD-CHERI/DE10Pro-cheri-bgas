import Cap2024 :: *;
import Cap2024_02 :: *;
import Cap2024_02_Decode_Comb :: *;
import GetPut :: *;
import SourceSink :: *;
import FIFOF :: *;
import Tests :: *;

(* synthesize *)
module mkCap2024_02_Decode_Comb_Tb(CapDecodeTb#(Cap2024_02));
    FIFOF#(Cap2024_02) in <- mkFIFOF;
    FIFOF#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) out <- mkFIFOF;
    mkCombCapDecode(toGet(in), toPut(out));

    interface stimulusIn = toSink(in);
    interface stimulusOut = toSource(out);
endmodule
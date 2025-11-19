import IOCapAxi_Exposers :: *;
import IOCapAxi_Konata :: *;
import Tests :: *;
import BlueBasics :: *;
import IOCapAxi_Checker3s :: *;

(* synthesize *)
module mkSingleChecker3_2percycle(IOCapAxiChecker3_Write);
    let mod <- mkSimpleIOCapAxiChecker3V1_FastDecode_2CycleAES_Write(KONATA_OFF);
    return mod;
endmodule

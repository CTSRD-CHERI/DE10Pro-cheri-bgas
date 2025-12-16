import IOCapAxi_Exposers :: *;
import IOCapAxi_Konata :: *;
import Tests :: *;
import BlueBasics :: *;
import IOCapAxi_Checker3s :: *;

(* synthesize *)
module mkSingleChecker3_null(IOCapAxiChecker3_Write);
    let mod <- mkNullIOCapAxiChecker3V1_Write(KONATA_OFF);
    return mod;
endmodule

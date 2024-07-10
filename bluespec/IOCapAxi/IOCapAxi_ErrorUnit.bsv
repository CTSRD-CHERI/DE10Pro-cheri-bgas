interface ErrorUnit#(type t_err);
    method Action assertError(t_err e);
endinterface

(* synthesize *)
module mkErrorUnit(ErrorUnit#(t_err)) provisos (Bits#(t_err, n_err_bits), NumAlias#(n_errs, TExp(n_err_bits)), FShow#(t_err));
    Vector#(n_errs, Reg#(Bool)) encounteredErr <- replicateM(mkReg(False));
    Vector#(n_errs, PulseWire) wantNotifyErr <- replicateM(mkPulseWireOR);

    rule checkErrs;
        for (Integer i = 0; i < n_errs; i = i + 1) begin
            if (wantNotifyErr[i]) begin
                encounteredErr[i] <= True;
            end
        end
    endrule

    method Action assertError(t_err e);
        wantNotifyErr[unpack(pack(e))].send();
        $error("Encountered error ", fshow(e));
    endmethod
endmodule
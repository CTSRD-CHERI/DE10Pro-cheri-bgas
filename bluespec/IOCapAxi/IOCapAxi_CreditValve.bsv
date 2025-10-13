import SourceSink :: *;
import IOCapAxi_Konata :: *;

typedef union tagged {
    void Pass;
    void Drop;
} CreditMode deriving (Bits, Eq);

interface CreditValve#(type t, numeric type count_n);
    (* always_ready, always_enabled *)
    method Bool canUpdateCredits(CreditMode mode);

    // Must ONLY be called if canUpdateCredits() is true.
// I can't put an actual condition on this function in the impl because it is argument-dependent?
    // TODO calculations/reasoning on how to choose count_n that will never overflow
    method Action updateCredits(CreditMode mode, UInt#(count_n) credits);

    interface Source#(t) out;
endinterface

module mkSimpleCreditValve#(KonataMode kMode, Source#(t) in)(CreditValve#(t, count_n));
    // The state is simple: keep the current mode (Pass items through in to out, or Drop) and the credits for that mode (e.g. Pass 11 flits through, or Drop 2)
    // Changing mode (from Pass -> Drop, Drop -> Pass) requires waiting for all the credits of the current mode to be exhausted, because we don't have registers to store (Pass 11 *then* Drop 2).
    // For ultimate non-blocking mode, you could keep a queue of (mode, credit) pairs, but that only affects the throughput of *changing* mode.
    Reg#(CreditMode) mode <- mkReg(Drop);
    Reg#(UInt#(count_n)) credits <- mkReg(0);

    // The state is updated in two phases: first, we check if we're supposed to drop the value exposed by `in`.
    // In the second phase, we update the credit count. This has to happen after the first phase, because dropping a value uses a credit and thus affects the count.
    PulseWire wantDrop <- mkPulseWire;
    PulseWire successfullyDropped <- mkPulseWire;

    RWire#(CreditMode) modeToUpdate <- mkRWire;
    Wire#(UInt#(count_n)) creditsToAdd <- mkDWire(0);
    

    function Bool allowPeek() = (mode == Pass) && (credits > 0) && in.canPeek;

    rule dropIfWanted if (in.canPeek);
        // We drop from the input if
        // - we have passthrough credits, and someone has peeked/dropped from our output
        // - we have drop credits
        Bool dropFromIn = (credits > 0) && (
            ((mode == Pass) || wantDrop) || (mode == Drop)
        );

        if (dropFromIn) begin
            in.drop();
            successfullyDropped.send();
        end
    endrule

    rule updateCreditsAndMode;
        UInt#(count_n) newCredits = credits;
        if (successfullyDropped) begin
            newCredits = credits - 1;
        end
        credits <= newCredits + creditsToAdd;

        if (modeToUpdate.wget() matches tagged Valid .newMode) begin
            mode <= newMode;
        end
    endrule

    method Bool canUpdateCredits(CreditMode newMode) = (mode == newMode) || (credits == 0);
    
    method Action updateCredits(CreditMode newMode, UInt#(count_n) newCredits) /* if canUpdateCredits(newMode) */;
        if (mode != newMode) begin
            modeToUpdate.wset(newMode);
        end

        creditsToAdd <= newCredits;
    endmethod

    interface out = interface Source#(t);
        method Bool canPeek;
            return allowPeek();
        endmethod
        method t peek if (allowPeek());
            return in.peek;
        endmethod
        method Action drop if (allowPeek());
            konataEvent(kMode, "ValuePassW", fshow(" "));
            wantDrop.send();
        endmethod
    endinterface;
endmodule
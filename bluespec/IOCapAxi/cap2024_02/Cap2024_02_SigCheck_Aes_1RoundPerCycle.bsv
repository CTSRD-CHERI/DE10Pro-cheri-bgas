package Cap2024_02_SigCheck_Aes_1RoundPerCycle;

import AesLib::*;
import StmtFSM::*;
import GetPut::*;
import ClientServer::*;
import Cap2024_02::*;
import FIFO::*;
import MattUtil::*;
import SamUtil::*;

export CapSigCheckIn(..);
export mk1RoundPerCycleCapSigCheck;

// The internals of the signature checker calculate a round of AES-128 encryption, generating the next round key on-the-fly.
// AES rounds have the following structure
// 1. state = addRoundKey(state, roundKey)
// 2. state = shiftRows(subBytes(state)) // not enabled on the final round
// 3. state = mixColumns(state) // enabled on most rounds, not the penultimate or final round
// 4. roundKey = getNextRoundKey(roundKey, round) // in parallel with 1, 2, 3
// 5. comparison of state to expectedSig
typedef struct {
    Bit#(4) round; // Should only ever be 0..=10
    AesState state;
    AesState roundKey;
} CalcInput deriving (Bits, FShow);

interface InternalCalc;
    method Bit#(4) newRound;     // If in.put() on the previous cycle, returns a value in the range 0..=11. It is the owners responsibility to reset the internal calc on seeing 11 and 11 MUST NOT BE PASSED BACK IN.
                                 //  Else returns ?
    method AesState newState;    // If in.put() on the previous cycle, returns a value which should be shoved back into in.put() or compared to the expected signature. Else returns ?
    method AesState newRoundKey; // on any cycle where in.round = 10, this emits ?. If no in.put() on the previous cycle, also returns ?

    // Set the new input for the next cycle
    interface Put#(CalcInput) in;
endinterface

/* AES-128 computation structure

--- round = 0
state = addRoundKey(state, roundKey);
state = shiftRows(subBytes(state));
nextState = mixColumns(state);
nextRoundKey = getNextRoundKey(roundKey, getRoundConstant(0))

--- round = 1 through 9
state = addRoundKey(state, roundKey);
state = shiftRows(subBytes(state));
nextState = mixColumns(state);
nextRoundKey = getNextRoundKey(roundKey, getRoundConstant(round))

--- round = 10
state = addRoundKey(state, roundKey);
// if this was the last encryption for this capability
matchesExpectedSig = (state == expected);

--- outside the internalcalc, serial dependency on round 10.state
// otherwise, setup the next encryption
nextState = nextText
// the key for the next encryption = the sig of the last encryption = the current state
nextRoundKey = state
// TODO if we can spare another 128-bit XOR we could start round 0 of the next encryption
// at the same time! I bet we can, because the (state == expected) operation prob also needs it...

// TODO if we tell our containing module "going to finish on this cycle" one cycle early,
// it can queue up another capability's text and key and immediately shove it into us as the computation after we're done.

*/

module mkInternalCalc(InternalCalc);
    // Inputs are clocked, outputs are not
    // Use vReg so we only do computations when requested
    Reg#(CalcInput) inReg <- mkVReg;

    Wire#(Bit#(4)) newRound_w <- mkDWire(?); // Should only ever be 0..=10
    Wire#(AesState) newState_w <- mkDWire(?);
    Wire#(AesState) newRoundKey_w <- mkDWire(?);

    rule comp;
        let in = inReg;

        // $display("(%04d) in: round %2d state 0x%16x roundKey 0x%16x", $time, in.round, in.state, in.roundKey);
        
        AesState state = addRoundKey(in.state, in.roundKey);
        if (in.round == 10) begin
            // $display("       out: round 11 state 0x%16x roundKey ?", state);
            newRound_w <= 11; // The containing module may or may not trigger a CalcInput on the next cycle and reset our newRound.
            newState_w <= state; 
            newRoundKey_w <= ?; // There isn't a *new* round key
        end else begin // Can only be 0..=9
            newRound_w <= in.round + 1;

            state = shiftRows(subBytes(state));
            if (in.round == 9) begin
                // $display("nomix");
                newState_w <= state;
            end else begin
                // $display("mix");
                newState_w <= mixColumns(state);
            end

            let rcon = getDynRoundConstant(in.round);
            // $display("getroundkey rcon %16x", rcon);
            newRoundKey_w <= getNextRoundKey(in.roundKey, rcon);
        end
    endrule

    method Bit#(4) newRound;
        return newRound_w;
    endmethod
    method AesState newState;
        return newState_w;
    endmethod
    method AesState newRoundKey;
        return newRoundKey_w;
    endmethod

    interface in = toPut(asIfc(inReg));
endmodule

typedef struct {
    Cap2024_02 cap;
    Bit#(128) expectedSig;
    Bit#(128) secret;
} CapSigCheckIn deriving (Bits, FShow);

function AesState stateOfCap(Cap2024_02 cap);
    return unpack(pack(cap));
endfunction

// TODO this needs an "abort" signal! in case the decoder fails early
module mk1RoundPerCycleCapSigCheck#(Get#(CapSigCheckIn) in, Put#(CapCheckResult#(Bit#(0))) out)(Empty);
    Reg#(Cap2024_02) currentCap <- mkReg(?);
    Reg#(Bit#(128)) currentSig <- mkReg(?);
    Reg#(Bit#(2)) currentCavLevel <- mkReg(?); // 0..=2
    Reg#(Bit#(2)) expectedCavLevel <- mkReg(?); // 0..=2
    InternalCalc calc <- mkInternalCalc;

    Reg#(Bool) fsmWorking <- mkReg(False);

    Stmt backendStmt = seq
        // While this FSM is active, InternalCalc.calc.out has data
        // If calc.newRound = 11, we still loop if currentCavLevel != expectedCavLevel.
        // If calc.newRound != 11 we loop
        while (calc.newRound != 11 || currentCavLevel != expectedCavLevel) seq
            // Wait for the AES to finish
            if (calc.newRound != 11) action
                calc.in.put(CalcInput {
                    round: calc.newRound,
                    state: calc.newState,
                    roundKey: calc.newRoundKey
                });
            endaction else action
                // calc.newRound == 11 && currentCavLevel != expectedCavLevel
                if (currentCavLevel == 0) begin
                    currentCavLevel <= 1;
                    // atCav1(currentCap) must be valid because expectedCavLevel != 0
                    calc.in.put(CalcInput {
                        round: 0,
                        state: stateOfCap(fromMaybe(?, atCav1(currentCap))),
                        roundKey: calc.newState
                    });
                end else begin 
                    // currentCavLevel must be 1.
                    // it can't be 2 because expectedCavLevel maxes out at 2, currentCavLevel != expectedCavLevel, and we don't increment currentCavLevel past expectedCavLevel.
                    currentCavLevel <= 2;
                    // atCav1(currentCap) must be valid because expectedCavLevel != 0
                    calc.in.put(CalcInput {
                        round: 0,
                        state: stateOfCap(currentCap),
                        roundKey: calc.newState
                    });
                end
            endaction
        endseq

        action
            // calc.newRound = 11 && currentCavLevel == expectedCavLevel
            // => we need to check that the signatures match
            if (pack(calc.newState) == currentSig)
                out.put(tagged Succ 0);
            else
                out.put(tagged Fail InvalidSignature);
            // TODO this is inefficient - could be doing fetch on this cycle...
            fsmWorking <= False;
        endaction
    endseq;

    // TODO continuous assert that currentCavLevel is never > expectedCavLevel.

    FSM backendFSM <- mkFSMWithPred(backendStmt, fsmWorking);

    rule fetch if (!fsmWorking);
        let i <- in.get();

        currentCap <= i.cap;
        currentSig <= i.expectedSig;

        currentCavLevel <= 0;

        if (atCav2(i.cap) matches tagged Valid .*)
            expectedCavLevel <= 2;
        else if (atCav1(i.cap) matches tagged Valid .*)
            expectedCavLevel <= 1;
        else
            expectedCavLevel <= 0;

        // TODO could check whether unused caveats are zeroed to skip the sig-check
        calc.in.put(CalcInput {
            round: 0,
            state: stateOfCap(atRoot(i.cap)),
            roundKey: unpack(i.secret)
        });
        fsmWorking <= True;
        backendFSM.start();
    endrule
endmodule

endpackage
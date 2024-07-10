package Cap2024_02_SigCheck_Aes_2RoundPerCycle;

import AesLib::*;
import StmtFSM::*;
import GetPut::*;
import ClientServer::*;
import Cap2024_02::*;
import FIFO::*;
import MattUtil::*;
import SamUtil::*;

import Cap2024_02_SigCheck_Aes_1RoundPerCycle::*; // Get CapSigCheckIn

export mk2RoundPerCycleCapSigCheck;

/* AES-128 computation structure

AES-128 has 6 cycles in a one-cycle-per-round, for two cycles there are a few possibilities

Tightly packed:

Zero-cav
0 1    S
2 3    S
4 5    S
6 7    S
8 9    9-at-end
10 X   10-at-start

One-cav
0 1    S
2 3    S
4 5    S
6 7    S
8 9    9-at-end
10 0'  10-at-start-continue
1' 2'  S
3' 4'  S
5' 6'  S
7' 8'  S
9' 10' 9-then-10

Two-cav
0 1    S
2 3    S
4 5    S
6 7    S
8 9    9-at-end
10 0'  10-at-start-continue
1' 2'  S
3' 4'  S
5' 6'  S
7' 8'  S
9' 10' 9-then-10
0'' 1'' S
2'' 3'' S
4'' 5'' S
6'' 7'' S
8'' 9'' 9-at-end
10'' X  10-at-start

A standard AES round consists of
state = addRoundKey(state, roundKey);
state = shiftRows(subBytes(state));
nextState = mixColumns(state);
nextRoundKey = getNextRoundKey(roundKey, getRoundConstant())

Round 9 turns off mixColumns,
Round 10 ignores everything except addRoundKey and either compares (state == expected) or changes (roundKey = state), (state = nextText).

Tightly packing means you save a cycle on one-cav and two-cav, but you have more states to reckon with and some more hardware.
in the one-cav 9-then-10 case you need an extra 128-bit XOR to do the comparisons after both rounds - the 10-at-start case can just reuse the XOR from the second empty round.
Having more states => both internal rounds need to store the full amount of data for RCONs, instead of half-data for the first round (because it must be even) and half-data for the second (because it must be odd).
It's a tradeoff for sure. Probably just need to implement both :/

Right now I implelement loose-packing
0: 0 1
1: 2 3
2: 4 5
3: 6 7
4: 8 9
5: 10 x

should be 6 cycles for zerocav, 12 for onecav, 18 for twocav
*/

// The internals of the signature checker calculate a round of AES-128 encryption, generating the next round key on-the-fly.
// AES rounds have the following structure
// 1. state = addRoundKey(state, roundKey)
// 2. state = shiftRows(subBytes(state)) // not enabled on the final round
// 3. state = mixColumns(state) // enabled on most rounds, not the penultimate or final round
// 4. roundKey = getNextRoundKey(roundKey, round) // in parallel with 1, 2, 3
// 5. comparison of state to expectedSig
typedef struct {
    Bit#(3) roundDivTwo; // Should only ever be 0..=10
    AesState state;
    AesState roundKey;
} CalcInput deriving (Bits, FShow);

interface InternalCalc;
    method Bit#(3) newRoundDivTwo; // Returns 0..=6, 6 signifies it's finished
    method AesState newState;
    method AesState newRoundKey;

    // Set the new input for the next cycle
    interface Put#(CalcInput) in;
endinterface

module mkInternalCalc(InternalCalc);
    // Inputs are clocked, outputs are not
    // Use vReg so we only do computations when requested
    Reg#(CalcInput) inReg <- mkVReg;

    Wire#(Bit#(3)) newRoundDivTwo_w <- mkDWire(?);
    Wire#(AesState) newState_w <- mkDWire(?);
    Wire#(AesState) newRoundKey_w <- mkDWire(?);

    rule comp;
        let in = inReg;
        case (in.roundDivTwo)
            0, 1, 2, 3: action
                // round 0, 2, 4, 6
                AesState state = addRoundKey(in.state, in.roundKey);
                state = subBytes(state);
                state = shiftRows(state);
                state = mixColumns(state);
                AesState midRoundKey = getNextRoundKey(in.roundKey, getDynRoundConstant({in.roundDivTwo, 0}));
                // round 1, 3, 5, 7
                state = addRoundKey(state, midRoundKey);
                state = subBytes(state);
                state = shiftRows(state);
                state = mixColumns(state);

                newRoundDivTwo_w <= in.roundDivTwo + 1;
                newState_w <= state;
                newRoundKey_w <= getNextRoundKey(midRoundKey, getDynRoundConstant({in.roundDivTwo, 1}));
            endaction
            4: action
                // round 8
                AesState state = addRoundKey(in.state, in.roundKey);
                state = subBytes(state);
                state = shiftRows(state);
                state = mixColumns(state);
                // round 9
                AesState midRoundKey = getNextRoundKey(in.roundKey, getRoundConstant(8));
                state = addRoundKey(state, midRoundKey);
                state = subBytes(state);
                state = shiftRows(state);
                // no mixcolumns in round 9!

                newRoundDivTwo_w <= in.roundDivTwo + 1;
                newState_w <= state;
                newRoundKey_w <= getNextRoundKey(midRoundKey, getRoundConstant(9));
            endaction
            5: action
                // round 10
                newRoundDivTwo_w <= 6;
                newState_w <= addRoundKey(in.state, in.roundKey);
                newRoundKey_w <= ?;
            endaction
            default: noAction;
        endcase
    endrule

    method Bit#(3) newRoundDivTwo;
        return newRoundDivTwo_w;
    endmethod
    method AesState newState;
        return newState_w;
    endmethod
    method AesState newRoundKey;
        return newRoundKey_w;
    endmethod

    interface in = toPut(asIfc(inReg));
endmodule

function AesState stateOfCap(Cap2024_02 cap);
    return unpack(pack(cap));
endfunction

// TODO this needs an "abort" signal! in case the decoder fails early
module mk2RoundPerCycleCapSigCheck#(Get#(CapSigCheckIn) in, Put#(CapCheckResult#(Bit#(0))) out)(Empty);
    Reg#(Cap2024_02) currentCap <- mkReg(?);
    Reg#(Bit#(128)) currentSig <- mkReg(?);
    Reg#(Bit#(2)) currentCavLevel <- mkReg(?); // 0..=2
    Reg#(Bit#(2)) expectedCavLevel <- mkReg(?); // 0..=2
    InternalCalc calc <- mkInternalCalc;

    Reg#(Bool) fsmWorking <- mkReg(False);

    Stmt backendStmt = seq
        // While this FSM is active, InternalCalc.calc.out has data
        // If calc.newRoundDivTwo = 6, we still loop if currentCavLevel != expectedCavLevel.
        // If calc.newRoundDivTwo != 6 we loop
        while (calc.newRoundDivTwo != 6 || currentCavLevel != expectedCavLevel) seq
            // Wait for the AES to finish
            if (calc.newRoundDivTwo != 6) action
                calc.in.put(CalcInput {
                    roundDivTwo: calc.newRoundDivTwo,
                    state: calc.newState,
                    roundKey: calc.newRoundKey
                });
            endaction else action
                // calc.newRoundDivTwo == 6 && currentCavLevel != expectedCavLevel
                if (currentCavLevel == 0) begin
                    currentCavLevel <= 1;
                    // atCav1(currentCap) must be valid because expectedCavLevel != 0
                    calc.in.put(CalcInput {
                        roundDivTwo: 0,
                        state: stateOfCap(fromMaybe(?, atCav1(currentCap))),
                        roundKey: calc.newState
                    });
                end else begin 
                    // currentCavLevel must be 1.
                    // it can't be 2 because expectedCavLevel maxes out at 2, currentCavLevel != expectedCavLevel, and we don't increment currentCavLevel past expectedCavLevel.
                    currentCavLevel <= 2;
                    // atCav1(currentCap) must be valid because expectedCavLevel != 0
                    calc.in.put(CalcInput {
                        roundDivTwo: 0,
                        state: stateOfCap(currentCap),
                        roundKey: calc.newState
                    });
                end
            endaction
        endseq

        action
            // calc.newRound = 6 && currentCavLevel == expectedCavLevel
            // => we need to check that the signatures match
            if (pack(calc.newState) == currentSig)
                out.put(tagged Succ 0);
            else
                out.put(tagged Fail InvalidSignature);
            // Do another one on the next cycle
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
            roundDivTwo: 0,
            state: stateOfCap(atRoot(i.cap)),
            roundKey: unpack(i.secret)
        });
        fsmWorking <= True;
        backendFSM.start();
    endrule
endmodule

endpackage
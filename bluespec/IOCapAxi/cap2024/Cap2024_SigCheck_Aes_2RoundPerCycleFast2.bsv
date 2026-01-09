package Cap2024_SigCheck_Aes_2RoundPerCycleFast2;

import AesLib::*;
import StmtFSM::*;
import GetPut::*;
import ClientServer::*;
import Cap2024::*;
import Cap2024_11::*;
import FIFO::*;
import Util1::*;
import Util2::*;
import IOCapAxi_Konata::*;

export mk2RoundPerCycleCapSigCheckFast2;

/* AES-128 computation structure

AES-128 has 6 cycles in a one-cycle-per-round, for two cycles there are a few possibilities

Tightly packed:

Zero-cav
0 1    S
2 3    S
4 5    S
6 7    S
8 9 10 End (10 is XORs, a bit of shuffling, with a round key, which should be cheap?)

One-cav and two-cav then simply repeat

A standard AES round consists of
state = addRoundKey(state, roundKey);
state = shiftRows(subBytes(state));
nextState = mixColumns(state);
nextRoundKey = getNextRoundKey(roundKey, getRoundConstant())

Round 9 turns off mixColumns,
Round 10 ignores everything except addRoundKey and either compares (state == expected) or changes (roundKey = state), (state = nextText).

This implements tight packing, should be 5 cycles for zerocav, 10 for onecav, 15 for twocav
*/
typedef union tagged {
    struct {
        AesState state;
        AesState roundKey;
        Bit#(3) roundDivTwo;
    } Standard; // S
    // struct {
    //     AesState state;
    //     AesState roundKey;
    // } End; // 10 X
} FsmState deriving (Bits, FShow, Eq);

module mk2RoundPerCycleCapSigCheckFast2#(ReadOnly#(Maybe#(CapSigCheckIn#(Cap2024_11))) in, WriteOnly#(CapCheckResult#(Bit#(0))) out)(Empty);
    Reg#(Cap2024_11) currentCap <- mkReg(?);
    Reg#(Bit#(128)) currentSig <- mkReg(?);
    Reg#(Bit#(2)) currentCavLevel <- mkReg(?); // 0..=2
    Reg#(Bit#(2)) expectedCavLevel <- mkReg(?); // 0..=2
    
    Reg#(Bool) fsmWorking <- mkReg(False);
    Reg#(FsmState) fsmState <- mkReg(?);

    rule process_item(fsmWorking);
        case (fsmState) matches
            tagged Standard { state: .inState, roundKey: .inRoundKey, roundDivTwo: .inRoundDivTwo }: action
                konataEvent(KONATA_OFF, "Aes2Round", fshow(inRoundDivTwo));

                // round 0, 2, 4, 6, 8
                AesState state = addRoundKey(inState, inRoundKey);
                state = subBytes(state);
                state = shiftRows(state);
                state = mixColumns(state);
                AesState midRoundKey = getNextRoundKey(inRoundKey, getDynRoundConstant({inRoundDivTwo, 0}));
                // round 1, 3, 5, 7, 9
                state = addRoundKey(state, midRoundKey);
                state = subBytes(state);
                state = shiftRows(state);

                if (inRoundDivTwo <= 3) begin
                    // round 1, 3, 5, 7 include mixColumns
                    state = mixColumns(state);

                    fsmState <= tagged Standard {
                        state: state,
                        roundKey: getNextRoundKey(midRoundKey, getDynRoundConstant({inRoundDivTwo, 1})),
                        roundDivTwo: inRoundDivTwo + 1
                    };
                end else begin
                    // no mixcolumns in round 9!

                    // round 10
                    // This getNextRoundKey is equivalent in terms of timing to the one up above,
                    // addRoundKey is an XOR
                    // So evaluating this should be marginally more expensive in terms of timing than the normal 0-7 rounds
                    // The main extra expense in timing should be the (X == Y) check we do down below.
                    AesState finalState = addRoundKey(state, getNextRoundKey(midRoundKey, getRoundConstant(9)));

                    if (currentCavLevel != expectedCavLevel) begin
                        // calc.newRoundDivTwo == 6 && currentCavLevel != expectedCavLevel
                        if (currentCavLevel == 0) begin
                            currentCavLevel <= 1;
                            // atCav1(currentCap) must be valid because expectedCavLevel != 0
                            fsmState <= tagged Standard {
                                state: stateOfCap(fromMaybe(?, atCav1(currentCap))),
                                roundKey: finalState,
                                roundDivTwo: 0
                            };
                        end else begin 
                            // currentCavLevel must be 1.
                            // it can't be 2 because expectedCavLevel maxes out at 2, currentCavLevel != expectedCavLevel, and we don't increment currentCavLevel past expectedCavLevel.
                            currentCavLevel <= 2;
                            // atCav1(currentCap) must be valid because expectedCavLevel != 0
                            fsmState <= tagged Standard {
                                state: stateOfCap(currentCap),
                                roundKey: finalState,
                                roundDivTwo: 0
                            };
                        end
                    end else begin
                        konataEvent(KONATA_OFF, "AesEnd", fshow(pack(finalState) == currentSig));
                        // calc.newRound = 6 && currentCavLevel == expectedCavLevel
                        // => we need to check that the signatures match
                        if (pack(finalState) == currentSig)
                            out <= (tagged Succ 0);
                        else
                            out <= (tagged Fail InvalidSignature);
                        // Do another one on the next cycle
                        // TODO this is inefficient - could be doing fetch on this cycle, BUT.
                        // we aren't going to get new things in that quickly!
                        fsmWorking <= False;
                    end
                end
            endaction
            default: noAction;
        endcase
    endrule

    // TODO continuous assert that currentCavLevel is never > expectedCavLevel.

    rule fetch if (!fsmWorking && isValid(in));
        let i = fromMaybe(?, in);

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
        // calc.in.put(CalcInput {
        //     roundDivTwo: 0,
        //     state: stateOfCap(atRoot(i.cap)),
        //     roundKey: unpack(i.secret)
        // });
        konataEvent(KONATA_OFF, "AesStart", fshow(""));
        fsmState <= tagged Standard {
            roundDivTwo: 0,
            state: stateOfCap(atRoot(i.cap)),
            roundKey: unpack(i.secret)
        };
        fsmWorking <= True;
    endrule
endmodule

endpackage
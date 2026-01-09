package Cap2024_SigCheck_Aes_1RoundPerCycleFast2;

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

export mk1RoundPerCycleCapSigCheckFast2;

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

*/

typedef union tagged {
    struct {
        AesState state;
        AesState roundKey;
        Bit#(4) round;
    } Standard; // S
    // struct {
    //     AesState state;
    //     AesState roundKey;
    // } End; // 10 X
} FsmState deriving (Bits, FShow, Eq);

module mk1RoundPerCycleCapSigCheckFast2#(ReadOnly#(Maybe#(CapSigCheckIn#(Cap2024_11))) in, WriteOnly#(CapCheckResult#(Bit#(0))) out)(Empty);
    Reg#(Cap2024_11) currentCap <- mkReg(?);
    Reg#(Bit#(128)) currentSig <- mkReg(?);
    Reg#(Bit#(2)) currentCavLevel <- mkReg(?); // 0..=2
    Reg#(Bit#(2)) expectedCavLevel <- mkReg(?); // 0..=2
    
    Reg#(Bool) fsmWorking <- mkReg(False);
    Reg#(FsmState) fsmState <- mkReg(?);

    rule process_item(fsmWorking);
        case (fsmState) matches
            tagged Standard { state: .inState, roundKey: .inRoundKey, round: .inRound }: action
                konataEvent(KONATA_OFF, "Aes1Round", fshow(inRound));

                // round 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                AesState state = addRoundKey(inState, inRoundKey);
                state = subBytes(state);
                state = shiftRows(state);
                AesState midRoundKey = getNextRoundKey(inRoundKey, getDynRoundConstant(inRound));


                if (inRound <= 8) begin
                    // round 0-8 include mixColumns
                    state = mixColumns(state);

                    fsmState <= tagged Standard {
                        state: state,
                        roundKey: midRoundKey,
                        round: inRound + 1
                    };
                end else begin
                    // no mixcolumns in round 9!

                    // include round 10
                    // addRoundKey is an XOR, should be ~same cost as mixColumns.
                    // The main extra expense in timing should be the (X == Y) check we do down below.
                    AesState finalState = addRoundKey(state, midRoundKey);

                    if (currentCavLevel != expectedCavLevel) begin
                        // calc.newRoundDivTwo == 6 && currentCavLevel != expectedCavLevel
                        if (currentCavLevel == 0) begin
                            currentCavLevel <= 1;
                            // atCav1(currentCap) must be valid because expectedCavLevel != 0
                            fsmState <= tagged Standard {
                                state: stateOfCap(fromMaybe(?, atCav1(currentCap))),
                                roundKey: finalState,
                                round: 0
                            };
                        end else begin 
                            // currentCavLevel must be 1.
                            // it can't be 2 because expectedCavLevel maxes out at 2, currentCavLevel != expectedCavLevel, and we don't increment currentCavLevel past expectedCavLevel.
                            currentCavLevel <= 2;
                            // atCav1(currentCap) must be valid because expectedCavLevel != 0
                            fsmState <= tagged Standard {
                                state: stateOfCap(currentCap),
                                roundKey: finalState,
                                round: 0
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
        //     round: 0,
        //     state: stateOfCap(atRoot(i.cap)),
        //     roundKey: unpack(i.secret)
        // });
        konataEvent(KONATA_OFF, "AesStart", fshow(""));
        fsmState <= tagged Standard {
            round: 0,
            state: stateOfCap(atRoot(i.cap)),
            roundKey: unpack(i.secret)
        };
        fsmWorking <= True;
    endrule
endmodule

endpackage
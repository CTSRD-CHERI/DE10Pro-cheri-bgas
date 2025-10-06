/// This module provides a mkMapFifo#(T)(depth, f :: T -> T) module, implemented as a vector of registers with a specific depth

import SourceSink :: *;
import Vector :: *;
import BlueBasics :: *;

interface MapFIFO#(type t);
    interface Sink#(t) enq;
    interface Source#(t) deq;
endinterface

module mkSizedMapFIFO#(NumProxy#(depth) proxy, function t mapItem(t item))(MapFIFO#(t)) provisos (
    Bits#(t, __a)
);
    Vector#(depth, Reg#(t)) regs <- replicateM(mkReg(?));
    // One-hot encoding of the first empty register. Bit #0 is set => regs[0] is empty.
    // If the top bit is high, the FIFO is empty.
    Reg#(Bit#(TAdd#(depth, 1))) oneHotEnqPtr <- mkReg(zeroExtend(1'b1));

    RWire#(t) enqWire <- mkRWire;
    PulseWire deqWire <- mkPulseWire;

    function t mapItemReg(Reg#(t) itemReg) = mapItem(itemReg._read());

    rule tickFifo;
        Vector#(depth, t) mappedItems = map(mapItemReg, regs);
        let deq = deqWire;
        let enq = enqWire.wget();

        // Compute the new oneHotEnqPtr
        let oldOneHotEnqPtr = oneHotEnqPtr;
        let newOneHotEnqPtr = oldOneHotEnqPtr;
        case (tuple2(enq, deq)) matches
            // no-push-no-pop and push-pop both don't move the pointer
            { tagged Invalid,    False } : begin end
            { tagged Valid .val,  True } : begin end

            // Push, no pop
            { tagged Valid .val, False } : begin
                // assert oneHotEnqPtr[valueOf(depth)] == 1'b0;
                newOneHotEnqPtr = oldOneHotEnqPtr << 1;
            end
            // Pop, no push
            { tagged Invalid,     True } : begin
                // assert oneHotEnqPtr[0] == 1'b0;
                newOneHotEnqPtr = oldOneHotEnqPtr >> 1;
            end
        endcase
        oneHotEnqPtr <= newOneHotEnqPtr;

        // Compute the new register values
        // If we're enqueueing and not dequeueing, the register at the old onehotpointer takes in mapItem(enq value)
        // If we're enqueueing and dequeueing, the register at the (onehotpointer >> 1) takes in mapItem(enq value)
            // this is equivalent to saying if we're enqueueing at all then the register at (newOneHotEnqPtr >> 1) takes in mapItem(enq value)
        // All other cases have the registers either take in their own index in mappedItems (if not dequeueing)
        // or the will take index+1 from mappedItems
        //
        // So for each register i
        // if enq && i == newonehotpointer => mapItem(enq)
        //    even if (enq && deq) the position at which to enqueue is the same
        // else if deq => mappedItems[i + 1]
        // else mappedItems[i]
        
        for (Integer i = 0; i < valueOf(depth); i = i + 1) begin
            t newItem = ?;
            if (enq matches tagged Valid .toEnq &&& newOneHotEnqPtr[i + 1] == 1) begin
                newItem = mapItem(toEnq);
            end else if (deq) begin
                if (i == valueOf(depth) - 1) begin
                    // we are dequeueing and not enqueueing,
                    // the final register doesn't have any new values to pull in so just keep it constant.
                    // The pointer is moving no matter what, so the value in this register doesn't matter.
                end else begin
                    newItem = mappedItems[i + 1];
                end
            end else begin
                // Nothing is moving, but we always apply the map() function
                newItem = mappedItems[i];
            end
            regs[i] <= newItem;
        end
    endrule

    interface enq = interface Sink;
        method canPut = (oneHotEnqPtr[valueOf(depth)] == 1'b0);
        method Action put(t item) if (oneHotEnqPtr[valueOf(depth)] == 1'b0) = action
            enqWire.wset(item);
        endaction;
    endinterface;

    interface deq = interface Source;
        method canPeek = (oneHotEnqPtr[0] == 1'b0);
        // TODO should we apply the map function here
        method peek if (oneHotEnqPtr[0] == 1'b0) = mapItem(regs[0]);
        method drop if (oneHotEnqPtr[0] == 1'b0) = deqWire.send();
    endinterface;
endmodule
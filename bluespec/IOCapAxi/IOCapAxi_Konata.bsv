import IOCapAxi_Flits :: *;
import SourceSink :: *;

export KFlitId;
export KonataMode(..);
export konataEvent;
export konataFlit;
export mkIOCapAxiFlitLabeller;

typedef struct {
`ifdef KANATA
    UInt#(63) sequentialId; 
    Bool isRead;
`else
    Bit#(0) data;
`endif
} KFlitId deriving (Bits, Eq);

function Fmt fshowAsUint(Bit#(n) x);
    UInt#(n) u = unpack(pack(x));
    return $format("%d", u); //fshow(u);
endfunction

instance FShow#(KFlitId);
    function Fmt fshow(KFlitId value) = fshowAsUint(pack(value));
endinstance

typedef enum {
    KONATA_OFF
`ifdef KANATA
    , KONATA_FLIT
    , KONATA_KEY
`endif
} KonataMode deriving (Bits, Eq, FShow);

function Action konataEvent(KonataMode kMode, String eventType, Fmt eventData) = action
`ifdef KANATA
    if (kMode != KONATA_OFF) begin
        $display("V\t", fshow(eventType), "\t", eventData);
    end
`endif
endaction;

function Action konataFlit(KonataMode kMode, Fmt data) = action
`ifdef KANATA
    if (kMode == KONATA_FLIT) begin
        $display(data);
    end
`endif
endaction;

// Wrap a Sink#((iocap_flit, KFlitId)) with something that takes in iocap_flit, figures out the KFlitId and attaches it.
module mkIOCapAxiFlitLabeller#(
    KonataMode kMode,
    Sink#(Tuple2#(iocap_flit, KFlitId)) sink,
    ReadOnly#(UInt#(n)) threadId,
    no_iocap_flit flitProxy
)(Sink#(iocap_flit)) provisos (
    IOCapPackableFlit#(iocap_flit, no_iocap_flit),
    AxiCtrlFlit64#(iocap_flit)
);
`ifdef KANATA
    Reg#(UInt#(63)) sequentialId <- mkReg(0);
    PulseWire incrementId <- mkPulseWire;

    rule checkId;
        if (incrementId) begin
            sequentialId <= sequentialId + 1;
        end
    endrule

    method Bool canPut = sink.canPut();
    method Action put(iocap_flit f);
        let flitId = KFlitId {
            sequentialId: sequentialId,
            isRead: isBurstRead(f)
        };
        // On the final flit, bump the ID
        IOCapFlitSpec#(no_iocap_flit) spec = unpackSpec(f);
        if (spec matches tagged CapBits3 .*) begin
            incrementId.send();
        end else if (spec matches tagged Start .*) begin
            konataFlit(kMode,
                $format("I\t") + fshow(flitId) + fshow("\t") + fshowAsUint(burstTid(f)) + fshow("\t") + fshow(threadId)
            );
            konataFlit(kMode,
                $format("L\t") + fshow(flitId) + $format("\t0\t") + fshow(burstAddr(f)) + $format(" %s#", (flitId.isRead ? "R" : "W")) + fshowAsUint(burstTid(f))
            );
        end
        sink.put(tuple2(f, flitId));
    endmethod
`else
    method Bool canPut = sink.canPut();
    method Action put(iocap_flit f);
        sink.put(tuple2(f, ?));
    endmethod
`endif
endmodule
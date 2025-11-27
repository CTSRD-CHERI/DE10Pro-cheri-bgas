import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;
import LeftShift :: *;
import IOCapAxi_ErrorUnit :: *;
import IOCapAxi_Types :: *;
import IOCapAxi_KeyManager2_Types :: *;
import IOCapAxi_KeyManager2_KeyStatePipe :: *;
import IOCapAxi_KeyManager2_KeyDataPipe :: *;
import IOCapAxi_Konata :: *;
import SamUtil :: *;
import MapFIFO :: *;

interface IOCapAxi_KeyManager2_MMIO_PerfCounterIfc;
    (* always_enabled *)
    method Action bumpPerfCounterGood();
    (* always_enabled *)
    method Action bumpPerfCounterBad();
endinterface

interface IOCapAxi_KeyManager2_MMIO_PerfCounters#(numeric type n_checkers);
    interface Vector#(n_checkers, IOCapAxi_KeyManager2_MMIO_PerfCounterIfc) read; 
    interface Vector#(n_checkers, IOCapAxi_KeyManager2_MMIO_PerfCounterIfc) write; 
endinterface

// Memory map:
// [0x0, 0x10, 0x20, 0x30, 0x40... 0x1000) = read/write key status
// [0x1000, 0x1010, 0x1020... 0x2000)      = write key values
// [0x1000, 0x1008, 0x1010, 0x1018]        = read performance counters
//                                           - good write
//                                           - bad write
//                                           - good read
//                                           - bad read
interface IOCapAxi_KeyManager2_MMIO#(type t_data, numeric type n_checkers);
    interface AXI4Lite_Slave#(TLog#('h2000), t_data, 0, 0, 0, 0, 0) hostFacingSlave;

    interface IOCapAxi_KeyManager2_MMIO_PerfCounters#(n_checkers) valvePerfCounters;

    // TODO make ReadOnly
    interface Vector#(n_checkers, RWire#(KeyId)) checkerKillKeyMessages;

    interface ReadOnly#(Maybe#(KeyId)) debugEnableKey;
    interface ReadOnly#(Maybe#(KeyId)) debugKillKey;
    interface ReadOnly#(UInt#(64)) debugGoodWrite;
    interface ReadOnly#(UInt#(64)) debugBadWrite;
    interface ReadOnly#(UInt#(64)) debugGoodRead;
    interface ReadOnly#(UInt#(64)) debugBadRead;
endinterface

module mkAXI4LiteShimCustomFF (AXI4Lite_Shim#(a, b, c, d, e, f, g));
  NumProxy#(4) n = ?;
  let awff <- mkSizedIdMapFIFO(n);
  let  wff <- mkSizedIdMapFIFO(n);
  let  bff <- mkSizedIdMapFIFO(n);
  let arff <- mkSizedIdMapFIFO(n);
  let  rff <- mkSizedIdMapFIFO(n);

//   rule disp;
//     $display("shim aw ", fshow(awff.occupancy), " w ", fshow(wff.occupancy));
//   endrule

  method clear = action
    awff.clear;
    wff.clear;
    bff.clear;
    arff.clear;
    rff.clear;
  endaction;
  interface master = interface AXI4Lite_Master;
    interface aw = toSource(awff);
    interface  w = toSource(wff);
    interface  b = toSink(bff);
    interface ar = toSource(arff);
    interface  r = toSink(rff);
  endinterface;
  interface slave = interface AXI4Lite_Slave;
    interface aw = toSink(awff);
    interface  w = toSink(wff);
    interface  b = toSource(bff);
    interface ar = toSink(arff);
    interface  r = toSource(rff);
  endinterface;
endmodule

module mkIOCapAxi_KeyManager2_MMIO#(KonataMode kMode, IOCapAxi_KeyManager2_KeyStatePipe_MMIOIfc keyState, IOCapAxi_KeyManager2_KeyDataPipe_MMIOIfc keyData, KeyManager2ErrorUnit error)(IOCapAxi_KeyManager2_MMIO#(t_data, n_checkers))  provisos (
    // t_data must be divisible by 8
    // i.e. (t_data/8) * 8 == t_data
    Mul#(TDiv#(t_data, 8), 8, t_data),
    // t_data must be smaller than or equal to 128 - the size of a key
    Add#(t_data, a__, 128),
    // t_data must be smaller than or equal to 64 - the size of a performance counter
    Add#(t_data, b__, 64),
    // Same thing for t_data/8 - ugh, why can't this be proven implicitly
    Add#(TDiv#(t_data, 8), c__, 16),
    // t_data must be greater than or equal to 2 - the size of keyStatus
    Add#(2, d__, t_data)
);
    let axiShim <- mkAXI4LiteShimFF;
    // mkAXI4LiteShimPipelineFF; // mkAXI4LiteShimSizedFIFOF4; // mkAXI4LiteShimBypassFIFOF; // mkAXI4LiteShimFF;

    Vector#(n_checkers, PulseWire) reqGoodWrite <- replicateM(mkPulseWire);
    Vector#(n_checkers, PulseWire) reqBadWrite <- replicateM(mkPulseWire);
    Vector#(n_checkers, PulseWire) reqGoodRead <- replicateM(mkPulseWire);
    Vector#(n_checkers, PulseWire) reqBadRead <- replicateM(mkPulseWire);
    Reg#(UInt#(64)) goodWrite <- mkReg(0);
    Reg#(UInt#(64)) badWrite <- mkReg(0);
    Reg#(UInt#(64)) goodRead <- mkReg(0);
    Reg#(UInt#(64)) badRead <- mkReg(0);

    // killKey RWire is on a long path.
    // break up that path with a single-cycle delay between the internal kill-key signal
    // and the external one, via a Reg
    RWire#(KeyId) killKeyInternal <- mkRWire;
    Reg#(Maybe#(KeyId)) killKeyNextCycle <- mkReg(tagged Invalid);
    RWire#(KeyId) killKeyExternal <- mkRWire;
    let killKeyReadOnly <- mkRwireToReadOnlyDirect(killKeyExternal);

    // Just used for debug
    RWire#(KeyId) enableKeyInternal <- mkRWire;
    let enableKeyReadOnly <- mkRwireToReadOnlyDirect(enableKeyInternal);

    function UInt#(64) nPulsedWires(Vector#(n_checkers, PulseWire) wires);
        UInt#(64) n = 0;
        for (Integer i = 0; i < valueOf(n_checkers); i = i + 1)
            if (wires[i]) begin
                n = n + 1;
            end
        return n;
    endfunction

    rule update_perf_counters;
        let nGoodWrite = nPulsedWires(reqGoodWrite);
        let nBadWrite  = nPulsedWires(reqBadWrite);
        let nGoodRead  = nPulsedWires(reqGoodRead);
        let nBadRead   = nPulsedWires(reqBadRead);

        if (nGoodWrite != 0) begin
            goodWrite <= goodWrite + nGoodWrite;
            $display("// IOCap stats - good writes %d", (goodWrite + nGoodWrite));
        end
        if (nBadWrite != 0) begin
            badWrite <= badWrite + nBadWrite;
            $display("// IOCap stats - bad writes %d", (badWrite + nBadWrite));
        end
        if (nGoodRead != 0) begin
            goodRead <= goodRead + nGoodRead;
            $display("// IOCap stats - good reads %d", (goodRead + nGoodRead));
        end
        if (nBadRead != 0) begin
            badRead <= badRead + nBadRead;
            $display("// IOCap stats - bad reads %d", (badRead + nBadRead));
        end
    endrule

    // Reads are purely for status, they can return immediately
    rule handle_read;
        let ar <- get (axiShim.master.ar);
        
        let response = tagged Invalid;

        // Can only read the status area - [0x0, 0x1000)
        if ((ar.araddr & 'h1000) == 0) begin
            KeyId k = ar.araddr[11:4]; // Memory map is byte-addressed, each secret key is 16 bytes = 4 address bits

            response = tagged Valid (zeroExtend(pack(keyState.keyStatus(k))));
        end else if (ar.araddr < 'h1020) begin
            // We're between [0x1000 and 0x1020)
            // Read a performance counter
            
            // Each perf counter is 64-bits
            Bit#(2) perfId = ar.araddr[4:3];
            Bit#(3) startByteWithinCounter = ar.araddr[2:0];
            Bit#(4) endByteWithinCounter = zeroExtend(startByteWithinCounter) + fromInteger(valueOf(t_data) / 8);

            if (
                // Reads can't overlap two counters
                endByteWithinCounter <= 8
            ) begin
                Bit#(64) counter = ?;
                case (perfId) matches
                    2'b00 : counter = pack(goodWrite);
                    2'b01 : counter = pack(badWrite);
                    2'b10 : counter = pack(goodRead);
                    2'b11 : counter = pack(badRead);
                endcase
                
                // TODO need a comb_right_shift lol
                Bit#(t_data) contents = ?;
                case (startByteWithinCounter) matches
                    0 : contents = truncate(counter >> 0);
                    1 : contents = truncate(counter >> 8);
                    2 : contents = truncate(counter >> 16);
                    3 : contents = truncate(counter >> 24);
                    4 : contents = truncate(counter >> 32);
                    5 : contents = truncate(counter >> 40);
                    6 : contents = truncate(counter >> 48);
                    7 : contents = truncate(counter >> 56);
                endcase

                response = tagged Valid truncate(contents);
            end
        end else begin
            error.assertError(tagged InvalidRead);
        end

        let flit = ?;
        case (response) matches
            tagged Valid .rdata : begin
                flit = AXI4Lite_RFlit {
                      rdata: rdata
                    , rresp: OKAY // Read was valid
                    , ruser: ?
                };
            end
            tagged Invalid : begin
                flit = AXI4Lite_RFlit {
                      rdata: 0
                    , rresp: SLVERR // Read was to invalid, write-only location
                    , ruser: ?
                };
            end
        endcase
        konataEvent(kMode, "MmioRd", fshow(ar) + $format(" - ") + fshow(response));
        axiShim.master.r.put(flit);
    endrule

    rule handle_write if (keyData.ready);
        let aw <- get (axiShim.master.aw);
        let w <- get (axiShim.master.w);

        let validWrite = False;

        // Writes to [0x0, 0x1000) set status
        if ((aw.awaddr & 'h1000) == 0) begin
            KeyId k = aw.awaddr[11:4]; // Memory map is byte-addressed, each secret key is 16 bytes = 4 address bits
            Bit#(4) startByteWithinKey = aw.awaddr[3:0];

            if (
                // Valid writes must write to the first byte of the key.
                (startByteWithinKey == 0)
                // That first byte must be enabled,
                && (w.wstrb[0] == 1)
                // and the first byte must only ever be 0 or 1,
                && (w.wdata[7:1] == 0)
                // and all the other enabled bytes must be zero.
                && (((w.wdata & beToMask(w.wstrb)) >> 8) == 0)
            ) begin
                // We're either trying to write 0 (invalid) or 1 (valid)
                if (w.wdata[0] == 0) begin
                    validWrite <- keyData.tryRevokeAndClearKey(k);
                    if (validWrite) begin
                        killKeyInternal.wset(k);
                    end
                    // TODO error for this if it fails
                end else begin
                    validWrite <- keyState.tryEnableKey(k);
                    if (validWrite) begin
                        enableKeyInternal.wset(k);
                    end
                    // TODO error for this if it fails
                end
            end else begin
                error.assertError(tagged InvalidStatusWrite);
            end

        // Writes to [0x1000, 0x2000) write to key (requires the key is revoked)
        end else begin
            KeyId k = aw.awaddr[11:4]; // Memory map is byte-addressed, each secret key is 16 bytes = 4 address bits
            Bit#(4) startByteWithinKey = aw.awaddr[3:0];
            Bit#(5) endByteWithinKey = zeroExtend(startByteWithinKey) + fromInteger(valueOf(t_data) / 8);
    
            if (
                // Writes to key data can't overlap two keys
                endByteWithinKey <= 16
                // The given key must be invalid without being in a revocation (checked below)
            ) begin
                // Move wstrb and wdata into the 128-bit space based on their offset within the key.

                // wstrb = TDiv#(t_data, 8) bits long
                // We've just checked that startByteWithinKey + TDiv#(t_data, 8) <= 16
                // => the top bit of wstrb will only ever go up to the top bit of Bit#(16), and won't be shifted out.
                Bit#(16) bramByteEnable = left_shift_comb(zeroExtend(w.wstrb), unpack(startByteWithinKey));
                // wdata = t_data bits long
                // We've just checked that startByteWithinKey + TDiv#(t_data, 8) <= 16
                // => (startByteWithinKey + t_data/8) * 8 <= 128
                // => (startByteWithinKey * 8) + t_data <= 128
                // => no bits will be shifted out
                Bit#(128) bramWriteData = left_shift_comb(zeroExtend(w.wdata), unpack({startByteWithinKey, 3'b0}));

                // keys.portA.request.put(BRAMRequestBE {
                //     writeen: bramByteEnable,
                //     // Don't send a write-response, we pipe responses 1:1 out into keyResponses
                //     responseOnWrite: False,
                //     address: k,
                //     datain: bramWriteData
                // });
                
                validWrite <- keyData.tryWriteKey(k, bramWriteData, bramByteEnable);
                // TODO error for this if it fails

                // TODO improve display
                $display("IOCap - BRAM write - address ", fshow(k), " - datain ", fshow(w.wdata));
            end else begin
                error.assertError(tagged InvalidDataWrite);
            end
        end

        let flit = ?;
        if (validWrite) begin
            flit = AXI4Lite_BFlit {
                  bresp: OKAY
                , buser: ?
            };
        end else begin
            flit = AXI4Lite_BFlit {
                  bresp: SLVERR
                , buser: ?
            };
        end
        konataEvent(kMode, "MmioWr", fshow(aw) + $format(" - ") + fshow(w) + $format(" - ") + fshow(validWrite));
        axiShim.master.b.put(flit);
    endrule

    rule send_kill_key;
        if (killKeyNextCycle matches tagged Valid .key) begin
            killKeyExternal.wset(key);
        end
        killKeyNextCycle <= killKeyInternal.wget();
    endrule

    // Helper functions for generating IOCapAxi_KeyManager2_MMIO_PerfCounterIfc for different permutations of (read/write, index)
    function IOCapAxi_KeyManager2_MMIO_PerfCounterIfc makeReadPerfCounter(Integer idx) = interface IOCapAxi_KeyManager2_MMIO_PerfCounterIfc;
        method Action bumpPerfCounterGood() = reqGoodRead[idx].send();
        method Action bumpPerfCounterBad() = reqBadRead[idx].send();
    endinterface;

    function IOCapAxi_KeyManager2_MMIO_PerfCounterIfc makeWritePerfCounter(Integer idx) = interface IOCapAxi_KeyManager2_MMIO_PerfCounterIfc;
        method Action bumpPerfCounterGood() = reqGoodWrite[idx].send();
        method Action bumpPerfCounterBad() = reqBadWrite[idx].send();
    endinterface;

    interface hostFacingSlave = axiShim.slave;

    interface valvePerfCounters = interface IOCapAxi_KeyManager2_MMIO_PerfCounters;
        interface read = genWith(makeReadPerfCounter);
        interface write = genWith(makeWritePerfCounter);
    endinterface;

    interface checkerKillKeyMessages = replicate(killKeyExternal);
    interface debugEnableKey = enableKeyReadOnly;
    interface debugKillKey = killKeyReadOnly;
    interface debugGoodWrite = regToReadOnly(goodWrite);
    interface debugBadWrite = regToReadOnly(badWrite);
    interface debugGoodRead = regToReadOnly(goodRead);
    interface debugBadRead = regToReadOnly(badRead);
endmodule
import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;

typedef UInt#(1) Epoch;
typedef Bit#(128) Key;
// 4096 keys => 12 bit ID
typedef Bit#(12) KeyId;

interface IOCap_KeyManager#(numeric type t_data);
    method Action bumpPerfCounterGoodWrite();
    method Action bumpPerfCounterBadWrite();
    method Action bumpPerfCounterGoodRead();
    method Action bumpPerfCounterBadRead();

    interface Sink#(KeyId) keyRequests;
    interface Source#(Tuple2#(KeyId, Maybe#(Key))) keyResponses;

    interface Source#(Epoch) newEpochRequests;
    interface Sink#(Epoch) finishedEpochs;

    interface AXI4Lite_Slave#(13 /* TLog#('h2000) */, t_data, 0, 0, 0, 0, 0) hostFacingSlave;
endinterface

module mkSimpleIOCapKeyManager(IOCap_KeyManager#(t_data)) provisos (
    // t_data must be divisible by 8
    // i.e. (t_data/8) * 8 == t_data
    Mul#(TDiv#(t_data, 8), 8, t_data),
    // t_data must be smaller than or equal to 128 - the size of a key
    Add#(t_data, a__, 128),
    // Same thing for t_data/8 - ugh, why can't this be proven implicitly
    Add#(TDiv#(t_data, 8), b__, 16)
);
    // Memory map:
    // [0x0, 0x10, 0x20, 0x30, 0x40... 0x1000) = read/write key status
    // [0x1000, 0x1010, 0x1020... 0x2000)      = write key values

    // Need a BRAM with key data
    // Set up the secret BRAM
    BRAM_Configure keysConfig = BRAM_Configure {
        memorySize: 0, // Inferred from the KeyId parameter to BRAM1Port below
        latency: 2, // (address is registered, data is too because this isn't latency sensitive)
        loadFormat: None, // tagged Hex "exhibition_validallcavs_secrets.memh",
        outFIFODepth: 3, // latency+2
        allowWriteResponseBypass: False // TODO check if this is fine
    };
    // TODO second BRAM port for clearing keys?
    // Addressed by KeyId
    // Holds items of type Key
    // 16 byte-enable wires
    BRAM1PortBE#(KeyId, Key, 16) keys <- mkBRAM1ServerBE(keysConfig);
    // Need a Vector of 4096 KeyStates
    Vector#(4096, Reg#(Bool)) keyValid <- replicateM(mkReg(False));

    PulseWire reqGoodWrite <- mkPulseWire;
    Reg#(UInt#(64)) goodWrite <- mkReg(0);
    PulseWire reqBadWrite <- mkPulseWire;
    Reg#(UInt#(64)) badWrite <- mkReg(0);
    PulseWire reqGoodRead <- mkPulseWire;
    Reg#(UInt#(64)) goodRead <- mkReg(0);
    PulseWire reqBadRead <- mkPulseWire;
    Reg#(UInt#(64)) badRead <- mkReg(0);

    rule update_perf_counters;
        if (reqGoodWrite) begin
            goodWrite <= goodWrite + 1;
            $display("IOCap stats - good writes %d", (goodWrite + 1));
        end
        if (reqBadWrite) begin
            badWrite <= badWrite + 1;
            $display("IOCap stats - bad writes %d", (badWrite + 1));
        end
        if (reqGoodRead) begin
            goodRead <= goodRead + 1;
            $display("IOCap stats - good reads %d", (goodRead + 1));
        end
        if (reqBadRead) begin
            badRead <= badRead + 1;
            $display("IOCap stats - bad reads %d", (badRead + 1));
        end
    endrule

    let axiShim <- mkAXI4LiteShimFF;

    Reg#(Epoch) currentEpoch <- mkReg(0);
    // Used for reading status to distinguish "has finished invalidation" for a given key from "currently waiting to finish invalidation"
    Reg#(Maybe#(KeyId)) waitingForEpochToInvalidate <- mkReg(tagged Invalid);

    rule sanity_check;
        if (waitingForEpochToInvalidate matches tagged Valid .keyToInvalidate) begin
            if (keyValid[keyToInvalidate]) begin
                // TODO use errorunit
                $error("Key marked as valid while we're trying to invalidate it");
            end
        end
    endrule

    let newEpochRequest <- mkFIFOF;
    let epochCompleteResponse <- mkFIFOF;

    // Reads are purely for status, they can return immediately
    rule handle_read;
        let ar <- get (axiShim.master.ar);
        
        let response = tagged Invalid;

        // Can only read the status area - [0x0, 0x1000)
        if ((ar.araddr & 'h1000) == 0) begin
            KeyId k = truncate(ar.araddr >> 4); // Memory map is byte-addressed, each secret key is 16 bytes = 4 address bits

            // The status of a key is 
            // 0 = invalid, revoked
            // 1 = valid
            // 2 = invalid, waiting for revocation to finish

            let keyStatus = 0;
            if (keyValid[k]) begin
                keyStatus = 1;
            end
            if (waitingForEpochToInvalidate matches tagged Valid .keyToInvalidate &&& keyToInvalidate == k) begin
                keyStatus = 2;
            end

            response = tagged Valid keyStatus;
        end else begin
            // TODO signal failure
            $error("IOCap - mkSimpleIOCapKeyManager - Read to invalid address");
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
        $display("IOCap - key manager - handle_read - ", fshow(ar), " - ", fshow(response));
        axiShim.master.r.put(flit);
    endrule

    // For simplicity, writes can only happen while we aren't transitioning epochs.
    // A write may be handled and return immediately even if it *starts* an epoch transition,
    // but subsequent writes won't be handled until the epoch is over.
    // When we request a new epoch, it's the responsibility of the Exposer to flush keyRespFF (and thus the pendingKeyIdFF and BRAM responses as well?) before telling us the epoch has completed, because those responses *may* be in the old epoch.
    rule handle_write(waitingForEpochToInvalidate matches tagged Invalid);
        let aw <- get (axiShim.master.aw);
        let w <- get (axiShim.master.w);

        let validWrite = False;

        // Writes to [0x0, 0x1000) set status
        if ((aw.awaddr & 'h1000) == 0) begin
            KeyId k = truncate(aw.awaddr >> 4); // Memory map is byte-addressed, each secret key is 16 bytes = 4 address bits
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
                case (tuple2(keyValid[k], w.wdata[0])) matches
                    { True, 1 } : noAction;
                    { False, 0 } : noAction;

                    // Re-enable a key - go from False to True
                    { False, 1 } : begin
                        keyValid[k] <= True;
                    end

                    // Disable a key - go from True to False
                    { True, 0 } : begin
                        keyValid[k] <= False;
                        newEpochRequest.enq(currentEpoch + 1);
                        waitingForEpochToInvalidate <= tagged Valid k;
                    end
                endcase

                validWrite = True;
            end else begin
                // TODO signal failure
                $error("IOCap - mkSimpleIOCapKeyManager - Invalid status write");
            end

        // Writes to [0x1000, 0x2000) write to key (requires the key is revoked)
        end else begin
            KeyId k = truncate(aw.awaddr >> 4); // Memory map is byte-addressed, each secret key is 16 bytes = 4 address bits
            Bit#(4) startByteWithinKey = aw.awaddr[3:0];
            Bit#(5) endByteWithinKey = zeroExtend(startByteWithinKey) + fromInteger(valueOf(t_data) / 8);
    
            if (
                // Writes to key data can't overlap two keys
                endByteWithinKey <= 16
                // The given key must be invalid
                && keyValid[k] == False
                // and we can't be in the middle of a revocation (already checked above)
            ) begin
                // Move wstrb and wdata into the 128-bit space based on their offset within the key.

                // wstrb = TDiv#(t_data, 8) bits long
                // We've just checked that startByteWithinKey + TDiv#(t_data, 8) <= 16
                // => the top bit of wstrb will only ever go up to the top bit of Bit#(16), and won't be shifted out.
                Bit#(16) bramByteEnable = zeroExtend(w.wstrb) << startByteWithinKey;
                // wdata = t_data bits long
                // We've just checked that startByteWithinKey + TDiv#(t_data, 8) <= 16
                // => (startByteWithinKey + t_data/8) * 8 <= 128
                // => (startByteWithinKey * 8) + t_data <= 128
                // => no bits will be shifted out
                Bit#(128) bramWriteData = zeroExtend(w.wdata) << (startByteWithinKey * 8);

                keys.portA.request.put(BRAMRequestBE {
                    writeen: bramByteEnable,
                    // Don't send a write-response, we pipe those 1:1 out into keyResponses
                    responseOnWrite: False,
                    address: k,
                    datain: bramWriteData
                });
            end else begin
                // TODO signal failure
                $error("IOCap - mkSimpleIOCapKeyManager - Invalid data write");
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
        $display("IOCap - key manager - handle_write - ", fshow(aw), " - ", fshow(w), " - ", fshow(validWrite));
        axiShim.master.b.put(flit);
    endrule

    rule epoch_complete_sanitycheck(waitingForEpochToInvalidate matches tagged Invalid);
        epochCompleteResponse.deq();
        // TODO signal failure
        $error("IOCap - mkSimpleIOCapKeyManager - got epochCompleteResponse while not in the middle of an epoch.");
    endrule 

    // Doesn't conflict with handle_write, which could *initiate* a new epoch,
    // because that doesn't fire unless waitingForEpochToInvalidate is Invalid.
    rule complete_epoch(waitingForEpochToInvalidate matches tagged Valid .*);
        epochCompleteResponse.deq();
        currentEpoch <= epochCompleteResponse.first;
        waitingForEpochToInvalidate <= tagged Invalid;
        $display("IOCap - key manager - complete epoch");
    endrule

    // Queue of incoming requests for keys
    let keyReqFF <- mkFIFOF;
    // Queue of (KeyID, keyValid[KeyID])s requested from BRAM - should match 1:1 with responses from BRAM.
    // Carries the keyValid signal because we don't want it to "time-travel".
    // If we request a key from BRAM while it's invalid, then AXI requests write to the BRAM *and* set keyValid, we may receive old write-data but use the new keyValid and effectively reanimate stale key data.
    // If we request a key from BRAM while it's valid, then AXI requests *unset* keyValid, it's not technically a problem - the key is now invalid. TODO allow invalidations to time-travel forwards?
    // TODO eventually replace with a fixed-length pipeline - requires keyRespFF to never have backpressure, which requires the Exposer to never give it backpressure....?
    let pendingKeyIdFF <- mkFIFOF;
    // Queue of outgoing responses to the Exposer with (KeyID, Key) pairs.
    let keyRespFF <- mkFIFOF;

    (* descending_urgency = "handle_write, start_retrieve_key" *)
    // Access BRAM with lower priority than handle_write
    rule start_retrieve_key;
        keyReqFF.deq();
        let keyId = keyReqFF.first(); 
        pendingKeyIdFF.enq(tuple2(keyId, keyValid[keyId]));
        keys.portA.request.put(BRAMRequestBE {
            writeen: 0,
            responseOnWrite: False,
            address: keyId,
            datain: ?
        });
        $display("IOCap - key manager - start retrieve key ", fshow(keyId), " - ", fshow(keyValid[keyId]));
    endrule

    // Push reads from the BRAM directly into the keyRespFF (start_retrieve_key is the only rule that starts BRAM reads)
    rule receive_key_from_bram;
        pendingKeyIdFF.deq();

        let keyId_valid_tuple = pendingKeyIdFF.first;
        let keyId = tpl_1(keyId_valid_tuple);
        // keyValid[k] may have changed between requesting and receiving the key from BRAM.
        // For simplicity we want to model all changes to key status and data as atomic and ordered together => retain the keyValid state from when the data was requested.
        // *don't* use keyValid directly here.
        let valid = tpl_2(keyId_valid_tuple);
        let key <- keys.portA.response.get();

        if (valid) begin
            keyRespFF.enq(tuple2(keyId, tagged Valid key));
        end else begin
            keyRespFF.enq(tuple2(keyId, tagged Invalid));
        end

        $display("IOCap - key manager - receive_key_from_bram ", fshow(keyId), " - ", fshow(key), " - ", fshow(valid));
    endrule

    method Action bumpPerfCounterGoodWrite() = reqGoodWrite.send();
    method Action bumpPerfCounterBadWrite() = reqBadWrite.send();
    method Action bumpPerfCounterGoodRead() = reqGoodRead.send();
    method Action bumpPerfCounterBadRead() = reqBadRead.send();

    interface keyRequests = toSink(keyReqFF);
    interface keyResponses = toSource(keyRespFF);
    interface newEpochRequests = toSource(newEpochRequest);
    interface finishedEpochs = toSink(epochCompleteResponse);
    interface hostFacingSlave = axiShim.slave;
endmodule
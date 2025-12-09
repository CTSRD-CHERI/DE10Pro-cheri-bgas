import IOCapAxi_ErrorUnit :: *;

typedef union tagged {
    void KeyValidWhileInvalidating;
    void InvalidRead;
    void InvalidStatusWrite;
    void InvalidDataWrite;
    // The refcount pipeline needs to always be able to respond to
    // new revocations, so it's a hard error if it ever stalls out
    // or induces backpressure
    void RefCountPipeStalled;
} KeyManager2Error deriving (Bits, FShow, Eq);

// There aren't 8 possible values, there are 5 - but KeyManagerError is encoded in 3 bits.
typedef ErrorUnit#(KeyManager2Error, 8) KeyManager2ErrorUnit;

typedef enum {
    KeyInvalidRevoked,       // = 0
    KeyValid,                // = 1
    KeyInvalidPendingRevoke  // = 2
} KeyStatus deriving (FShow, Eq);

function Bit#(2) keyStatusMmioPack(KeyStatus k) = case (k) matches
    KeyInvalidRevoked : 2'd0;
    KeyValid : 2'd1;
    KeyInvalidPendingRevoke : 2'd2;
endcase;

// Experiment: use one-hot encodings so that lookups in large vectors of KeyStatus
// (which are usually comparing to a specific KeyStatus value, e.g. checking valid)
// only have to use one bit
instance Bits#(KeyStatus, 3);
    function Bit#(3) pack(KeyStatus k) = case (k) matches
        KeyInvalidRevoked : 3'b001;
        KeyValid : 3'b010;
        KeyInvalidPendingRevoke : 3'b100;
    endcase;
    function KeyStatus unpack(Bit#(3) b) = case (b) matches
        3'b001 : KeyInvalidRevoked;
        3'b010 : KeyValid;
        3'b100 : KeyInvalidPendingRevoke;
        default: ?;
    endcase;
endinstance

function Bool keyStatusIsValid(KeyStatus k) = (pack(k)[1] == 1);
function Bool keyStatusNotValid(KeyStatus k) = (pack(k)[1] == 0);
function Bool keyStatusNotInvalidRevoked(KeyStatus k) = (pack(k)[0] == 0);
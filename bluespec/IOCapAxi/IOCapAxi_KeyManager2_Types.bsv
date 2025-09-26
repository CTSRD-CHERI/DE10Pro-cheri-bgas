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
} KeyStatus deriving (Bits, FShow, Eq);
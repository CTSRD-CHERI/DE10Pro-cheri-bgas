import IOCapAxi_ErrorUnit :: *;

typedef union tagged {
    void KeyValidWhileInvalidating;
    void InvalidRead;
    void InvalidStatusWrite;
    void InvalidDataWrite;
    void PrematurelyCompletedEpoch;
} KeyManager2Error deriving (Bits, FShow);

// There aren't 8 possible values, there are 5 - but KeyManagerError is encoded in 3 bits.
typedef ErrorUnit#(KeyManager2Error, 8) KeyManager2ErrorUnit;

typedef enum {
    KeyInvalidRevoked,       // = 0
    KeyValid,                // = 1
    KeyInvalidPendingRevoke  // = 2
} KeyStatus deriving (Bits, FShow);
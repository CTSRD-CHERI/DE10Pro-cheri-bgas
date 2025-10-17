package Cap2024;

import AesLib::*;

function Maybe#(b) maybeMap(function b f(a val), Maybe#(a) m);
    case (m) matches
        tagged Invalid : return tagged Invalid;
        tagged Valid .val : return tagged Valid f(val);
    endcase
endfunction

typedef enum {
    Read,
    Write,
    ReadWrite
} CapPerms deriving (Bits, Eq, Bounded, FShow);

// The permissions for a given capability must be encoded as a chain, because some caveats may restrict permissions as well as space.
// Because we only support Read|Write|ReadWrite, a restriction can only happen at one point and there are fifteen possible chains.
// The bit values are set manually here for compatibility with Rust.
typedef enum {
    /// The initial resource is read-only, and no caveats are active
    ///
    /// `[Read]`
    ReadOnly_0Cav = 4'b00_00,
    /// The initial resource is read-only, and the first caveat is active
    ///
    /// `[Read, Read]`
    ReadOnly_1Cav = 4'b00_01,
    /// The initial resource is read-only, and both caveats are active
    ///
    /// `[Read, Read, Read]`
    ReadOnly_2Cav = 4'b00_10,
    /// The initial resource is wrute-only, and no caveats are active
    ///
    /// `[Write]`
    WriteOnly_0Cav = 4'b11_00,
    /// The initial resource is write-only, and the first caveat is active
    ///
    /// `[Write, Write]`
    WriteOnly_1Cav = 4'b11_01,
    /// The initial resource is write-only, and both caveats are active
    ///
    /// `[Write, Write, Write]`
    WriteOnly_2Cav = 4'b11_10,
    /// The initial resource is read-write, and no caveats are active
    ///
    /// `[ReadWrite]`
    ReadWrite_0Cav = 4'b00_11,
    /// The initial resource is read-write, the first caveat is active and didn't shrink the permissions
    ///
    /// `[ReadWrite, ReadWrite]`
    ReadWrite_1Cav = 4'b01_11,
    /// The initial resource is read-write, both caveats are active and neither shrunk the permissions
    ///
    /// `[ReadWrite, ReadWrite, ReadWrite]`
    ReadWrite_2Cav = 4'b10_11,
    /// The initial resource was read-write, the first caveat is active and shrunk the permissions to read-only
    ///
    /// `[ReadWrite, Read]`
    ShrunkToRead_1Cav = 4'b10_10,
    /// The initial resource was read-write, the first caveat is active and shrunk the permissions to write-only
    ///
    /// `[ReadWrite, Write]`
    ShrunkToWrite_1Cav = 4'b01_10,
    /// The initial resource was read-write, both caveats are active and the first caveat shrunk the permissions to read-only
    ///
    /// `[ReadWrite, Read, Read]`
    ShrunkToRead_2Cav_ByCav1 = 4'b10_00,
    /// The initial resource was read-write, both caveats are active and the second caveat shrunk the permissions to read-only
    ///
    /// `[ReadWrite, ReadWrite, Read]`
    ShrunkToRead_2Cav_ByCav2 = 4'b10_01,
    /// The initial resource was read-write, both caveats are active and the first caveat shrunk the permissions to write-only
    ///
    /// `[ReadWrite, Write, Write]`
    ShrunkToWrite_2Cav_ByCav1 = 4'b01_00,
    /// The initial resource was read-write, both caveats are active and the second caveat shrunk the permissions to write-only
    ///
    /// `[ReadWrite, ReadWrite, Write]`
    ShrunkToWrite_2Cav_ByCav2 = 4'b01_01

    // 4'b11_11 not used - Maybe#(CapPermsChain) should use it for Invalid
} CapPermsChain deriving (Bits, Eq, Bounded, FShow);

function Maybe#(CapPermsChain) previous(CapPermsChain c);
    case (c)
        ReadOnly_0Cav, WriteOnly_0Cav, ReadWrite_0Cav : return tagged Invalid;
        ReadOnly_1Cav : return tagged Valid ReadOnly_0Cav;
        ReadOnly_2Cav : return tagged Valid ReadOnly_1Cav;
        WriteOnly_1Cav : return tagged Valid WriteOnly_0Cav;
        WriteOnly_2Cav : return tagged Valid WriteOnly_1Cav;
        ReadWrite_1Cav : return tagged Valid ReadWrite_0Cav;
        ReadWrite_2Cav : return tagged Valid ReadWrite_1Cav;
        ShrunkToRead_1Cav : return tagged Valid ReadWrite_0Cav;
        ShrunkToWrite_1Cav : return tagged Valid ReadWrite_0Cav;
        ShrunkToRead_2Cav_ByCav1 : return tagged Valid ShrunkToRead_1Cav;
        ShrunkToRead_2Cav_ByCav2 : return tagged Valid ReadWrite_1Cav;
        ShrunkToWrite_2Cav_ByCav1 : return tagged Valid ShrunkToWrite_1Cav;
        ShrunkToWrite_2Cav_ByCav2 : return tagged Valid ReadWrite_1Cav;
    endcase
endfunction

function CapPerms permsOfChain(CapPermsChain chain);
    case (chain)
        ReadWrite_0Cav, ReadWrite_1Cav, ReadWrite_2Cav: return ReadWrite;
        ReadOnly_0Cav, ReadOnly_1Cav, ReadOnly_2Cav, ShrunkToRead_1Cav, ShrunkToRead_2Cav_ByCav1, ShrunkToRead_2Cav_ByCav2: return Read;
        WriteOnly_0Cav, WriteOnly_1Cav, WriteOnly_2Cav, ShrunkToWrite_1Cav, ShrunkToWrite_2Cav_ByCav1, ShrunkToWrite_2Cav_ByCav2: return Write;
    endcase
endfunction

// Maybe#(CapPermsChain) can be packed into 4 bits because there's an invalid encoding - useful for parsing
instance Bits#(Maybe#(CapPermsChain), 4);
    function Bit#(4) pack(Maybe#(CapPermsChain) value);
        case (value) matches
            tagged Valid .chain : return pack(chain);
            tagged Invalid : return 4'b11_11;
        endcase
    endfunction
    function Maybe#(CapPermsChain) unpack(Bit#(4) bits);
        if (bits == 4'b11_11) begin
            return tagged Invalid;
        end else begin
            return tagged Valid unpack(bits);
        end
    endfunction
endinstance

// Successfully evaluating a capability gives an address range of valid accesses.
typedef struct {
    Bit#(64) base;
    Bit#(65) top;
} CapRange deriving (Bits, Eq, FShow);

// Failing to evaluate a capability + signature can happen for one of three reasons
typedef enum {
    InvalidCaveat,
    InvalidCapPermsChain,
    InvalidSignature,
    UnexpectedCaveat
} CapFailReason deriving (Bits, Eq, FShow);

// Helper output struct for things that check the signature+text of a capability
typedef union tagged {
    a Succ;
    CapFailReason Fail;
} CapCheckResult#(type a) deriving (Bits, Eq, FShow);

// For the signature checking, we need to be able to create three versions of the capability: its root, its 1-caveat version (if it has 1 or 2 caveats), and its 2-caveat version (if it has 2 caveats).
// We need to implement this behaviour for the permissions chain and the capability as a whole, so make a typeclass
typeclass UpToTwoCaveats#(type a);
    // Every valid capability/perms chain can find its root value
    function a atRoot(a val);
    // If a capability/perms chain uses 1 or 2 caveats, it can find its 1-caveat value
    function Maybe#(a) atCav1(a mayHaveCav1);
    // If a capability/perms chain uses 2 caveats, it is its 2-caveat value
    function Maybe#(a) atCav2(a mayHaveCav2);
endtypeclass

instance UpToTwoCaveats#(CapPermsChain);
    function CapPermsChain atRoot(CapPermsChain val);
        case (val)
            // ReadOnly originate from ReadOnly
            ReadOnly_0Cav, ReadOnly_1Cav, ReadOnly_2Cav : return ReadOnly_0Cav;
            // WriteOnly originate from WriteOnly
            WriteOnly_0Cav, WriteOnly_1Cav, WriteOnly_2Cav : return WriteOnly_0Cav;
            // Everything else originates from ReadWrite
            ReadWrite_0Cav, ReadWrite_1Cav, ReadWrite_2Cav,
            ShrunkToRead_1Cav, ShrunkToWrite_1Cav,
            ShrunkToRead_2Cav_ByCav1, ShrunkToRead_2Cav_ByCav2,
            ShrunkToWrite_2Cav_ByCav1, ShrunkToWrite_2Cav_ByCav2 : return ReadWrite_0Cav;
        endcase
    endfunction

    function Maybe#(CapPermsChain) atCav1(CapPermsChain mayHaveCav1);
        case (mayHaveCav1)
            // 0-caveat chains don't have a cav1
            ReadOnly_0Cav, WriteOnly_0Cav, ReadWrite_0Cav : return tagged Invalid;
            // 1-caveat chains *are* their cav1
            ReadOnly_1Cav : return tagged Valid mayHaveCav1;
            WriteOnly_1Cav : return tagged Valid mayHaveCav1;
            ReadWrite_1Cav : return tagged Valid mayHaveCav1;
            ShrunkToRead_1Cav : return tagged Valid mayHaveCav1;
            ShrunkToWrite_1Cav : return tagged Valid mayHaveCav1;
            // 2-caveat chains are based on a cav1
            ReadOnly_2Cav : return tagged Valid ReadOnly_1Cav;
            WriteOnly_2Cav : return tagged Valid WriteOnly_1Cav;
            ReadWrite_2Cav : return tagged Valid ReadWrite_1Cav;
            ShrunkToRead_2Cav_ByCav1 : return tagged Valid ShrunkToRead_1Cav;
            ShrunkToRead_2Cav_ByCav2 : return tagged Valid ReadWrite_1Cav;
            ShrunkToWrite_2Cav_ByCav1 : return tagged Valid ShrunkToWrite_1Cav;
            ShrunkToWrite_2Cav_ByCav2 : return tagged Valid ReadWrite_1Cav;
        endcase
    endfunction

    function Maybe#(CapPermsChain) atCav2(CapPermsChain mayHaveCav2);
        case (mayHaveCav2)
            // 0-caveat chains don't have a cav2
            ReadOnly_0Cav, WriteOnly_0Cav, ReadWrite_0Cav : return tagged Invalid;
            // 1-caveat chains don't have a cav2
            ReadOnly_1Cav, WriteOnly_1Cav, ReadWrite_1Cav, ShrunkToRead_1Cav, ShrunkToWrite_1Cav : return tagged Invalid;
            // 2-caveat chains *are* their cav2
            ReadOnly_2Cav : return tagged Valid mayHaveCav2;
            WriteOnly_2Cav : return tagged Valid mayHaveCav2;
            ReadWrite_2Cav : return tagged Valid mayHaveCav2;
            ShrunkToRead_2Cav_ByCav1 : return tagged Valid mayHaveCav2;
            ShrunkToRead_2Cav_ByCav2 : return tagged Valid mayHaveCav2;
            ShrunkToWrite_2Cav_ByCav1 : return tagged Valid mayHaveCav2;
            ShrunkToWrite_2Cav_ByCav2 : return tagged Valid mayHaveCav2;
        endcase
    endfunction
endinstance

typeclass Cap#(type tcap) provisos (UpToTwoCaveats#(tcap), Bits#(tcap, 128), FShow#(tcap));
    // Pack a capability into an AES-able blob
    function AesState stateOfCap(tcap cap);
endtypeclass

typedef struct {
    tcap cap;
    Bit#(128) expectedSig;
    Bit#(128) secret;
} CapSigCheckIn#(type tcap) deriving (Bits, FShow);

endpackage
import BlueAXI4 :: *;
import Connectable :: *;

typedef struct  {
    Bool start;
    UInt#(2) flitnum;
} IOCapAXI4_AddrUserBits deriving (Bits); // TODO RIGHT NOW ASSUMING BITS=3

interface IOCapAXI4_Master#(numeric type t_id, numeric type t_data);
    interface AXI4_Master#(
          t_id
        // Currently hardcoded for 64-bit addressing, so we know it takes 3 cycles to pack a capability into the user bits
        , 64 // t_addr
        , t_data
        // Write address, read address have enough user bits to store IOCapAXI4_AddrUserBits
        , 3 // t_aw_user
        // Write data, write response, read response don't have user bits 
        , 0 // t_w_user
        , 0 // t_b_user
        , 3 // t_ar_user
        , 0 // t_r_user
    ) axiSignals;
endinterface

interface IOCapAXI4_Slave#(numeric type t_id, numeric type t_data);
    interface AXI4_Slave#(
          t_id
        // Currently hardcoded for 64-bit addressing, so we know it takes 3 cycles to pack a capability into the user bits
        , 64 // t_addr
        , t_data
        // Write address, read address have enough user bits to store IOCapAXI4_AddrUserBits
        , 3 // t_aw_user
        // Write data, write response, read response don't have user bits 
        , 0 // t_w_user
        , 0 // t_b_user
        , 3 // t_ar_user
        , 0 // t_r_user
    ) axiSignals;
endinterface

instance Connectable#(
  IOCapAXI4_Master#(t_id, t_data),
  IOCapAXI4_Slave#(t_id, t_data));
  module mkConnection#(
    IOCapAXI4_Master#(t_id, t_data) m,
    IOCapAXI4_Slave#(t_id, t_data) s)
    (Empty);
    mkConnection(m.axiSignals, s.axiSignals);
  endmodule
endinstance


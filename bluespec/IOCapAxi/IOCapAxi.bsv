import BlueAXI4 :: *;

typedef struct  {
    Bool start;
    UInt#(2) flitnum;
} IOCapAXI4_AddrUserBits deriving (Bits#(3));

interface IOCapAXI4_Master#(numeric type t_id, numeric type t_data) 
    provisos (
        // Currently hardcoded for 64-bit addressing, so we know it takes 3 cycles to pack a capability into the user bits
        NumAlias #( t_addr, 64 ),
        // Write address, read address have enough user bits to store IOCapAXI4_AddrUserBits
        NumAlias #( t_aw_user, 3 ),
        NumAlias #( t_ar_user, 3 ),
        // Write data, write response, read response don't have user bits 
        NumAlias #( t_w_user, 0 ),
        NumAlias #( t_b_user, 0 ),
        NumAlias #( t_r_user, 0 )
    );

    interface AXI4_Master#(t_id, t_addr, t_data, t_aw_user, t_w_user, t_b_user, t_ar_user, t_r_user) axiSignals;
endinterface

interface IOCapAXI4_Slave#(numeric type t_id, numeric type t_data) 
    provisos (
        // Currently hardcoded for 64-bit addressing, so we know it takes 3 cycles to pack a capability into the user bits
        NumAlias #( t_addr, 64 ),
        // Write address, read address have enough user bits to store IOCapAXI4_AddrUserBits
        NumAlias #( t_aw_user, 3 ),
        NumAlias #( t_ar_user, 3 ),
        // Write data, write response, read response don't have user bits 
        NumAlias #( t_w_user, 0 ),
        NumAlias #( t_b_user, 0 ),
        NumAlias #( t_r_user, 0 )
    );

    interface AXI4_Slave#(t_id, t_addr, t_data, t_aw_user, t_w_user, t_b_user, t_ar_user, t_r_user) axiSignals;
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


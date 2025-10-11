import FIFOF :: *;
import SpecialFIFOs :: *;
import BlueAXI4 :: *;
import SourceSink :: *;
import BRAM :: *;
import Vector :: *;
import BlueBasics :: *;

import IOCapAxi_Types :: *;
import IOCapAxi_Flits :: *;
import IOCapAxi_KeyManagers :: *;
import IOCapAxi_CreditValve :: *;
import IOCapAxi_Checkers :: *;

import Cap2024_02 :: *;

import IOCapAxi_Exposers_V1 :: *;
import IOCapAxi_Exposers_V2 :: *;
import IOCapAxi_Exposers_V3 :: *;
import IOCapAxi_Exposers_V4 :: *;
import IOCapAxi_Exposers_V5 :: *;
import IOCapAxi_Exposers_V6 :: *;

export IOCapAxi_Exposers_V1 :: *;
export IOCapAxi_Exposers_V2 :: *;
export IOCapAxi_Exposers_V3 :: *;
export IOCapAxi_Exposers_V4 :: *;
export IOCapAxi_Exposers_V5 :: *;
export IOCapAxi_Exposers_V6 :: *;

export mkStrippingIOCapExposer;

// An IOCapSingleExposer that strips off capability metadata instead of using it
module mkStrippingIOCapExposer(IOCapSingleExposer#(t_id, t_data));
    // This doesn't have any key storage or checking logic yet! It just receives IOCapAXI and converts it back to plain AXI.

    AddressChannelCapUnwrapper#(AXI4_AWFlit#(t_id, 64, 3), AXI4_AWFlit#(t_id, 64, 0), Cap2024_02) aw <- mkSimpleAddressChannelCapUnwrapper(Proxy{});
    FIFOF#(AXI4_WFlit#(t_data, 0)) wff <- mkFIFOF;
    FIFOF#(AXI4_BFlit#(t_id, 0)) bff <- mkFIFOF;
    AddressChannelCapUnwrapper#(AXI4_ARFlit#(t_id, 64, 3), AXI4_ARFlit#(t_id, 64, 0), Cap2024_02) ar <- mkSimpleAddressChannelCapUnwrapper(Proxy{});
    FIFOF#(AXI4_RFlit#(t_id, t_data, 0)) rff <- mkFIFOF;

    function t_flit stripCapFromAuthFlit(AuthenticatedFlit#(t_flit, Cap2024_02) authFlit) = authFlit.flit;

    interface iocapsIn = interface IOCapAXI4_Slave;
        interface axiSignals = interface AXI4_Slave;
            interface aw = toSink(aw.in);
            interface  w = toSink(wff);
            interface  b = toSource(bff);
            interface ar = toSink(ar.in);
            interface  r = toSource(rff);
        endinterface;
    endinterface;

    interface sanitizedOut = interface AXI4_Master;
        interface aw = mapSource(stripCapFromAuthFlit, aw.out);
        interface  w = toSource(wff);
        interface  b = toSink(bff);
        interface ar = mapSource(stripCapFromAuthFlit, ar.out);
        interface  r = toSink(rff);
    endinterface;
endmodule
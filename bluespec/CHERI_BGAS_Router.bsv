/*-
 * Copyright (c) 2022 Alexandre Joannou
 * All rights reserved.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package CHERI_BGAS_Router;

import BlueAXI4 :: *;
import Routable :: *;
import Vector :: *;
import ConfigReg :: *;
import Connectable :: *;
import BlueBasics :: *;
import FIFOF :: *;

// An "AXI4 router port" consists of a pair of one AXI4 manager and one AXI4
// subordinate
interface AXI4_Router_Port #(
  numeric type t_id, numeric type t_addr, numeric type t_data
, numeric type t_awuser, numeric type t_wuser, numeric type t_buser
, numeric type t_aruser, numeric type t_ruser );
  interface AXI4_Master #( t_id, t_addr, t_data
                         , t_awuser, t_wuser, t_buser
                         , t_aruser, t_ruser) manager;
  interface AXI4_Slave #( t_id, t_addr, t_data
                        , t_awuser, t_wuser, t_buser
                        , t_aruser, t_ruser) subordinate;
endinterface
instance Connectable #( AXI4_Router_Port #(a,b,c,d,e,f,g,h)
                      , AXI4_Router_Port #(a,b,c,d,e,f,g,h) );
  module mkConnection #( AXI4_Router_Port #(a,b,c,d,e,f,g,h) x
                       , AXI4_Router_Port #(a,b,c,d,e,f,g,h) y ) (Empty);
    mkConnection (x.manager, y.subordinate);
    mkConnection (y.manager, x.subordinate);
  endmodule
endinstance

// A "global port" consists of a pair of one "payload source" and one
// "payload sink"
interface Global_Port #(type t_payload);
  interface Source #(t_payload) tx;
  interface Sink #(t_payload) rx;
endinterface
instance Connectable #(Global_Port #(a), Global_Port #(a))
  provisos (Bits #(a, a_sz));
  module mkConnection #(Global_Port #(a) x, Global_Port #(a) y) (Empty);
    mkConnection (x.tx, y.rx);
    mkConnection (y.tx, x.rx);
  endmodule
endinstance

module mkCHERI_BGAS_NoRouteTile
  (AXI4_Router_Port #( t_id, t_addr, t_data
                     , t_awuser, t_wuser, t_buser
                     , t_aruser, t_ruser ));

  Slave #( AXI4_WriteFlit#(t_id, t_addr, t_data, t_awuser, t_wuser)
         , AXI4_BFlit#(t_id, t_buser) ) noWrites <- mkNoRouteSlave;
  Slave #( AXI4_ARFlit#(t_id, t_addr, t_aruser)
         , AXI4_RFlit#(t_id, t_data, t_ruser) ) noReads <- mkNoRouteSlave;
  let noWritesAWReqFF <- mkFIFOF;
  let noWritesWReqFF <- mkFIFOF;
  let noWritesReq <- mergeWrite ( toSource (noWritesAWReqFF)
                                , toSource (noWritesWReqFF) );
  mkConnection (noWritesReq, noWrites.req);

  interface manager = interface AXI4_Master;
    interface aw = nullSource;
    interface w  = nullSource;
    interface b  = nullSink;
    interface ar = nullSource;
    interface r  = nullSink;
  endinterface;
  interface subordinate = interface AXI4_Slave;
    interface aw = toSink (noWritesAWReqFF);
    interface w  = toSink (noWritesWReqFF);
    interface b  = noWrites.rsp;
    interface ar = noReads.req;
    interface r  = noReads.rsp;
  endinterface;

endmodule

// A RouterId type, split between an X coordinate and a Y coordinate
typedef struct {
  Bit #(t_y_sz) y;
  Bit #(t_x_sz) x;
} RouterId #(numeric type t_x_sz, numeric type t_y_sz) deriving (Bits);

instance Literal #(RouterId #(xsz, ysz));
  function RouterId #(xsz, ysz) fromInteger (Integer x);
    Bit #(TAdd #(xsz, ysz)) tmp = fromInteger (x);
    return unpack (tmp);
  endfunction
  function Bool inLiteralRange (RouterId #(xsz, ysz) target, Integer x);
    Bit #(TAdd #(xsz, ysz)) tmp = pack (target);
    return inLiteralRange (tmp, x);
  endfunction
endinstance

// low x coordinate indicate west, high x coordinate indicate east
// low y coordinate indicate south, high y coordinate indicate north

// Router for "standard" mesh topology, with 5 Ports
//   - 0: local port
Integer localPortIdx = 0;
//   - 1: west port
Integer westPortIdx = 1;
//   - 2: south port
Integer southPortIdx = 2;
//   - 3: east port
Integer eastPortIdx = 3;
//   - 4: north port
Integer northPortIdx = 4;
module mkAXI4Router #(Maybe #(RouterId #(t_x_sz, t_y_sz)) routerId)
  (Vector #(5, AXI4_Router_Port #( t_id, t_addr, t_data
                                 , t_awuser, t_wuser, t_buser
                                 , t_aruser, t_ruser )))
  provisos ( NumAlias #(t_router_id_sz, SizeOf #(RouterId #(t_x_sz, t_y_sz)))
           , Add #(a__, t_router_id_sz, t_id)
           , Add #(b__, t_router_id_sz, t_addr) );
  // NOTE: the router id changing mechanism assumes:
  //   - to handle bursts, the underlying interconnect fabric does not try to
  //   allocate a route more than once per request (if the router id is changed
  //   in the middle of a burst, the rest of the wflits will reach the same
  //   destination)
  //   - to handle responses, the masters who want to change the router id are
  //   responsible to do so only when all outstanding requests have received
  //   their response
  Bool acceptTraffic = isValid (routerId);
  // interface shims
  Vector #(5, AXI4_Shim #( t_id, t_addr, t_data
                         , t_awuser, t_wuser, t_buser
                         , t_aruser, t_ruser ))
    inShims <- replicateM (mkAXI4Shim);
  Vector #(5, AXI4_Shim #( t_id, t_addr, t_data
                         , t_awuser, t_wuser, t_buser
                         , t_aruser, t_ruser ))
    outShims <- replicateM (mkAXI4Shim);
  // helper functions
  function AXI4_Master #(a,b,c,d,e,f,g,h) getShimMaster
           (AXI4_Shim #(a,b,c,d,e,f,g,h) shim) = shim.master;
  function AXI4_Slave #(a,b,c,d,e,f,g,h) getShimSlave
           (AXI4_Shim #(a,b,c,d,e,f,g,h) shim) = shim.slave;
  function AXI4_Router_Port #(a,b,c,d,e,f,g,h) wrapRouterPort
    ( AXI4_Shim #(a,b,c,d,e,f,g,h) inpt
    , AXI4_Shim #(a,b,c,d,e,f,g,h) outpt ) =
      interface AXI4_Router_Port;
        interface manager = outpt.master;
        interface subordinate = guard_AXI4_Slave (inpt.slave, !acceptTraffic);
      endinterface;
  // routing functions
  // the MSB of the address indicates global routing, the next m bits are the
  // router id
  function Bit #(m) routerIdFromAXIAddr (Bit #(n) axiAddr)
    provisos (Add #(_, m, n)) = axiAddr [ valueOf (n) - 2
                                        : valueOf (n) - valueOf (m) - 1 ];
  // the MSBs of the axi ID field are the router id, the LSBs are a local ID
  // within the node
  function Bit #(m) routerIdFromAXIId (Bit #(n) axiId)
    provisos (Add #(_, m, n)) = truncateLSB (axiId);
  // route x first (find correct column)
  // then y (within column, find correct row)
  function Vector #(5, Bool) routeXY (RouterId #(nx, ny) ownId, Bit #(n) dest)
    provisos (Add #(nx, ny, n));
    RouterId #(nx, ny) destId = unpack (dest);
    Vector #(5, Bool) res = replicate (False);
    // XY routing
    // X routing first, a.k.a. find correct column (west <-> east) first
    if (ownId.x > destId.x) // current node too far east, go west
      res [westPortIdx] = True;
    else if (ownId.x < destId.x) // current node too far west, go east
      res [eastPortIdx] = True;
    // correct column found
    // Y routing now, a.k.a. find correct row (north <-> south)
    else if (ownId.y < destId.y) // current node too far south, go north
      res [northPortIdx] = True;
    else if (ownId.y > destId.y) // current node too far north, go south
      res [southPortIdx] = True;
    // correct row found
    else // only remaining case, reached destination node
      res [localPortIdx] = True;
    return res;
  endfunction
  function routeReq = compose (routeXY (routerId.Valid), routerIdFromAXIAddr);
  function routeRsp = compose (routeXY (routerId.Valid), routerIdFromAXIId);
  // wire up switches // TODO: change to one bus per destination
  mkAXI4Switch ( routeReq, routeRsp
               , map (getShimMaster, inShims)
               , map (getShimSlave, outShims));
  // return interface
  return zipWith (wrapRouterPort, inShims, outShims);
endmodule

// Turn AXI4 traffic into global traffic by adding some credit-based flow
// control and gathering incoming traffic together and outgoing traffic
// together.
module mkAXI4StreamBridge
  #( parameter NumProxy #(t_max_credit) maxCreditProxy
   , function module #(Empty) bundle (
       Sink #(t_global_axi_flit) gSnk
     , Source #(t_aw_flit) awSrc
     , Source #(t_w_flit)  wSrc
     , Source #(t_b_flit)  bSrc
     , Source #(t_ar_flit) arSrc
     , Source #(t_r_flit)  rSrc )
   , function module #(Empty) unbundle (
       Source #(t_global_axi_flit) gSrc
     , Sink #(t_aw_flit) awSnk
     , Sink #(t_w_flit)  wSnk
     , Sink #(t_b_flit)  bSnk
     , Sink #(t_ar_flit) arSnk
     , Sink #(t_r_flit)  rSnk )
   , AXI4_Router_Port #( t_id, t_addr, t_data
                       , t_awuser, t_wuser, t_buser
                       , t_aruser, t_ruser ) rPort
   , Bool guard )
  (Global_Port #(t_global_flit_container))
  provisos (
  // local type aliases
    Alias #(t_aw_flit, AXI4_AWFlit #(t_id, t_addr, t_awuser))
  , Alias #(t_w_flit,  AXI4_WFlit #(t_data, t_wuser))
  , Alias #(t_b_flit,  AXI4_BFlit #(t_id, t_buser))
  , Alias #(t_ar_flit, AXI4_ARFlit #(t_id, t_addr, t_aruser))
  , Alias #(t_r_flit,  AXI4_RFlit #(t_id, t_data, t_ruser))
  , Alias #( t_global_axi_flit
           , Tuple5 #( Maybe# (t_aw_flit), Maybe# (t_w_flit), Maybe# (t_b_flit)
                     , Maybe# (t_ar_flit), Maybe# (t_r_flit) ) )
  , Alias #(t_global_credits_flit, Vector #(5, Bool))
  , Alias #(t_global_flit, Tuple2 #(t_global_axi_flit, t_global_credits_flit))
  // constraints
  , Bits #(t_global_axi_flit, t_global_axi_flit_sz)
  , Bits #(t_global_credits_flit, 5)
  , Bits #(t_global_flit, t_global_flit_sz)
  , Bits #(t_global_flit_container, t_global_flit_container_sz)
  //, Add #(_, t_global_flit_sz, t_global_flit_container_sz)
  , Add #(_, TAdd #(t_global_axi_flit_sz, 5), t_global_flit_container_sz)
  );

  // local buffers and resources definitions
  //////////////////////////////////////////////////////////////////////////////
  FIFOF #(t_global_axi_flit) globalAXIOutFF <- mkFIFOF;
  FIFOF #(t_global_flit)         globalInFF <- mkFIFOF;
  FIFOF #(t_global_flit)        globalOutFF <- mkFIFOF;

  // augment AXI4 channels with credit-based flow control
  //////////////////////////////////////////////////////////////////////////////
  function cSrc = toSourceWithCredit (maxCreditProxy);
  SourceWithCredit #(t_aw_flit) awOut <- cSrc (rPort.manager.aw);
  SourceWithCredit #(t_w_flit)   wOut <- cSrc (rPort.manager.w);
  SourceWithCredit #(t_b_flit)   bOut <- cSrc (rPort.subordinate.b);
  SourceWithCredit #(t_ar_flit) arOut <- cSrc (rPort.manager.ar);
  SourceWithCredit #(t_r_flit)   rOut <- cSrc (rPort.subordinate.r);
  function cSnk = toSinkWithCredit (maxCreditProxy);
  SinkWithCredit #(t_aw_flit) awIn <- cSnk (rPort.subordinate.aw);
  SinkWithCredit #(t_w_flit)   wIn <- cSnk (rPort.subordinate.w);
  SinkWithCredit #(t_b_flit)   bIn <- cSnk (rPort.manager.b);
  SinkWithCredit #(t_ar_flit) arIn <- cSnk (rPort.subordinate.ar);
  SinkWithCredit #(t_r_flit)   rIn <- cSnk (rPort.manager.r);

  // Outgoing traffic
  //////////////////////////////////////////////////////////////////////////////
  // bundle the individual AXI flits into a single global AXI flit
  bundle ( toSink (globalAXIOutFF)
         , awOut.data, wOut.data, bOut.data, arOut.data, rOut.data );
  // bundle the individual credits into a single global credits bundle
  Vector #(5, Source #(Bit #(0))) creditOutSrcs = ?;
  creditOutSrcs[0] = awIn.credit;
  creditOutSrcs[1] = wIn.credit;
  creditOutSrcs[2] = bIn.credit;
  creditOutSrcs[3] = arIn.credit;
  creditOutSrcs[4] = rIn.credit;
  Source #(t_global_credits_flit) globalCreditsOutSrc
    <- mergeAnyHomogeneousSources (isValid, creditOutSrcs);
  // send all outgoing traffic
  function globalMerge (mAXIFlit, mCreditsFlit) =
    tuple2 ( fromMaybe ( tuple5 (Invalid, Invalid, Invalid, Invalid, Invalid)
                       , mAXIFlit )
           , fromMaybe (replicate (False), mCreditsFlit) );
  Source #(t_global_flit)
    mergedSrc <- mergeAnyHeterogeneousSources ( globalMerge
                                              , toSource (globalAXIOutFF)
                                              , globalCreditsOutSrc );
  mkConnection (mergedSrc, toSink (globalOutFF));

  // Incoming traffic
  //////////////////////////////////////////////////////////////////////////////
  // receive all global incoming traffic and split it into a global AXI flit and
  // a global credits flit
  //function globalSplitAXI = compose (Valid, tpl_1);
  function Maybe #(a) globalSplitAXI (Tuple2 #(a, b) x) = Valid (tpl_1 (x));
  //function globalSplitCredits = compose (Valid, tpl_2);
  function Maybe #(b) globalSplitCredits (Tuple2 #(a, b) x) = Valid (tpl_2 (x));
  match {.axiInSrc, .creditsInSrc} <-
    splitAnyHeterogeneousSource ( globalSplitAXI
                                , globalSplitCredits
                                , toSource (globalInFF) );
  // unbundle incoming global AXI traffic into individual AXI channels
  unbundle ( axiInSrc
           , awIn.data, wIn.data, bIn.data, arIn.data, rIn.data );
  // unbundle incoming global credits traffic into individual credit channels
  function Maybe #(Bit #(0)) splitCredits (Bool x) = (x) ? Valid (?) : Invalid;
  let creditInSrcs <- splitAnyHomogeneousSource (splitCredits, creditsInSrc);
  mkConnection (creditInSrcs[0], awOut.credit);
  mkConnection (creditInSrcs[1], wOut.credit);
  mkConnection (creditInSrcs[2], bOut.credit);
  mkConnection (creditInSrcs[3], arOut.credit);
  mkConnection (creditInSrcs[4], rOut.credit);

  // XXX TODO augment with uart "debug & control" channel

  // Module interface
  //////////////////////////////////////////////////////////////////////////////
  function t_global_flit_container toContainer (t_global_flit x) =
    unpack (zeroExtend (pack (x)));
  function t_global_flit fromContainer (t_global_flit_container x) =
    unpack (truncate (pack (x)));
  interface tx = guardSource( mapSource (toContainer, toSource (globalOutFF))
                            , guard);
  interface rx = mapSink (fromContainer, toSink (globalInFF));

endmodule

module mkCHERI_BGAS_StreamBridge
 #( parameter NumProxy #(t_max_credit) maxCreditProxy
  , Bool guard
  , AXI4_Router_Port #( t_id, t_addr, t_data
                      , t_awuser, t_wuser, t_buser
                      , t_aruser, t_ruser ) inPort )
  (Global_Port #(t_global_flit_container))
  provisos (
  // local type aliases
    Alias #(t_aw_flit, AXI4_AWFlit #(t_id, t_addr, t_awuser))
  , Alias #(t_w_flit, AXI4_WFlit #(t_data, t_wuser))
  , Alias #(t_b_flit, AXI4_BFlit #(t_id, t_buser))
  , Alias #(t_ar_flit, AXI4_ARFlit #(t_id, t_addr, t_aruser))
  , Alias #(t_r_flit, AXI4_RFlit #(t_id, t_data, t_ruser))
  , Alias #( t_global_axi_flit
           , Tuple5 #( Maybe# (t_aw_flit), Maybe# (t_w_flit), Maybe# (t_b_flit)
                     , Maybe# (t_ar_flit), Maybe# (t_r_flit) ) )
  // constraints
  , Bits #(t_global_axi_flit, t_global_axi_flit_sz)
  , Bits #(t_global_flit_container, t_global_flit_container_sz)
  , Add #(_, TAdd #(t_global_axi_flit_sz, 5), t_global_flit_container_sz)
  );

  // flits bundling
  module bundle #( Sink #(t_global_axi_flit) gSnk
                 , Source #(t_aw_flit) awSrc
                 , Source #(t_w_flit) wSrc
                 , Source #(t_b_flit) bSrc
                 , Source #(t_ar_flit) arSrc
                 , Source #(t_r_flit) rSrc ) (Empty);

    module wrapChannel #(Bool doDrop, Source #(t) src)
                        (Maybe #(t))
      provisos (Bits #(t, t_sz));
      FIFOF #(t) ff <- mkFIFOF; mkConnection (src, toSink (ff));
      let tWire <- mkDWire (Invalid);
      (* fire_when_enabled *)
      rule tWireWrite (ff.notEmpty); tWire <= Valid (ff.first); endrule
      (* fire_when_enabled *)
      rule ffDrop (doDrop && ff.notEmpty); ff.deq; endrule
      return tWire;
    endmodule

    let doDrop <- mkPulseWire;
    let mAW <- wrapChannel (doDrop, awSrc);
    let  mW <- wrapChannel (doDrop,  wSrc);
    let  mB <- wrapChannel (doDrop,  bSrc);
    let mAR <- wrapChannel (doDrop, arSrc);
    let  mR <- wrapChannel (doDrop,  rSrc);

    let newGuard =    isValid (mAW) || isValid (mW) || isValid (mB)
                   || isValid (mAR) || isValid (mR);
    let newSrc = interface Source;
      method canPeek = newGuard;
      method peek if (newGuard) = tuple5 (mAW, mW, mB, mAR, mR);
      method drop if (newGuard) = doDrop.send;
    endinterface;
    mkConnection (newSrc, gSnk);

  endmodule
  // flit unbundling
  module unbundle #( Source #(t_global_axi_flit) gSrc
                   , Sink #(t_aw_flit) awSnk
                   , Sink #(t_w_flit) wSnk
                   , Sink #(t_b_flit) bSnk
                   , Sink #(t_ar_flit) arSnk
                   , Sink #(t_r_flit) rSnk ) (Empty);

    Reg #(Bool) awProduced [2] <- mkCReg (2, False);
    let awNeedProcude = gSrc.canPeek && isValid (tpl_1 (gSrc.peek));
    (* fire_when_enabled *)
    rule awProduce (!awProduced[0] && awNeedProcude && awSnk.canPut);
      awSnk.put (fromMaybe (?, tpl_1 (gSrc.peek)));
      awProduced[0] <= True;
    endrule

    Reg #(Bool) wProduced [2] <- mkCReg (2, False);
    let wNeedProcude = gSrc.canPeek && isValid (tpl_2 (gSrc.peek));
    (* fire_when_enabled *)
    rule wProduce (!wProduced[0] && wNeedProcude && wSnk.canPut);
      wSnk.put (fromMaybe (?, tpl_2 (gSrc.peek)));
      wProduced[0] <= True;
    endrule

    Reg #(Bool) bProduced [2] <- mkCReg (2, False);
    let bNeedProcude = gSrc.canPeek && isValid (tpl_3 (gSrc.peek));
    (* fire_when_enabled *)
    rule bProduce (!bProduced[0] && bNeedProcude && bSnk.canPut);
      bSnk.put (fromMaybe (?, tpl_3 (gSrc.peek)));
      bProduced[0] <= True;
    endrule

    Reg #(Bool) arProduced [2] <- mkCReg (2, False);
    let arNeedProcude = gSrc.canPeek && isValid (tpl_4 (gSrc.peek));
    (* fire_when_enabled *)
    rule arProduce (!arProduced[0] && arNeedProcude && arSnk.canPut);
      arSnk.put (fromMaybe (?, tpl_4 (gSrc.peek)));
      arProduced[0] <= True;
    endrule

    Reg #(Bool) rProduced [2] <- mkCReg (2, False);
    let rNeedProcude = gSrc.canPeek && isValid (tpl_5 (gSrc.peek));
    (* fire_when_enabled *)
    rule rProduce (!rProduced[0] && rNeedProcude && rSnk.canPut);
      rSnk.put (fromMaybe (?, tpl_5 (gSrc.peek)));
      rProduced[0] <= True;
    endrule

    (* fire_when_enabled *)
    rule dropInput (    (!awNeedProcude || awProduced[1])
                     && (!wNeedProcude  || wProduced[1])
                     && (!bNeedProcude  || bProduced[1])
                     && (!arNeedProcude || arProduced[1])
                     && (!rNeedProcude  || rProduced[1]) );
      gSrc.drop;
      awProduced[1] <= False;
      wProduced[1]  <= False;
      bProduced[1]  <= False;
      arProduced[1] <= False;
      rProduced[1]  <= False;
    endrule
  endmodule
  // wrap ports and return interface
  let outPort <- mkAXI4StreamBridge ( maxCreditProxy
                                    , bundle
                                    , unbundle
                                    , inPort
                                    , guard );
  return outPort;
endmodule

interface CHERI_BGAS_Router_Ifc #(
  // management interface
  numeric type t_mngnt_id, numeric type t_mngnt_addr, numeric type t_mngnt_data
, numeric type t_mngnt_awuser, numeric type t_mngnt_wuser
, numeric type t_mngnt_buser
, numeric type t_mngnt_aruser, numeric type t_mngnt_ruser
  // traffic interface
, numeric type t_s_id, numeric type t_m_id
, numeric type t_addr, numeric type t_data
, numeric type t_awuser, numeric type t_wuser, numeric type t_buser
, numeric type t_aruser, numeric type t_ruser
  // container type for global flits
, type t_global_flit_container
`ifdef BSIM
, numeric type t_x_sz
, numeric type t_y_sz
`endif
);
  interface AXI4_Slave #( t_mngnt_id, t_mngnt_addr, t_mngnt_data
                        , t_mngnt_awuser, t_mngnt_wuser, t_mngnt_buser
                        , t_mngnt_aruser, t_mngnt_ruser ) mngmntSubordinate;
  interface AXI4_Master #( t_m_id, t_addr, t_data
                         , t_awuser, t_wuser, t_buser
                         , t_aruser, t_ruser ) localManager;
  interface AXI4_Slave #( t_s_id, t_addr, t_data
                        , t_awuser, t_wuser, t_buser
                        , t_aruser, t_ruser ) localSubordinate;
  interface Global_Port #(t_global_flit_container) westPort;
  interface Global_Port #(t_global_flit_container) southPort;
  interface Global_Port #(t_global_flit_container) eastPort;
  interface Global_Port #(t_global_flit_container) northPort;
`ifdef BSIM
  method Action setRouterId (Maybe#(RouterId#(t_x_sz, t_y_sz)) _);
`endif
endinterface

module mkCHERI_BGAS_Router #(Maybe #(RouterId #(t_x_sz, t_y_sz)) initRouterId)
  (CHERI_BGAS_Router_Ifc #( t_mngnt_id, t_mngnt_addr, t_mngnt_data
                          , t_mngnt_awuser, t_mngnt_wuser, t_mngnt_buser
                          , t_mngnt_aruser, t_mngnt_ruser
                          , t_s_id, t_m_id, t_addr, t_data
                          , t_awuser, t_wuser, t_buser
                          , t_aruser, t_ruser
                          , t_global_flit_container
                          `ifdef BSIM
                          , t_x_sz
                          , t_y_sz
                          `endif
                          ))
  provisos (
    // local type aliases
    NumAlias #(t_id, TAdd #(t_s_id, t_router_id_sz))
  , NumAlias #(t_router_id_sz, SizeOf #(RouterId #(t_x_sz, t_y_sz)))
  , Alias #(t_aw_flit, AXI4_AWFlit #(t_id, t_addr, t_awuser))
  , Alias #(t_w_flit, AXI4_WFlit #(t_data, t_wuser))
  , Alias #(t_b_flit, AXI4_BFlit #(t_id, t_buser))
  , Alias #(t_ar_flit, AXI4_ARFlit #(t_id, t_addr, t_aruser))
  , Alias #(t_r_flit, AXI4_RFlit #(t_id, t_data, t_ruser))
  , Alias #( t_global_axi_flit
           , Tuple5 #( Maybe# (t_aw_flit), Maybe# (t_w_flit), Maybe# (t_b_flit)
                     , Maybe# (t_ar_flit), Maybe# (t_r_flit) ) )
  , NumAlias #(nbIdRegistrations, 8)
    // constraints
  , Add #(a__, t_router_id_sz, t_id)
  , Add #(b__, t_router_id_sz, t_addr)
  , Add #(c__, t_router_id_sz, t_mngnt_data)
  , Add #(d__, TLog #(nbIdRegistrations), t_m_id)
    // XXX t_mngnt_data is a multiple of 8
  , Mul #(TDiv #(t_mngnt_data, 8), 8, t_mngnt_data)
  , Bits #(t_global_axi_flit, t_global_axi_flit_sz)
  , Bits #(t_global_flit_container, t_global_flit_container_sz)
  , Add #(_, TAdd #(t_global_axi_flit_sz, 5), t_global_flit_container_sz) );

  // CHERI BGAS router management
  let mngntShim <- mkAXI4Shim;
  Reg #(Maybe #(RouterId #(t_x_sz, t_y_sz)))
    routerId <- mkConfigReg (initRouterId);
  // handle reads
  rule management_read (  mngntShim.master.ar.canPeek
                       && mngntShim.master.r.canPut );
    let arflit <- get (mngntShim.master.ar);
    mngntShim.master.r.put (AXI4_RFlit {
        rid: arflit.arid
      , rdata: zeroExtend (pack (fromMaybe (unpack (~0), routerId)))
      , rresp: arflit.arlen == 0 ? OKAY : SLVERR
      , rlast: True
      , ruser: ? });
  endrule
  // handle writes
  let awflit = mngntShim.master.aw.peek;
  let wflit = mngntShim.master.w.peek;
  Bool isLegalAW = awflit.awlen == 0;
  Bool isLegalW = wflit.wlast;
  (* descending_urgency = "legal_write, illegal_write" *)
  rule legal_write (  mngntShim.master.aw.canPeek && isLegalAW
                   && mngntShim.master.w.canPeek && isLegalW
                   && mngntShim.master.b.canPut );
    mngntShim.master.aw.drop;
    mngntShim.master.w.drop;
    routerId <= Valid (unpack (truncate (mergeWithBE ( wflit.wstrb
                                                     , 0
                                                     , wflit.wdata ))));
    mngntShim.master.b.put (AXI4_BFlit {
        bid: awflit.awid
      , bresp: OKAY
      , buser: ? });
  endrule
  rule illegal_write (  mngntShim.master.aw.canPeek
                     && mngntShim.master.w.canPeek
                     && mngntShim.master.b.canPut );
    mngntShim.master.w.drop;
    if (wflit.wlast) begin
      mngntShim.master.aw.drop;
      mngntShim.master.b.put (AXI4_BFlit {
          bid: awflit.awid
        , bresp: SLVERR
        , buser: ? });
    end
  endrule

  // route AXI4 traffic
  Vector #(5, AXI4_Router_Port #( t_id, t_addr, t_data
                                , t_awuser, t_wuser, t_buser
                                , t_aruser, t_ruser ))
    ports <- mkAXI4Router (routerId);
  // ID field adapter for local port connection
  let localRouterPort = head (ports);
  // create the outgoing local -> global ID conversion
  // (simple concat of unique id for now)
  let localSub =
    guard_AXI4_Slave ( prepend_AXI4_Slave_id ( pack (routerId.Valid)
                                             , localRouterPort.subordinate )
                     , !isValid (routerId) );
  // create the incoming global -> local ID conversion
  // (ID realocation)
  NumProxy #(nbIdRegistrations) proxyTableSz = ?;
  NumProxy #(8)  proxyMaxSameId = ?;
  let localMngr <- change_AXI4_Master_Id ( proxyTableSz, proxyMaxSameId
                                         , localRouterPort.manager );

  // wrap ports and return interface
  NumProxy #(32) maxCreditProxy = ?;
  Vector #(4, Global_Port #(t_global_flit_container))
    remotePorts <- mapM ( mkCHERI_BGAS_StreamBridge ( maxCreditProxy
                                                    , !isValid (routerId) )
                        , tail (ports) );
  // interfaces
  interface mngmntSubordinate = mngntShim.slave;
  interface localManager = localMngr;
  interface localSubordinate = localSub;
  interface westPort  = remotePorts[0];
  interface southPort = remotePorts[1];
  interface eastPort  = remotePorts[2];
  interface northPort = remotePorts[3];
`ifdef BSIM
  method setRouterId = routerId._write;
`endif
endmodule

endpackage

//
// Copyright (C) 2010 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

//
// Token ring, based on a standard ring.
//

import FIFOF::*;

`include "asim/provides/librl_bsv_base.bsh"


//
// Token ring interface looks like a FIFO, except that enq() takes an extra
// argument for the destination node ID.
//
interface Connection_TokenRing#(type t_NODE_ID, type t_MSG);
    // Outgoing portion of the interface
    method Action enq(t_NODE_ID dstNode, t_MSG data);
    method Bool notFull();

    // Incoming portion
    method t_MSG first();
    method Action deq();
    method Bool notEmpty();
endinterface


//
// Internal types
//
typedef struct
{
    Bool token;
    Maybe#(Tuple2#(t_NODE_ID, t_MSG)) data;
}
TOKEN_RING_MSG#(type t_NODE_ID, type t_MSG)
    deriving (Eq, Bits);


//
// mkConnection_TokenRingNode --
//     Build a node on a token ring.
//
//     WARNING:  there must be a node with a NODE_ID of 0.  The token starts
//               on this node.
//
module [Connected_Module] mkConnection_TokenRingNode#(Integer chainNum,
                                                      t_NODE_ID myID)
    // Interface:
    (Connection_TokenRing#(t_NODE_ID, t_MSG))
    provisos (Bits#(t_MSG, t_MSG_SZ),
              Bits#(t_NODE_ID, t_NODE_ID_SZ),
              Eq#(t_NODE_ID),
       
              Alias#(TOKEN_RING_MSG#(t_NODE_ID, t_MSG), t_RING_MSG),

              // Message fits in a ring?
              Bits#(t_RING_MSG, t_RING_MSG_SZ),
              Add#(t_RING_MSG_SZ, m__, CON_CHAIN_DATA_SZ));

    // Allocate a node on the physical chain
    Connection_Chain#(t_RING_MSG) chain <- mkConnection_Chain(chainNum);

    // Inbound & outbound FIFOs
    FIFOF#(t_MSG) recvQ <- mkFIFOF();
    FIFOF#(Tuple2#(t_NODE_ID, t_MSG)) sendQ <- mkFIFOF();

    COUNTER#(1) sawToken <- mkLCounter(0);
    COUNTER#(1) haveToken <- mkLCounter(0);

    PulseWire localSentMsg <- mkPulseWire();
    PulseWire forwardMsg <- mkPulseWire();


    //
    // Initialization -- Only NODE_ID 0 has initialization (sends the token)
    //
    Reg#(Bool) didInit <- mkReg(pack(myID) != 0);

    rule doInit (! didInit);
        didInit <= True;
        chain.sendToNext(TOKEN_RING_MSG { token: True, data: tagged Invalid });
    endrule


    //
    // newMsgIsForMe --
    //     Does incoming message on the ring have data for this node?
    //
    function Bool newMsgIsForMe();
        if (chain.peekFromPrev().data matches tagged Valid {.tgt, .msg})
            return tgt == myID;
        else
            return False;
    endfunction


    //
    // recvFromRing --
    //     Receive a new message from the ring destined for this node.
    //
    rule recvFromRing (newMsgIsForMe());
        let r <- chain.recvFromPrev();
        
        // Does the message have the token?
        if (r.token)
        begin
            haveToken.up();
        end

        recvQ.enq(tpl_2(validValue(r.data)));
    endrule


    //
    // sendToRing --
    //     This node has a new message for the ring and has permission to send
    //     either because it has seen the token since last sending or there is
    //     no message to forward.
    //
    rule sendToRing (didInit &&
                     ((sawToken.value() != 0) ||
                      ! chain.recvNotEmpty() ||
                      newMsgIsForMe()));
        t_RING_MSG r;

        // Is this node holding the token?  Forward it.
        r.token = (haveToken.value() != 0);
        if (r.token)
        begin
            haveToken.down();
        end

        // Clear the local token
        if (sawToken.value() != 0)
        begin
            sawToken.down();
        end

        r.data = tagged Valid sendQ.first();
        sendQ.deq();

        chain.sendToNext(r);
        localSentMsg.send();
    endrule


    //
    // forwardOnRing --
    //     Local node did not send a message this cycle and message coming
    //     from ring is not for this node.  Is there a message to forward?
    //
    rule forwardOnRing (didInit && ! localSentMsg && ! newMsgIsForMe());
        let r <- chain.recvFromPrev();

        if (r.token)
        begin
            sawToken.setC(1);
        end

        chain.sendToNext(r);
        forwardMsg.send();
    endrule


    //
    // forwardTokenOnRing --
    //     No ring activity this cycle.  Is this node holding the token?  If
    //     so, send the token on to the next hop.
    //
    rule forwardTokenOnRing (didInit && ! localSentMsg && ! forwardMsg &&
                             (haveToken.value() != 0));
        t_RING_MSG r;
        r.token = True;
        r.data = tagged Invalid;

        chain.sendToNext(r);
        sawToken.setC(1);
        haveToken.down();
    endrule


    //
    // Methods...
    //

    //
    // Outbound messages
    //
    method Action enq(t_NODE_ID dstNode, t_MSG data);
        sendQ.enq(tuple2(dstNode, data));
    endmethod

    method Bool notFull() = sendQ.notFull();


    //
    // Incoming messages
    //
    method t_MSG first() = recvQ.first();

    method Action deq();
        recvQ.deq();
    endmethod

    method Bool notEmpty() = recvQ.notEmpty();
endmodule

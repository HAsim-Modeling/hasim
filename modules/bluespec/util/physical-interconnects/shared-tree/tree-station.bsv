//
// Copyright (C) 2008 Massachusetts Institute of Technology
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

import List::*;
import FIFOF::*;

// tree-station.bsv

// The actual physical tree station which routes between parent and children.


module mkPhysicalStation#(List#(PHYSICAL_STATION) children, 
                          ROUTING_TABLE routing_table)
    // interface:
        (PHYSICAL_STATION);

    // Some conveniences

    let numChildren   = List::length(children);

    // ****** Local State ****** //

    // Signal successful transmission from the parent to me.
    RWire#(MESSAGE_DOWN)           fromParentTry <- mkRWire();
    PulseWire                      fromParentAck <- mkPulseWire();

    // Queues leading from the incoming connections.
    FIFOF#(Tuple2#(ROUTING_DECISION, PHYSICAL_PAYLOAD)) fromParentQ <- mkFIFOF();
    FIFOF#(Tuple2#(ROUTING_DECISION, PHYSICAL_PAYLOAD)) fromChildQ  <- mkFIFOF();
    
    // Queues leading to the outgoing connections.
    FIFOF#(MESSAGE_UP)                           toParentQ <- mkFIFOF();
    FIFOF#(Tuple2#(CHILD_IDX, MESSAGE_DOWN))     toChildQ  <- mkFIFOF();

    // Wires to signal if we're trying to transmit to a child.
    List#(PulseWire)  childTry       = List::nil;
    
    // Multicasting scoreboarding.
    Reg#(PHYSICAL_PAYLOAD) multiPayload <- mkRegU();
    List#(Reg#(Maybe#(LOCAL_DST))) multiChildNeed = List::nil;
    List#(PulseWire)  multiChildTry  = List::nil;

    // Instantiate all lists.

    for (Integer x = 0; x < numChildren; x = x + 1)
    begin
        let childTryW <- mkPulseWire();
        childTry = List::cons(childTryW, childTry);
        let multiChildNeedR <- mkReg(Invalid);
        multiChildNeed = List::cons(multiChildNeedR, multiChildNeed);
        let multiChildTryW <- mkPulseWire();
        multiChildTry = List::cons(multiChildTryW, multiChildTry);
    end


    // ****** Helper functions ****** //
    
    // multicasting
    
    // Returns true if we are attempting to send a multicast to any child station.    

    function Bool multicasting();
    
        Bool res = False;
    
        for (Integer x = 0; x < numChildren; x = x + 1)
        begin
            res = res || isValid(multiChildNeed[x]);
        end
        
        return res;
    
    endfunction

    // anyChildTry
    
    // Returns true if any child has data to transmit.

    function Bool anyChildTry();
    
        Bool res = False;
        for (Integer x = 0; x < numChildren; x = x + 1)
        begin
            res = res || (children[x].outgoing.notEmpty());
        end
        
        return res;
    
    endfunction

    // anyChildAck
    
    // Returns true if we tried to send to a child and that child ack'd.

    function Bool anyChildAck();
    
        Bool res = False;
        for (Integer x = 0; x < numChildren; x = x + 1)
        begin
            res = res || (childTry[x] && children[x].incoming.success());
        end
        
        return res;
    
    endfunction


    // ****** Rules ****** //

    // toChildTry
    
    // When:   We have something to route to a specific child. (And there's no multicast in progress.)
    // Effect: Try to transmit the data. Note which guy we're sending to using a wire.

    rule toChildTry (toChildQ.first() matches {.child_idx, .msg} &&& !isValid(multiChildNeed[child_idx]));

        children[child_idx].incoming.try(msg);
        childTry[child_idx].send();

    endrule

    
    // toChildAck
    
    // When:   Later in the same clock cycle after toChildTry, and we receive an Ack.
    // Effect: Transmission was successful so we can dequeue the FIFO.

    rule toChildAck (anyChildAck());

       toChildQ.deq();

    endrule 

    
    // toChildMultiTry
    
    // When:   We are multicasting to children. Urgency ensures that multicasts are statically favored.
    // Effect: Try to transmit to those children which need it simultaneously.

    (* descending_urgency="toChildMultiTry, toChildTry" *)
    rule toChildMultiTry (multicasting);

        for (Integer x = 0; x < numChildren; x = x + 1)
        begin
            if (multiChildNeed[x] matches tagged Valid .dst)
            begin

                let msg = MESSAGE_DOWN
                          {
                              destination: dst,
                              payload: multiPayload
                          };

                children[x].incoming.try(msg);
                multiChildTry[x].send();
            end
        end
    
    endrule


    // toChildMultiAck
    
    // When:   Later in the clock cycle after toChildMultiTry. 
    // Effect: All successful transmissions are marked as no longer needed.

    rule toChildMultiAck (multicasting);

        for (Integer x = 0; x < numChildren; x = x + 1)
        begin
            if (multiChildTry[x])
            begin
                if (children[x].incoming.success())
                begin
                    multiChildNeed[x] <= tagged Invalid;
                end
            end
        end
    
    endrule


    // fromChild

    // When:   Any child has data to transmit.
    // Effect: Make a routing decision, enqueue into fromChildQ and deq the data.
    //         Note that this has no fairness currently.

    rule fromChild (anyChildTry());
    
        Maybe#(Integer) winner = Invalid;
      
        for (Integer x = 0; x < numChildren; x = x + 1)
        begin
        
            if (children[x].outgoing.notEmpty())
            begin
                winner = tagged Valid x;
            end
        
        end
        
        if (winner matches tagged Valid .idx)
        begin
            let msg = children[idx].outgoing.first();
            children[idx].outgoing.deq();
            let route_child = routing_table.fromChild[idx];
            if (List::length(route_child) == 0)
            begin
                $display("Station routing error: received a send from an unrouted receive-only station %0d, origin %0d", idx, msg.origin);
                $finish(1);
            end
            else
            begin
                let route_dst = route_child[msg.origin];
                fromChildQ.enq(tuple2(route_dst, msg.payload));
            end
        end
    
    endrule


    // fromParentQToParentQ
    
    // When:   When something really bad has happened: a message from our 
    //         parent which we think should go back to our parent.
    // Effect: End the world and yell.
    
    rule fromParentQToParentQ (fromParentQ.first() matches {.route_dst, .payload} &&&
                               route_dst matches tagged ROUTE_parent .orig);
    
        $display("Error: Station received message from parent to parent origin %0d.", orig);
        $finish(1);
        fromParentQ.deq();
    
    endrule


    // fromParentQToChildQ

    // When:   A mesage from our parent goes to a child, and is not a multicast.
    // Effect: Move it to the appropriate queue.
    // Note:   This intermediate queue could be removed later to reduce latency.

    rule fromParentQToChildQ (fromParentQ.first() matches {.route_dst, .payload} &&&
                              route_dst matches tagged ROUTE_child {.child_idx, .dst});
    
        let msg = MESSAGE_DOWN
                  {
                      destination: dst,
                      payload: payload
                  };

        toChildQ.enq(tuple2(child_idx, msg));
        fromParentQ.deq();
    
    endrule


    // fromParentQBeginMulticast
    
    // When:   A message from our parent is a multicast, and we're not already multicasting.
    // Effect: Begin the multicast by marking which children need this payload.
    //         This set is determined by the specific logical recv they are sending to.
    // Note:   If this multicast is marked to go back to the parent, something is really wrong.

    rule fromParentQBeginMulticast (fromParentQ.first() matches {.route_dst, .payload} &&&
                                    route_dst matches tagged ROUTE_multicast .multi_idx &&&
                                    !multicasting);
        
        let info = routing_table.fromMulti[multi_idx];
    
        for (Integer x = 0; x < numChildren; x = x + 1)
        begin

            multiChildNeed[x] <= info.childrenNeed[x];

        end

        if (info.parentNeed matches tagged Valid .src)
        begin
            $display("Error: Station received multicast from parent to parent, origin %0d", src);
            $finish(1);
        end

        multiPayload <= payload;
        fromParentQ.deq();

    endrule


    // fromChildQToParentQ
    
    // When:   A message from our child should go to our parent.
    // Effect: Move it to the appropriate queue.
    // Note:   In the future this intermediate queue could be removed to reduce latency.

    rule fromChildQToParentQ (fromChildQ.first() matches {.route_dst, .payload} &&&
                              route_dst matches tagged ROUTE_parent .orig);
    
        let msg = MESSAGE_UP
                  {
                      origin:  orig,
                      payload: payload
                  };

        toParentQ.enq(msg);
        fromChildQ.deq();
    
    endrule


    // fromChildQToChildQ
    
    // When:   A message from one of our children should be routed to a different child.
    //         (Messages from the parent are statically favored in urgency.)
    // Effect: Put it in the appropriate queue.
    // Note:   This intermediate queue could be removed to reduce latency.

    (* descending_urgency="fromParentQToChildQ, fromChildQToChildQ" *)
    rule fromChildQToChildQ (fromChildQ.first() matches {.route_dst, .payload} &&&
                             route_dst matches tagged ROUTE_child {.child_idx, .dst});
    
        let msg = MESSAGE_DOWN
                  {
                      destination: dst,
                      payload: payload
                  };

        toChildQ.enq(tuple2(child_idx, msg));
        fromChildQ.deq();
    
    endrule


    // fromChildQBeginMulticast

    // When:   A message from a child is a multicast, and we're not already multicasting.
    //         (Non-multicast messages are statically favored, as are multicasts from the parent.)
    // Effect: Begin the multicast by marking which children need this payload.
    //         This set is determined by the specific logical recv they are sending to.
    //         If this multicast is marked to go back to the parent, enqueue it now.

    (* descending_urgency="fromChildQToParentQ, fromParentQBeginMulticast, fromChildQBeginMulticast" *)
    rule fromChildQBeginMulticast (fromChildQ.first() matches {.route_dst, .payload} &&&
                                   route_dst matches tagged ROUTE_multicast .multi_idx &&&
                                   !multicasting);
        
        let info = routing_table.fromMulti[multi_idx];
    
        for (Integer x = 0; x < numChildren; x = x + 1)
        begin

            multiChildNeed[x] <= info.childrenNeed[x];

        end

        if (info.parentNeed matches tagged Valid .orig)
        begin
            let msg = MESSAGE_UP
                      {
                          origin:  orig,  
                          payload: payload
                      };

            toParentQ.enq(msg);
        end
        multiPayload <= payload;
        fromChildQ.deq();

    endrule


    // fromParent
    
    // When:   The parent is trying to send us a message.
    // Effect: Do the routing, put it in the queue, and ack the message.

    rule fromParent (fromParentTry.wget() matches tagged Valid .msg);
    
        let route_dst = routing_table.fromParent[msg.destination];
        fromParentQ.enq(tuple2(route_dst, msg.payload));
        fromParentAck.send();
    
    endrule


    // ******* Methods ******* //
    
    // These just interact with our queues and wires.
    
    interface PHYSICAL_CONNECTION_IN incoming;

        method Action try(MESSAGE_DOWN msg);
            fromParentTry.wset(msg);
        endmethod
        
        method Bool success() = fromParentAck;

    endinterface

    interface PHYSICAL_CONNECTION_OUT outgoing;

       method MESSAGE_UP first() = toParentQ.first();
       method Bool notEmpty()  = toParentQ.notEmpty();
       method Action deq() = toParentQ.deq();

    endinterface

endmodule
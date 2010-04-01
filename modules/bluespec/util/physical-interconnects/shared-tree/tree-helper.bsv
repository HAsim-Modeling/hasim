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


// tree-helper.bsv

// Convenient helper functions for the tree network.


// mkEmptyRoot

// Rather than have a special case for the root node, it's easier to just 
// wrap the root in something that drops all incoming requests.
// Note that these requests should only originate from multicasts.

module mkEmptyRoot#(PHYSICAL_STATION st) ();

    rule dropInc (st.outgoing.notEmpty);
        st.outgoing.deq();
    endrule

endmodule

// checkConnectionTypes

// When connecting two logical connections, test if they were originally the same type.

module checkConnectionTypes#(LOGICAL_SEND_INFO send_info, LOGICAL_RECV_INFO recv_info) ();

    if (send_info.logicalType != recv_info.logicalType)
    begin
        messageM("Mismatched types for connection " + send_info.logicalName); 
        messageM("        Send type: " + send_info.logicalType);
        messageM("     Receive type: " + recv_info.logicalType);
        return error("Connection type error.");
    end

endmodule


// checkMultiConnectionTypes

// When we find two listeners to the same multicast, make sure they are the same type.

module checkMultiConnectionTypes#(LOGICAL_RECV_INFO recv_info1, LOGICAL_RECV_INFO recv_info2) ();

    if (recv_info1.logicalType != recv_info2.logicalType)
    begin
        messageM("Inconsistent types for one-to-many connection " + recv_info1.logicalName);
        messageM("       First type: " + recv_info1.logicalType);
        messageM("      Second type: " + recv_info2.logicalType);
        return error("Connection type error.");
    end

endmodule


// Printing Functions

// Various functions for printing debugging information

module printStationInfo#(PHYSICAL_STATION_INFO info) ();

    messageM("Outgoing Info:");

    for (Integer x = 0; x < length(info.outgoingInfo); x = x + 1)
    begin
        let sendInfo = info.outgoingInfo[x];
        messageM(sendInfo.logicalName);
    
    end

    messageM("Incoming Info:");

    for (Integer x = 0; x < length(info.incomingInfo); x = x + 1)
    begin
        let recvInfo = info.incomingInfo[x];
        messageM(recvInfo.logicalName);
    
    end

    messageM("From Parent:");

    for (Integer x = 0; x < length(info.routingTable.fromParent); x = x + 1)
    begin
        let parentInfo = info.routingTable.fromParent[x];
        printRoutingDecision(parentInfo);
    
    end

    messageM("From Children:");

    if (length(info.routingTable.fromChild) == 0)
        messageM("<NIL>");

    for (Integer x = 0; x < length(info.routingTable.fromChild); x = x + 1)
    begin
        messageM("From Child:");
        let childInfo = info.routingTable.fromChild[x];
        if (length(childInfo) == 0)
            messageM("<NIL>");
        for (Integer y = 0; y < length(childInfo); y = y + 1)
        begin
            printRoutingDecision(childInfo[y]);
        end
    
    end

    messageM("From Multi:");

    if (length(info.routingTable.fromMulti) == 0)
        messageM("<NIL>");

    for (Integer x = 0; x < length(info.routingTable.fromMulti); x = x + 1)
    begin
        let multiInfo = info.routingTable.fromMulti[x];
        printMulticastInfo(multiInfo);
    
    end
endmodule

module printMulticastInfo#(MULTICAST_INFO info) ();

    messageM("Children Need:");

    for (Integer x = 0; x < length(info.childrenNeed); x = x + 1)
    begin
        case (info.childrenNeed[x]) matches
            tagged Invalid: messageM("N");
            tagged Valid .d: messageM("Y " + bitToString(d));
        endcase
    end
    
    if (info.parentNeed matches tagged Valid .d)
    begin
        messageM("Parent Needs " + bitToString(d));
    end

endmodule

module printRoutingDecision#(ROUTING_DECISION r) ();

    case (r) matches
        tagged ROUTE_parent .d: messageM("ToParent " + bitToString(d));
        tagged ROUTE_child {.c, .d}: messageM("ToChild " + bitToString(c) + " " + bitToString(d));
        tagged ROUTE_multicast .midx: messageM("Multi " + bitToString(midx));
    endcase

endmodule

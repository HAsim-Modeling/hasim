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


// tree-interface.bsv

// Interface definitions and typedefs for the tree interconnect topology.

`include "asim/provides/soft_connections.bsh"

// Parameters from AWB.
typedef Bit#(`TREE_STATION_IDX_SIZE)  STATION_IDX;
typedef Bit#(40) PHYSICAL_PAYLOAD;

// Some handy typedefs.
typedef STATION_IDX CHILD_IDX;
typedef STATION_IDX LOCAL_SRC;
typedef STATION_IDX LOCAL_DST;
typedef STATION_IDX MULTICAST_IDX;

// MESSAGE_UP

// The type of message which flows up a tree, from child to parent.
// These are distinguished by origin.
// Example: Child #3 is sending something. Parent knows #3 is logical send "fet2dec" and can route appropriately.

typedef struct
{
    LOCAL_SRC origin;
    PHYSICAL_PAYLOAD payload;
}
    MESSAGE_UP
        deriving (Eq, Bits);


// MESSAGE_DOWN

// Messages which flow down the tree, from parent to child.
// These are distinguished by destination, which is an index into the local routing table.
// IE parent knows that child's destination #3 is logical "fet2dec", so it can produce messages to that location.

// A message_up can result in one or more message_down, but message_down
// cannot produce a message_up, since we route things up to the least-common-ancestor.

typedef struct
{
    LOCAL_DST destination;
    PHYSICAL_PAYLOAD payload;
}
    MESSAGE_DOWN
        deriving (Eq, Bits);

// A physical incoming connection
interface PHYSICAL_STATION_IN;

  method Action enq(MESSAGE_DOWN d);

endinterface

// A physical outgoing connection
interface PHYSICAL_STATION_OUT;

  method Bool notEmpty();
  method MESSAGE_UP first();
  method Action deq();

endinterface

// PHYSICAL_STATION

// At the end of the day a physical station is just the wires which the parent interarcts with.

interface PHYSICAL_STATION;

    interface PHYSICAL_STATION_IN  incoming;
    interface PHYSICAL_STATION_OUT   outgoing;

endinterface


// ROUTING_DECISION

// A local decision about where a message should be sent to.

typedef union tagged
{
    Tuple2#(CHILD_IDX, LOCAL_DST)    ROUTE_child;     // Send to child #x, dest #y
    LOCAL_SRC                        ROUTE_parent;    // Send to parent, giving it origin #x
    MULTICAST_IDX                    ROUTE_multicast; // It goes to multiple people. Look in the multicast table to see who.
}
    ROUTING_DECISION
        deriving (Eq, Bits);


// ROUTING_TABLE

// A routing table is all the information a tree-station needs to route mesages from parents and children.

//    fromParent[x] is how to route MESSAGE_DOWNs with dst x.
//    fromChild[x] is how to route MESSAGE_UPs from child x.
//        Thus fromChild[x][y] is how to route MESSAGE_UPS with origin y.
//    fromMulti[x] is the information for ROUTE_multicast x.

typedef struct
{
    List#(ROUTING_DECISION)        fromParent;
    List#(List#(ROUTING_DECISION)) fromChild;
    List#(MULTICAST_INFO)          fromMulti;
}
    ROUTING_TABLE;


// MULTICAST_INFO

// Shows which children should get this message, and if it should be sent to the parent.
//    If a child is to get it, shows which dst should we send it to.
//    If the parent is to get it, shows which origin we should give.

typedef struct
{
    List#(Maybe#(LOCAL_DST)) childrenNeed;
    Maybe#(LOCAL_SRC)   parentNeed;
}
    MULTICAST_INFO;


// PHYSICAL_STATION_INFO

// All the information about a tree station.
//     outgoingInfo[x] shows information on MESSAGE_UP x (used by parent for receives)
//     incomingInfo[x] shows information on MESSAGE_DOWN x (used by parent for sends)

typedef struct
{
    ROUTING_TABLE routingTable;
    List#(LOGICAL_SEND_INFO) outgoingInfo;
    List#(LOGICAL_RECV_INFO) incomingInfo;
}
    PHYSICAL_STATION_INFO;


// LOGICAL_{SEND,RECV}_MAP

// A logical map combines the logical information with a routing decision.

typedef List#(Tuple2#(LOGICAL_RECV_INFO, ROUTING_DECISION)) LOGICAL_RECV_MAP;
typedef List#(Tuple2#(LOGICAL_RECV_INFO, ROUTING_DECISION)) LOGICAL_SEND_MAP;


// LOGICAL_MAP

// The map you get by grouping together all of a station's logical receives.
// This can include multicast receives, so these are grouped separately.

typedef struct
{
    LOGICAL_RECV_MAP recvMap;
    List#(MULTICAST_INFO) fromMulti;
}
    LOGICAL_MAP;

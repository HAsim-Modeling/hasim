`include "asim/provides/soft_connections_common.bsh"

// This file deals with connections that are logical point-to-point connections, but that
// are actually implemented via a shared physical interconnect such as a ring or tree.

// These interconnects consist of stations. Shared connections register themselves to particular
// stations. Later the stations themselves are connected into a particular physical topology,
// and a routing table between them is determined.


// Create a new station for a logical tree. If there's already
// a station out there, become it's child. Otherwise we are
// the root node.

module [SoftConnectionModule] mkStationLocal#(String station_name)
    // interface:
        (STATION);

    registerStation(station_name, "TREE", "0");

    // If there's a parent station in existence, then add this as a child.
    let currM <- getCurrentStationM();
    
    if (currM matches tagged Valid .parent)
    begin
        registerChildToStation(parent.name, station_name);
    end

    method String name() = station_name;

endmodule

// 1-to-1 logical connection that is implemented via a shared interconnect.
module [SoftConnectionModule] mkConnectionSendShared#(String name, STATION station) (CONNECTION_SEND#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- mkPhysicalConnectionSend(name, tagged Valid station, False, False);
   return m;

endmodule

module [SoftConnectionModule] mkConnectionRecvShared#(String name, STATION station) (CONNECTION_RECV#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- mkPhysicalConnectionRecv(name, tagged Valid station, False, False);
   return m;

endmodule

module [SoftConnectionModule] mkConnectionClientShared#(String name, STATION station) (CONNECTION_CLIENT#(t_REQ, t_RSP))
    provisos
        (Bits#(t_REQ, t_REQ_SIZE),
         Bits#(t_RSP, t_RSP_SIZE),
         Transmittable#(t_REQ),
         Transmittable#(t_RSP));

   let m <- mkPhysicalConnectionClient(name, tagged Valid station, False, False);
   return m;

endmodule

module [SoftConnectionModule] mkConnectionClientMultiShared#(String name, STATION station) (CONNECTION_CLIENT#(t_REQ, t_RSP))
    provisos
        (Bits#(t_REQ, t_REQ_SIZE),
         Bits#(t_RSP, t_RSP_SIZE),
         Transmittable#(t_REQ),
         Transmittable#(t_RSP));

   let m <- mkPhysicalConnectionClient(name, tagged Valid station, True, False);
   return m;

endmodule

module [SoftConnectionModule] mkConnectionServerShared#(String name, STATION station) (CONNECTION_SERVER#(t_REQ, t_RSP))
    provisos
        (Bits#(t_REQ, t_REQ_SIZE),
         Bits#(t_RSP, t_RSP_SIZE),
         Transmittable#(t_REQ),
         Transmittable#(t_RSP));

   let m <- mkPhysicalConnectionServer(name, tagged Valid station, False, False);
   return m;

endmodule

module [SoftConnectionModule] mkConnectionServerMultiShared#(String name, STATION station) (CONNECTION_SERVER#(t_REQ, t_RSP))
    provisos
        (Bits#(t_REQ, t_REQ_SIZE),
         Bits#(t_RSP, t_RSP_SIZE),
         Transmittable#(t_REQ),
         Transmittable#(t_RSP));

   let m <- mkPhysicalConnectionServer(name, tagged Valid station, True, False);
   return m;

endmodule

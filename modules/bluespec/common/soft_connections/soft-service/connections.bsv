import FIFOF::*;

//-------------------- Soft Connections --------------------------//
//                                                                //
// This file contains the user-visible interfaces and modules     //
// for Soft Connections, which are conected by name at compilation//
// time.                                                          //
//----------------------------------------------------------------//


// The basic sending half of a connection.

interface CONNECTION_SEND#(type t_MSG);
  
  method Action send(t_MSG data);
  method Bool notFull();
  
endinterface


// The basic receiving connection.

interface CONNECTION_RECV#(type t_MSG);
  
  method Action deq();
  method Bool   notEmpty();
  method t_MSG  receive();

endinterface


// A client sends requests and receives responses
// (which may not come instantly)

interface CONNECTION_CLIENT#(type t_REQ, type t_RSP);

  method Action makeReq(t_REQ data);
  method Bool   reqNotFull();

  method Bool   rspNotEmpty();
  method t_RSP  getRsp();
  method Action deq();
  
endinterface


// A server receives requests and gives back responses
// It can take any amount of time, and there is no assumption
// that the responses are FIFO.

interface CONNECTION_SERVER#(type t_REQ, type t_RSP);

  method t_REQ  getReq();
  method Bool   reqNotEmpty();
  method Action deq();

  method Action makeRsp(t_RSP data);
  method Bool   rspNotFull();
  
endinterface


// Connection Constructors

// These are implemented as parameters to the actual constructor later.


// Base 1-to-1 send
module [ConnectedModule] mkConnectionSend#(String name) (CONNECTION_SEND#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- liftSCM(mkPhysicalConnectionSend(name, Invalid, False, False));
   return m;

endmodule

// 1-to-1 send that is optional. (No error if unconnected at top level.)
module [ConnectedModule] mkConnectionSendOptional#(String name) (CONNECTION_SEND#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- liftSCM(mkPhysicalConnectionSend(name, Invalid, False, True));
   return m;

endmodule

// 1-to-Many send. These are always optional.
module [ConnectedModule] mkConnectionBroadcast#(String name) (CONNECTION_SEND#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- liftSCM(mkPhysicalConnectionSend(name, Invalid, True, True));
   return m;

endmodule

// 1-to-Many send using a Station. These are always optional.
module [ConnectedModule] mkConnectionBroadcastShared#(String name, STATION station) (CONNECTION_SEND#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- liftSCM(mkPhysicalConnectionSend(name, tagged Valid station, True, True));
   return m;

endmodule

// Base 1-to-1 Receive.
module [ConnectedModule] mkConnectionRecv#(String name) (CONNECTION_RECV#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- liftSCM(mkPhysicalConnectionRecv(name, Invalid, False, False));
   return m;

endmodule

// 1-to-1 Receive that is optional. (No error if unconnected at top level.)
module [ConnectedModule] mkConnectionRecvOptional#(String name) (CONNECTION_RECV#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- liftSCM(mkPhysicalConnectionRecv(name, Invalid, False, True));
   return m;

endmodule

// Many-to-1 receive. These are always optional.
module [ConnectedModule] mkConnectionListener#(String name) (CONNECTION_RECV#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- liftSCM(mkPhysicalConnectionRecv(name, Invalid, True, True));
   return m;

endmodule

// Many-to-1 listener using a station.
module [ConnectedModule] mkConnectionListenerShared#(String name, STATION station) (CONNECTION_RECV#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- liftSCM(mkPhysicalConnectionRecv(name, tagged Valid station, True, True));
   return m;

endmodule

// Base Client: send requests to 1 server, receive responses.
module [ConnectedModule] mkConnectionClient#(String name) (CONNECTION_CLIENT#(t_REQ, t_RSP))
    provisos
        (Bits#(t_REQ, t_REQ_SIZE),
         Bits#(t_RSP, t_RSP_SIZE),
         Transmittable#(t_REQ),
         Transmittable#(t_RSP));

   let m <- liftSCM(mkPhysicalConnectionClient(name, Invalid, False, False));
   return m;

endmodule

// Client of Many Servers. Connect to as many servers as are out there. Broadcast requests to all of them.
// These are always optional.
module [ConnectedModule] mkConnectionClientMulti#(String name) (CONNECTION_CLIENT#(t_REQ, t_RSP))
    provisos
        (Bits#(t_REQ, t_REQ_SIZE),
         Bits#(t_RSP, t_RSP_SIZE),
         Transmittable#(t_REQ),
         Transmittable#(t_RSP));

   let m <- liftSCM(mkPhysicalConnectionClient(name, Invalid, True, False));
   return m;

endmodule


// Base Server: receive requests from 1 client, make responses.
module [ConnectedModule] mkConnectionServer#(String name) (CONNECTION_SERVER#(t_REQ, t_RSP))
    provisos
        (Bits#(t_REQ, t_REQ_SIZE),
         Bits#(t_RSP, t_RSP_SIZE),
         Transmittable#(t_REQ),
         Transmittable#(t_RSP));

   let m <- liftSCM(mkPhysicalConnectionServer(name, Invalid, False, False));
   return m;

endmodule

// Server of Many Clients. Connect to as many clients as are out there. Accept requests from
// them and send the response only to the requester.
module [ConnectedModule] mkConnectionServerMulti#(String name) (CONNECTION_SERVER#(t_REQ, t_RSP))
    provisos
        (Bits#(t_REQ, t_REQ_SIZE),
         Bits#(t_RSP, t_RSP_SIZE),
         Transmittable#(t_REQ),
         Transmittable#(t_RSP));

   let m <- liftSCM(mkPhysicalConnectionServer(name, Invalid, True, False));
   return m;

endmodule


// The actual instatiation of a physical send. Contains a FIFO.

module [SoftConnectionModule] mkPhysicalConnectionSend#(String send_name, Maybe#(STATION) m_station, Bool oneToMany, Bool optional)
    //interface:
                (CONNECTION_SEND#(t_MSG))
    provisos
            (Bits#(t_MSG, t_MSG_SIZE),
	     Transmittable#(t_MSG));

    // Local Clock and reset
    Clock localClock <- exposeCurrentClock();
    Reset localReset <- exposeCurrentReset();

    // ****** Local State ****** //

    // This queue could be turned into a BypassFIFO to reduce latency. 
    FIFOF#(t_MSG) q <- mkFIFOF();
    
    // Bind a local interface to a name for convenience.
    let outg = (interface PHYSICAL_CONNECTION_OUT;

                 // Trying to transmit something means marshalling up the first thing in the queue.
	         method PHYSICAL_CONNECTION_DATA first() = marshall(q.first());
                 
                 // Sometimes its useful to have an explicit notEmpty
                 method Bool notEmpty() = q.notEmpty();
                 
                 // If we were successful we can dequeue.
	         method Action deq() = q.deq();

                 interface Clock clock = localClock;
                 interface Reset reset = localReset;

	       endinterface);

    // Figure out my type for typechecking.
    t_MSG msg = ?;
    String my_type = printType(typeOf(msg));

    // Collect up our info.
    let info = 
        LOGICAL_SEND_INFO 
        {
            logicalName: send_name, 
            logicalType: my_type, 
            oneToMany: oneToMany, 
            optional: optional, 
            outgoing: outg
        };

    // Is this a shared connection?
    if (m_station matches tagged Valid .station)
    begin

        // Yes, so register ourselves with that station.
        registerSendToStation(info, station.name);

    end
    else
    begin

        // Nope, so just register and try to find a match.
        registerSend(info);

    end


    // ****** Interface to User ****** //

    // send
    
    // Our send method just sticks things in the queue.

    method Action send(t_MSG data);

        q.enq(data);

    endmethod

    method Bool notFull() = q.notFull();

endmodule


// The actual instantation of the physical receive. Just contains wires.

module [SoftConnectionModule] mkPhysicalConnectionRecv#(String recv_name, Maybe#(STATION) m_station, Bool manyToOne, Bool optional)
    //interface:
                (CONNECTION_RECV#(t_MSG))
    provisos
            (Bits#(t_MSG, t_MSG_SIZE),
	     Transmittable#(t_MSG));

    // Local Clock and reset
    Clock localClock <- exposeCurrentClock();
    Reset localReset <- exposeCurrentReset();

    // ****** Local State ****** //

    // Some wires for interacting with our counterpart.
    PulseWire      enW    <- mkPulseWire();
    RWire#(t_MSG)  dataW  <- mkRWire();

    // Bind a local interface to a name for convenience.
    let inc = (interface PHYSICAL_CONNECTION_IN;

                 // Part 1 of the anti-method to get(). Unmarshall the data and send it on the wire.
	         method Action try(x);
	           dataW.wset(unmarshall(x));
	         endmethod

	         // Part 2 of the anti-method to get(). If someone actually was listening, then the get() succeeded.
                 method Bool success();
	           return enW;
	         endmethod

                 interface Clock clock = localClock;
                 interface Reset reset = localReset;

	       endinterface);

    //Figure out my type for typechecking
    t_MSG msg = ?;
    String my_type = printType(typeOf(msg));

    // Collect up our info.
    let info = 
        LOGICAL_RECV_INFO 
        {
            logicalName: recv_name, 
            logicalType: my_type, 
            manyToOne: manyToOne, 
            optional: optional, 
            incoming: inc
        };

    // Is this a shared connection?
    if (m_station matches tagged Valid .station)
    begin

        // Yes, but we don't do any actual connecting now, because if our counterpart registers with the same station
        // we become point-to-point.
        registerRecvToStation(info, station.name);

    end
    else
    begin

        // Nope, so just register and try to find a match.
        registerRecv(info);

    end


    // ****** Interface ****** //

    // receive
    
    // If someone's trying to transmit, then our listener can get it here.

    method t_MSG receive() if (dataW.wget() matches tagged Valid .val);

        return val;

    endmethod


    // notEmpty
    
    // The user can also see if anyone's trying to transmit at all.

    method Bool notEmpty();

        return isValid(dataW.wget());

    endmethod


    // deq
    
    // If the user is done with this message we send a deq() back.

    method Action deq() if (dataW.wget() matches tagged Valid .val);

        enW.send();

    endmethod

endmodule


// A convenience which bundles up sending and receiving

module [SoftConnectionModule] mkPhysicalConnectionClient#(String client_name, Maybe#(STATION) shared, Bool multicast, Bool optional)
    //interface:
                (CONNECTION_CLIENT#(t_REQ, t_RSP))
    provisos
            (Bits#(t_REQ, t_REQ_SIZE),
	     Bits#(t_RSP, t_RSP_SIZE),
	     Transmittable#(t_REQ),
	     Transmittable#(t_RSP));

    let send_name = client_name + "_req";
    let recv_name = client_name + "_rsp";

    Connection_Send#(t_REQ)    reqConn <- mkPhysicalConnectionSend(send_name, shared, multicast, optional);
    Connection_Receive#(t_RSP) rspConn <- mkPhysicalConnectionRecv(recv_name, shared, multicast, optional);

    method Action makeReq(t_REQ data);
      reqConn.send(data);
    endmethod

    method Bool reqNotFull() = reqConn.notFull();

    method t_RSP getRsp();
      return rspConn.receive();
    endmethod

    method Bool rspNotEmpty();
      return rspConn.notEmpty();
    endmethod

    method Action deq();
      rspConn.deq();
    endmethod

endmodule

module [SoftConnectionModule] mkPhysicalConnectionServer#(String server_name, Maybe#(STATION) shared, Bool multicast, Bool optional)
    //interface:
                (CONNECTION_SERVER#(t_REQ, t_RSP))
    provisos
            (Bits#(t_REQ, t_REQ_SIZE),
	     Bits#(t_RSP, t_RSP_SIZE),
	     Transmittable#(t_REQ),
	     Transmittable#(t_RSP));

    let send_name = server_name + "_rsp";
    let recv_name = server_name + "_req";

    Connection_Receive#(t_REQ) reqConn <- mkPhysicalConnectionRecv(recv_name, shared, multicast, optional);
    Connection_Send#(t_RSP)    rspConn <- mkPhysicalConnectionSend(send_name, shared, multicast, optional);

    method Action makeRsp(t_RSP data);
      rspConn.send(data);
    endmethod
    
    method Bool rspNotFull() = rspConn.notFull();

    method t_REQ getReq();
      return reqConn.receive();
    endmethod

    method Bool reqNotEmpty();
      return reqConn.notEmpty();
    endmethod

    method Action deq();
      reqConn.deq();
    endmethod

endmodule

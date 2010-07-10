import FIFOF::*;
import Vector::*;
import Connectable::*;
import GetPut::*;

//------------------------- Connections --------------------------//
//                                                                //
// Connections are the plumbing of HAsim. They represent basic	  //
// point-to-point communication. The advantage over traditional   //
// Bluespec Connectables is that they are easier to use, are	  //
// connected automatically, and can easily be extended to include //
// model latency (ASim Ports).  				  //
// 								  //
// These might eventually be donated to the Bluespec library.	  //
// 								  //
//                                                                //
//----------------------------------------------------------------//


//The basic sending half of a connection.

interface Connection_Send#(type msg_T);
  
  method Action send(msg_T data);
  method Bool notFull();
endinterface


//The basic receiving connection.

interface Connection_Receive#(type msg_T);

  method Bool notEmpty(); 
  method Action deq();
  method msg_T  receive();

endinterface


// A client sends requests and receives responses
// (which may not come instantly)

interface Connection_Client#(type req_T, type resp_T);

  method Action makeReq(req_T data);
  method Bool   reqNotFull(); 

  method resp_T getResp();
  method Action deq();
  method Bool   respNotEmpty(); 
  
endinterface


// A server receives requests and gives back responses
// It can take any amount of time, and there is no assumption
// that the responses are FIFO.

interface Connection_Server#(type req_T, type resp_T);

  method Bool   reqNotEmpty(); 
  method req_T  getReq();
  method Action deq();

  method Action makeResp(resp_T data);
  method Bool   respNotFull(); 
  
endinterface

// A chain is a link which has a previous to receive from and
// a next to send to.

interface Connection_Chain#(type msg_T);

  method ActionValue#(msg_T) recvFromPrev();
  method msg_T               peekFromPrev();
  method Bool                recvNotEmpty();

  method Action              sendToNext(msg_T data);
  method Bool                sendNotFull();
  
endinterface

//Connection Implementations


module [Connected_Module] mkConnection_Send#(String portname)
    //interface:
                (Connection_Send#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ));

    let c <- mkConnection_SendDispatch(portname, False);
    
    return c;

endmodule

module [Connected_Module] mkConnectionSendOptional#(String portname)
    //interface:
                (Connection_Send#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ));

    let c <- mkConnection_SendDispatch(portname, True);
    
    return c;

endmodule

module [Connected_Module] mkConnection_SendDispatch#(String portname, Bool optional)
    //interface:
                (Connection_Send#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Div#(msg_SZ, PHYSICAL_CONNECTION_SIZE, t_NUM_PHYSICAL_CONNS));


    //Figure out my type for typechecking
    msg_T msg = ?;
    String conntype = printType(typeOf(msg));
    
    let c <- case (valueof(t_NUM_PHYSICAL_CONNS))
                0: mkConnectionSendPhysical(portname, optional, conntype);
                1: mkConnectionSendPhysical(portname, optional, conntype);
                default: mkConnectionSendVector(portname, optional, conntype);
            endcase;

  method Action send(msg_T data) if (c.notFull);
    c.send(data);
  endmethod
  
  method Bool notFull() = c.notFull();

endmodule

module [Connected_Module] mkConnectionSendPhysical#(String portname, Bool optional, String mytype)
    //interface:
                (Connection_Send#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ));

  Clock clock <- exposeCurrentClock();
  Reset reset <- exposeCurrentReset();

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.
  
  FIFOF#(msg_T) q <- mkUGSizedFIFOF(`CON_BUFFERING);
  
  //Bind the interface to a name for convenience
  let outg = (interface CON_Out;
  
	       method CON_Data try() if (q.notEmpty());
                 Bit#(msg_SZ) tmp = pack(q.first());
                 return zeroExtendNP(tmp);
               endmethod
	       
	       method Action success = q.deq();

               interface clk = clock;

               interface rst = reset;

	     endinterface);

  //Add our interface to the ModuleCollect collection
  let info = CSend_Info {cname: portname, ctype: mytype, optional: optional, conn: outg};
  addToCollection(tagged LSend info);

  method Action send(msg_T data);
    q.enq(data);
  endmethod
  
  method Bool notFull() = q.notFull();

endmodule

module [Connected_Module] mkConnectionSendVector#(String portname, Bool optional, String origtype)
    //interface:
                (Connection_Send#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Div#(msg_SZ, PHYSICAL_CONNECTION_SIZE, t_NUM_PHYSICAL_CONNS));

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.
  
  Vector#(t_NUM_PHYSICAL_CONNS, Connection_Send#(Bit#(PHYSICAL_CONNECTION_SIZE))) v = newVector();
  
  for (Integer x = 0; x < valueof(t_NUM_PHYSICAL_CONNS); x = x + 1)
  begin
    v[x] <- mkConnectionSendPhysical(portname + "_chunk_" + integerToString(x), optional, origtype);
  end
  
  method Action send(msg_T data);
  
      Bit#(msg_SZ) p = pack(data);
      Bit#(TMul#(t_NUM_PHYSICAL_CONNS, PHYSICAL_CONNECTION_SIZE)) p2 = zeroExtendNP(p);
      Vector#(t_NUM_PHYSICAL_CONNS, Bit#(PHYSICAL_CONNECTION_SIZE)) tmp = unpack(p2);
  
      for (Integer x = 0; x < valueof(t_NUM_PHYSICAL_CONNS); x = x + 1)
      begin
        v[x].send(tmp[x]);
      end

  endmethod
  
  method Bool notFull();
  
    return v[0].notFull();
  
  endmethod

endmodule


module [Connected_Module] mkConnection_Receive#(String portname)
    //interface:
                (Connection_Receive#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ));

    let c <- mkConnection_ReceiveDispatch(portname, False);
    
    return c;

endmodule

module [Connected_Module] mkConnectionRecvOptional#(String portname)
    //interface:
                (Connection_Receive#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ));

    let c <- mkConnection_ReceiveDispatch(portname, True);
    
    return c;

endmodule

module [Connected_Module] mkConnection_ReceiveDispatch#(String portname, Bool optional)
    //interface:
                (Connection_Receive#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Div#(msg_SZ, PHYSICAL_CONNECTION_SIZE, t_NUM_PHYSICAL_CONNS));

  //Figure out my type for typechecking
  msg_T msg = ?;
  String conntype = printType(typeOf(msg));

  let c <- case (valueof(t_NUM_PHYSICAL_CONNS))
            0: mkConnectionRecvPhysical(portname, optional, conntype);
            1: mkConnectionRecvPhysical(portname, optional, conntype);
            default: mkConnectionRecvVector(portname, optional, conntype);
           endcase;

  method msg_T receive() if (c.notEmpty());
    return c.receive();
  endmethod

  method Bool notEmpty();
    return c.notEmpty();
  endmethod

  method Action deq() if (c.notEmpty());
    c.deq();
  endmethod

endmodule

module [Connected_Module] mkConnectionRecvPhysical#(String portname, Bool optional, String mytype)
    //interface:
                (Connection_Receive#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ));

  Clock clock <- exposeCurrentClock();
  Reset reset <- exposeCurrentReset();

  PulseWire      en_w    <- mkPulseWire();
  RWire#(msg_T)  data_w  <- mkRWire();
  
  //Bind the interface to a name for convenience
  let inc = (interface CON_In;
  
	       method Action get_TRY(CON_Data x);
                 Bit#(msg_SZ) tmp = truncateNP(x);
	         data_w.wset(unpack(tmp));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w;
	       endmethod

               interface clk = clock;
  
               interface rst = reset;

	     endinterface);


  //Add our interface to the ModuleCollect collection
  let info = CRecv_Info {cname: portname, ctype: mytype, optional: optional, conn: inc};
  addToCollection(tagged LRecv info);
  
  method msg_T receive();
    return validValue(data_w.wget());
  endmethod

  method Bool notEmpty();
    return isValid(data_w.wget());
  endmethod

  method Action deq();
    en_w.send();
  endmethod

endmodule

module [Connected_Module] mkConnectionRecvVector#(String portname, Bool optional, String origtype)
    //interface:
                (Connection_Receive#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Div#(msg_SZ, PHYSICAL_CONNECTION_SIZE, t_NUM_PHYSICAL_CONNS));

  Vector#(t_NUM_PHYSICAL_CONNS, Connection_Receive#(Bit#(PHYSICAL_CONNECTION_SIZE))) v = newVector();
  
  for (Integer x = 0; x < valueof(t_NUM_PHYSICAL_CONNS); x = x + 1)
  begin
    v[x] <- mkConnectionRecvPhysical(portname + "_chunk_" + integerToString(x), optional, origtype);
  end
  
  method msg_T receive();

      Vector#(t_NUM_PHYSICAL_CONNS, Bit#(PHYSICAL_CONNECTION_SIZE)) tmp = newVector();
  
      for (Integer x = 0; x < valueof(t_NUM_PHYSICAL_CONNS); x = x + 1)
      begin
        tmp[x] = v[x].receive();
      end

      Bit#(TMul#(t_NUM_PHYSICAL_CONNS, PHYSICAL_CONNECTION_SIZE)) p = pack(tmp);
      Bit#(msg_SZ) p2 = truncateNP(p);
      return unpack(p2);

  endmethod

  method Bool notEmpty();
    return v[0].notEmpty();
  endmethod

  method Action deq();

      for (Integer x = 0; x < valueof(t_NUM_PHYSICAL_CONNS); x = x + 1)
      begin
        v[x].deq();
      end

  endmethod

endmodule

// ========================================================================
//
// mkConnection_Client & mkConnection_Server --
//     A convenience which bundles up sending and receiving.
//
// ========================================================================

//
// First define functions for generating connection names for clients wishing
// to use separate mkConnection_Send and Receive to talk to a mkConnection_Server.
//
    
function String genConnectionClientSendName(String portname);
    return strConcat(portname, "_req");
endfunction

function String genConnectionClientReceiveName(String portname);
    return strConcat(portname, "_resp");
endfunction

module [Connected_Module] mkConnection_Client#(String portname)
    //interface:
                (Connection_Client#(req_T, resp_T))
    provisos
            (Bits#(req_T,  req_SZ),
	     Bits#(resp_T, resp_SZ));

  let sendname = genConnectionClientSendName(portname);
  let recvname = genConnectionClientReceiveName(portname);
  
  Connection_Send#(req_T) reqconn <- mkConnection_Send(sendname);
  Connection_Receive#(resp_T) respconn <- mkConnection_Receive(recvname);

  method Bool reqNotFull();
    return reqconn.notFull();
  endmethod
  
  method Action makeReq(req_T data);
    reqconn.send(data);
  endmethod
  
  method Bool respNotEmpty();
    return respconn.notEmpty();
  endmethod
  
  method resp_T getResp();
    return respconn.receive();
  endmethod

  method Action deq();
    respconn.deq();
  endmethod

endmodule

module [Connected_Module] mkConnectionClientOptional#(String portname)
    //interface:
                (Connection_Client#(req_T, resp_T))
    provisos
            (Bits#(req_T,  req_SZ),
	     Bits#(resp_T, resp_SZ));

  let sendname = genConnectionClientSendName(portname);
  let recvname = genConnectionClientReceiveName(portname);
  
  Connection_Send#(req_T) reqconn <- mkConnectionSendOptional(sendname);
  Connection_Receive#(resp_T) respconn <- mkConnectionRecvOptional(recvname);

  method Bool reqNotFull();
    return reqconn.notFull();
  endmethod
  
  method Action makeReq(req_T data);
    reqconn.send(data);
  endmethod
  
  method Bool respNotEmpty();
    return respconn.notEmpty();
  endmethod
  
  method resp_T getResp();
    return respconn.receive();
  endmethod

  method Action deq();
    respconn.deq();
  endmethod

endmodule

module [Connected_Module] mkConnection_Server#(String portname)
    //interface:
                (Connection_Server#(req_T, resp_T))
    provisos
            (Bits#(req_T,  req_SZ),
	     Bits#(resp_T, resp_SZ));

  let sendname = genConnectionClientReceiveName(portname);
  let recvname = genConnectionClientSendName(portname);
  
  Connection_Receive#(req_T) reqconn <- mkConnection_Receive(recvname);
  Connection_Send#(resp_T) respconn <- mkConnection_Send(sendname);

  method Bool respNotFull();
    return respconn.notFull();
  endmethod
  
  method Action makeResp(resp_T data);
    respconn.send(data);
  endmethod
  
  method Bool reqNotEmpty();
    return reqconn.notEmpty();
  endmethod
  
  method req_T getReq();
    return reqconn.receive();
  endmethod
  
  method Action deq();
    reqconn.deq();
  endmethod

endmodule

module [Connected_Module] mkConnectionServerOptional#(String portname)
    //interface:
                (Connection_Server#(req_T, resp_T))
    provisos
            (Bits#(req_T,  req_SZ),
	     Bits#(resp_T, resp_SZ));

  let sendname = genConnectionClientReceiveName(portname);
  let recvname = genConnectionClientSendName(portname);
  
  Connection_Receive#(req_T) reqconn <- mkConnectionRecvOptional(recvname);
  Connection_Send#(resp_T) respconn <- mkConnectionSendOptional(sendname);

  method Bool respNotFull();
    return respconn.notFull();
  endmethod
  
  method Action makeResp(resp_T data);
    respconn.send(data);
  endmethod
  
  method Bool reqNotEmpty();
    return reqconn.notEmpty();
  endmethod
  
  method req_T getReq();
    return reqconn.receive();
  endmethod
  
  method Action deq();
    reqconn.deq();
  endmethod

endmodule

// ========================================================================
//
// mkConnection_Chain --
//     A Connection in a Chain
//
// ========================================================================


module [Connected_Module] mkConnection_Chain#(Integer chain_num)
    //interface:
		(Connection_Chain#(msg_T))
    provisos
	    (Bits#(msg_T, msg_SZ),
             Add#(msg_SZ, t_TMP, CON_CHAIN_DATA_SZ));

  Clock clock <- exposeCurrentClock();
  Reset reset <- exposeCurrentReset();

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.

  RWire#(msg_T)  data_w  <- mkRWire();
  PulseWire      en_w    <- mkPulseWire();
  FIFOF#(msg_T)  q       <- mkUGSizedFIFOF(`CON_BUFFERING);

  if (valueof(msg_SZ) > valueof(CON_CHAIN_DATA_SZ))
    error("Connection Chain Error: Message size " + 
    integerToString(valueof(msg_SZ)) + 
    " does not fit into chain width of " +
    integerToString(valueof(CON_CHAIN_DATA_SZ)) +
    ". Please increase chain width."); 

  let inc = (interface CON_CHAIN_In;
  
	       method Action get_TRY(CON_CHAIN_Data x);
                 Bit#(msg_SZ) tmp = truncate(x);
	         data_w.wset(unpack(tmp));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w;
	       endmethod

               interface clk = clock;
  
               interface rst = reset;

	     endinterface);

  let outg = (interface CON_CHAIN_Out;
  
	       method CON_CHAIN_Data try() if (q.notEmpty());
                 Bit#(msg_SZ) tmp = pack(q.first());
                 return zeroExtend(tmp);
               endmethod
	       
	       method Action success = q.deq();

               interface clk = clock;
  
               interface rst = reset;

	     endinterface);

  let chn = (interface CON_Chain;
               
	       interface incoming = inc;
	       interface outgoing = outg;
	     endinterface);

  //Figure out my type for typechecking
  msg_T msg = ?;
  String mytype = printType(typeOf(msg));

  //Add the chain to the ModuleCollect collection
  let info = CChain_Info {cnum: chain_num, ctype: mytype, conn: chn};
  addToCollection(tagged LChain info);


  method Action sendToNext(msg_T data) if (q.notFull());
    q.enq(data);
  endmethod

  method Bool sendNotFull = q.notFull();


  method ActionValue#(msg_T) recvFromPrev() if (data_w.wget() matches tagged Valid .val);
    en_w.send();
    return val;
  endmethod

  method msg_T peekFromPrev() if (data_w.wget() matches tagged Valid .val);
    return val;
  endmethod

  method Bool recvNotEmpty() = isValid(data_w.wget());

endmodule

//Helper functions

instance Connectable#(Get#(data_t),Connection_Send#(data_t));
  module mkConnection#(Get#(data_t) server,
                       Connection_Send#(data_t) client) (Empty);
  
    rule connect;
      let data <- server.get();
      client.send(data);
    endrule

  endmodule
endinstance

instance Connectable#(Connection_Receive#(data_t),Put#(data_t));
  module mkConnection#(Connection_Receive#(data_t) server,
                       Put#(data_t) client) (Empty);
  
    rule connect;
      server.deq();
      client.put(server.receive());
    endrule

  endmodule
endinstance


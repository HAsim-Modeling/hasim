import Clocks::*;
import Connectable::*;

// ****** Helper Functions ******

// Connections can be hooked up using the standard mkConnection function

instance Connectable#(PHYSICAL_CONNECTION_OUT, PHYSICAL_CONNECTION_IN);

  function m#(Empty) mkConnection(PHYSICAL_CONNECTION_OUT cout, PHYSICAL_CONNECTION_IN cin)
    provisos (IsModule#(m, c));
  
    return connectOutToIn(cout, cin);
    
  endfunction

endinstance

instance Connectable#(PHYSICAL_CONNECTION_IN, PHYSICAL_CONNECTION_OUT);

  function m#(Empty) mkConnection(PHYSICAL_CONNECTION_IN cin, PHYSICAL_CONNECTION_OUT cout)
    provisos (IsModule#(m, c));
  
    return connectOutToIn(cout, cin);
    
  endfunction

endinstance

// InOut pairs like Servers and Clients can also be connected.

instance Connectable#(PHYSICAL_CONNECTION_INOUT, PHYSICAL_CONNECTION_INOUT);

  function m#(Empty) mkConnection(PHYSICAL_CONNECTION_INOUT c1, PHYSICAL_CONNECTION_INOUT c2)
    provisos (IsModule#(m, c));
  
    return connectInOutToInOut(c1, c2);
    
  endfunction

endinstance

module connectInOutToInOut#(PHYSICAL_CONNECTION_INOUT inout1, PHYSICAL_CONNECTION_INOUT inout2) ();

  mkConnection(inout1.outgoing, inout2.incoming);
  mkConnection(inout2.outgoing, inout1.incoming);

endmodule

// {send,recv}NameMatches

// Does the send/recv's name match the param
// In this world, not matching a logical platform is also a non-match
// and must be handled at the top level

typeclass Matchable#(type t);
  function String getLogicalName(t val);
  function String getComputePlatform(t val);
endtypeclass

instance Matchable#(LOGICAL_RECV_INFO);
  function String getLogicalName(LOGICAL_RECV_INFO rinfo);
    return rinfo.logicalName;
  endfunction

  function String getComputePlatform(LOGICAL_RECV_INFO rinfo);
    return rinfo.computePlatform;
  endfunction
endinstance

instance Matchable#(LOGICAL_SEND_INFO);
  function String getLogicalName(LOGICAL_SEND_INFO sinfo);
    return sinfo.logicalName;
  endfunction

  function String getComputePlatform(LOGICAL_SEND_INFO sinfo);
    return sinfo.computePlatform;
  endfunction
endinstance

function Bool nameMatches(r rinfo, s sinfo)
  provisos(Matchable#(r),
           Matchable#(s));
  
  return (getLogicalName(sinfo) == getLogicalName(rinfo)) && 
         (getComputePlatform(sinfo) == getComputePlatform(rinfo));
 
endfunction

function Bool nameDoesNotMatch(r rinfo, s sinfo)
  provisos(Matchable#(r),
           Matchable#(s));
  
  return !nameMatches(rinfo,sinfo);
endfunction

instance Eq#(PHYSICAL_CONNECTION_OUT);

    function \== (PHYSICAL_CONNECTION_OUT x, PHYSICAL_CONNECTION_OUT y) = False;
    function \/= (PHYSICAL_CONNECTION_OUT x, PHYSICAL_CONNECTION_OUT y) = True;

endinstance

instance Eq#(PHYSICAL_CONNECTION_IN);

    function \== (PHYSICAL_CONNECTION_IN x, PHYSICAL_CONNECTION_IN y) = False;
    function \/= (PHYSICAL_CONNECTION_IN x, PHYSICAL_CONNECTION_IN y) = True;

endinstance

instance Eq#(STATION);

    function \== (STATION x, STATION y) = False;
    function \/= (STATION x, STATION y) = True;

endinstance


function Bool sendIsOneToMany(LOGICAL_SEND_INFO sinfo);

  return sinfo.oneToMany;

endfunction

function Bool recvIsManyToOne(LOGICAL_RECV_INFO rinfo);

  return rinfo.manyToOne;

endfunction

function Bool sendIsNotOneToMany(LOGICAL_SEND_INFO sinfo);

  return !sinfo.oneToMany;

endfunction

function Bool recvIsNotManyToOne(LOGICAL_RECV_INFO rinfo);

  return !rinfo.manyToOne;

endfunction

// connectOutToIn

// This is the module that actually performs the connection between two
// physical endpoints. This is for 1-to-1 communication only.

module connectOutToIn#(PHYSICAL_CONNECTION_OUT cout, PHYSICAL_CONNECTION_IN cin) ();
  

  if(sameFamily(cin.clock,cout.clock) && (cin.reset == cout.reset))
  begin
      rule trySend (True);
          // Try to move the data
          let x = cout.first();
          cin.try(x);
      endrule

      rule success (cin.success());
          // We succeeded in moving the data
          cout.deq();
    
      endrule
  end
  else
  begin
      messageM("CrossDomain@ Found");

      // choose a size large enough to cover latency of fifo
      let domainFIFO <- mkSyncFIFO(8,
                                   cout.clock, 
                                   cout.reset,
                                   cin.clock);

      rule receive;
          let x = cout.first();
          domainFIFO.enq(x);
          cout.deq();
      endrule
  
      rule trySend;
          cin.try(domainFIFO.first());
      endrule

      rule succeedSend(cin.success());
          domainFIFO.deq;
      endrule
  end
endmodule

module printSend#(LOGICAL_SEND_INFO send) (Empty);
  messageM("Send: " + send.logicalName + " " + send.computePlatform);
endmodule

module printSends#(List#(LOGICAL_SEND_INFO) sends) (Empty);
  for (Integer x = 0; x < length(sends); x = x + 1)
    begin
      printSend(sends[x]);
    end
endmodule

module printRecv#(LOGICAL_RECV_INFO recv) (Empty);
  messageM("Recv: " + recv.logicalName + " " + recv.computePlatform);
endmodule

module printRecvs#(List#(LOGICAL_RECV_INFO) recvs) (Empty);
  for (Integer x = 0; x < length(recvs); x = x + 1)
    begin
      printRecv(recvs[x]);
    end
endmodule


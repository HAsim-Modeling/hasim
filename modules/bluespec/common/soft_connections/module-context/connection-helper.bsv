
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

// Does the send/recv's name match the param?

function Bool sendNameMatches(String rname, LOGICAL_SEND_INFO sinfo);

  return (sinfo.logicalName == rname);

endfunction

function Bool recvNameMatches(String sname, LOGICAL_RECV_INFO rinfo);

  return (rinfo.logicalName == sname);

endfunction


function Bool sendNameDoesNotMatch(String rname, LOGICAL_SEND_INFO sinfo);

  return (sinfo.logicalName != rname);

endfunction

function Bool recvNameDoesNotMatch(String sname, LOGICAL_RECV_INFO rinfo);

  return (rinfo.logicalName != sname);

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

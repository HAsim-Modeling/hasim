
import soft_connections_alg::*;

//Instantiate a module with connections exposed

module [Module] instantiateSmartBoundary#(Connected_Module#(inter_T) m) (WithConnections);

  match {.m, .col} <- getCollection(m);
  
  let x <- toWithConnections(col, m);
  return x;

endmodule

// Connect soft connections as normal, but dangling connections are not an error
// Instead they're exposed as a WithConnections interface and messages are entered
// into the compilation log recording their address index.
// Connection Chains are not "tied off" but exposed as head and tail

//toWithConnections :: [ConnectionData] -> Module WithConnections

module [Module] toWithConnections#(List#(ConnectionData) ld, inter_T i)       (WithConnections);

  //Group connections by type  
  match {.sends, .recvs, .chns} = splitConnections(ld);
  
  match {.dsends, .drecvs} <- connect(sends, recvs);

  let outs     <- exposeDanglingSends(dsends);
  let ins      <- exposeDanglingRecvs(drecvs);
  let mychains <- connectChains(chns);

  interface outgoing = outs;
  interface incoming = ins;
  interface chains = mychains;
  
endmodule  


//Expose dangling sends to other synthesis boundaries via compilation messages

//exposeDangingSends :: [CSend_Info] -> Module [CON_Out]

module exposeDanglingSends#(List#(CSend_Info) dsends) (Vector#(CON_Addr, CON_Out));

  Vector#(CON_Addr, CON_Out) res = newVector();
  Integer cur_out = 0;

  //Output a compilation message and tie it to the next free outport
  for (Integer x = 0; x < length(dsends); x = x + 1)
  begin
    let cur = dsends[x];
    messageM(strConcat(strConcat(strConcat(strConcat(strConcat("Dangling Send {", ""),"} ["), integerToString(cur_out)), "]: "), cur.cname));
    res[cur_out] = cur.conn;
    cur_out = cur_out + 1;
    if (cur_out >= valueof(CON_Addr))
      error("ERROR: Too many dangling Send Connections. Increase the parameter BOUNDARY_CON_NUMBER");
  end
  
  //Zero out unused dangling sends
  for (Integer x = cur_out; x < valueOf(CON_Addr); x = x + 1)
    res[x] = ?;
  
  return res;
  
endmodule

//Expose dangling receives to other synthesis boundaries via compilation messages

//exposeDangingRecvs :: [CRecv_Info] -> Module [CON_In]

module exposeDanglingRecvs#(List#(CRecv_Info) drecvs) (Vector#(CON_Addr, CON_In));

  Vector#(CON_Addr, CON_In) res = newVector();
  Integer cur_in = 0;
  
  //Output a compilation message and tie it to the next free inport
  for (Integer x = 0; x < length(drecvs); x = x + 1)
  begin
    let cur = drecvs[x];
    messageM(strConcat(strConcat(strConcat(strConcat(strConcat("Dangling Rec {", ""), "} ["), integerToString(cur_in)), "]: "), cur.cname));
    res[cur_in] = cur.conn;
    cur_in = cur_in + 1;
    if (cur_in >= valueof(CON_Addr))
      error("ERROR: Too many dangling Receive Connections. Increase the parameter BOUNDARY_CON_NUMBER");
  end
  
  //Zero out unused dangling recvs
  for (Integer x = cur_in; x < valueOf(CON_Addr); x = x + 1)
    res[x] = ?;
  
  return res;

endmodule
  
  

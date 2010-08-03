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

import soft_connections_alg::*;

//Instantiate a module with connections exposed

module instantiateSmartBoundary#(Connected_Module#(inter_T) m) (WithConnections#(numIn, numOut));

  match {.m, .col} <- liftModule(getCollection(m));
  
  let x <- toWithConnections(col, m);
  return x;

endmodule

// Connect soft connections as normal, but dangling connections are not an error
// Instead they're exposed as a WithConnections interface and messages are entered
// into the compilation log recording their address index.
// Connection Chains are not "tied off" but exposed as head and tail

//toWithConnections :: [ConnectionData] -> Module WithConnections

module toWithConnections#(List#(ConnectionData) ld, inter_T i)       (WithConnections#(numIn, numOut));

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

module exposeDanglingSends#(List#(CSend_Info) dsends) (Vector#(n, CON_Out));

  Vector#(n, CON_Out) res = newVector();
  Integer cur_out = 0;

  //Output a compilation message and tie it to the next free outport
  for (Integer x = 0; x < length(dsends); x = x + 1)
  begin
    if (cur_out >= valueof(n))
      error(strConcat(strConcat("ERROR: Too many dangling Send Connections (max ", integerToString(valueof(n))), "). Increase the numOut parameter to WithConnections."));

    let cur = dsends[x];
    messageM(strConcat(strConcat(strConcat(strConcat(strConcat("Dangling Send {", cur.ctype),"} ["), integerToString(cur_out)), "]: "), cur.cname));
    res[cur_out] = cur.conn;
    cur_out = cur_out + 1;
  end
  
  //Zero out unused dangling sends
  for (Integer x = cur_out; x < valueOf(n); x = x + 1)
    res[x] = CON_Out{clk: noClock, rst: noReset, success: ?, try: ?};
  
  return res;
  
endmodule

//Expose dangling receives to other synthesis boundaries via compilation messages

//exposeDangingRecvs :: [CRecv_Info] -> Module [CON_In]

module exposeDanglingRecvs#(List#(CRecv_Info) drecvs) (Vector#(n, CON_In));

  Vector#(n, CON_In) res = newVector();
  Integer cur_in = 0;
  
  //Output a compilation message and tie it to the next free inport
  for (Integer x = 0; x < length(drecvs); x = x + 1)
  begin
    if (cur_in >= valueof(n))
      error(strConcat(strConcat("ERROR: Too many dangling Receive Connections (max ", integerToString(valueof(n))), "). Increase the numIn parameter to WithConnections."));

    let cur = drecvs[x];
    messageM(strConcat(strConcat(strConcat(strConcat(strConcat("Dangling Rec {", cur.ctype), "} ["), integerToString(cur_in)), "]: "), cur.cname));
    res[cur_in] = cur.conn;
    cur_in = cur_in + 1;
  end
  
  //Zero out unused dangling recvs
  for (Integer x = cur_in; x < valueOf(n); x = x + 1)
    res[x] = CON_In{clk: noClock, rst: noReset, get_SUCCESS: ?, get_TRY: ?};
  
  return res;

endmodule
  
  

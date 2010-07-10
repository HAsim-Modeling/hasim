//
// Copyright (C) 2010 Massachusetts Institute of Technology
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

import Clocks::*;

//************** Helper functions **************//

//
// listDuplicates :: [a] -> [a]
//     Return a new list containing only elements that had duplicates in the
//     original list.
//
function List#(a) listDuplicates(List#(a) l)
    provisos (Ord#(a),
              Eq#(a));

    function isDup(List#(g) gr) = (length(gr) > 1);

    let groups = List::group(List::sort(l));
    let dups = List::filter(isDup, groups);

    List#(a) d = Nil;
    for (Integer x = 0; x < length(dups); x = x + 1)
    begin
        d = List::cons(dups[x][0], d);
    end

    return d;
endfunction


//Group connections by name. Unfound connections are dangling.

//groupByName :: [CSend_Info] -> [CRecv_Info] -> ([CSend_Info], [CRecv_Info], [(CSend_Info, CRecv_Info)])

function Tuple3#(List#(CSend_Info),
                 List#(CRecv_Info),
                 List#(Tuple2#(CSend_Info, CRecv_Info))) groupByName(List#(CSend_Info) u_sends, List#(CRecv_Info) u_recvs);

    //
    // groupBySortedName --
    //     Variation on a recursive merge sort, taking advantage of the
    //     requirement that the two input lists are sorted.
    //
    function Tuple3#(List#(CSend_Info),
                     List#(CRecv_Info),
                     List#(Tuple2#(CSend_Info, CRecv_Info))) groupBySortedName(List#(CSend_Info) sends, List#(CRecv_Info) recvs);
        if (sends matches tagged Nil)
        begin
            return tuple3(Nil, recvs, Nil);
        end
        else if (recvs matches tagged Nil)
        begin
            return tuple3(sends, Nil, Nil);
        end
        else
        begin
            // Both lists still have elements
            let s = List::head(sends);
            let r = List::head(recvs);

            if (s.cname == r.cname)
            begin
                // Head elements are equal.  Join them.
                match {.s_list, .r_list, .f_list} = groupBySortedName(List::tail(sends),
                                                                      List::tail(recvs));
                return tuple3(s_list, r_list, List::cons(tuple2(s, r), f_list));
            end
            else if (s.cname < r.cname)
            begin
                // Send head is lexically earlier.  It has no match in recvs.
                match {.s_list, .r_list, .f_list} = groupBySortedName(List::tail(sends),
                                                                      recvs);
                return tuple3(List::cons(s, s_list), r_list, f_list);
            end
            else
            begin
                // Recv head is lexically earlier.  It has no match in sends.
                match {.s_list, .r_list, .f_list} = groupBySortedName(sends,
                                                                      List::tail(recvs));
                return tuple3(s_list, List::cons(r, r_list), f_list);
            end
        end
    endfunction

    return groupBySortedName(List::sort(u_sends), List::sort(u_recvs));
endfunction


//splitConnections :: [ConnectionData] -> ([CSend_Info], [LRec], [CChain_Info])

function Tuple3#(List#(CSend_Info), List#(CRecv_Info), List#(CChain_Info)) splitConnections(List#(ConnectionData) l);

  case (l) matches
    tagged Nil: return tuple3(Nil, Nil, Nil);
    default:
    begin
      match {.sends, .recs, .chns} = splitConnections(List::tail(l));
      case (List::head(l)) matches
        tagged LSend .inf:
          return tuple3(List::cons(inf, sends), recs, chns);
        tagged LRecv .inf:
          return tuple3(sends, List::cons(inf, recs), chns);
        tagged LChain .inf:
          return tuple3(sends, recs, List::cons(inf, chns));
      endcase
    end
  endcase

endfunction


//checkDuplicateSends :: [CSend_Info] -> Module Integer

module checkDuplicateSends#(List#(CSend_Info) sends) (Integer);

  function String getSendName(CSend_Info s);
    return s.cname;
  endfunction
  
  let dups = listDuplicates(List::map(getSendName, sends));
  let nDups = length(dups);
  
  for (Integer x = 0; x < nDups; x = x + 1)
  begin
    messageM(strConcat("ERROR: Duplicate Send Connection: ", dups[x]));
  end
  
  return nDups;

endmodule

//checkDuplicateRecvs :: [CRecv_Info] -> Module Integer

module checkDuplicateRecvs#(List#(CRecv_Info) recvs) (Integer);

  function String getRecvName(CRecv_Info r);
    return r.cname;
  endfunction
  
  let dups = listDuplicates(List::map(getRecvName, recvs));
  let nDups = length(dups);
  
  for (Integer x = 0; x < nDups; x = x + 1)
  begin
    messageM(strConcat("ERROR: Duplicate Receive Connection: ", dups[x]));
  end
  
  return nDups;

endmodule


//Group chain links by chain index

//groupChains :: [CChain_Info] -> [[CChain_Info]]

function Vector#(CON_NumChains, List#(CChain_Info)) groupChains(List#(CChain_Info) l);

  Vector#(CON_NumChains, List#(CChain_Info)) res = replicate(List::nil);

  let nLinks = length(l);
  for (Integer x = 0; x < nLinks; x = x + 1)
  begin
    let cur = l[x];
    res[cur.cnum] = List::cons(cur, res[cur.cnum]);
  end

  return res;

endfunction

//Connections can be hooked up using the standard mkConnection function

instance Connectable#(PHYSICAL_CON_Out#(t_MSG), PHYSICAL_CON_In#(t_MSG))
  provisos(Bits#(t_MSG, t_MSG_Sz));

  function m#(Empty) mkConnection(PHYSICAL_CON_Out#(t_MSG) cout, PHYSICAL_CON_In#(t_MSG) cin)
    provisos (IsModule#(m, c));
  
    return connectOutToIn(cout, cin);
    
  endfunction

endinstance

instance Connectable#(PHYSICAL_CON_In#(t_MSG), PHYSICAL_CON_Out#(t_MSG))
  provisos(Bits#(t_MSG, t_MSG_Sz));

  function m#(Empty) mkConnection(PHYSICAL_CON_In#(t_MSG) cin, PHYSICAL_CON_Out#(t_MSG) cout)
    provisos (IsModule#(m, c));
  
    return connectOutToIn(cout, cin);
    
  endfunction

endinstance

module connectOutToIn#(PHYSICAL_CON_Out#(t_MSG) cout, PHYSICAL_CON_In#(t_MSG) cin) ()
  provisos(Bits#(t_MSG, t_MSG_Sz));


  if(sameFamily(cin.clk,cout.clk))
  begin
      rule trySend (True);
          //Try to move the data
          let x = cout.try();
          cin.get_TRY(x);
      endrule

      rule success (cin.get_SUCCESS());
          //We succeeded in moving the data
          cout.success();
      endrule
  end
  else
  begin
      // choose a size large enough to cover latency of fifo
      let domainFIFO <- mkSyncFIFO(8,
                                   cout.clk, 
                                   cout.rst,
                                   cin.clk);

      rule receive;
          let x = cout.try();
          domainFIFO.enq(x);
          cout.success();
      endrule
  
      rule trySend;
          cin.get_TRY(domainFIFO.first);
      endrule

      rule succeedSend(cin.get_SUCCESS());
          domainFIFO.deq;
      endrule

  end
endmodule

//Chains can also be hooked up with mkConnection

instance Connectable#(CON_Chain, CON_Chain);

  function m#(Empty) mkConnection(CON_Chain cout, CON_Chain cin)
    provisos (IsModule#(m, c));
  
    return connectOutToIn(cout.outgoing, cin.incoming);
    
  endfunction

endinstance



//************** Helper functions **************//

//Union two lists

//listunion :: [a] -> [a] -> [a]

function List#(a) listunion(List#(a) l1, List#(a) l2)
  provisos
          (Eq#(a));

  List#(a) res = l1;
  
  for (Integer x = 0; x < length(l2); x = x + 1)
  begin
    let cur = l2[x];
    res = List::elem(cur, res) ? res : List::cons(cur, res);
  end

  return res;

endfunction

function Maybe#(CSend_Info) lookupSend(String nm, List#(CSend_Info) ls);

  case (ls) matches
    tagged Nil: return tagged Invalid;
    default: return (List::head(ls).cname == nm) ? tagged Valid List::head(ls) : lookupSend(nm, (List::tail(ls)));
  endcase

endfunction

function Maybe#(CRecv_Info) lookupRecv(String nm, List#(CRecv_Info) ls);

  case (ls) matches
    tagged Nil: return tagged Invalid;
    default: return (List::head(ls).cname == nm) ? tagged Valid List::head(ls) : lookupRecv(nm, List::tail(ls));
  endcase

endfunction

//Group connections by name. Unfound connections are dangling.

//groupByName :: [CSend_Info] -> [CRecv_Info] -> ([CSend_Info], [CRecv_Info], [(CSend_Info, CRecv_Info)])

function Tuple3#(List#(CSend_Info),
                 List#(CRecv_Info),
                 List#(Tuple2#(CSend_Info, CRecv_Info))) groupByName(List#(CSend_Info) sends, List#(CRecv_Info) recvs);

  List#(CSend_Info) dsends = Nil;
  List#(CRecv_Info) drecvs = Nil;
  List#(Tuple2#(CSend_Info, CRecv_Info)) found = Nil;

  function String getSendName(CSend_Info s);
    return s.cname;
  endfunction

  function String getRecvName(CRecv_Info r);
    return r.cname;
  endfunction

  let send_names = List::map(getSendName, sends);
  let recv_names = List::map(getRecvName, recvs);
  
  let all_names = listunion(send_names, recv_names);

  for (Integer x = 0; x < length(all_names); x = x + 1)
  begin
    
    let msend = lookupSend(all_names[x], sends);
    let mrecv = lookupRecv(all_names[x], recvs);
    case (msend) matches
      tagged Invalid:
        case (mrecv) matches
          tagged Invalid:
            let err = error(strConcat("EXCEPTION: Soft Connections gave up on Connection ", all_names[x]));
          tagged Valid .recv:
            drecvs = List::cons(recv, drecvs);
        endcase
      tagged Valid .send:
        case (mrecv) matches
          tagged Invalid:
            dsends = List::cons(send, dsends);
          tagged Valid .recv:
            found = List::cons(tuple2(send, recv), found);
        endcase
    endcase
  end

  
  return tuple3(dsends, drecvs, found);

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

//getDuplicates :: [String] -> [String]

function List#(String) getDuplicates(List#(String) l);

  case (l) matches
    tagged Nil: return Nil;
    default:
    begin
      let ds = getDuplicates(List::tail(l));
      let s  = (List::head(l));
      return List::elem(s,ds) ? List::cons(s, ds) : ds;
    end
  endcase

endfunction

//checkDuplicateSends :: [CSend_Info] -> Module Integer

module checkDuplicateSends#(List#(CSend_Info) sends) (Integer);

  function String getSendName(CSend_Info s);
    return s.cname;
  endfunction
  
  let dups = getDuplicates(List::map(getSendName, sends));
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
  
  let dups = getDuplicates(List::map(getRecvName, recvs));
  let nDups = length(dups);
  
  for (Integer x = 0; x < nDups; x = x + 1)
  begin
    messageM(strConcat("ERROR: Duplicate Receive Connection: ", dups[x]));
  end
  
  return nDups;

endmodule

//splitWith :: (a -> Bool) -> [a] -> ([a], [a])

function Tuple2#(List#(a), List#(a)) splitWith(function Bool fn(a x), List#(a) l);

  case (l) matches
    tagged Nil: return tuple2(Nil, Nil);
    default:
    begin
      match {.xs, .ys} = splitWith(fn, List::tail(l));
      let cur = List::head(l);
      if (fn(cur))
        return tuple2(List::cons(cur, xs), ys);
      else
        return tuple2(xs, List::cons(cur, ys));
    end
  endcase

endfunction


//groupByIndex :: (Eq a) => [(a, b)] -> [[b]]

function List#(List#(b)) groupByIndex(List#(Tuple2#(a, b)) l) provisos (Eq#(a));

  function Bool eqIndex(a x, Tuple2#(a, b) t) provisos (Eq#(a)) = (x == t.fst);

  case (l) matches
    tagged Nil: return Nil;
    default:
    begin
      match {.idx, .val} = List::head(l);
      match {.same, .rest} = splitWith(eqIndex(idx), List::tail(l));
      let same2 = List::map(tpl_2, same);
      return List::cons(List::cons(val, same2), groupByIndex(rest));
    end
  endcase

endfunction

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

instance Connectable#(CON_Out, CON_In);

  function m#(Empty) mkConnection(CON_Out cout, CON_In cin)
    provisos (IsModule#(m, c));
  
    return connectOutToIn(cout, cin);
    
  endfunction

endinstance

instance Connectable#(CON_In, CON_Out);

  function m#(Empty) mkConnection(CON_In cin, CON_Out cout)
    provisos (IsModule#(m, c));
  
    return connectOutToIn(cout, cin);
    
  endfunction

endinstance

module connectOutToIn#(CON_Out cout, CON_In cin) ();

  rule trySend (True);
    //Try to move the data
    let x = cout.try();
    cin.get_TRY(x);
  
  endrule

  rule success (cin.get_SUCCESS());
    //We succeeded in moving the data
    cout.success();
    
  endrule

endmodule

//Chains can also be hooked up with mkConnection

instance Connectable#(CON_Chain, CON_Chain);

  function m#(Empty) mkConnection(CON_Chain cout, CON_Chain cin)
    provisos (IsModule#(m, c));
  
    return connectOutToIn(cout.outgoing, cin.incoming);
    
  endfunction

endinstance


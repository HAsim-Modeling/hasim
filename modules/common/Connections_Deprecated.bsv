import GetPut::*;
import ClientServer::*;
import RegFile::*;
import Vector::*;
import List::*;
import FIFO::*;
import ModuleCollect::*;
import Connectable::*;

//Instantiate a module with connections exposed

module [Module] instantiateWithConnections#(Connected_Module#(inter_T) m) (WithConnections);

  match {.m, .col} <- getCollection(m);
  
  let x <- connectDangling(col, m);
  return x;

endmodule

//Instantiate a module with connections exposed

module [Module] instantiateTopLevel#(Connected_Module#(inter_T) m) (TopLevel);

  match {.m, .col1} <- getCollection(m);
  match {.m2, .col2} <- getCollection(mkFPGALib);
  let col = List::append(col1, col2);
  
  connectTopLevel(col, m);
  return m2;
  
endmodule

//Connection map from conname to local address

typedef Tuple2#(String, Integer) ConMap;

//Re-bury connections which have been exposed at synthesis boundaries

module [HASim_Module] addConnections#(WithConnections mod, List#(ConMap) sends, List#(ConMap) recs) ();
   
   //Add Sends
   
   let nSends = length(sends);
   for (Integer x = 0; x < nSends; x = x + 1)
   begin
     match {.nm, .idx} = sends[x];
     addToCollection(tagged LSend tuple2(nm, mod.outgoing[x]));
   end

   //Add Recs

   let nRecs = length(recs);
   for (Integer x = 0; x < nRecs; x = x + 1)
   begin
     match {.nm, .idx} = recs[x];
     addToCollection(tagged LRec tuple2(nm, mod.incoming[x]));
   end
   
   //Add Chains
   for (Integer x = 0; x < valueof(CON_NumChains); x = x + 1)
   begin
     addToCollection(tagged LChain tuple2(x, mod.chains[x]));
   end
   
endmodule

//The main connection algorithm 

module [Module] connectDangling#(List#(ConnectionData) ld, inter_T i)       (WithConnections);
    
  match {.sends, .recs, .chns} = splitConnections(ld);
  match {.send_nms, .*} = List::unzip(sends);
  match {.rec_nms, .*} = List::unzip(recs);

  let dup_sends = getDuplicates(send_nms);
  let dup_recs  = getDuplicates(rec_nms);
  
  let nDupSends = length(dup_sends);
  let nDupRecs = length(dup_recs);
  
  for (Integer x = 0; x < nDupSends; x = x + 1)
    messageM(strConcat("ERROR: Duplicate Send: ", dup_sends[x]));
  for (Integer x = 0; x < nDupRecs; x = x + 1)
    messageM(strConcat("ERROR: Duplicate Receive: ", dup_recs[x]));
    
  if (nDupSends != 0 || nDupRecs != 0)
    error("Duplicate connection names detected.");
  
  //match {.dsends, .drecs, .cncts} = groupByName(sends, recs);
  let tup = groupByName(sends, recs);
  List#(Tuple2#(String, CON_Out)) dsends = tpl_1(tup);
  List#(Tuple2#(String, CON_In))  drecs = tpl_2(tup);
  match {.*, .*, .cncts} = tup;
  
  let numout = length(dsends);
  let numin  = length(drecs);
  
  let nCncts = length(cncts);
  
  //Actually Connect the Connections
  for (Integer x = 0; x < nCncts; x = x + 1)
  begin
    match {.nm, .cin, .cout} = cncts[x];
    //Type-Checking goes here
    messageM(strConcat("Connecting: ", nm));
    mkConnection(cin, cout);
  
  end
  
  Vector#(CON_Addr, CON_Out) outs = newVector();
  Vector#(CON_Addr, CON_In) ins = newVector();
  Integer cur_out = 0;
  Integer cur_in = 0;
  
  //Final Dangling sends
  for (Integer x = 0; x < length(dsends); x = x + 1)
  begin
    match {.cnm, .cout} = dsends[x];
    messageM(strConcat(strConcat(strConcat("Dangling Send [", integerToString(cur_out)), "]: "), cnm));
    outs[cur_out] = cout;
    cur_out = cur_out + 1;
  end
  
  for (Integer x = cur_out; x < valueOf(CON_Addr); x = x + 1)
    outs[x] = ?;
  
  //Final Dangling recs
  for (Integer x = 0; x < length(drecs); x = x + 1)
  begin
    match {.cnm, .cin} = drecs[x];
    messageM(strConcat(strConcat(strConcat("Dangling Rec [", integerToString(cur_in)), "]: "), cnm));
    ins[cur_in] = cin;
    cur_in = cur_in + 1;
  end
  
  for (Integer x = cur_in; x < valueOf(CON_Addr); x = x + 1)
    ins[x] = ?;
  
  Vector#(CON_NumChains, CON_Chain) mychains = newVector();
  
  //Chain connections
  List#(List#(CON_Chain)) cs = groupByIndex(chns);
  let nChains = length(cs);
  
  for (Integer x = 0; x < valueOf(CON_NumChains); x = x + 1)
  begin
    List#(CON_Chain) clinks = (x < nChains) ? cs[x] : Nil;
    Integer nLinks = length(clinks);
    CON_Chain tmp <- (nLinks == 0) ? mkPassThrough(x) : connectLocalChain(clinks, x);

    mychains[x] = tmp;
  end
  
  interface outgoing = outs;
  interface incoming = ins;
  interface chains = mychains;
  
endmodule

//Top-Level connections

module [Module] connectTopLevel#(List#(ConnectionData) ld, inter_T i)       ();
    
  match {.sends, .recs, .chns} = splitConnections(ld);
  match {.send_nms, .*} = List::unzip(sends);
  match {.rec_nms, .*} = List::unzip(recs);

  let dup_sends = getDuplicates(send_nms);
  let dup_recs  = getDuplicates(rec_nms);
  
  let nDupSends = length(dup_sends);
  let nDupRecs = length(dup_recs);
  
  for (Integer x = 0; x < nDupSends; x = x + 1)
    messageM(strConcat("ERROR: Duplicate Send: ", dup_sends[x]));
  for (Integer x = 0; x < nDupRecs; x = x + 1)
    messageM(strConcat("ERROR: Duplicate Receive: ", dup_recs[x]));
    
  if (nDupSends != 0 || nDupRecs != 0)
    error("Duplicate connection names detected.");

  //match {.dsends, .drecs, .cncts} = groupByName(sends, recs);
  let tup = groupByName(sends, recs);
  List#(Tuple2#(String, CON_Out)) dsends = tpl_1(tup);
  List#(Tuple2#(String, CON_In))  drecs = tpl_2(tup);
  match {.*, .*, .cncts} = tup;
  
  let numout = length(dsends);
  let numin  = length(drecs);
  
  let nCncts = length(cncts);
  
  //Actually Connect Connections
  for (Integer x = 0; x < nCncts; x = x + 1)
  begin
    match {.nm, .cin, .cout} = cncts[x];
    //Type-Check here
    messageM(strConcat("Connecting: ", nm));
    mkConnection(cin, cout);
  
  end
  
  Vector#(CON_Addr, CON_Out) outs = newVector();
  Vector#(CON_Addr, CON_In) ins = newVector();
  Integer cur_out = 0;
  Integer cur_in = 0;
  Bool error_occurred = False;

  //Final Dangling sends
  for (Integer x = 0; x < length(dsends); x = x + 1)
  begin
    match {.cnm, .cout} = dsends[x];
    messageM(strConcat(strConcat(strConcat("Dangling Send [", integerToString(cur_out)), "]: "), cnm));
    cur_out = cur_out + 1;
    error_occurred = True;
  end
  
  //Final Dangling recs
  for (Integer x = 0; x < length(drecs); x = x + 1)
  begin
    match {.cnm, .cin} = drecs[x];
    messageM(strConcat(strConcat(strConcat("Dangling Rec [", integerToString(cur_in)), "]: "), cnm));
    cur_in = cur_in + 1;
    error_occurred = True;
  end
    
  Vector#(CON_NumChains, CON_Chain) mychains = newVector();
  
  //Chain connections
  List#(List#(CON_Chain)) cs = groupByIndex(chns);
  let nChains = length(cs);

  for (Integer x = 0; x < valueOf(CON_NumChains); x = x + 1)
  begin
  
    List#(CON_Chain) clinks = (x < nChains) ? cs[x] : Nil;
    Integer nLinks = length(clinks);
    CON_Chain tmp <- (nLinks == 0) ? mkPassThrough(x) : connectLocalChain(clinks, x);

    //Close the chain
    mkConnection(tmp, tmp);
  end

  if (error_occurred)
    error("Error: dangling connections at top-level.");
  
endmodule


module connectLocalChain#(List#(CON_Chain) l, Integer x) (CON_Chain);

  case (l) matches
    tagged Nil:
      return error("Internal Chain Connection failed");
    default:
    begin
      messageM(strConcat(strConcat("Adding Link Chain [", integerToString(x)), "]"));
      CON_Chain c = l[0];
      CON_Chain cbegin = c;
      let nLinks = length(l);
      //Connect internal chains
      for (Integer y = 1; y < nLinks; y = y + 1)
      begin
	CON_Chain c2 =l[y];
	mkConnection(c, c2);
        messageM(strConcat(strConcat("Adding Chain Link [", integerToString(x)), "]"));
	c = c2;
      end
      CON_Chain cend = c;
      return (interface CON_Chain;
                method first() = cend.first();
		method deq() = cend.deq();
		method enq() = cbegin.enq();
		method clear = noAction; //If you want to implement this, broadcast
	      endinterface);
    end
  endcase

endmodule


module [Module] mkPassThrough#(Integer chainNum)
    //interface:
                (CON_Chain);

  FIFO#(CON_Data) passQ <- mkFIFO();

  method CON_Data first() = passQ.first();
  method Action deq() = passQ.deq();
  method Action clear() = passQ.clear();
  method Action enq(CON_Data x) = passQ.enq(x);

endmodule

//************** Helper functions **************//

//lookup :: Eq a => a -> [(a, b)] -> Maybe b

function Maybe#(b) lookup (a data, List#(Tuple2#(a, b)) l)
  provisos (Eq#(a));
  
  case (l) matches
    tagged Nil:
      return tagged Invalid;
    default:
    begin
      match {.d, .v} = List::head(l);
      return (d == data) ? (tagged Valid v) : lookup(data, List::tail(l));
    end
  endcase

endfunction

//removeItem :: Eq a => a -> [(a, b)] -> [(a,b)]  

function List#(Tuple2#(a, b)) removeItem(a s, List#(Tuple2#(a, b)) l)
  provisos (Eq#(a));

  case (l) matches
    tagged Nil: return Nil;
    default:
    begin
      match {.nm, .v} = List::head(l);
      if (nm == s) 
	return List::tail(l);
      else
	return List::cons(tuple2(nm, v), removeItem(s, List::tail(l)));
    end
  endcase
endfunction

//groupByName :: Eq a => [(a, b)] -> [(a, c)] -> ([(a, b)], [(a, c)], [(a, b, c)])

function Tuple3#(List#(Tuple2#(a, b)),
	         List#(Tuple2#(a, c)),
		 List#(Tuple3#(a, b, c))) groupByName(List#(Tuple2#(a, b)) xs,
		                                      List#(Tuple2#(a, c)) ys)
     provisos
             (Eq#(a));

  if (isNull(xs) || isNull(ys))
    return tuple3(xs, ys, List::nil); //Return dangling items
  else
    begin
      match {.try, .x} = List::head(xs);

      case (lookup(try, ys)) matches
	tagged Valid .y:
	begin
	  match {.das, .dbs, .gs} = groupByName(List::tail(xs), removeItem(try, ys));
	  return tuple3(das, dbs, List::cons(tuple3(try, x, y), gs));
	end
	default:
	begin
	  match {.das, .dbs, .gs} = groupByName(List::tail(xs), ys);
	  return tuple3(List::cons(tuple2(try, x), das), dbs, gs);
	end
      endcase
    end
    
endfunction

//splitConnections :: [ConnectionData] -> ([(String, CON_Out)], [(String, CON_In)], [(String, CON_Chain)])

function Tuple3#(List#(Tuple2#(String, CON_Out)), 
                 List#(Tuple2#(String, CON_In)),
		 List#(Tuple2#(Integer, CON_Chain))) splitConnections(List#(ConnectionData) l);

  case (l) matches
    tagged Nil: return tuple3(Nil, Nil, Nil);
    default:
    begin
      match {.sends, .recs, .chns} = splitConnections(List::tail(l));
      case (List::head(l)) matches
	tagged LSend .t:
	  return tuple3(List::cons(t,sends), recs, chns);
	tagged LRec .t:
	  return tuple3(sends, List::cons(t, recs), chns);
	tagged LChain .t:
	  return tuple3(sends, recs, List::cons(t, chns));
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

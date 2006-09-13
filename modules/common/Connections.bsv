import GetPut::*;
import ClientServer::*;
import RegFile::*;
import Vector::*;
import List::*;
import FIFO::*;
import ModuleCollect::*;
import Connectable::*;

import hasim_base::*;
import hasim_fpgalib::*;

//Instantiate a module with connections exposed

module [Module] instantiateWithConnections#(Connected_Module#(inter_T) m,
                                            List#(DanglingInfo) children) (WithConnections);

  match {.m, .col} <- getCollection(m);
  
  let x <- connectDangling(col, m, children);
  return x;

endmodule

//Instantiate a module with connections exposed

module [Module] instantiateTopLevel#(Connected_Module#(inter_T) m,
                                     List#(DanglingInfo) children) (TopLevel);

  match {.m, .col1} <- getCollection(m);
  match {.m2, .col2} <- getCollection(mkFPGALib);
  let col = List::append(col1, col2);
  
  connectTopLevel(col, m, children);
  return m2;
  
endmodule

//Connection map from conname to local address

typedef Tuple2#(String, Integer) ConMap;

//For each child module, dangling send and receive connections

//               child           sends          recs
typedef Tuple3#(WithConnections, List#(ConMap), List#(ConMap)) DanglingInfo;

//The main connection algorithm 

module [Module] connectDangling#(List#(ConnectionData) ld, 
                                 inter_T i,
                                 List#(DanglingInfo) children)       (WithConnections);
    
  match {.sends, .recs, .chns} = splitConnections(ld);
  
  //XXX Add duplicate name check
  
  //match {.dsends, .drecs, .cncts} = groupByName(sends, recs);
  let tup = groupByName(sends, recs);
  List#(Tuple2#(String, CON_Out)) dsends = tpl_1(tup);
  List#(Tuple2#(String, CON_In))  drecs = tpl_2(tup);
  match {.*, .*, .cncts} = tup;
  
  let numout = length(dsends);
  let numin  = length(drecs);
  
  let nCncts = length(cncts);
  
  //Internal Connections
  for (Integer x = 0; x < nCncts; x = x + 1)
  begin
    match {.nm, .cin, .cout} = cncts[x];
    
    messageM(strConcat("Connecting: ", nm));
    mkConnection(cin, cout);
  
  end
  
  //Children's connections
  let nChildren = length(children);
  List#(DanglingInfo) childs = children; //We can't write to parameters
  Vector#(CON_Addr, CON_Out) outs = newVector();
  Vector#(CON_Addr, CON_In) ins = newVector();
  Integer cur_in = 0;
  Integer cur_out = 0;
  
  
  for (Integer x = 0; x < nChildren; x = x + 1)
  begin
    match {.child, .osends, .orecs} = childs[x];
    
    //Child's send connections
    let nOSends = length(osends);
    
    for (Integer y = 0; y < nOSends; y = y + 1)
    begin
    
      match {.cnm, .caddr} = osends[y];
      
      //First check the current module for a rec
      
      case (lookup(cnm, drecs)) matches
        tagged Valid .cin:
	begin
          messageM(strConcat("Connecting to Child Send: ", cnm));
	  mkConnection(child.outgoing[caddr], cin);
	  drecs = removeItem(cnm, drecs);
	end
	tagged Invalid: //Then check the other children
        begin
	
	  Bool found = False;
	  for (Integer z = x; (z < nChildren) && (!found); z = z + 1)
	  begin
	    match {.c2, .ss, .rs} = childs[z];
	    case (lookup(cnm, rs)) matches
	      tagged Valid .inaddr:
	      begin
	        messageM(strConcat("Connecting Child to Child: ", cnm));
		mkConnection(child.outgoing[caddr], c2.incoming[inaddr]);
		found = True;
		childs[z] = tuple3(c2, ss, removeItem(cnm, rs));
	      end
	    endcase
	  end
	  if (!found) //It's a pass-through
	  begin
	    outs[cur_out] = (interface CON_Out;
		               method CON_Data try() = child.outgoing[caddr].try();
			       method Action success() = child.outgoing[caddr].success();
			     endinterface);
            messageM(strConcat(strConcat(strConcat("Dangling Send [", integerToString(cur_out)), "]: "), cnm));
	    cur_out = cur_out + 1;
	  end
	end
      endcase
    end
    
    //Child's Rec connections
    let nORecs = length(orecs);
    
    for (Integer y = 0; y < nORecs; y = y + 1)
    begin
    
      match {.cnm, .caddr} = orecs[y];
      
      //First check the current module for a send
      
      case (lookup(cnm, dsends)) matches
        tagged Valid .cout:
	begin
          messageM(strConcat("Connecting to Child Rec: ", cnm));
	  mkConnection(cout, child.incoming[caddr]);
	  dsends = removeItem(cnm, dsends);
	end
	tagged Invalid: //Then check the other children
        begin
	
	  Bool found = False;
	  for (Integer z = x; (z < nChildren) && (!found); z = z + 1)
	  begin
	    match {.c2, .ss, .rs} = childs[z];
	    case (lookup(cnm, ss)) matches
	      tagged Valid .outaddr:
	      begin
	        messageM(strConcat("Connecting Child to Child: ", cnm));
		mkConnection(c2.outgoing[outaddr], child.incoming[caddr]);
		found = True;
		childs[z] = tuple3(c2, removeItem(cnm, ss), rs);
	      end
	    endcase
	  end
	  if (!found) //It's a pass-through
	  begin
	    ins[cur_in] = (interface CON_In;
		               method Action get_TRY(CON_Data x) = child.incoming[caddr].get_TRY(x);
			       method Bool get_SUCCESS() = child.incoming[caddr].get_SUCCESS();
			     endinterface);
            messageM(strConcat(strConcat(strConcat("Dangling Rec [", integerToString(cur_in)), "]: "), cnm));
	    cur_in = cur_in + 1;
	  end
	end
      endcase
    end
    
    
  end
  
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

    let ifcs = List::map(tpl_1, childs);
    CON_Chain tmp2 <- prependChildren(ifcs, tmp, x);
 
    mychains[x] = tmp2;
  end
  
  interface outgoing = outs;
  interface incoming = ins;
  interface chains = mychains;
  
endmodule

//Top-Level connections

module [Module] connectTopLevel#(List#(ConnectionData) ld, 
                                 inter_T i,
                                 List#(DanglingInfo) children)       ();
    
  match {.sends, .recs, .chns} = splitConnections(ld);
  
  //XXX Add duplicate name check
  
  //match {.dsends, .drecs, .cncts} = groupByName(sends, recs);
  let tup = groupByName(sends, recs);
  List#(Tuple2#(String, CON_Out)) dsends = tpl_1(tup);
  List#(Tuple2#(String, CON_In))  drecs = tpl_2(tup);
  match {.*, .*, .cncts} = tup;
  
  let numout = length(dsends);
  let numin  = length(drecs);
  
  let nCncts = length(cncts);
  
  //Internal Connections
  for (Integer x = 0; x < nCncts; x = x + 1)
  begin
    match {.nm, .cin, .cout} = cncts[x];
    
    messageM(strConcat("Connecting: ", nm));
    mkConnection(cin, cout);
  
  end
  
  //Children's connections
  let nChildren = length(children);
  List#(DanglingInfo) childs = children; //We can't write to parameters
  Vector#(CON_Addr, CON_Out) outs = newVector();
  Vector#(CON_Addr, CON_In) ins = newVector();
  Integer cur_in = 0;
  Integer cur_out = 0;
  Bool error_occurred = False;
  
  
  for (Integer x = 0; x < nChildren; x = x + 1)
  begin
    match {.child, .osends, .orecs} = childs[x];
    
    //Child's send connections
    let nOSends = length(osends);
    
    for (Integer y = 0; y < nOSends; y = y + 1)
    begin
    
      match {.cnm, .caddr} = osends[y];
      
      //First check the current module for a rec
      
      case (lookup(cnm, drecs)) matches
        tagged Valid .cin:
	begin
          messageM(strConcat("Connecting to Child Send: ", cnm));
	  mkConnection(child.outgoing[caddr], cin);
	  drecs = removeItem(cnm, drecs);
	end
	tagged Invalid: //Then check the other children
        begin
	
	  Bool found = False;
	  for (Integer z = x; (z < nChildren) && (!found); z = z + 1)
	  begin
	    match {.c2, .ss, .rs} = childs[z];
	    case (lookup(cnm, rs)) matches
	      tagged Valid .inaddr:
	      begin
	        messageM(strConcat("Connecting Child to Child: ", cnm));
		mkConnection(child.outgoing[caddr], c2.incoming[inaddr]);
		found = True;
		childs[z] = tuple3(c2, ss, removeItem(cnm, rs));
	      end
	    endcase
	  end
	  if (!found) //It's a pass-through
	  begin
            messageM(strConcat(strConcat(strConcat("Dangling Send [", integerToString(cur_out)), "]: "), cnm));
	    cur_out = cur_out + 1;
	    error_occurred = True;
	  end
	end
      endcase
    end
    
    //Child's Rec connections
    let nORecs = length(orecs);
    
    for (Integer y = 0; y < nORecs; y = y + 1)
    begin
    
      match {.cnm, .caddr} = orecs[y];
      
      //First check the current module for a send
      
      case (lookup(cnm, dsends)) matches
        tagged Valid .cout:
	begin
          messageM(strConcat("Connecting to Child Rec: ", cnm));
	  mkConnection(cout, child.incoming[caddr]);
	  dsends = removeItem(cnm, dsends);
	end
	tagged Invalid: //Then check the other children
        begin
	
	  Bool found = False;
	  for (Integer z = x; (z < nChildren) && (!found); z = z + 1)
	  begin
	    match {.c2, .ss, .rs} = childs[z];
	    case (lookup(cnm, ss)) matches
	      tagged Valid .outaddr:
	      begin
	        messageM(strConcat("Connecting Child to Child: ", cnm));
		mkConnection(c2.outgoing[outaddr], child.incoming[caddr]);
		found = True;
		childs[z] = tuple3(c2, removeItem(cnm, ss), rs);
	      end
	    endcase
	  end
	  if (!found) //It's a pass-through
	  begin
            messageM(strConcat(strConcat(strConcat("Dangling Rec [", integerToString(cur_in)), "]: "), cnm));
	    cur_in = cur_in + 1;
	    error_occurred = True;
	  end
	end
      endcase
    end
    
    
  end
  
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

    let ifcs = List::map(tpl_1, childs);
    CON_Chain tmp2 <- prependChildren(ifcs, tmp, x);
 
    //Close the chain
    mkConnection(tmp2, tmp2);
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

module prependChildren#(List#(WithConnections) childs, CON_Chain chn, Integer x) (CON_Chain);

  CON_Chain c = chn;
  let nChildren = length(childs);
  for (Integer z = (nChildren-1); z >= 0; z = z - 1)
  begin
    CON_Chain c2 = childs[z].chains[x];
    messageM(strConcat(strConcat("Linking Chain to Child [", integerToString(x)), "]"));
    mkConnection(c2, c);
    c = c2;
  end

  return (interface CON_Chain;
            method first() = chn.first();
	    method deq() = chn.deq();
	    method enq() = c.enq();
	    method clear = noAction; //If you want to implement this, broadcast
	  endinterface);

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
      return Invalid;
    default:
    begin
      match {.d, .v} = List::head(l);
      return (d == data) ? (Valid v) : lookup(data, List::tail(l));
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

//splitConnections :: [ConnectionData] -> ([(String, CON_Out)], [(String, CON_In)])

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

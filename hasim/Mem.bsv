import Datatypes::*;

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

module [Module] mkMem(Memory#(Addr, Inst, Value, Token)) provisos(Bits#(PRName, psz));

  FIFO#(MemResp#(Value))        f <- mkFIFO();

  RegFile#(Addr, Inst)  imemory <- mkRegFileFull();
  Wire#(Inst) iresp <- mkWire();
  
  RegFile#(Addr, Value) dmemory <- mkRegFileFull();

  Reg#(Vector#(TExp#(psz), Bool))                              tvalids <- mkReg(Vector::replicate(False));
  Reg#(Vector#(TExp#(psz), Tuple3#(Token,Addr,Value)))          tokens <- mkRegU();

  function maybify(b,x) = b ? Just(x): Nothing;

  let mtokens = Vector::zipWith(maybify, tvalids, tokens);

  function canStore(v) = !(pack(v) != maxBound);  // not all True

  function matchAddr(a,b,x);
    match {.*,.addr,.*} = x;
    return (b && (a == addr)) ? Just(x) : Nothing;
  endfunction
 
  interface Server imem;
   
    interface Put request;
      method Action put(Addr a);
        iresp <= imemory.sub(a);
      endmethod
    endinterface
 
    interface Get response;
      method ActionValue#(Inst) get();
        return iresp;
      endmethod
    endinterface
  
  endinterface
 
  interface Server dmem;
    
    interface Put request;

      method Action put(MemReq#(Addr, Token, Value) req);// if (canStore(tvalids));

	function Value getResult(Token youngest, Addr a);
	  function youngerToken(Token x, Token y) = (youngest - x) < (youngest - y);

	  let mmtokens = Vector::zipWith(matchAddr(a),tvalids, tokens);

	  function pickYoungest (mta,mtb) = (!isJust(mta)) ? mtb:
	                                    (!isJust(mtb)) ? Nothing:
                                            youngerToken((unJust(mta)).fst,(unJust(mtb)).fst)? mta:mtb;
	  let finalChoice = Vector::fold(pickYoungest, mmtokens);

	  case(finalChoice) matches
            tagged Nothing: return dmemory.sub(a); // goto memory
            tagged Just {.*,.*,.v}: return v;
	  endcase
	endfunction

	 case (req) matches
	   tagged Ld .ld_info:
             begin
               let v = getResult(ld_info.token, ld_info.addr);
               f.enq(LdResp v);
             end
	   tagged St .st_info:
             begin
               //Response
               let v = getResult(st_info.token, st_info.addr); // use this as the "old value"
               f.enq(StResp);
               //drop in Buffer

	       let num_mtokens = Vector::zip(mtokens,genVector);

               function choose (a,b) = (!isJust(a.fst)) ? a : b;

               Vector#(TExp#(psz), Bool)                       new_tvalids = newVector();
               Vector#(TExp#(psz), Tuple3#(Token,Addr,Value))  new_tokens = newVector();

               Integer i = 0;
               Bool done = False;
               for(i = 0; i < valueOf(TExp#(psz)); i = i + 1)
        	 begin
        	   new_tvalids[i] = (done || tvalids[i]) ? tvalids[i]: True;
        	   new_tokens[i]  = (done || tvalids[i]) ? tokens[i]:
                                                	   tuple3(st_info.token, st_info.addr, st_info.val);

        	   if (!tvalids[i])
                     done = True;
        	 end

        	tvalids <= new_tvalids;
        	tokens  <= new_tokens;
             end
	 endcase
      endmethod
      
    endinterface
  
    interface Get response;

      method ActionValue#(MemResp#(Value)) get();
	f.deq();
	return f.first();
      endmethod

    endinterface
  
  endinterface

  method Action      commit(Token token);
    function matchToken(t,mx)  =
      case (mx) matches
	tagged Just {.tok,.*,.*}: return (t == tok);
        tagged Nothing        : return False;
      endcase;	

    function ff(ma, mb) = matchToken(token,ma) ? ma: mb;
    
    let mresult = fold(ff, mtokens); // the value

    case (mresult) matches
      tagged Just {.*,.addr,.val}:
        dmemory.upd(addr, val);
      tagged Nothing:
        noAction;
    endcase

    function flattenToken(b,x);
      match {.tok,.*,.*} = x;
      return (token == tok) ? False: b;
    endfunction

    tvalids <= Vector::zipWith(flattenToken, tvalids,tokens);

  endmethod

  method Action        killRange(Token lb, Token ub); 
     function flattenToken(b,x);
       match {.tok,.*,.*} = x;
       return (ub-lb > (tok - lb)) ? False: b;
     endfunction

     tvalids <= Vector::zipWith(flattenToken, tvalids,tokens);
  endmethod

  interface magic_imem = imemory;
  interface magic_dmem = dmemory;

endmodule

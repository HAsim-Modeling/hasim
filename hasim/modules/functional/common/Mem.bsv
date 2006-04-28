//Memory system with ports

import HASim::*;
import Ports::*;

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import SVector::*;
import BypassFIFO::*;

module [Module] mkMem
    //interface:
                (Memory#(token_T,   //Token type
		         addr_T,    //Address type
		         inst_T,    //Instruction type
			 value_T))  //Value type
    provisos
            (Bits#(addr_T, addr_SZ),
	     Bits#(inst_T, inst_SZ),
	     Bits#(value_T, value_SZ),
	     Bits#(token_T, token_SZ),
	     Bits#(MemReq#(token_T, addr_T, value_T), memreq_SZ),
	     Bits#(MemResp#(value_T), memresp_SZ),
	     Eq#(addr_T),
	     Eq#(token_T),
	     Bounded#(addr_T),
	     Arith#(token_T),
	     Ord#(token_T),
	     PrimIndex#(token_T),
	     Add#(1, n1, TExp#(token_SZ))); //Token size must be greater than one.

  //State elements

  FIFO#(MemResp#(value_T)) f <- mkFIFO();

  RegFile#(addr_T, inst_T) imemory <- mkRegFileFull();
  FIFO#(inst_T) iresp <- mkBypassFIFO();
  
  RegFile#(addr_T, value_T) dmemory <- mkRegFileFull();

  Reg#(SVector#(TExp#(token_SZ), Bool)) tvalids <- mkReg(SVector::replicate(False));
  Reg#(SVector#(TExp#(token_SZ), Tuple3#(token_T, addr_T, value_T))) tokens <- mkRegU();

  //Ports
  
  Port_Server#(addr_T, inst_T) port_imem <- mkPort_Server("mem_imem");
  Port_Server#(MemReq#(token_T, addr_T, value_T), MemResp#(value_T)) port_dmem <- mkPort_Server("mem_dmem");
  Port_Receive#(token_T) port_commit <- mkPort_Receive("mem_commit");
  Port_Receive#(Tuple2#(token_T, token_T)) port_killRange <- mkPort_Receive("mem_killRange");

  //maybify :: Bool -> any -> Maybe any

  function Maybe#(any_T) maybify(Bool b, any_T x);
  
    return b ? Just(x) : Nothing;
  
  endfunction

  //getNextFree :: SVector n Bool -> Integer
  
  function Nat getNextFree(SVector#(n, Bool) vs);

    Integer k = valueof(n) - 1;

    Nat res = fromInteger(k);

    for (Integer x = k; x > 0; x = x - 1)
      res = !vs[x] ? fromInteger(x) : res;

    return res;
  
  endfunction


  SVector#(TExp#(token_SZ), Maybe#(Tuple3#(token_T, addr_T, value_T))) mtokens = SVector::zipWith(maybify, tvalids, tokens);
 
 
  //matchAddr :: Maybe (token, addr, value) -> addr -> Bool -> Maybe (token, addr, value)

  function Maybe#(Tuple3#(token_T, addr_T, value_T)) matchAddr(addr_T a, Bool b, Tuple3#(token_T, addr_T, value_T) x);
  
    match {.*, .addr, .*} = x;
    
    return (b && (a == addr)) ? Just(x) : Nothing;
    
  endfunction
 
  //handleIMEM
  
  //Handles all IMem requests

  rule handleIMEM (True);
  
    addr_T a <- port_imem.getReq();
    port_imem.makeResp(imemory.sub(a));
    
  endrule
 
  //handleDMEM
  
  //handles Dmem loads/stores but not commits/rollbacks
 
  rule handleDMEM (True);

   function value_T getResult(token_T youngest, addr_T a);

     //youngerToken :: token -> token -> Bool

     function Bool youngerToken(token_T x, token_T y)
	            provisos 
	                    (Ord#(token_T));

	return (youngest - x) < (youngest - y);

     endfunction

     let mmtokens = SVector::zipWith(matchAddr(a), tvalids, tokens);

     //pickYoungest :: Maybe (Bool, token) -> Maybe (Bool, token) -> Maybe#(Bool, token)

     function Maybe#(Tuple3#(token_T, addr_T, value_T)) pickYoungest(Maybe#(Tuple3#(token_T, addr_T, value_T)) mta, 
	                                                             Maybe#(Tuple3#(token_T, addr_T, value_T)) mtb);

       return (!isJust(mta)) ? mtb :
	      (!isJust(mtb)) ? Nothing :
               youngerToken((unJust(mta)).fst,(unJust(mtb)).fst)? mta : mtb;

     endfunction

     Maybe#(Tuple3#(token_T, addr_T, value_T)) finalChoice = SVector::fold(pickYoungest, mmtokens);

     case (finalChoice) matches
       tagged Nothing: return dmemory.sub(a); // goto memory
       tagged Just {.*,.*,.v}: return v;
     endcase
   endfunction
    
    MemReq#(token_T, addr_T, value_T) req <- port_dmem.getReq();
    
    case (req) matches
      tagged Ld .ld_info:
        begin
          let v = getResult(ld_info.token, ld_info.addr);
          port_dmem.makeResp(LdResp v);
        end
      tagged St .st_info:
        begin
          //Response
          let v = getResult(st_info.token, st_info.addr); // use this as the "old value"
          port_dmem.makeResp(StResp);
          //drop in Buffer

	  let num_tvalids = SVector::zip(tvalids, genSVector);

	  Nat i = getNextFree(tvalids);

	  //tvalids <= update(tvalids, i, True); //XXX
	  tvalids <= unpack(pack(tvalids) | 1 << i);
	  tokens <= update(tokens, i, tuple3(st_info.token, st_info.addr, st_info.val));

        end
    endcase
  
  endrule
 
  //handleCommit
  
  //Actually commits stores
 
  rule handleCommit (True);
  
    token_T token <- port_commit.receive();

    //matchToken :: token -> Maybe (token, addr, value) -> Bool

    function Bool matchToken(token_T t, Maybe#(Tuple3#(token_T, addr_T, value_T)) mx);

      return case (mx) matches
	tagged Just {.tok,.*,.*}: return (t == tok);
        tagged Nothing        : return False;
      endcase;

    endfunction	

    //ff :: Maybe (token, addr, value) -> Maybe (token, addr, value) -> Maybe (token, addr, value)

    function Maybe#(Tuple3#(token_T, addr_T, value_T)) ff(Maybe#(Tuple3#(token_T, addr_T, value_T)) ma, Maybe#(Tuple3#(token_T, addr_T, value_T)) mb);

      return matchToken(token, ma) ? ma: mb;

    endfunction

    Maybe#(Tuple3#(token_T, addr_T, value_T)) mresult = fold(ff, mtokens); // the value

    case (mresult) matches

      tagged Just {.*, .addr, .val}:
        dmemory.upd(addr, val);
      tagged Nothing:
        noAction;

    endcase

    //XXX Commented out because this is expensive
    /*
    //flattenToken :: Bool -> (token, addr, value) -> Bool
    function Bool flattenToken(Bool b, Tuple3#(token_T, addr_T, value_T) x);
      match {.tok,.*,.*} = x;
      return (token == tok) ? False: b;
    endfunction

    tvalids <= SVector::zipWith(flattenToken, tvalids, tokens);
    */
    
    tvalids <= unpack(pack(tvalids) & ~(1 << toIndex(token)));

  endrule
  
  //handleKillRange
  
  //Rolls back killed tokens
  
  rule handleKillRange (True);
  
    Tuple2#(token_T, token_T) tup <- port_killRange.receive();
    match {.lb, .ub} = tup;

    function flattenToken(b, x);
      match {.tok, .*, .*} = x;
      return (ub-lb > (tok - lb)) ? False: b;
    endfunction

    tvalids <= SVector::zipWith(flattenToken, tvalids, tokens);
  endrule
 
  //IMem interface (used by FP Fetch)
  interface imem = port_imem.server;
  
  //DMem interface (used by FP Mem)
  interface dmem = port_dmem.server;
 
  //commit (used by FP Global Commit)
  interface commit = port_commit.incoming;


  //killRange (used by FP.killToken)
  interface killRange = port_killRange.incoming;
  
  //Magic interface for testharness

  interface magic_imem = imemory;
  interface magic_dmem = dmemory;

endmodule

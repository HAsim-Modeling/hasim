//Memory system with links

import HASim::*;

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import SVector::*;
import BypassFIFO::*;

/************* Memory System Interface *************/


// Data Memory request

typedef union tagged 
{
  struct {token_T token; addr_T addr;              } Ld;
  struct {token_T token; addr_T addr; value_T val; } St;
}
  MemReq#(parameter type token_T,
          parameter type addr_T,
	  parameter type value_T) 
    deriving
            (Eq,Bits);


// Data Memory Response

typedef union tagged {
  value_T LdResp;
  void    StResp;
}
  MemResp#(parameter type value_T) 
    deriving
            (Eq, Bits);


// Memory System Interface

// The memory system consists of two major parts: the IMem and DMem.
// The IMem is a simple Server (Address, Instruction)
// The DMem uses the above MemReq/MemResp types
// Additionally requests can be committed or killed in the DMem
// They are committed by Global Commit and killed by killToken

// For now the memory also has a "magic" link for the controller to
// load the test case. This may disappear in the future.

interface Memory#(type token_T,  //Token tye
                  type addr_T,   //Address t,ype
                  type inst_T,   //Instruction type
		  type value_T); //Value type

  interface Server#(addr_T, inst_T) imem;
  interface Server#(MemReq#(token_T, addr_T, value_T), MemResp#(value_T)) dmem;
  
  interface Put#(token_T) commit;
  interface Put#(Tuple2#(token_T, token_T)) killRange; 
  
  //Magic link for the test harness to load the program
  interface RegFile#(addr_T, inst_T) magic_imem;
  interface RegFile#(addr_T, value_T) magic_dmem;

endinterface

/************* Simple Memory System Implementation *************/

// This is intended for software simulation. An FPGA version would
// be a memory controller.

module [Module] mkMem_Software
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
	     PrimIndex#(token_T, token_PK),
	     Add#(1, n1, TExp#(token_SZ))); //Token size must be greater than one.

  //State elements

  FIFO#(MemResp#(value_T)) f <- mkFIFO();

  RegFile#(addr_T, inst_T) imemory <- mkRegFileFull();
  FIFO#(inst_T) iresp <- mkBypassFIFO();
  
  RegFile#(addr_T, value_T) dmemory <- mkRegFileFull();

  Reg#(SVector#(TExp#(token_SZ), Bool)) tvalids <- mkReg(SVector::replicate(False));
  Reg#(SVector#(TExp#(token_SZ), Tuple3#(token_T, addr_T, value_T))) tokens <- mkRegU();

  //Links
  
  Link_Server#(addr_T, inst_T) link_imem <- mkLink_Server("mem_imem");
  Link_Server#(MemReq#(token_T, addr_T, value_T), MemResp#(value_T)) link_dmem <- mkLink_Server("mem_dmem");
  Link_Receive#(token_T) link_commit <- mkLink_Receive("mem_commit");
  Link_Receive#(Tuple2#(token_T, token_T)) link_killRange <- mkLink_Receive("mem_killRange");

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
  
    addr_T a <- link_imem.getReq();
    link_imem.makeResp(imemory.sub(a));
    
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
    
    MemReq#(token_T, addr_T, value_T) req <- link_dmem.getReq();
    
    case (req) matches
      tagged Ld .ld_info:
        begin
          let v = getResult(ld_info.token, ld_info.addr);
          link_dmem.makeResp(LdResp v);
        end
      tagged St .st_info:
        begin
          //Response
          let v = getResult(st_info.token, st_info.addr); // use this as the "old value"
          link_dmem.makeResp(StResp);
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
  
    token_T token <- link_commit.receive();

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
    
    tvalids <= unpack(pack(tvalids) & ~(1 << token));

  endrule
  
  //handleKillRange
  
  //Rolls back killed tokens
  
  rule handleKillRange (True);
  
    Tuple2#(token_T, token_T) tup <- link_killRange.receive();
    match {.lb, .ub} = tup;

    function flattenToken(b, x);
      match {.tok, .*, .*} = x;
      return (ub-lb > (tok - lb)) ? False: b;
    endfunction

    tvalids <= SVector::zipWith(flattenToken, tvalids, tokens);
  endrule
 
  //IMem interface (used by FP Fetch)
  interface imem = link_imem.server;
  
  //DMem interface (used by FP Mem)
  interface dmem = link_dmem.server;
 
  //commit (used by FP Global Commit)
  interface commit = link_commit.incoming;


  //killRange (used by FP.killToken)
  interface killRange = link_killRange.incoming;
  
  //Magic interface for testharness

  interface magic_imem = imemory;
  interface magic_dmem = dmemory;

endmodule

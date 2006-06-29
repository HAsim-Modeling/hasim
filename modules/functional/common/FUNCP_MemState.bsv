//Memory system with connections

import HASim::*;
import ISA::*;

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;
import BypassFIFO::*;

/************* Memory System Interface *************/


// Data Memory request

typedef union tagged 
{
  struct {Token token; Addr addr;            } Ld;
  struct {Token token; Addr addr; Value val; } St;
}
  MemReq 
    deriving
            (Eq, Bits);


// Data Memory Response

typedef union tagged {
  Value LdResp;
  void  StResp;
}
  MemResp 
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

interface Memory;

  //interface Server#(Addr, inst_T) imem;
  //interface Server#(MemReq#(Token, Addr, Value), MemResp#(Value)) dmem;
  
  //interface Put#(Token) commit;
  //interface Put#(Tuple2#(Token, Token)) killRange; 
  
  //Magic link for the test harness to load the program
  interface RegFile#(Addr, Inst)  magic_imem;
  interface RegFile#(Addr, Value) magic_dmem;

endinterface

/************* Simple Memory System Implementation *************/

// This is intended for software simulation. An FPGA version would
// be a memory controller.

module [HASim_Module] mkMem_Software
    //interface:
                (Memory)
    provisos
            (Bits#(Token, token_SZ),
	     Transmittable#(Addr),
	     Transmittable#(Inst),
	     Transmittable#(Value),
	     Transmittable#(Token),
	     Transmittable#(MemReq),
	     Transmittable#(MemResp),
	     Transmittable#(Tuple2#(Token, Token)),
	     Add#(1, n1, TExp#(token_SZ)));  //Token size must be greater than one.
	    
  SoftAddr maxSoftAddr = maxBound();
  Addr maxAddr = zeroExtend(maxSoftAddr);

  //State elements

  FIFO#(MemResp) f <- mkFIFO();

  RegFile#(SoftAddr, Inst) imemory <- mkRegFileFull();
  
  RegFile#(SoftAddr, Value) dmemory <- mkRegFileFull();

  Reg#(Vector#(TExp#(token_SZ), Bool)) tvalids <- mkReg(Vector::replicate(False));
  Reg#(Vector#(TExp#(token_SZ), Tuple3#(Token, SoftAddr, Value))) tokens <- mkRegU();

  //Connections
  
  Connection_Server#(Addr, Inst)             link_imem <- mkConnection_Server("mem_imem");
  Connection_Server#(MemReq, MemResp)        link_dmem <- mkConnection_Server("mem_dmem");
  Connection_Receive#(Token)                 link_commit <- mkConnection_Receive("mem_commit");
  //Connection_Receive#(Tuple2#(Token, Token)) link_killRange <- mkConnection_Receive("mem_killRange");

  //maybify :: Bool -> any -> Maybe any

  function Maybe#(any_T) maybify(Bool b, any_T x);
  
    return b ? Just(x) : Nothing;
  
  endfunction

  //getNextFree :: Vector n Bool -> Integer
  
  function Nat getNextFree(Vector#(n, Bool) vs);

    Integer k = valueof(n) - 1;

    Nat res = fromInteger(k);

    for (Integer x = k; x > 0; x = x - 1)
      res = !vs[x] ? fromInteger(x) : res;

    return res;
  
  endfunction


  Vector#(TExp#(token_SZ), Maybe#(Tuple3#(Token, SoftAddr, Value))) mtokens = Vector::zipWith(maybify, tvalids, tokens);
 
 
  //matchAddr :: Maybe (token, addr, value) -> addr -> Bool -> Maybe (token, addr, value)

  function Maybe#(Tuple3#(Token, SoftAddr, Value)) matchAddr(SoftAddr a, Bool b, Tuple3#(Token, SoftAddr, Value) x);
  
    match {.*, .addr, .*} = x;
    
    return (b && (a == addr)) ? Just(x) : Nothing;
    
  endfunction
 
  //handleIMEM
  
  //Handles all IMem requests

  rule handleIMEM (True);
  
    Addr a <- link_imem.getReq();
    
    if (a > maxAddr)
      $display("WARNING [0]: Address 0x%h out of bounds. Increase software address length!", a);
    
    SoftAddr sa = truncate(a);
    link_imem.makeResp(imemory.sub(sa));
    
  endrule
 
  //handleDMEM
  
  //handles Dmem loads/stores but not commits/rollbacks
 
  rule handleDMEM (True);

   function Value getResult(Token youngest, SoftAddr a);

     //youngerToken :: token -> token -> Bool

     function Bool youngerToken(Token x, Token y)
	            provisos 
	                    (Ord#(Token));

	return (youngest - x) < (youngest - y);

     endfunction

     let mmtokens = Vector::zipWith(matchAddr(a), tvalids, tokens);

     //pickYoungest :: Maybe (Bool, token) -> Maybe (Bool, token) -> Maybe#(Bool, token)

     function Maybe#(Tuple3#(Token, SoftAddr, Value)) pickYoungest(Maybe#(Tuple3#(Token, SoftAddr, Value)) mta, 
	                                                             Maybe#(Tuple3#(Token, SoftAddr, Value)) mtb);

       return (!isJust(mta)) ? mtb :
	      (!isJust(mtb)) ? Nothing :
               youngerToken((unJust(mta)).fst,(unJust(mtb)).fst)? mta : mtb;

     endfunction

     Maybe#(Tuple3#(Token, SoftAddr, Value)) finalChoice = Vector::fold(pickYoungest, mmtokens);

     case (finalChoice) matches
       tagged Nothing: return dmemory.sub(a); // goto memory
       tagged Just {.*,.*,.v}: return v;
     endcase
   endfunction
    
    MemReq req <- link_dmem.getReq();
    
    case (req) matches
      tagged Ld .ld_info:
        begin
	  if (ld_info.addr > maxAddr)
            $display("WARNING [1]: Address 0x%h out of bounds. Increase software address length!", ld_info.addr);

	  SoftAddr sa = truncate(ld_info.addr);
          
	  let v = getResult(ld_info.token, sa);
          link_dmem.makeResp(LdResp v);
	  
        end
      tagged St .st_info:
        begin
	  if (st_info.addr > maxAddr)
            $display("WARNING [2]: Address 0x%h out of bounds. Increase software address length!", st_info.addr);
          
	  SoftAddr sa = truncate(st_info.addr);
	  
          //Response
          let v = getResult(st_info.token, sa); // use this as the "old value"
          link_dmem.makeResp(StResp);
          //drop in Buffer

	  let num_tvalids = Vector::zip(tvalids, genVector);

	  Nat i = getNextFree(tvalids);

	  //tvalids <= update(tvalids, i, True); //XXX
	  tvalids <= unpack(pack(tvalids) | 1 << i);
	  tokens <= update(tokens, i, tuple3(st_info.token, sa, st_info.val));

        end
    endcase
  
  endrule
 
  //handleCommit
  
  //Actually commits stores
 
  rule handleCommit (True);
  
    Token token <- link_commit.receive();

    //matchToken :: token -> Maybe (token, addr, value) -> Bool

    function Bool matchToken(Token t, Maybe#(Tuple3#(Token, SoftAddr, Value)) mx);

      return case (mx) matches
	tagged Just {.tok,.*,.*}: return (t == tok);
        tagged Nothing        : return False;
      endcase;

    endfunction	

    //ff :: Maybe (token, addr, value) -> Maybe (token, addr, value) -> Maybe (token, addr, value)

    function Maybe#(Tuple3#(Token, SoftAddr, Value)) ff(Maybe#(Tuple3#(Token, SoftAddr, Value)) ma, Maybe#(Tuple3#(Token, SoftAddr, Value)) mb);

      return matchToken(token, ma) ? ma: mb;

    endfunction

    Maybe#(Tuple3#(Token, SoftAddr, Value)) mresult = fold(ff, mtokens); // the value

    case (mresult) matches

      tagged Just {.*, .addr, .val}:
        dmemory.upd(addr, val);
      tagged Nothing:
        noAction;

    endcase

    //XXX Commented out because this is expensive
    /*
    //flattenToken :: Bool -> (token, addr, value) -> Bool
    function Bool flattenToken(Bool b, Tuple3#(Token, Addr, Value) x);
      match {.tok,.*,.*} = x;
      return (token == tok) ? False: b;
    endfunction

    tvalids <= Vector::zipWith(flattenToken, tvalids, tokens);
    */
    
    tvalids <= unpack(pack(tvalids) & ~(1 << token));

  endrule
  
  //handleKillRange
  
  //Rolls back killed tokens
  /*
  rule handleKillRange (True);
  
    Tuple2#(Token, Token) tup <- link_killRange.receive();
    match {.lb, .ub} = tup;

    function flattenToken(b, x);
      match {.tok, .*, .*} = x;
      return (ub-lb > (tok - lb)) ? False: b;
    endfunction

    tvalids <= Vector::zipWith(flattenToken, tvalids, tokens);
  endrule
 */
  //IMem interface (used by FP Fetch)
  //interface imem = link_imem.server;
  
  //DMem interface (used by FP Mem)
  //interface dmem = link_dmem.server;
 
  //commit (used by FP Global Commit)
  //interface commit = link_commit.incoming;


  //killRange (used by FP.killToken)
  //interface killRange = link_killRange.incoming;
  
  //Magic interface for testharness

  interface RegFile magic_imem;
  
    method upd(a,v) = imemory.upd(truncate(a), v);
    method sub(a) = imemory.sub(truncate(a));
  
  endinterface
  
  interface RegFile magic_dmem;

    method upd(a,v) = dmemory.upd(truncate(a), v);
    method sub(a) = dmemory.sub(truncate(a));
  
  endinterface

endmodule

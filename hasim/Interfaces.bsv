import GetPut::*;
import ClientServer::*;


interface FM_Unit#(type tick_T, type token_T,  type init_T, type req_T, type resp_T, type next_T);

  interface Put#(Tuple2#(token_T, init_T)) in;

  //method Action                                     putPrevFM(Tuple2#(token_T, init_T) x);

  
  interface Server#(Tuple3#(token_T, req_T, tick_T), Tuple2#(token_T, resp_T)) server;
  
  //method Action                                     putTM(Tuple3#(token_T, req_T, tick_T) x);
  //method ActionValue#(Tuple2#(token_T, resp_T))     getTM(tick_T t);


  interface Get#(Tuple2#(token_T, next_T)) out;
  //method ActionValue#(Tuple2#(token_T, next_T))     getNextFM();

 
  method Action                                     killToken(token_T t);

endinterface

typedef UInt#(9) Token;
typedef UInt#(64) Tick;
typedef Bit#(32)  Addr;
typedef Bit#(5) RName;
typedef Bit#(32) Value;

typedef Server#(Tuple3#(token_T, init_T, req_T), Tuple3#(token_T, resp_T, next_T))
        Unit#(type token_T, type init_T, type req_T, type resp_T, type next_T);
/*
interface Unit#(type token_T, type init_T, type req_T, type resp_T, type next_T);
  method Action request(token_T t, init_T i, req_T a);
  method ActionValue#(Tuple3#(token_T,resp_T,next_T)) response();
endinterface
*/

interface BypassUnit#(type vreg_T, type preg_T, type value_T, type token_T);
  // first is new pointer, second is old. if no new pointer, the first will be undefined
  method ActionValue#(Tuple2#(preg_T,preg_T)) makeMapping(Maybe#(vreg_T) x, token_T tok); //token is the ref name
  method preg_T lookup1(vreg_T v);
  method preg_T lookup2(vreg_T v);

  method Maybe#(value_T) read1(preg_T i);
  method Maybe#(value_T) read2(preg_T i);
  method Maybe#(value_T) read3(preg_T i);
  method Maybe#(value_T) read4(preg_T i);

  method Action write1(preg_T i, value_T v);
  method Action write2(preg_T i, value_T v);

  method Action freePReg(preg_T x);
  method Action rewindtoToken(token_T tok);
endinterface

interface Memory#(type req_T, type resp_T, type token_T);

  interface Server#(req_T, resp_T) server;
  method Action      commit(token_T token);
  method Action        killRange(token_T lb, token_T ub); 
endinterface


interface FunctionalPartition#(type tick_T, type token_T,
                               type fet_req_T, type fet_resp_T,
			       type dec_req_T, type dec_resp_T,
			       type exe_req_T, type exe_resp_T,
			       type mem_req_T, type mem_resp_T,
			       type lcm_req_T, type lcm_resp_T,
			       type gcm_req_T, type gcm_resp_T);

  interface Server#(Tuple3#(token_T, void, tick_T), Tuple2#(token_T, void))            tokgen;
  interface Server#(Tuple3#(token_T, fet_req_T, tick_T), Tuple2#(token_T, fet_resp_T)) fetch;
  interface Server#(Tuple3#(token_T, dec_req_T, tick_T), Tuple2#(token_T, dec_resp_T)) decode;
  interface Server#(Tuple3#(token_T, exe_req_T, tick_T), Tuple2#(token_T, exe_resp_T)) execute;
  interface Server#(Tuple3#(token_T, mem_req_T, tick_T), Tuple2#(token_T, mem_resp_T)) memory;
  interface Server#(Tuple3#(token_T, lcm_req_T, tick_T), Tuple2#(token_T, lcm_resp_T)) local_commit;
  interface Server#(Tuple3#(token_T, gcm_req_T, tick_T), Tuple2#(token_T, gcm_resp_T)) global_commit;
  
  method Action killToken(token_T t);

endinterface

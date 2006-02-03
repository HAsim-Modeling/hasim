interface FM_Unit#(type tick_T, type token_T,  type init_T, type req_T, type resp_T, type next_T);

  method Action                                     putPrevFM(Tuple2#(token_T, init_T) x);

  
  method Action                                     putTM(Tuple3#(token_T, req_T, tick_T) x);
  method ActionValue#(Tuple2#(token_T, resp_T))     getTM(tick_T t);


  method ActionValue#(Tuple2#(token_T, next_T))     getNextFM();

 
  method Action                                     killToken(token_T t);

endinterface

interface Unit#(type token_T, type init_T, type req_T, type resp_T, type next_T);
  method Action request(token_T t, init_T i, req_T a);
  method ActionValue#(Tuple3#(token_T,resp_T,next_T)) response();
endinterface

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
  method Action request(req_T req);
  method ActionValue#(resp_T) response();
  method Action      commit(token_T token);
  method Action        kill(token_T lb, token_T ub); 
endinterface
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


interface FM_Unit#(type tick_T, type init_T, type token_T, type req_T, type resp_T, type next_T);

  method Action                                     putPrevFM(Tuple2#(token_T, init_T) x);
  
  method ActionValue#(token_T)                      putTM(Tuple2#(req_T, tick_T) x);
  method ActionValue#(Tuple2#(token_T, resp_T))     getTM(tick_T t);

  method ActionValue#(Tuple2#(token_T, next_T))     getNextFM();
 
  method Action                                     killToken(token_T t);

endinterface


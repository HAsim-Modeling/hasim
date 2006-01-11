

interface Connection#(type a);
  method 


endinterface

interface FM_fetch(type ia, type instr, type em_clk, type  token);
  method Action                          request_inst_to_tm(ia x, em_clk t);
  method ActionValue#(token)             requested_inst_token();
  method ActionValue#(<token,instr>)     response_inst_to_tm();
  method ActionValue#(<inst,token>)      data_to_FM_decode();

  method Action killToken(token);
endinterface


interface FM_unit(type em_clk, type token, type req_T, type resp_T, type next_T);
  method Action put_from_previous_FM_stage(<token, init_T>);
  
  method ActionValue#(<token>)         request_from_TM_stage(<req_T,em_clk>);
  method ActionValue#(<resp_T, token>) get_response_to_TM_stage(em_clk);

  method ActionValue#(<token, next_T>) get_to_next_FM_stage();
 
  method Action killToken(<token>);

endinterface
import hasim_common::*;

module [HASIM_MODULE] mkAFU_Alg ();
   
  Connection_Server#(Bit#(256), Bit#(256)) link_dme <- mkConnection_Server("dme_to_afu");
  
  rule passThrough (True);
  
    let d = link_dme.getReq();
    link_dme.deq();
    
    link_dme.makeResp(d);
    
  endrule
  
endmodule

import hasim_common::*;

module [HASim_Module] mkAFU_Alg ();
   
  Connection_Server#(Bit#(256), Bit#(256)) link_dme <- mkConnection_Server("dme_to_afu");
  
  rule passThrough (True);
  
    let d <- link_dme.getReq();
    link_dme.makeResp(d + 1);
    
  endrule
  
endmodule
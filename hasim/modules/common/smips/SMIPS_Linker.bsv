
//mkTOY_FP :: FunctionalPartition

(* synthesize *)
module [Module] mkTOY_FP
    //interface:						       
  		(TOY_FunctionalPartition#(TOY_Tick,             //tick type
		                	  TOY_Token,            //token type
		                	  TOY_Addr,             //address type
					  TOY_Inst,             //instruction type
					  TOY_Value,            //value type
		                	  void, void,           //tokenReq, tokenResp,
  					  TOY_Addr, TOY_Inst,   //fetchReq, fetchResp
  					  void, TOY_DepInfo,    //decodeReq, decodeResp
  					  void, TOY_InstResult, //execReq, execResp
  					  void, void,           //memReq, memResp
  					  void, void,           //lcommitReq, lcommitResp
  					  void, void));         //gcommitReq, gcommitResp  

  let bypass <- mkBypassUnit();
  
  let port_to_imem           <- mkPort_Client("mem_imem");
  let port_to_dmem           <- mkPort_Client("mem_dmem");
  let port_to_mem_commit     <- mkPort_Send("mem_commit");
  let port_to_mem_killRange  <- mkPort_Send("mem_killRange");
  
  let tok <- mkFP_TokGen();  //TokGen is from library
  let fet <- mkTOY_Fetch(port_to_imem);
  let dec <- mkTOY_Decode(bypass);
  let exe <- mkTOY_Execute(bypass);
  let mem <- mkTOY_Mem(bypass, port_to_dmem);
  let lco <- mkTOY_LocalCommit(bypass);
  let gco <- mkTOY_GlobalCommit(port_to_mem_commit);
  
  let fp <- mkFunctionalPartition(tok, fet, 
                                  dec, exe, 
				  mem, lco, 
				  gco, 
				  bypass, 
				  port_to_imem,
				  port_to_dmem,
				  port_to_mem_commit,
				  port_to_mem_killRange,
				  8);

  return fp;

endmodule

//mkTOY_CPU :: CPU

(* synthesize *)
module [Module] mkTOY_CPU (TOY_CPU#(TOY_Token, TOY_Addr, TOY_Inst, TOY_Value));

  let fp <- mkTOY_FP();
  let tp <- mkTOY_TP_Simple();
  
  //Connect up the partitions
  mkConnection(tp.tokgen,        fp.tokgen);
  mkConnection(tp.fetch,         fp.fetch);
  mkConnection(tp.decode,        fp.decode);
  mkConnection(tp.execute,       fp.execute);
  mkConnection(tp.memory,        fp.memory);
  mkConnection(tp.local_commit,  fp.local_commit);
  mkConnection(tp.global_commit, fp.global_commit);
  
  //TModule Interface
  
  interface original = tp.original;

  //Interface For Memory Links
  
  interface to_dmem = fp.to_dmem;
  interface to_imem = fp.to_imem;
  
  interface commit    = fp.commit;
  interface killRange = fp.killRange; 
endmodule


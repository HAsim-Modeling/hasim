
//HASim library imports
import HASim::*;
import FUNCP_Base::*;
import FUNCP_TokGen::*;
import FUNCP_RegState::*;

//Model-specific imports
import ISA::*;
import FUNCP_FetchAlg::*;
import FUNCP_DecodeAlg::*;
import FUNCP_ExecuteAlg::*;
import FUNCP_MemAlg::*;
import FUNCP_LocalCommitAlg::*;
import FUNCP_GlobalCommitAlg::*;


module [HASim_Module] mkFUNCP_Pipeline (); 

  let tok_stage <- mkFUNCP_Stage_TOK();
  let fet_stage <- mkFUNCP_Stage_FET();
  let dec_stage <- mkFUNCP_Stage_DEC();
  let exe_stage <- mkFUNCP_Stage_EXE();
  let mem_stage <- mkFUNCP_Stage_MEM();
  let lco_stage <- mkFUNCP_Stage_LCO();
  let gco_stage <- mkFUNCP_Stage_GCO();
  
endmodule

module [HASim_Module] mkFUNCP_Stage_FET ();

  FUNCP_Stage#(void, Addr, Inst, Tuple2#(Addr, Inst))
  //...
       stage <- mkFUNCP_Stage("FET", 
                              "link_fet",
			      "fp_fet",
			      "tok_to_fet",
			      "fet_to_dec", 
			      8);  
  mkFUNCP_FetchAlg();

endmodule

module [HASim_Module] mkFUNCP_Stage_DEC ();

  FUNCP_Stage#(Tuple2#(Addr, Inst), void, DepInfo, Tuple2#(Addr, DecodedInst))
  //...
       stage <- mkFUNCP_Stage("DEC", 
                              "link_dec",
			      "fp_dec",
			      "fet_to_dec",
			      "dec_to_exe", 
			      8);  
  mkFUNCP_DecodeAlg();

endmodule

module [HASim_Module] mkFUNCP_Stage_EXE ();

  FUNCP_Stage#(Tuple2#(Addr, DecodedInst), void, InstResult, ExecedInst)
  //...
       stage <- mkFUNCP_Stage("EXE", 
                              "link_exe",
		              "fp_exe",
		              "dec_to_exe",
		              "exe_to_mem", 
		              8);
  
  mkFUNCP_ExecuteAlg();

endmodule

module [HASim_Module] mkFUNCP_Stage_MEM ();

  FUNCP_Stage#(ExecedInst, void, void, ExecedInst)
  //...
       stage <- mkFUNCP_Stage("MEM", 
                	      "link_mem",
			      "fp_mem",
			      "exe_to_mem",
			      "mem_to_lco", 
			      8);

  mkFUNCP_MemAlg();

endmodule

module [HASim_Module] mkFUNCP_Stage_LCO ();

  FUNCP_Stage#(ExecedInst, void, void, ExecedInst)
  //...
       stage <- mkFUNCP_Stage("LCO", 
                              "link_lco",
			      "fp_lco",
			      "mem_to_lco",
			      "lco_to_gco", 
			      8);

  mkFUNCP_LocalCommitAlg();

endmodule

module [HASim_Module] mkFUNCP_Stage_GCO ();

  FUNCP_Stage#(ExecedInst, void, void, void)
  //...
       stage <- mkFUNCP_Stage("GCO", 
                              "link_gco",
			      "fp_gco",
			      "lco_to_gco",
			      "gco_to_tok", 
			      8);
  
  mkFUNCP_GlobalCommitAlg();

endmodule


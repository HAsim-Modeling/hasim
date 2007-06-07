
//HASim library imports

import hasim_common::*;

import hasim_isa::*;
import hasim_funcp_base::*;

import FUNCP_TokGen::*;

//Model-specific imports

import hasim_funcp_fetch_alg::*;
import hasim_funcp_decode_alg::*;
import hasim_funcp_execute_alg::*;
import hasim_funcp_mem_alg::*;
import hasim_funcp_localcommit_alg::*;
import hasim_funcp_globalcommit_alg::*;


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

  FUNCP_Stage#(void, Addr, PackedInst, Tuple2#(Addr, PackedInst))
  //...
       stage <- mkFUNCP_Stage("FET", 
                              "fp_fet_stage",
			      "fp_fet",
			      "fp_tok_to_fet",
			      "fp_fet_to_dec", 
			      "fp_fet_kill");  
  mkFUNCP_FetchAlg();

endmodule

module [HASim_Module] mkFUNCP_Stage_DEC ();

  FUNCP_Stage#(Tuple2#(Addr, PackedInst), void, DepInfo, Tuple2#(Addr, DecodedInst))
  //...
       stage <- mkFUNCP_Stage("DEC", 
                              "fp_dec_stage",
			      "fp_dec",
			      "fp_fet_to_dec",
			      "fp_dec_to_exe",
			      "fp_dec_kill");  
  mkFUNCP_DecodeAlg();

endmodule

module [HASim_Module] mkFUNCP_Stage_EXE ();

  FUNCP_Stage#(Tuple2#(Addr, DecodedInst), void, InstResult, ExecedInst)
  //...
       stage <- mkFUNCP_Stage("EXE", 
                              "fp_exe_stage",
		              "fp_exe",
		              "fp_dec_to_exe",
		              "fp_exe_to_mem",
			      "fp_exe_kill");
  
  mkFUNCP_ExecuteAlg();

endmodule

module [HASim_Module] mkFUNCP_Stage_MEM ();

  FUNCP_Stage#(ExecedInst, void, void, InstWBInfo)
  //...
       stage <- mkFUNCP_Stage("MEM", 
                	      "fp_mem_stage",
			      "fp_mem",
			      "fp_exe_to_mem",
			      "fp_mem_to_lco", 
			      "fp_mem_kill");

  mkFUNCP_MemAlg();

endmodule

module [HASim_Module] mkFUNCP_Stage_LCO ();

  FUNCP_Stage#(InstWBInfo, void, void, InstWBInfo)
  //...
       stage <- mkFUNCP_Stage("LCO", 
                              "fp_lco_stage",
			      "fp_lco",
			      "fp_mem_to_lco",
			      "fp_lco_to_gco", 
			      "fp_lco_kill");

  mkFUNCP_LocalCommitAlg();

endmodule

module [HASim_Module] mkFUNCP_Stage_GCO ();

  FUNCP_Stage#(InstWBInfo, void, void, void)
  //...
       stage <- mkFUNCP_Stage("GCO", 
                              "fp_gco_stage",
			      "fp_gco",
			      "fp_lco_to_gco",
			      "fp_gco_to_tok", 
			      "fp_gco_kill");
  
  mkFUNCP_GlobalCommitAlg();

endmodule


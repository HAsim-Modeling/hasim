
//HASim library imports
import HASim::*;
import FUNCP_Base::*;
import FUNCP_TokGen::*;
import FUNCP_BypassUnit::*;

//Model-specific imports
import FUNCP_Fetch_Alg::*;
import FUNCP_Decode_Alg::*;
import FUNCP_Execute_Alg::*;
import FUNCP_Mem_Alg::*;
import FUNCP_LocalCommit_Alg::*;
import FUNCP_GlobalCommit_Alg::*;


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

  let s <- mkFUNCP_Stage("FET", 
                         "link_fet",
			 "fet_server",
			 "tok_to_fet",
			 "fet_to_dec", 
			 8);  
  mkFUNCP_Fetch_Alg();

endmodule

module [HASim_Module] mkFUNCP_Stage_DEC ();

  let s <- mkFUNCP_Stage("DEC", 
                         "link_dec",
			 "dec_server",
			 "fet_to_dec",
			 "dec_to_exe", 
			 8);
  
  mkFUNCP_Decode_Alg();

endmodule

module [HASim_Module] mkFUNCP_Stage_EXE ();

  let s <- mkFUNCP_Stage("EXE", 
                         "link_exe",
		         "exe_server",
		         "dec_to_exe",
		         "exe_to_mem", 
		         8);
  
  mkFUNCP_Execute_Alg();

endmodule

module [HASim_Module] mkFUNCP_Stage_MEM ();

  let s <- mkFUNCP_Stage("MEM", 
                	 "link_mem",
			 "mem_server",
			 "exe_to_mem",
			 "mem_to_lco", 
			 8);

  mkFUNCP_Mem_Alg();

endmodule

module [HASim_Module] mkFUNCP_Stage_LCO ();

  let s <- mkFUNCP_Stage("LCO", 
                         "link_lco",
			 "lco_server",
			 "mem_to_lco",
			 "lco_to_gco", 
			 8);
  
  mkFUNCP_LocalCommit_Alg();

endmodule

module [HASim_Module] mkFUNCP_Stage_GCO ();

  let s <- mkFUNCP_Stage("GCO", 
                         "link_gco",
			 "gco_server",
			 "lco_to_gco",
			 "gco_to_tok", 
			 8);
  
  mkFUNCP_GlobalCommit_Alg();

endmodule


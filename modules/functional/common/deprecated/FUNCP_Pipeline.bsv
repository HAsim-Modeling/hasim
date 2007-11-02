
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

`define HASIM_FUNCP_PIPELINE_LOGFILE "hasim_funcp_pipeline.out"

module [HASim_Module] mkFUNCP_Pipeline (); 
  
  let debug_log <- mkReg(InvalidFile);
  Reg#(Bool) initialized <- mkReg(False);
  Reg#(Tick) curCC <- mkReg(0);

  let tok_stage <- mkFUNCP_Stage_TOK(debug_log, curCC);
  let fet_stage <- mkFUNCP_Stage_FET(debug_log, curCC);
  let dec_stage <- mkFUNCP_Stage_DEC(debug_log, curCC);
  let exe_stage <- mkFUNCP_Stage_EXE(debug_log, curCC);
  let mem_stage <- mkFUNCP_Stage_MEM(debug_log, curCC);
  let lco_stage <- mkFUNCP_Stage_LCO(debug_log, curCC);
  let gco_stage <- mkFUNCP_Stage_GCO(debug_log, curCC);
  
  rule initialize (!initialized);
  
    let fd <- $fopen(`HASIM_FUNCP_PIPELINE_LOGFILE, "w");
    
    if (fd == InvalidFile)
    begin
      $display("ERROR: FUNCP: Pipeline: Could not create file %s", `HASIM_FUNCP_PIPELINE_LOGFILE);
      $finish(1);
    end
    
    debug_log <= fd;
    initialized <= True;
  
  endrule
  
  rule count (True);
  
    curCC <= curCC + 1;
  
  endrule
  
endmodule

module [HASim_Module] mkFUNCP_Stage_FET#(File debug_log, Tick curCC) ();

  FUNCP_Stage#(void, Addr, PackedInst, Tuple2#(Addr, PackedInst))
  //...
       stage <- mkFUNCP_Stage("FET", 
                              "fp_fet_stage",
                              "fp_fet",
                              "fp_tok_to_fet",
                              "fp_fet_to_dec", 
                              "fp_fet_kill");  
  mkFUNCP_FetchAlg(debug_log, curCC);

endmodule

module [HASim_Module] mkFUNCP_Stage_DEC#(File debug_log, Tick curCC) ();

  FUNCP_Stage#(Tuple2#(Addr, PackedInst), void, DepInfo, Tuple2#(Addr, DecodedInst))
  //...
       stage <- mkFUNCP_Stage("DEC", 
                              "fp_dec_stage",
                              "fp_dec",
                              "fp_fet_to_dec",
                              "fp_dec_to_exe",
                              "fp_dec_kill");  
  mkFUNCP_DecodeAlg(debug_log, curCC);

endmodule

module [HASim_Module] mkFUNCP_Stage_EXE#(File debug_log, Tick curCC) ();

  FUNCP_Stage#(Tuple2#(Addr, DecodedInst), void, InstResult, ExecedInst)
  //...
       stage <- mkFUNCP_Stage("EXE", 
                              "fp_exe_stage",
                              "fp_exe",
                              "fp_dec_to_exe",
                              "fp_exe_to_mem",
                              "fp_exe_kill");
  
  mkFUNCP_ExecuteAlg(debug_log, curCC);

endmodule

module [HASim_Module] mkFUNCP_Stage_MEM#(File debug_log, Tick curCC) ();

  FUNCP_Stage#(ExecedInst, void, void, InstWBInfo)
  //...
       stage <- mkFUNCP_Stage("MEM", 
                              "fp_mem_stage",
                              "fp_mem",
                              "fp_exe_to_mem",
                              "fp_mem_to_lco", 
                              "fp_mem_kill");

  mkFUNCP_MemAlg(debug_log, curCC);

endmodule

module [HASim_Module] mkFUNCP_Stage_LCO#(File debug_log, Tick curCC) ();

  FUNCP_Stage#(InstWBInfo, void, void, InstWBInfo)
  //...
       stage <- mkFUNCP_Stage("LCO", 
                              "fp_lco_stage",
                              "fp_lco",
                              "fp_mem_to_lco",
                              "fp_lco_to_gco", 
                              "fp_lco_kill");

  mkFUNCP_LocalCommitAlg(debug_log, curCC);

endmodule

module [HASim_Module] mkFUNCP_Stage_GCO#(File debug_log, Tick curCC) ();

  FUNCP_Stage#(InstWBInfo, void, void, void)
  //...
       stage <- mkFUNCP_Stage("GCO", 
                              "fp_gco_stage",
                              "fp_gco",
                              "fp_lco_to_gco",
                              "fp_gco_to_tok", 
                              "fp_gco_kill");
  
  mkFUNCP_GlobalCommitAlg(debug_log, curCC);

endmodule


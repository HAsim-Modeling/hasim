import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import hasim_common::*;

import hasim_funcp_pipeline::*;
import hasim_funcp_regstate::*;
import hasim_funcp_memstate::*;


module [HASim_Module] mkFUNCP ();
  
  Empty pipeline  <- mkFUNCP_Pipeline();
  Empty regstate  <- mkFUNCP_Regstate();
  Empty memstate  <- mkFUNCP_Memstate();
  
endmodule

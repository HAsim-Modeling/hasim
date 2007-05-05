import GetPut::*;
import ClientServer::*;
import RegFile::*;
import Connectable::*;
import FIFO::*;


import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_funcp::*;
import hasim_chip::*;



//The simplest system is a Chip and a Timing Partition


module [HASim_Module] mkSystem
    //interface:
                (TModule#(Command, Response));
    
    TModule#(Command, Response) funcp <- mkFUNCP();
    
    TModule#(Command, Response) chip <- mkChip();
    
    FIFO#(Response) respQ <- mkFIFO();
    
    rule getFuncResp (True);
      let res <- funcp.response();
      respQ.enq(res);
    endrule
    
    rule getChipResp (True);
      let res <- chip.response();
      respQ.enq(res);
    endrule
    
    method Action exec(Command com);
    
      case (com) matches
        tagged COM_RunProgram:
	  chip.exec(com);
	default:
	  funcp.exec(com);
      endcase
    endmethod
    
    method ActionValue#(Response) response();
    
      respQ.deq();
      return respQ.first();
    
    endmethod

endmodule

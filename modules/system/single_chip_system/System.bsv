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



//The simplest system is a Chip and a Functional Partition


module [HASim_Module] mkSystem
    //interface:
                ();
    
    let funcp <- mkFUNCP();
    
    let chip <- mkChip();
    
endmodule

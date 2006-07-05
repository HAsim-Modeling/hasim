package System;

import HASim::*;
import NullISA::*;

import TL0::*;


module [HASim_Module] mkSystem (TModule#(Command, Response));
   let dut <- sysTL;
   
   Reg#(UInt#(32)) ctr <- mkReg(0);

   rule inc_ctr;
      ctr <= ctr + 1;
   endrule

   rule stop (ctr > 100);
      $finish(0);
   endrule
endmodule

endpackage

     

import Interfaces::*;
import Types::*;

import GetPut::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

typedef Bit#(4) SnapShotPtr;
typedef Bit#(16) SnapShotUsage;
 
interface UsageMap#(type token_T, type ssptr, type ssusage);
  method Action flatten(ssptr p);
  method Action alloc(token_T t, ssptr p, ssusage u);
  method Action complete(token_T t, Bool mispred); // nail all dependant branches on mispred
endinterface

module mkUsageMap(UsageMap#(Token, SnapShotPtr, SnapShotUsage));
  let vsz = TExp#(TBits#(SnapShotPtr));
 
  Vector#(vsz, Reg#(SnapShotUsage)) usages <- replicateM(mkReg(0));

  method Action flatten(ssptr p);
  for(Integer i = 0; i < valueOf(vsz); i = i + 1)
    usages[i][p] <= 1'b0;



  endmethod





endmodule 




(* synthesize *)
module [Module] mkBypassUnit(BypassUnit#(RName, PRName, Value, Token))
  provisos(
    Bits#(RName,rsz), Eq#(RName), Bounded#(RName), 
    Bits#(PRName,psz)
    );


  RName maxR = maxBound;
  PRName minInitFL = zeroExtend(maxR) + 1;
  PRName maxInitFL = maxBound;
 
  Reg#(Vector#(TExp#(rsz), PRName)) map <- mkReg#(genVector); // init with [0 .. max]

  Reg #(SnapShotUsage) 	                             ss_usage <- mkReg(0); // init none used
  RegFile#(SnapShotPtr, Vector#(TExp#(rsz), PRName))  ss_hold <- mkRegFileFull();

  // XXX initialized with max+1 to maxBound ..
  RegFile#(PRName,PRName)                      freelist <- mkRegFileInit();
  Reg #(PRName)                                 fl_read <- mkReg#(minInitFL);
  Reg #(PRName)                                fl_write <- mkReg#(minInitFL); 

  
  





/*


  //new old
  method ActionValue#(Tuple2#(PRName,PRName)) makeMapping(Maybe#(RName) x, Token tok); //token is the ref name
    
   return(?);
  endmethod


  method preg_T lookup1(vreg_T v);
  method preg_T lookup2(vreg_T v);

  method Maybe#(value_T) read1(preg_T i);
  method Maybe#(value_T) read2(preg_T i);

  method Action write(preg_T i, value_T v);

  method Action freePReg(preg_T x);
  method Action rewindtoToken(token_T tok);

*/


  return (?);

endmodule



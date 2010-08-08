
import Clocks::*;
import ModuleContext::*;
import HList::*;
                  `include "asim/provides/fpga_components.bsh"
`include "asim/provides/clocks_device.bsh"
`include "asim/provides/soft_services_lib.bsh"
`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_clocks_lib.bsh"
//`include "asim/provides/soft_services_deps.bsh"

instance InitializableContext#(LOGICAL_CLOCK_INFO);
  module initializeContext (LOGICAL_CLOCK_INFO);
    let clock <- exposeCurrentClock();
    let reset <- exposeCurrentReset();
    // If the model clock frequency and the crystal clock frequency don't square
    // we should bail out.  

    if(`MODEL_CLOCK_FREQ != `CRYSTAL_CLOCK_FREQ*
                            `MODEL_CLOCK_MULTIPLIER/
                            `MODEL_CLOCK_DIVIDER)
      begin
        errorM("ERROR: Model Clock Frequency and Calculated frequency not equivalent");
      end

    return LOGICAL_CLOCK_INFO {clk: clock, rst: reset};
 endmodule
endinstance

instance FinalizableContext#(LOGICAL_CLOCK_INFO);
  module [Module] finalizeContext#(LOGICAL_CLOCK_INFO info) (Empty);
      // Currently nothing to do here.      
  endmodule
endinstance

instance ExposableContext#(LOGICAL_CLOCK_INFO,LOGICAL_CLOCK_INFO);
  // ugh manually spilt the contexts
  module [Module] exposeContext#(LOGICAL_CLOCK_INFO m) (LOGICAL_CLOCK_INFO);
    return m;
  endmodule

endinstance

instance BuriableContext#(SoftServicesModule,LOGICAL_CLOCK_INFO);
  module [SoftServicesModule] buryContext#(LOGICAL_CLOCK_INFO m) (Empty);
    SoftServicesContext ctxt <- getContext();
    putContext(putIt(ctxt,m));
  endmodule
endinstance


module [SoftServicesModule] mkSoftClock#(Integer outputFreq) (UserClock);

  // Get a reference to the known clock
  SoftServicesContext ctxt <- getContext();
  LOGICAL_CLOCK_INFO modelClock = getIt(ctxt);
  let returnClock <- mkUserClockFromFrequency(`MODEL_CLOCK_FREQ,
                                              outputFreq,
                                              clocked_by modelClock.clk, 
                                              reset_by modelClock.rst);
  return returnClock; 
endmodule

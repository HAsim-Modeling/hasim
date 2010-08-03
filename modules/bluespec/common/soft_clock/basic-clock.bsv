
import Clocks::*;
import ModuleContext::*;
                      
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/clocks_device.bsh"
`include "asim/provides/soft_services_lib.bsh"
`include "asim/provides/soft_connections_lib.bsh"
`include "asim/provides/soft_clocks_lib.bsh"

instance InitializableContext#(LOGICAL_CLOCK_INFO);
  module initializeContext (LOGICAL_CLOCK_INFO);
    let clock <- exposeCurrentClock();
    let reset <- exposeCurrentReset();
    return LOGICAL_CLOCK_INFO {clock: clock, reset: reset, frequency: `MODEL_CLOCK_FREQ};
 endmodule
endinstance

instance FinalizableContext#(LOGICAL_CLOCK_INFO);
  module [Module] finalizeContext#(LOGICAL_CLOCK_INFO info) (Empty);
      // Currently nothing to do here.      
  endmodule
endinstance

module [SoftServicesModule] mkSoftClock#(Integer outputFreq) (UserClock);

  // Get a reference to the known clock
  LOGICAL_CLOCK_INFO modelClock <- getContext();
  let returnClock <- mkUserClockFromFrequency(modelClock.frequency,outputFreq,clocked_by modelClock.clock, reset_by modelClock.reset);
  return returnClock; 
endmodule
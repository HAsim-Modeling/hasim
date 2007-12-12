
import hasim_common::*;

import hasim_controller::*;
import hasim_system::*;
import platform_interface::*;
import physical_platform::*;

module [HASim_Module] mkModel (TOP_LEVEL_WIRES);

  let system <- mkSystem();
  let controller <- mkController();
  let pi <- mkPlatformInterface();

  return pi;
endmodule

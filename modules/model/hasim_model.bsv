
import hasim_common::*;

import hasim_controller::*;
import hasim_system::*;
import platform_interface::*;
import toplevel_wires::*;

module [HASim_Module] mkModel (TopLevelWires);

  let system <- mkSystem();
  let controller <- mkController();
  let pi <- mkPlatformInterface();

  return pi;
endmodule

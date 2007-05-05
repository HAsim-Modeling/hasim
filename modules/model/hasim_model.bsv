import hasim_base::*;
import hasim_common::*;
import hasim_controller::*;
import hasim_system::*;

module [HASim_Module] mkModel ();

  let system <- mkSystem();
  Controller controller <- mkController(system);

endmodule

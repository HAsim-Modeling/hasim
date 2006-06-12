import HASim::*;
import Controller::*;
import System::*;


module [HASim_Module] mkModel ();

  let system <- mkSystem();
  Controller controller <- mkController(system);

endmodule

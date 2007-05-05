
import hasim_base::*;
import hasim_common::*;

import hasim_traffic_light_function::*;


module [HASim_Module] mkSystem (TModule#(Command, Response));

   let tl <- mk_traffic_light();
  
endmodule

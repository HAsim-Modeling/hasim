
//HASim library imports
import hasim_common::*;
import connection_terminus::*;

//************* Null Controller **************

// module [HASim_Module] mkController#(TModule#(Command, Response) th) ();
module [HASim_Module] mkController();
   
   let t <- mkConnectionTerminus();
   
endmodule


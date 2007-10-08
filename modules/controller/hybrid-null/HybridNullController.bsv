
//HASim library imports
import hasim_common::*;
import rrr::*;
import soft_connections::*;
import software_controller::*;

//************* Null Controller **************

module [HASim_Module] mkController();

    SoftwareController swcon <- mkSoftwareController();

endmodule


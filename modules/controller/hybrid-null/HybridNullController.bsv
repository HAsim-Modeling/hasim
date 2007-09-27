
//HASim library imports
import hasim_common::*;
import connection_terminus::*;
import rrr::*;
import soft_connections::*;
import software_controller::*;

//************* Null Controller **************

module [HASim_Module] mkController();

    let t <- mkConnectionTerminus();
    SoftwareController swcon <- mkSoftwareController();

endmodule


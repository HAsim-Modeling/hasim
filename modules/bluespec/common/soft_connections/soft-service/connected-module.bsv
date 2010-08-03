import ModuleContext::*;
import Connectable::*;
import Vector::*;
import Clocks::*;

`include "asim/provides/soft_connections_common.bsh"


//------------------ Connection Information ----------------------//
//                                                                //
// We gather information about each module's connections using the//
// ModuleContext library. The connections are then hooked together//
// using this info with the algorithms in connections.bsv         //
//                                                                //
//----------------------------------------------------------------//

// let's define a sub module context for the cases in which we want to process 
// the old module context.  This will make life a bit easier
typedef ModuleContext#(LOGICAL_CONNECTION_INFO) SoftConnectionModule;

typedef SoftServicesModule ConnectedModule;
typedef ConnectedModule Connected_Module;
typedef Connected_Module CONNECTED_MODULE;


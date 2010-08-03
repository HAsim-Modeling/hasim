import HList::*;
import ModuleContext::*;

`include "asim/provides/soft_connections_common.bsh"
`include "asim/provides/soft_clocks_lib.bsh"

typedef ModuleContext#(HList2#(LOGICAL_CONNECTION_INFO,LOGICAL_CLOCK_INFO)) SoftServicesModule;

typedef HList2#(LOGICAL_CONNECTION_INFO,LOGICAL_CLOCK_INFO) ConnectedModuleContext;

// Legacy typdefs
// These should probably be somewhere else, since they are definitely needed 
// by many other modules



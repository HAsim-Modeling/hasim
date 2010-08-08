import HList::*;
import ModuleContext::*;

`include "asim/provides/soft_connections_common.bsh"

typedef ModuleContext#(HList1#(LOGICAL_CONNECTION_INFO)) SoftServicesModule;

typedef HList1#(LOGICAL_CONNECTION_INFO) SoftServicesContext;

typedef WITH_CONNECTIONS#(nI,nO) SoftServicesSynthesisInterface#(parameter numeric type nI, parameter numeric type nO);

// Legacy typdefs
// These should probably be somewhere else, since they are definitely needed 
// by many other modules


function WITH_CONNECTIONS#(nI,nO) extractWithConnections(SoftServicesSynthesisInterface#(nI,nO) ifc);

  return ifc;

endfunction
import HList::*;
import ModuleContext::*;

`include "asim/provides/soft_connections_common.bsh"
`include "asim/provides/soft_clocks_lib.bsh"

typedef ModuleContext#(HList2#(LOGICAL_CONNECTION_INFO,LOGICAL_CLOCK_INFO)) SoftServicesModule;

typedef HList2#(LOGICAL_CONNECTION_INFO,LOGICAL_CLOCK_INFO) SoftServicesContext;

typedef Tuple2#(WITH_CONNECTIONS#(nI,nO),LOGICAL_CLOCK_INFO) SoftServicesSynthesisInterface#(parameter numeric type nI, parameter numeric type nO);

function WITH_CONNECTIONS#(nI,nO) extractWithConnections(SoftServicesSynthesisInterface#(nI,nO) ifc);

  return tpl_1(ifc);

endfunction


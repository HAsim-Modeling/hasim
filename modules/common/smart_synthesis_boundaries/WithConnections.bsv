import Vector::*;
import ModuleCollect::*;
import List::*;

import soft_connections::*;

//Chain 0: Events
//Chain 1: Stats
//Chain 2: Commands
//Chain 3: Responses

//The interface of a module with Connections
interface WithConnections#(parameter numeric type numIn,
                           parameter numeric type numOut);

  interface Vector#(numIn, CON_In)  incoming;
  interface Vector#(numOut, CON_Out) outgoing;
  interface Vector#(CON_NumChains, CON_Chain) chains;

endinterface

import Vector::*;
import ModuleCollect::*;
import List::*;

import soft_connections::*;

typedef `BOUNDARY_CON_WIDTH CON_Width;
typedef `BOUNDARY_CON_NUMBER CON_Addr;

//Chain 0: Events
//Chain 1: Stats
//Chain 2: Commands
//Chain 3: Responses

//The interface of a module with Connections
interface WithConnections;

  interface Vector#(CON_Addr, CON_In)  incoming;
  interface Vector#(CON_Addr, CON_Out) outgoing;
  interface Vector#(CON_NumChains, CON_Chain) chains;

endinterface

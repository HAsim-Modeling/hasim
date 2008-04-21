// memstate_cache_null

// A null cache that simply passes all requests on to the Memory virtual device.

// Library imports.

import FIFO::*;

// Project foundation imports.

`include "hasim_common.bsh"
`include "soft_connections.bsh"

// The memory virtual device

`include "funcp_memory.bsh"

// mkFUNCP_Cache

// A null cache which is simply a pass-through.


module [HASIM_MODULE] mkFUNCP_Cache ();

  // ***** Soft Connections ***** //

  Connection_Server#(MEM_REQUEST, MEM_VALUE)   link_memstate          <- mkConnection_Server("mem_cache");
  Connection_Client#(MEM_REQUEST, MEM_VALUE)   link_vdev_memory       <- mkConnection_Client("vdev_memory");
  Connection_Receive#(MEM_ADDRESS)             link_vdev_memory_inval <- mkConnection_Receive("vdev_memory_invalidate");

  // ***** Rules ***** //

  // request
  

  // When:   When the mem state requests a load.
  // Effect: Pass all requests onto the Memory Virtual Device

  rule request (True);
  
    let r = link_memstate.getReq();
    link_memstate.deq();
    
    link_vdev_memory.makeReq(r);
    
  endrule
  
  // response
  
  // When:   When a response come back from the virual device.
  // Effect: Pass the response back to the mem state.
  
  rule response (True);
  
    MEM_VALUE v = link_vdev_memory.getResp();
    link_vdev_memory.deq();
    
    link_memstate.makeResp(v);
        
  endrule
  
  // invalidate
  
  // When:   When the virtual device sends an invalidate.
  // Effect: Since we are not caching anything, we can ignore invalidates.
    
  rule invalidate (True);
  
    MEM_VALUE a = link_vdev_memory_inval.receive();
    link_vdev_memory_inval.deq();
        
  endrule

endmodule

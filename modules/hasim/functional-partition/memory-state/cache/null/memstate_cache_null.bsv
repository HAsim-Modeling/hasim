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

  Connection_Server#(MEM_REQUEST, MEMSTATE_RESP)   link_memstate          <- mkConnection_Server("mem_cache");
  Connection_Client#(MEM_REQUEST, MEM_REPLY)   link_vdev_memory       <- mkConnection_Client("funcp_memory");
  Connection_Receive#(MEM_ADDRESS)             link_vdev_memory_inval <- mkConnection_Receive("funcp_memory_invalidate");
  Connection_Server#(Bool, Bool)      link_funcp_memory_prep_for_emul <- mkConnection_Server("funcp_memory_prepare_to_emulate");

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
  
    MEM_REPLY v = link_vdev_memory.getResp();
    link_vdev_memory.deq();
    
    if (v matches tagged MEM_REPLY_LOAD .val)
        link_memstate.makeResp(v);
  endrule
  
  // invalidate
  
  // When:   When the virtual device sends an invalidate.
  // Effect: Since we are not caching anything, we can ignore invalidates.
    
  rule invalidate (True);
  
    MEM_ADDRESS a = link_vdev_memory_inval.receive();
    link_vdev_memory_inval.deq();
        
  endrule

  // invalidate_all
  
  // When:   When the regstate sends an invalidate_all before beginning isa-emulation
  // Effect: Since we are not caching anything, we can ignore invalidates.
    
  rule invalidate_all (True);
  
    link_funcp_memory_prep_for_emul.deq();
    link_funcp_memory_prep_for_emul.makeResp(?);
        
  endrule

endmodule

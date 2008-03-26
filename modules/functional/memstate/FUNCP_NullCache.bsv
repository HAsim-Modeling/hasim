//Null cache

import FIFO::*;

import hasim_common::*;
import soft_connections::*;
import memory::*;

//The Null Cache: simply a pass-through

module [HASim_Module] mkFUNCP_Cache ();

  //Connections

  Connection_Server#(MEM_REQUEST, MEM_VALUE)   link_memstate     <- mkConnection_Server("mem_cache");
  Connection_Client#(MEM_REQUEST, MEM_VALUE)   link_vdev_memory     <- mkConnection_Client("vdev_memory");
  Connection_Receive#(MEM_ADDRESS)               link_vdev_memory_inval     <- mkConnection_Receive("vdev_memory_invalidate");

  //handleRequest
 
  //Pass all requests onto the Memory Virtual Device

  rule handleRequest (True);
  
    let r = link_memstate.getReq();
    link_memstate.deq();
    
    link_vdev_memory.makeReq(r);
    
  endrule
  
  //Pass all responses back to the user
  
  rule handleResponse (True);
  
    MEM_VALUE v = link_vdev_memory.getResp();
    link_vdev_memory.deq();
    
    link_memstate.makeResp(v);
        
  endrule
  
  //Ignore invalidates
  
  rule handleInvalidate (True);
  
    MEM_VALUE a = link_vdev_memory_inval.receive();
    link_vdev_memory_inval.deq();
        
  endrule

endmodule

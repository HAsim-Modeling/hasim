`include "hasim_common.bsh"
`include "soft_connections.bsh"
`include "rrr.bsh"
`include "channelio.bsh"

// ***** Modules *****

// mkFuncpMemory

module [HASIM_MODULE] mkFUNCP_Memory
    // interface:
        ();

    // ***** Local State *****
    
    // Our state.
    Reg#(Bit#(8)) state <- mkReg(0);
    
    // Links to stubs to the funcp_memory RRR service.
    Connection_Client#(MEM_ADDRESS, MEM_VALUE) link_Load
        <- mkConnection_Client("rrr_client_FUNCP_MEMORY_Load");
    
    Connection_Send#(MEM_STORE_INFO)           link_Store
        <- mkConnection_Send("rrr_client_FUNCP_MEMORY_Store");
    
    Connection_Receive#(MEM_ADDRESS)           link_Invalidate
        <- mkConnection_Receive("rrr_server_FUNCP_MEMORY_Invalidate");

    // Links that we expose to the outside world
    Connection_Server#(MEM_REQUEST, MEM_VALUE) link_memory       <- mkConnection_Server("funcp_memory");
    Connection_Send#(MEM_ADDRESS)              link_memory_inval <- mkConnection_Send("funcp_memory_invalidate");

    // ***** Rules ******

    // Service a funcp_memory request by passing it on to the RRR service.

    rule make_mem_request (state == 0);

        // pop a request from the link
        MEM_REQUEST req = link_memory.getReq();
        link_memory.deq();
        
        // process request
        case (req) matches
            tagged MEM_LOAD .addr:
            begin

                // send request via RRR
                link_Load.makeReq(addr);
                
                // wait for response
                state <= 1;

            end
            
            tagged MEM_STORE .stinfo:
            begin

                // send request via RRR
                link_Store.send(MEM_STORE_INFO {addr: stinfo.addr, val: stinfo.val});
                
                // done
                state <= 0;
                
            end
        endcase

    endrule
  
    // Get a response from the stub and pass it back to the user.

    rule get_mem_response (state == 1);

        MEM_VALUE v = link_Load.getResp();
        link_Load.deq();  
        state <= 0;

        link_memory.makeResp(v);

    endrule


    // Get an invalidate request from the RRR service and pass it to anybody caching the memory.

    rule get_invalidate_request (True);

        MEM_ADDRESS inval_addr = link_Invalidate.receive();
        link_Invalidate.deq();

        link_memory_inval.send(inval_addr);

    endrule

endmodule

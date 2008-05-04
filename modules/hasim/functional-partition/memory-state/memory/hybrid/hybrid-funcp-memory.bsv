`include "hasim_common.bsh"
`include "soft_connections.bsh"
`include "rrr.bsh"
`include "channelio.bsh"

`include "asim/rrr/remote_client_stub_FUNCP_MEMORY.bsh"
`include "asim/rrr/remote_server_stub_FUNCP_MEMORY.bsh"

// ***** Modules *****

// mkFuncpMemory

module [HASIM_MODULE] mkFUNCP_Memory
    // interface:
        ();

    // ***** Local State *****
    
    // Our state.
    Reg#(Bit#(8)) state <- mkReg(0);
    
    // Stubs to the funcp_memory RRR service.
    ClientStub_FUNCP_MEMORY client_stub <- mkClientStub_FUNCP_MEMORY();
    ServerStub_FUNCP_MEMORY server_stub <- mkServerStub_FUNCP_MEMORY();
    
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
                client_stub.makeRequest_Load(addr);
                
                // wait for response
                state <= 1;

            end
            
            tagged MEM_STORE .stinfo:
            begin

                // send request via RRR
                client_stub.makeRequest_Store(MEM_STORE_INFO {addr: stinfo.addr, val: stinfo.val});
                
                // done
                state <= 0;
                
            end
        endcase

    endrule
  
    // Get a response from the stub and pass it back to the user.

    rule get_mem_response (state == 1);

        MEM_VALUE v <- client_stub.getResponse_Load();
        state <= 0;

        link_memory.makeResp(v);

    endrule


    // Get an invalidate request from the RRR service and pass it to anybody caching the memory.

    rule get_invalidate_request (True);

        MEM_ADDRESS inval_addr <- server_stub.acceptRequest_Invalidate();

        link_memory_inval.send(inval_addr);

    endrule

endmodule

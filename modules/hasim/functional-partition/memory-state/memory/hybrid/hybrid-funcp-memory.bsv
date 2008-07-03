`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/rrr.bsh"
`include "asim/provides/channelio.bsh"

`include "asim/rrr/remote_client_stub_FUNCP_MEMORY.bsh"
`include "asim/rrr/remote_server_stub_FUNCP_MEMORY.bsh"

// Can't include hasim_isa.bsh here or it causes a loop
typedef MEM_VALUE ISA_ADDRESS;

// ***** Modules *****

// mkFuncpMemory

module [HASIM_MODULE] mkFUNCP_Memory
    // interface:
        ();

    // ***** Local State *****
    
    // Stubs to the funcp_memory RRR service.
    ClientStub_FUNCP_MEMORY client_stub <- mkClientStub_FUNCP_MEMORY();
    ServerStub_FUNCP_MEMORY server_stub <- mkServerStub_FUNCP_MEMORY();
    
    // Links that we expose to the outside world
    Connection_Server#(MEM_REQUEST, MEM_REPLY)   link_memory <- mkConnection_Server("funcp_memory");
    Connection_Server#(ISA_ADDRESS, MEM_ADDRESS) link_tlb    <- mkConnection_Server("funcp_memory_VtoP");
    Connection_Send#(MEM_ADDRESS)          link_memory_inval <- mkConnection_Send("funcp_memory_invalidate");

    // ***** Rules ******

    // Service a funcp_memory request by passing it on to the RRR service.

    rule make_mem_request (True);

        // pop a request from the link
        MEM_REQUEST req = link_memory.getReq();
        link_memory.deq();
        
        // process request
        case (req) matches
            tagged MEM_LOAD .addr:
            begin

                // send request via RRR
                client_stub.makeRequest_Load(addr);

            end
            
            tagged MEM_LOAD_CACHELINE .addr:
            begin

                // send request via RRR
                client_stub.makeRequest_LoadCacheLine(addr);

            end

            tagged MEM_STORE .stinfo:
            begin

                // send request via RRR
                client_stub.makeRequest_Store(stinfo);
                
            end

            tagged MEM_STORE_CACHELINE .stinfo:
            begin

                // send request via RRR
                client_stub.makeRequest_StoreCacheLine(stinfo);
                
            end
        endcase

    endrule
  
    // Get a response from the stub and pass it back to the user.

    rule get_mem_response (True);

        MEM_VALUE v <- client_stub.getResponse_Load();

        link_memory.makeResp(tagged MEM_REPLY_LOAD v);

    endrule

    rule get_mem_response2 (True);

        MEM_CACHELINE v <- client_stub.getResponse_LoadCacheLine();

        link_memory.makeResp(tagged MEM_REPLY_LOAD_CACHELINE v);

    endrule


    // Get an invalidate request from the RRR service and pass it to anybody caching the memory.

    rule get_invalidate_request (True);

        MEM_ADDRESS inval_addr <- server_stub.acceptRequest_Invalidate();

        link_memory_inval.send(inval_addr);

    endrule


    //
    // Virtual to physical translation
    //

    rule translate_VtoP_request (True);

        ISA_ADDRESS va = link_tlb.getReq();
        link_tlb.deq();

        client_stub.makeRequest_VtoP(va);

    endrule

    rule translate_VtoP_response (True);

        MEM_ADDRESS pa <- client_stub.getResponse_VtoP();
        link_tlb.makeResp(pa);

    endrule

endmodule

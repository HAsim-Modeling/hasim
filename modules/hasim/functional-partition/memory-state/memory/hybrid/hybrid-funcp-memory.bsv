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

    Connection_Client#(MEM_INVAL_CACHELINE_INFO, Bool)  link_memory_inval <- mkConnection_Client("funcp_memory_cache_invalidate");
    Connection_Client#(Bool, Bool)                  link_memory_inval_all <- mkConnection_Client("funcp_memory_cache_invalidate_all");

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
                client_stub.makeRequest_Load(addr);
            end
            
            tagged MEM_LOAD_CACHELINE .addr:
            begin
                client_stub.makeRequest_LoadCacheLine(addr);
            end

            tagged MEM_STORE .stinfo:
            begin
                client_stub.makeRequest_Store(stinfo);
            end

            tagged MEM_STORE_CACHELINE .stinfo:
            begin
                client_stub.makeRequest_StoreCacheLine(stinfo);
            end

            tagged MEM_STORE_CACHELINE_SYNC .stinfo:
            begin
                client_stub.makeRequest_StoreCacheLine_Sync(stinfo);
            end
        endcase

    endrule
  
    // Get a response from the stub and pass it back to the user.

    rule get_mem_response (True);
        MEM_VALUE v <- client_stub.getResponse_Load();
        link_memory.makeResp(tagged MEM_REPLY_LOAD v);
    endrule

    rule get_mem_response_ldline (True);
        MEM_CACHELINE v <- client_stub.getResponse_LoadCacheLine();
        link_memory.makeResp(tagged MEM_REPLY_LOAD_CACHELINE v);
    endrule

    rule get_mem_response_stline_sync (True);
        let dummy <- client_stub.getResponse_StoreCacheLine_Sync();
        link_memory.makeResp(tagged MEM_REPLY_STORE_CACHELINE_ACK True);
    endrule


    //
    // Pass invalidate and flush requests from the software side to local FPGA
    // caches.
    //

    rule get_invalidate_request (True);
        // Number of lines comes in as a 32 bit quantity instead of 8 due to
        // data packing in the channel.
        MEM_INVAL_CACHELINE_INFO_RRR_HACK info <- server_stub.acceptRequest_Invalidate();

        MEM_INVAL_CACHELINE_INFO inval_info;
        inval_info.onlyFlush = info.onlyFlush;
        inval_info.nLines = info.nLines;
        inval_info.addr = info.addr;

        link_memory_inval.makeReq(inval_info);
    endrule

    rule get_invalidate_response (True);
        link_memory_inval.deq();
        server_stub.sendResponse_Invalidate(?);
    endrule


    rule get_invalidate_all_request (True);
        let dummy <- server_stub.acceptRequest_InvalidateAll();
        link_memory_inval_all.makeReq(?);
    endrule

    rule get_invalidate_all_response (True);
        link_memory_inval_all.deq();
        server_stub.sendResponse_InvalidateAll(?);
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

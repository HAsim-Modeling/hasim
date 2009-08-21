
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/shared_memory.bsh"

`include "asim/provides/soft_connections.bsh"


module [CONNECTED_MODULE] mkSharedMemoryService#(VIRTUAL_DEVICES vdevs)
    // interface:
        ();
    
    let sharedMemory = vdevs.sharedMemory;

    Connection_Receive#(SHARED_MEMORY_REQUEST) link_shmem_req        <- mkConnectionRecvOptional("vdev_shmem_req");
    Connection_Send#(SHARED_MEMORY_DATA)       link_shmem_data_read  <- mkConnectionSendOptional("vdev_shmem_data_read");
    Connection_Receive#(SHARED_MEMORY_DATA)    link_shmem_data_write <- mkConnectionRecvOptional("vdev_shmem_data_write");

    // ====================================================================
    //
    // Shared Memory connections.
    //
    // ====================================================================

    rule send_shmem_req (True);
        
        let req = link_shmem_req.receive();
        link_shmem_req.deq();
        case (req) matches
            tagged SHARED_MEMORY_READ  .info: sharedMemory.readBurstReq(info.addr, info.len);
            tagged SHARED_MEMORY_WRITE .info: sharedMemory.writeBurstReq(info.addr, info.len);
        endcase

    endrule
    
    rule recv_shmem_read_data (True);
        
        let data <- sharedMemory.readBurstResp();
        link_shmem_data_read.send(data);
        
    endrule
    
    rule send_shmem_write_data (True);
        
        let data = link_shmem_data_write.receive();
        link_shmem_data_write.deq();
        sharedMemory.writeBurstData(data);
        
    endrule

endmodule

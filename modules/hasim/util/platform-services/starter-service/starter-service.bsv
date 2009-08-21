
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/starter_device.bsh"

`include "asim/provides/soft_connections.bsh"


module [CONNECTED_MODULE] mkStarterService#(VIRTUAL_DEVICES vdevs)
    // interface:
        ();
    
    let starter = vdevs.starter;
    
    // ====================================================================
    //
    // Starter connections.
    //
    // ====================================================================


    Connection_Receive#(Bit#(8)) linkStarterFinishRun <- mkConnectionRecvOptional("vdev_starter_finish_run");

    rule sendStarterFinishRun (True);
        
        let exit_code = linkStarterFinishRun.receive();
        linkStarterFinishRun.deq();
        starter.makeRequest_End(exit_code);
        
    endrule


endmodule

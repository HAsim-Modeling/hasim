
`include "asim/provides/virtual_platform.bsh"
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/streams.bsh"
`include "asim/provides/starter_device.bsh"

`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS.bsh"

typedef enum 
    {
        STATE_start, 
        STATE_exit,
        STATE_finish 
    } 
    STATE deriving(Bits,Eq);


module mkApplication#(VIRTUAL_PLATFORM virtualPlatform)();

    Streams streams = virtualPlatform.virtualDevices.streams;
    
    STARTER starter = virtualPlatform.virtualDevices.starter;

    Reg#(STATE) state <- mkReg(STATE_start);

    rule hello (state == STATE_start);
    
       starter.acceptRequest_Start();

       streams.makeRequest(`STREAMID_MESSAGE,
                           `STREAMS_MESSAGE_HELLO,
                           ?,
                           ?);
  
       state <= STATE_exit;

    endrule


    rule exit (state == STATE_exit);
    
       starter.makeRequest_End(0);

    /*
       streams.makeRequest(`STREAMID_MESSAGE,
                           `STREAMS_MESSAGE_EXIT,
                           0,
                           ?);
    */
  
       state <= STATE_finish;

    endrule


    rule finish (state == STATE_finish);
       noAction;
    endrule

endmodule

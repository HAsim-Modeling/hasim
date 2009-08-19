
`include "asim/provides/virtual_platform.bsh"
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/streams.bsh"
`include "asim/provides/starter_device.bsh"

`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS.bsh"

typedef enum 
    {
        STATE_start,
        STATE_say_hello,
        STATE_exit,
        STATE_finish 
    } 
    STATE deriving(Bits,Eq);


module mkApplication#(VIRTUAL_PLATFORM virtualPlatform)();

    STARTER starter = virtualPlatform.virtualDevices.starter;

    Streams streams = virtualPlatform.virtualDevices.streams;
    

    Reg#(STATE) state <- mkReg(STATE_start);

    rule start (state == STATE_start);
    
       starter.acceptRequest_Start();

       state <= STATE_say_hello;

    endrule

    rule hello (state == STATE_say_hello);
  
       streams.makeRequest(`STREAMID_MESSAGE,
                           `STREAMS_MESSAGE_HELLO,
                           ?,
                           ?);

       state <= STATE_exit;

    endrule


    rule exit (state == STATE_exit);
    
       starter.makeRequest_End(0);

       state <= STATE_finish;

    endrule


    rule finish (state == STATE_finish);
       noAction;
    endrule

endmodule


`include "front_panel.bsh"
`include "physical_platform.bsh"
`include "low_level_platform_interface.bsh"

`include "streams.bsh"

`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS.bsh"

typedef enum 
    {
        STATE_start, 
        STATE_exit,
        STATE_finish 
    } 
    STATE deriving(Bits,Eq);


module mkSystem#(LowLevelPlatformInterface llpi)();

    // instantiate virtual devices
    Streams    streams <- mkStreams(llpi);

    Reg#(STATE) state <- mkReg(STATE_start);

    rule hello (state == STATE_start);

       streams.makeRequest(`STREAMID_MESSAGE,
                           `STREAMS_MESSAGE_HELLO,
                           ?,
                           ?);
  
       state <= STATE_exit;

    endrule


    rule exit (state == STATE_exit);

       streams.makeRequest(`STREAMID_MESSAGE,
                           `STREAMS_MESSAGE_EXIT,
                           0,
                           ?);
  
       state <= STATE_finish;

    endrule


    rule finish (state == STATE_finish);
       noAction;
    endrule

endmodule

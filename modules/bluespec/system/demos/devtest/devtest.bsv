
`include "front_panel.bsh"
`include "physical_platform.bsh"
`include "low_level_platform_interface.bsh"

`include "streams.bsh"

`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS.bsh"

typedef enum 
    {
        STATE_start, 
        STATE_doit,
        STATE_debounce,
        STATE_finish,
        STATE_shutdown
    } 
    STATE deriving(Bits,Eq);


module mkSystem#(LowLevelPlatformInterface llpi)();

    // instantiate virtual devices

    FrontPanel       fp       <- mkFrontPanel(llpi);
    Streams          streams  <- mkStreams(llpi);

    // instatiate our local state

    Reg#(FRONTP_LEDS) value   <- mkReg(0);
    Reg#(STATE)       state   <- mkReg(STATE_start);

    rule start (state == STATE_start);

       let newvalue = 0;

       value <= newvalue;
       fp.writeLEDs(newvalue, '1);

       streams.makeRequest(`STREAMID_MESSAGE,
                           `STREAMS_MESSAGE_START,
                           newvalue,
                           ?);
  

       state <= STATE_debounce;
    endrule

    //
    // Wait until no button is pushed
    //

    rule debounce (state == STATE_debounce);

      let buttons = fp.readButtons();

      if (buttons == 0)
      begin
        state <= STATE_doit;          
      end

    endrule


    //
    // Watch for a button push and do the right thing
    //

    rule doit (state == STATE_doit);
      FRONTP_LEDS       newvalue;

      let buttons = fp.readButtons();

      // OK button
   
      if (buttons[2] == 1)
      begin      
        newvalue = fp.readSwitches();

        state <= STATE_debounce;
      end

      // UP button

      else if (buttons[0] == 1)
      begin
        newvalue = value + 1;

        state <= STATE_debounce;
      end

      // DOWN button

      else if (buttons[4] == 1)
      begin
        newvalue = value - 1;

        state <= STATE_debounce;
      end

      // LEFT button

      else if (buttons[1] == 1)
      begin
        newvalue = value;

        state <= STATE_start;
      end

      // RIGHT button

      else if (buttons[3] == 1)
      begin
        newvalue = value;

        state <= STATE_finish;
      end
      else
      begin
        newvalue = value;
      end

      // Write the LEDs and save the value

      fp.writeLEDs(zeroExtend(newvalue), '1);
      value <= zeroExtend(newvalue);

    endrule

    //
    // Write final message
    //

    rule finish (state == STATE_finish);

       streams.makeRequest(`STREAMID_MESSAGE,
                           `STREAMS_MESSAGE_FINISH,
                           zeroExtend(value),
                           ?);
  
       value <= 0;
       state <= STATE_shutdown;
    endrule

    //
    // Shut down
    //

    rule shutdown (state == STATE_shutdown);
       noAction;
    endrule

endmodule

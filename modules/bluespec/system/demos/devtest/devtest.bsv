//
// Devtest
//
// This is a small demo program that exercises a variety of 
// of the virtual platform devices, i.e., the front panel
// buttons, switches and LEDs and streams I/O. It operates as 
// follows:
//
//     Button          Action
//     ------          ------
//     Ok              Load switches into LEDs
//     Up              Increment LEDs by 1
//     Down            Decrement LEDs by 1
//     Left            Return to initial state
//     Right           Finish
//

// Include platform

`include "asim/provides/low_level_platform_interface.bsh"
`include "asim/provides/physical_platform.bsh"
`include "asim/provides/virtual_platform.bsh"


// Include virtual devices

`include "asim/provides/virtual_devices.bsh"

`include "asim/provides/starter_device.bsh"
`include "asim/provides/front_panel.bsh"
`include "asim/provides/common_utility_devices.bsh"
`include "asim/provides/streams_device.bsh"

// Include symbol defintions

`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS.bsh"

typedef enum 
    {
        STATE_start,
        STATE_initialize,
        STATE_debounce,
        STATE_doit,
        STATE_finish,
        STATE_shutdown
    } 
    STATE deriving(Bits,Eq);


module mkApplication#(VIRTUAL_PLATFORM virtualPlatform)();

    // Instantiate virtual devices

    STARTER          starter = virtualPlatform.virtualDevices.starter;
    FrontPanel       fp      = virtualPlatform.virtualDevices.frontPanel;
    STREAMS          streams = virtualPlatform.virtualDevices.commonUtilities.streams;
    

    // Instantiate our local state

    Reg#(FRONTP_LEDS) value   <- mkReg(0);
    Reg#(STATE)       state   <- mkReg(STATE_start);


    //
    // Start state
    //

    rule start (state == STATE_start);

       starter.acceptRequest_Start();

       state <= STATE_initialize;

    endrule

    //
    // Initialization state
    //

    rule initialize (state == STATE_initialize);

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
        newvalue = zeroExtend(fp.readSwitches());

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

        state <= STATE_initialize;
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

       starter.makeRequest_End(0);
  
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

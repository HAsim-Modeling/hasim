

// SOFT_STATS_STATE

// An internal datatype to track the state of the stats controller

typedef enum
{
  SC_Idle,           // Not executing any commands
  SC_GettingLengths, // Executing the GetVectorLengths command
  SC_Dumping,        // Executing the Dump command
  SC_Toggling,       // Executing the Toggle command
  SC_Reseting        // Executing the Reset command
}
  STATS_SERVICE_STATE
               deriving (Eq, Bits);


module [CONNECTED_MODULE] mkStatsService#(STATS statsDevice)
    // interface:
        ();

    // ****** State Elements ******

    // Communication link to the Stats themselves
    Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);

    // Our internal state
    Reg#(STATS_SERVICE_STATE)  state <- mkReg(SC_Idle);

    // ****** Rules ******

    // processResp

    // Process a response from an individual stat. 
    // Most of the time this is just sent on to the outputQ.

    rule processResp (True);

      let st <- chain.receive_from_prev();

      case (st) matches
        tagged ST_VAL .stinfo: // A stat to dump
        begin
          statsDevice.reportStat(stinfo.statID, stinfo.index, stinfo.value);
        end
        tagged ST_LENGTH .stinfo: // A stat vector length
        begin
          statsDevice.setVectorLength(stinfo.statID, stinfo.length);
        end
        tagged ST_OVERFLOW .stinfo: // A stat overflowed its counter.
        begin
            // Tell the software to increment it by MAX_INT
            statsDevice.statOverflow(stinfo.statID, stinfo.index);
        end
        tagged ST_GET_LENGTH:  // We're done getting lengths
        begin
          statsDevice.finishVectorLengths();
          state <= SC_Idle;
        end
        tagged ST_DUMP:  // We're done dumping
        begin
          statsDevice.finishDump();
          state <= SC_Idle;
        end
        tagged ST_RESET:  // We're done reseting
        begin
          statsDevice.finishReseting();
          state <= SC_Idle;
        end
        tagged ST_TOGGLE:  // We're done toggling
        begin
          statsDevice.finishToggling();
          state <= SC_Idle;
        end
      endcase

    endrule
    
    //
    // startVector --
    //    
    // Start getting vector lengths
    //
    rule startVector (state == SC_Idle && statsDevice.gettingVectorLengths());
        
        chain.send_to_next(ST_GET_LENGTH);
        state <= SC_GettingLengths;

    endrule

    //
    // startDump --
    //    
    // Begin a stat dump.
    //
    rule startDump (state == SC_Idle && statsDevice.dumping());
    
        chain.send_to_next(ST_DUMP);
        state <= SC_Dumping;

    endrule

    //
    // startToggle --
    //    
    // Begin toggling stats between enabled/disabled.
    //
    rule startToggle (state == SC_Idle && statsDevice.toggling());
    
        chain.send_to_next(ST_TOGGLE);
        state <= SC_Toggling;

    endrule

    //
    // startReset --
    //    
    // Begin reseting all the stats.
    //
    rule startReset (state == SC_Idle && statsDevice.reseting());
    
        chain.send_to_next(ST_RESET);
        state <= SC_Reseting;

    endrule

endmodule

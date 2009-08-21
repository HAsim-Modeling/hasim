

// SOFT_STATS_STATE

// An internal datatype to track the state of the stats controller

typedef enum
{
  SC_Initializing, //Starting up, doing local runtime initialization
  SC_Idle,         //Not executing any commands
  SC_Dumping,      //Executing the Dump command
  SC_Enabling,     //Executing the Enable command
  SC_Disabling,    //Executing the Disable command
  SC_Reseting      //Executing the Reset command
}
  SOFT_STATS_STATE
               deriving (Eq, Bits);


module [CONNECTED_MODULE] mkStatsIOService#(STATS_IO statsIO)
    // interface:
        ();

    // ****** State Elements ******

    // Communication link to the Stats themselves
    Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);

    // Track if we are done dumping
    Reg#(Bool) dumpFinished  <- mkReg(False);

    // Our internal state
    Reg#(SOFT_STATS_STATE)  state <- mkReg(SC_Idle);

    // ****** Rules ******

    // sendReq

    // Send a request to all the stats.
    // We only do this if we're in a state which requires new communication.
    // Afterwords we go back to idle.

    rule sendReq (!((state == SC_Idle) || (state == SC_Initializing)));

      let nextCommand = case (state) matches
                         tagged SC_Dumping:      return tagged ST_DUMP;
                         tagged SC_Enabling:     return tagged ST_ENABLE;
                         tagged SC_Disabling:    return tagged ST_DISABLE;
                         tagged SC_Reseting:     return tagged ST_RESET;
                         default:                return tagged ST_DUMP;
                       endcase;

      chain.send_to_next(nextCommand);
      state <= SC_Idle;

    endrule

    // processResp

    // Process a response from an individual stat. 
    // Most of the time this is just sent on to the outputQ.

    rule processResp (state != SC_Initializing);

      let st <- chain.receive_from_prev();

      case (st) matches
        tagged ST_VAL .stinfo: //A stat to dump
        begin
          statsIO.reportStat(stinfo.statID, stinfo.value);
        end
        tagged ST_DUMP:  //We're done dumping
        begin
          statsIO.finishDump();
          dumpFinished <= True;
        end
      endcase

    endrule
    
    //
    // dumpStart --
    //    
    // Begin a stat dump.
    //
    rule dumpStart (state == SC_Idle && statsIO.dumping());
    
        state <= SC_Dumping;

    endrule

endmodule

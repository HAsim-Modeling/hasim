// StatsController

// Controls all the stats throughout the hardware model.

// A StatsController can accept commands from the main hardware controller.
// After Dump command is asserted it returns the next stat with 
// getNextStat() until noMoreStats() is true.

interface StatsController;

  method Action                 doCommand(StatsCommand com);
  method ActionValue#(StatInfo) getNextStat();
  method Bool                   noMoreStats();

endinterface

// StatCommand

// Commands that can be given to the Stats Controller

typedef enum
{
  Stats_Enable,
  Stats_Disable,
  Stats_Reset,
  Stats_Dump
}
  StatsCommand
               deriving (Eq, Bits);

// StatInfo

// The datatype which the stat controller returns.
// Presumably the main controller passes this along to the stat dumper.

typedef struct
{
        Bit#(8)  statStringID;
        Bit#(32) statValue;
}
  StatInfo 
           deriving (Eq, Bits);


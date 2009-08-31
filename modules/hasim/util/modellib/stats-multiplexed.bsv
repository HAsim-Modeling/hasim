`include "asim/provides/hasim_common.bsh"

//
// mkStatCounter_Multiplexed --
//     Public module for the STAT_RECORDER_MULTIPLEXED multiple instances of
//     a single ID interface.
//


typedef STAT_VECTOR#(n_STATS) STAT_RECORDER_MULTIPLEXED#(numeric type n_STATS);


module [Connected_Module] mkStatCounter_Multiplexed#(STATS_DICT_TYPE myID)
    // interface:
    (STAT_RECORDER_MULTIPLEXED#(ni));

    Vector#(ni, STATS_DICT_TYPE) ids = replicate(myID);
    let m <- mkStatCounter_Vector(ids);
    return m;
endmodule


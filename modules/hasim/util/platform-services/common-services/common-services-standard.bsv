
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/common_utility_devices.bsh"

`include "asim/provides/soft_connections.bsh"

`include "asim/dict/RINGID.bsh"


module [CONNECTED_MODULE] mkCommonServices#(VIRTUAL_DEVICES vdevs)
    // interface:
        ();

    let com = vdevs.commonUtilities;

    let assertionsService <- mkAssertionsService(com.assertions);
    let debugScanService  <- mkDebugScanService(com.debugScan);
    let paramsService     <- mkDynamicParametersService(com.dynamicParameters);
    let statsService      <- mkStatsService(com.stats);
    let streamsService    <- mkStreamsService(com.streams);

endmodule

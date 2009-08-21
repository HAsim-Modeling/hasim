
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/common_utility_devices.bsh"

`include "asim/provides/soft_connections.bsh"

`include "asim/dict/RINGID.bsh"


module [CONNECTED_MODULE] mkCommonServices#(VIRTUAL_DEVICES vdevs)
    // interface:
        ();

    let com = vdevs.commonUtilities;

    let assertionsIOService <- mkAssertionsIOService(com.assertions);
    let debugScanIOService  <- mkDebugScanIOService(com.debugScan);
    let paramsIOService     <- mkDynamicParametersIOService(com.dynamicParameters);
    let statsIOService      <- mkStatsIOService(com.stats);
    let streamsIOService    <- mkStreamsIOService(com.streams);

endmodule

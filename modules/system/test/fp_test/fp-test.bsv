//
// Copyright (C) 2008 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

//
// This module isn't meant to be especially pretty.  It is a simple test harness
// for testing arithmetic operations.  The test reads in a series of 64 bit
// number pairs and passes them to a calculate rule.
//
//                            * * * * * * * * * * *
// There are benchmarks under hasim/demos/test that provide data inputs for
// this test harness.
//                            * * * * * * * * * * *
//

import Vector::*;

`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS_FPTEST.bsh"
`include "asim/dict/STREAMS_MESSAGE.bsh"
`include "asim/dict/PARAMS_HARDWARE_SYSTEM.bsh"


module [CONNECTED_MODULE] mkSystem ();

    // Link to streams
    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    // Dynamic parameters to feed to datapath.
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();
    Param#(32) paramOp1 <- mkDynamicParameter(`PARAMS_HARDWARE_SYSTEM_OP1, paramNode);
    Param#(32) paramOp2 <- mkDynamicParameter(`PARAMS_HARDWARE_SYSTEM_OP2, paramNode);
    Param#(32) paramRnd <- mkDynamicParameter(`PARAMS_HARDWARE_SYSTEM_ROUND, paramNode);
    Bool rounding = paramRnd != 0;

    // Datapath instantiation based on AWB parameter.
    FP_ACCEL dp <- mkFPAcceleratorCvtItoS();
    
    // Only send the answer once.
    Reg#(Bool) done <- mkReg(False);


    // Rule to read the dynamic parameters and send them to the datapath.

    rule dpReq (!done);

        FP_INPUT inp;
        inp.operandA = zeroExtend(paramOp1);
        inp.operandB = zeroExtend(paramOp2);

        dp.makeReq(inp);
        done <= True;

    endrule

    // Rule to read the results and report them via streams.
    

    rule dpRsp (True);

        let outp <- dp.getRsp();
        Bit#(64) res2 = (rounding) ? toDouble(roundToSingle(outp.result)) : outp.result;

        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_FPTEST,
                                            stringID: `STREAMS_FPTEST_RESULT,
                                            payload0: res2[63:32],
                                            payload1: res2[31:0]});
    endrule

endmodule

//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
//

import Clocks::*;

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/hasim_controller.bsh"
`include "asim/provides/hasim_system.bsh"
`include "asim/provides/platform_interface.bsh"
`include "asim/provides/physical_platform.bsh"
`include "asim/provides/fpga_components.bsh"

module [HASIM_MODULE] mkModel (TOP_LEVEL_WIRES);
    
    let userClock <- mkUserClockFromCrystal(`MODEL_CLOCK_FREQ);

    Clock topLevelClock <- exposeCurrentClock();
    Reset topLevelReset <- exposeCurrentReset();
    
    let _x <- mkModelWithUserClock(topLevelClock,
                                   topLevelReset,
                                   clocked_by userClock.clk,
                                   reset_by userClock.rst);
    return _x;

endmodule

module [HASIM_MODULE] mkModelWithUserClock#(Clock topLevelClock, Reset topLevelReset)
    // interface
        (TOP_LEVEL_WIRES);

    // expose current clock and reset
    Clock clock      <- exposeCurrentClock();
    Reset hard_reset <- exposeCurrentReset();

    // 0 is number of stages
    // False = do not start in reset
    MakeResetIfc soft_reset_wrapper <- mkResetSync(0, False, clock);

    // use mkResetEither as the output
    Reset new_reset <- mkResetEither(hard_reset, soft_reset_wrapper.new_rst);

    // instantiate system, controller and PI with new reset
    let system     <- mkSystem           (reset_by new_reset);
    let controller <- mkController       (reset_by new_reset);
    let pi         <- mkPlatformInterface(topLevelClock, topLevelReset, reset_by new_reset);
    
    // create a connection to receive reset requests
    Connection_Receive#(Bool) link_reset <- mkConnection_Receive("soft_reset", reset_by new_reset);
    
    // Timer to hold reset
    Reg#(Bool)    do_reset      <- mkReg(False);
    Reg#(Bit#(8)) reset_counter <- mkReg(0);

    // create a rule to assert our generated reset
    rule get_reset_req (True);
        
        // accept a reset request
        link_reset.deq();
        
        // blow up the entire model
        $display("Soft Reset!");
        do_reset <= True;
        reset_counter <= 0;
        
    endrule

    // Hold reset for 128 cycles.  May be overkill but seems safer than 1.
    rule pull_reset (do_reset);

        soft_reset_wrapper.assertReset();

        reset_counter <= reset_counter + 1;
        if (reset_counter[7] == 1)
            do_reset <= False;

    endrule

    // return top level wires interface
    return pi;

endmodule

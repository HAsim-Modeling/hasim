// Copyright 2000--2006 Bluespec, Inc.  All rights reserved.

`include "soft_connections.bsh"
`include "front_panel_service.bsh"
`include "clocks_device.bsh"
`include "soft_clocks.bsh"
`include "fpga_components.bsh"

`include "asim/rrr/server_stub_CLOCKTEST.bsh"

// Simple model of a traffic light
// (modeled after the light at the intersection of Rte 16 and Broadway
//  on the border between Arlington, MA and Somerville, MA)

// Version 0: just model the normal cycle of states
  
module [CONNECTED_MODULE] mkClockTest();
  ServerStub_CLOCKTEST serverStub <- mkServerStub_CLOCKTEST();
  UserClock domain1 <- mkSoftClock(40);
  UserClock domain2 <- mkSoftClock(75);
  let tl0 <- mkClockTestStage1(clocked_by domain1.clk, reset_by domain1.rst);
  let tl1 <- mkClockTestStage2(clocked_by domain2.clk, reset_by domain2.rst);

  Connection_Send#(Bit#(32)) to_pipeline <- mkConnection_Send("to_pipeline");
  Connection_Receive#(Bit#(32)) from_pipeline <- mkConnection_Receive("from_pipeline");

  rule initite;
    let test <- serverStub.acceptRequest_test();
    to_pipeline.send(test);
  endrule

  rule terminate;
    serverStub.sendResponse_test(from_pipeline.receive() - 40 - 75);
    from_pipeline.deq();
  endrule

endmodule

module [CONNECTED_MODULE] mkClockTestStage1();
   
   Connection_Send#(Bit#(32)) to_pipeline2 <- mkConnection_Send("to_pipeline2");
   Connection_Receive#(Bit#(32)) from_out <- mkConnection_Receive("to_pipeline");

   rule forward;
     to_pipeline2.send(from_out.receive() + 40);
     from_out.deq();
   endrule

endmodule

module [CONNECTED_MODULE] mkClockTestStage2();
   
   Connection_Send#(Bit#(32)) to_out <- mkConnection_Send("from_pipeline");
   Connection_Receive#(Bit#(32)) from_pipeline1 <- mkConnection_Receive("to_pipeline2");

   rule forward;
     to_out.send(from_pipeline1.receive() + 75);
     from_pipeline1.deq();
   endrule

endmodule

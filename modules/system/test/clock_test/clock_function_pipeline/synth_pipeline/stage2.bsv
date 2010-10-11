// Copyright 2000--2006 Bluespec, Inc.  All rights reserved.

`include "soft_connections.bsh"
`include "front_panel_service.bsh"
`include "clocks_device.bsh"
`include "soft_clocks.bsh"
`include "fpga_components.bsh"

module [CONNECTED_MODULE] mkClockTestStage2();
   
   Connection_Send#(Bit#(32)) to_out <- mkConnection_Send("from_pipeline");
   Connection_Receive#(Bit#(32)) from_pipeline1 <- mkConnection_Receive("to_pipeline2");

   rule forward;
     to_out.send(from_pipeline1.receive() + 75);
     from_pipeline1.deq();
   endrule

endmodule

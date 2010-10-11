// Copyright 2000--2006 Bluespec, Inc.  All rights reserved.

`include "soft_connections.bsh"
`include "front_panel_service.bsh"
`include "clocks_device.bsh"
`include "soft_clocks.bsh"
`include "fpga_components.bsh"

module [CONNECTED_MODULE] mkClockTestStage1();
   
   Connection_Send#(Bit#(32)) to_pipeline2 <- mkConnection_Send("to_pipeline2");
   Connection_Receive#(Bit#(32)) from_out <- mkConnection_Receive("to_pipeline");

   rule forward;
     to_pipeline2.send(from_out.receive() + 40);
     from_out.deq();
   endrule

endmodule



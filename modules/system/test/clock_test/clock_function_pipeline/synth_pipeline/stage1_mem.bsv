// Copyright 2000--2006 Bluespec, Inc.  All rights reserved.

import FIFO::*;

`include "soft_connections.bsh"
`include "front_panel_service.bsh"
`include "clocks_device.bsh"
`include "soft_clocks.bsh"
`include "fpga_components.bsh"
`include "asim/provides/scratchpad_memory.bsh"
`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/mem_services.bsh"

`include "asim/dict/VDEV_SCRATCH.bsh"

typedef Bit#(16) Addr;

module [CONNECTED_MODULE] mkClockTestStage1();

   MEMORY_IFC#(Addr, Bit#(32))   scratchpad <- mkScratchpad(`VDEV_SCRATCH_PIPELINE_MEM, SCRATCHPAD_UNCACHED);
   
   Connection_Send#(Bit#(32)) to_pipeline2 <- mkConnection_Send("to_pipeline2");
   Connection_Receive#(Bit#(32)) from_out <- mkConnection_Receive("to_pipeline");

   FIFO#(Addr) tokensWrite <- mkSizedFIFO(8);
   FIFO#(Bit#(1)) tokensRead  <- mkSizedFIFO(8);

   rule forwardMem;
     scratchpad.write(truncate(from_out.receive()),from_out.receive() + 40);
     from_out.deq();     
     tokensWrite.enq(truncate(from_out.receive()));
   endrule

   rule reqMem;
     scratchpad.readReq(tokensWrite.first());
     tokensRead.enq(?);
     tokensWrite.deq();
   endrule

   rule respMem;
     let data <- scratchpad.readRsp();
     tokensRead.deq();
     to_pipeline2.send(data);
   endrule

endmodule
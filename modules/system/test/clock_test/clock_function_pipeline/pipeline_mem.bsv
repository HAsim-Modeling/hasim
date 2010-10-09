import FIFO::*;

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/clocks_device.bsh"
`include "asim/provides/soft_clocks.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/scratchpad_memory.bsh"
`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/mem_services.bsh"

`include "asim/dict/VDEV_SCRATCH.bsh"

`include "asim/rrr/server_stub_CLOCKTEST.bsh"

// Simple model of a traffic light
// (modeled after the light at the intersection of Rte 16 and Broadway
//  on the border between Arlington, MA and Somerville, MA)

// Version 0: just model the normal cycle of states
  
module [CONNECTED_MODULE] mkClockTest();
  ServerStub_CLOCKTEST serverStub <- mkServerStub_CLOCKTEST();
  UserClock domain1 <- mkSoftClock(40);
  UserClock domain2 <- mkSoftClock(60);
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

module [CONNECTED_MODULE] mkClockTestStage2();
   
   Connection_Send#(Bit#(32)) to_out <- mkConnection_Send("from_pipeline");
   Connection_Receive#(Bit#(32)) from_pipeline1 <- mkConnection_Receive("to_pipeline2");

   rule forward;
     to_out.send(from_pipeline1.receive() + 75);
     from_pipeline1.deq();
   endrule

endmodule

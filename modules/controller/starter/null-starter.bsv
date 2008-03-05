import hasim_common::*;

interface Starter;
    // service methods: requests
    method Action acceptRequest_Run();
    method Action acceptRequest_Pause();
    method Action acceptRequest_Sync();
    method Action acceptRequest_DumpStats();

    // service methods: responses
    method Action sendResponse_DumpStats();

    // client methods
    method Action makeRequest_EndSim(Bit#(1) success);
endinterface

typedef enum
{
  ST_Idle,
  ST_Running,
  ST_Syncing,
  ST_Dumping,
  ST_Finishing
}
  STARTER_STATE deriving (Eq, Bits);

module [HASim_Module] mkStarter (Starter);

    Reg#(Bit#(32)) fpga_cc <- mkReg(0);
    Reg#(STARTER_STATE) state <- mkReg(ST_Idle);
    Reg#(Bit#(1)) result <- mkRegU();

    rule tick (True);
    
      fpga_cc <= fpga_cc + 1;
    
    endrule

    method Action acceptRequest_Pause() if (False) = noAction; //Disabled

    method Action acceptRequest_Run() if (state == ST_Idle);
    
      $display("[%0d]: Starter: Beginning Run.", fpga_cc);
      state <= ST_Running;
    
    endmethod
    
    method Action makeRequest_EndSim(Bit#(1) success) if (state == ST_Running);
    
        $display("[%0d]: Starter: Run Finished.", fpga_cc);

        if (success == 1)
        begin
          $display("[%0d]: Starter: Program Finished Succesfully.", fpga_cc);
        end
        else
        begin
          $display("[%0d]: Starter: One or more errors occurred.", fpga_cc);
        end

        result <= success;
        state <= ST_Syncing;

    endmethod

    method Action acceptRequest_Sync() if (state == ST_Syncing);

       $display("[%0d]: Starter: Synchronizing the system.", fpga_cc);
       state <= ST_Dumping;
    
    endmethod

    method Action acceptRequest_DumpStats() if (state == ST_Dumping);
    
      $display("[%0d]: Starter: Beginning Stat Dump.", fpga_cc);
      state <= ST_Finishing;
    
    endmethod


    method Action sendResponse_DumpStats() if (state == ST_Finishing);
    
      $display("[%0d]: Starter: Stat Dump Finished.", fpga_cc);
      $display("[%0d]: Starter: Exiting...", fpga_cc);
      $finish(zeroExtend(result));

    endmethod

endmodule


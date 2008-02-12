import hasim_common::*;

typedef Bit#(8) SIM_STATE;

interface Starter;
    method SIM_STATE getSimState();
    method Action    endSim(Bit#(1) success);
endinterface

module [HASim_Module] mkStarter(Starter);

    method SIM_STATE getSimState();
        return `HWSTATE_RUNNING;
    endmethod

    method Action endSim(Bit#(1) success);
        $finish(success);
    endmethod

endmodule


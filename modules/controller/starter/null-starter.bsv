import hasim_common::*;

typedef Bit#(8) SIM_STATE;

interface Starter;
    method SIM_STATE getSimState();
endinterface

module [HASim_Module] mkStarter(Starter);

    method SIM_STATE getSimState();
        return `HWSTATE_RUNNING;
    endmethod

endmodule


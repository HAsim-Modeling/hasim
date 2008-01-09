import hasim_common::*;
import rrr::*;
import soft_connections::*;

`include "rrr_service_ids.bsh"
`define SERVICE_ID  `STARTER_SERVICE_ID

typedef Bit#(8) SIM_STATE;

interface Starter;
    method SIM_STATE getSimState();
endinterface

module [HASim_Module] mkStarter(Starter);

    // ----------- state -----------
    Reg#(SIM_STATE) simState        <- mkReg(`HWSTATE_IDLE);
   
    Connection_Receive#(UINT32) link_Start <- mkConnection_Receive("rrr_service_STARTER_Start");
    Connection_Receive#(UINT32) link_Stop  <- mkConnection_Receive("rrr_service_STARTER_Stop");

    // ----------- rules ------------

    // accept start RRR request over connection
    rule accept_request_Start (True);
        UINT32 payload = link_Start.receive();
        link_Start.deq();

        if (simState == `HWSTATE_RUNNING)
        begin
            $display("starter: Start request received in RUNNING state");
            $finish(1);
        end

        simState <= `HWSTATE_RUNNING;
    endrule

    // accept stop RRR request over connection
    rule accept_request_Stop (True);
        UINT32 payload = link_Stop.receive();
        link_Stop.deq();

        if (simState != `HWSTATE_RUNNING)
        begin
            $display("starter: Stop request received in IDLE or STOPPED state");
            $finish(1);
        end

        simState <= `HWSTATE_STOPPED;
    endrule

    // ------------ methods ------------

    // return cached simulation state
    method SIM_STATE getSimState();
        return simState;
    endmethod

endmodule


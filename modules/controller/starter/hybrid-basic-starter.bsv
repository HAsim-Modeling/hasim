import hasim_common::*;
import rrr::*;
import soft_connections::*;

`include "asim/rrr/rrr_service_ids.bsh"
`define SERVICE_ID  `STARTER_SERVICE_ID

typedef Bit#(8) SIM_STATE;

interface Starter;
    method SIM_STATE getSimState();
    method Action endSim(Bit#(1) success);
endinterface

module [HASim_Module] mkStarter(Starter);

    // ----------- state -----------
    Reg#(SIM_STATE) simState        <- mkReg(`HWSTATE_IDLE);
   
    // ----------- links -----------
    Connection_Receive#(UINT32)   link_Start <- mkConnection_Receive("rrr_service_STARTER_Start");
    Connection_Receive#(UINT32)   link_Stop  <- mkConnection_Receive("rrr_service_STARTER_Stop");
    Connection_Send#(RRR_Request) link_rrr   <- mkConnection_Send("rrr_client_starter");

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

    // signal end of simulation
    method Action endSim(Bit#(1) success);
        link_rrr.send(RRR_Request { serviceID   : `SERVICE_ID,
                                    param0      : 0,
                                    param1      : zeroExtend(success),
                                    param2      : ?,
                                    param3      : ?,
                                    needResponse: False });
    endmethod

endmodule


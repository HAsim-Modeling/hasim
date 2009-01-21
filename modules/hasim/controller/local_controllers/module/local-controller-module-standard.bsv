//
// Copyright (C) 2008 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// LocalController.bsv                                                       //
//                                                                           //
// Local Controller instantiated by timing modules.                          //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

import Vector::*;
import FIFO::*;

// Project imports

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/dict/RINGID.bsh"


interface LocalController;

    method Action startModelCC();
    method Bool   running();
    method Bool   contextIsActive(CONTEXT_ID ctx_id);
    method Action endProgram(Bool passfail);

endinterface


typedef enum
{
    LC_Idle,               // Waiting for a command
    LC_Running,            // Running, allowing slip
    LC_Synchronizing,      // Running, attempting to synchronize
    LC_Stepping            // Run one modelCC
}
LC_STATE
    deriving (Eq, Bits);

module [HASIM_MODULE] mkLocalController#(Vector#(n, Port_Control) inports, Vector#(m, Port_Control) outports) (LocalController);

    Reg#(LC_STATE) state <- mkReg(LC_Idle);
  
    Reg#(Bool) balancedSinceQuery <- mkRegU();
    Reg#(Bool) checkBalanced <- mkReg(False);
  
    // Vector of active contexts
    LUTRAM#(CONTEXT_ID, Bool) contextActive <- mkLUTRAM(False);

    Connection_Chain#(CONTROLLER_COMMAND)  cmds  <- mkConnection_Chain(`RINGID_MODULE_COMMANDS);
    Connection_Chain#(CONTROLLER_RESPONSE) resps <- mkConnection_Chain(`RINGID_MODULE_RESPONSES);

    // Can this module read from this Port?
    function Bool canReadFrom(Port_Control inport);
        return case (state)
                   LC_Running:        return !inport.empty();
                   LC_Stepping:       return !inport.empty();
                   LC_Synchronizing:  return !inport.light();
                   default:           return False;
               endcase;
    endfunction

    function canWriteTo(Port_Control inport);
        return case (state)
                   LC_Running:        return !inport.full();
                   LC_Stepping:       return !inport.full();
                   LC_Synchronizing:  return !inport.heavy();
                   default:           return False;
               endcase;
    endfunction

    function Bool portsReady();
        Bool canRead  = True;
        Bool canWrite = True;

        for (Integer x = 0; x < valueof(n); x = x + 1)
            canRead = canRead && canReadFrom(inports[x]);

        for (Integer x = 0; x < valueof(m); x = x + 1)
            canWrite = canWrite && canWriteTo(outports[x]);

        return canRead && canWrite;
    endfunction

    function Bool balanced();
        Bool res = True;

        for (Integer x = 0; x < valueof(n); x = x + 1)
            res = res && inports[x].balanced();

        for (Integer x = 0; x < valueof(m); x = x + 1)
            res = res && outports[x].balanced();

        return res;
    endfunction

    (* descending_urgency="checkBalance, shiftCommand, shiftResponse" *)
    rule shiftCommand (True);

        let newcmd <- cmds.receive_from_prev();

        case (newcmd) matches
            tagged COM_RunProgram:
            begin
                state <= LC_Running;
            end

            tagged COM_Synchronize:
            begin
                state <= LC_Synchronizing;
            end

            tagged COM_StartSyncQuery:
            begin
                checkBalanced <= True;
                balancedSinceQuery <= True;
            end

            tagged COM_SyncQuery:
            begin
                checkBalanced <= False;
                if (balancedSinceQuery)
                    resps.send_to_next(RESP_Balanced);
                else
                    resps.send_to_next(RESP_UnBalanced);
            end

            tagged COM_Step:
            begin
                state <= LC_Stepping;
            end

            tagged COM_EnableContext .ctx_id:
            begin
                contextActive.upd(ctx_id, True);
            end

            tagged COM_DisableContext .ctx_id:
            begin
                contextActive.upd(ctx_id, False);
            end
        endcase

        // send it on
        cmds.send_to_next(newcmd);
    endrule
  
    rule checkBalance (checkBalanced);
        balancedSinceQuery <= balancedSinceQuery && balanced();
    endrule

    rule shiftResponse (True);
        let resp <- resps.receive_from_prev();
        // Just send it on
        resps.send_to_next(resp);
    endrule

    method Action startModelCC() if ((state != LC_Idle) && portsReady());
        case (state)
            LC_Idle:          noAction;
            LC_Running:       noAction;
            LC_Synchronizing: noAction;
            LC_Stepping:
            begin
                state <= LC_Idle;
            end
        endcase
    endmethod

    method Bool running();
        return case (state)
                   LC_Idle:          return False;
                   LC_Running:       return True;
                   LC_Synchronizing: return True;
                   LC_Stepping:      return True;
               endcase;
    endmethod

    method Bool contextIsActive(CONTEXT_ID ctx_id);
        return contextActive.sub(ctx_id);
    endmethod

    method Action endProgram(Bool passfail);
        resps.send_to_next(tagged RESP_DoneRunning passfail);
    endmethod

endmodule


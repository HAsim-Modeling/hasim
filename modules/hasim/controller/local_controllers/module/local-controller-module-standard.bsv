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


interface LOCAL_CONTROLLER;

    method ActionValue#(CONTEXT_ID) startModelCycle();
    method Action endModelCycle(CONTEXT_ID ctx, Bit#(8) path);
    method Action contextDone(CONTEXT_ID ctx_id, Bool passfail);

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

module [HASIM_MODULE] mkLocalController#(Vector#(n, Vector#(NUM_CONTEXTS, PORT_CONTROL)) inports, Vector#(m, Vector#(NUM_CONTEXTS, PORT_CONTROL)) outports)
    //interface:
        (LOCAL_CONTROLLER);

    Reg#(LC_STATE) state <- mkReg(LC_Idle);
  
    // Vector of active contexts
    Reg#(Vector#(NUM_CONTEXTS, Bool)) contextActive <- mkReg(replicate(False));
    // Vector of running contexts
    Reg#(Vector#(NUM_CONTEXTS, Bool)) contextRunning <- mkReg(replicate(False));
    // Track stepping state.
    Reg#(Vector#(NUM_CONTEXTS, Bool)) contextStepped <- mkReg(replicate(False));
    // Check balanced state.
    Reg#(Vector#(NUM_CONTEXTS, Bool)) contextBalancedSinceQuery <- mkReg(replicate(False));
    Reg#(Vector#(NUM_CONTEXTS, Bool)) contextCheckingBalance <- mkReg(replicate(False));
    
    // Are we checking if the ports have quiesced?
    Reg#(Bool) checkBalanced <- mkReg(False);

    Vector#(NUM_CONTEXTS, PulseWire)    startCycleW <- replicateM(mkPulseWire());
    Vector#(NUM_CONTEXTS, PulseWire)      endCycleW <- replicateM(mkPulseWire());
    Vector#(NUM_CONTEXTS, Wire#(Bit#(8))) pathDoneW <- replicateM(mkWire());
    
    
    // For now this local controller just goes round-robin over the contexts.
    // This is guaranteed to be correct accross multiple modules.
    // The performance of this could be improved, but the interaction with time-multiplexed
    // ports needs to be worked out.
    
    COUNTER#(CONTEXT_ID_SIZE) nextContext <- mkLCounter(0);
    
    Connection_Chain#(CONTROLLER_COMMAND)  cmds  <- mkConnection_Chain(`RINGID_MODULE_COMMANDS);
    Connection_Chain#(CONTROLLER_RESPONSE) resps <- mkConnection_Chain(`RINGID_MODULE_RESPONSES);
        
    function Bool allTrue(Vector#(k, Bool) v);
        return foldr(\&& , True, v);
    endfunction

    // Can this module read from this Port?
    function Bool canReadFrom(PORT_CONTROL inport);
        return case (state)
                   LC_Running:        return !inport.empty();
                   LC_Stepping:       return !inport.empty();
                   LC_Synchronizing:  return !inport.light();
                   default:           return False;
               endcase;
    endfunction

    function canWriteTo(PORT_CONTROL outport);
        return case (state)
                   LC_Running:        return !outport.full();
                   LC_Stepping:       return !outport.full();
                   LC_Synchronizing:  return !outport.heavy();
                   default:           return False;
               endcase;
    endfunction

    // This function will determine the next context in a non-round-robin manner when we're ready
    // to go that route. Currently this is unused.

    function Bool contextReady(CONTEXT_ID ctx_id);
        
        Bool canRead  = True;
        Bool canWrite = True;

        // Can we read/write all of the ports?
        for (Integer x = 0; x < valueOf(n); x = x + 1)
            canRead = canRead && canReadFrom(inports[x][ctx_id]);

        for (Integer x = 0; x < valueOf(m); x = x + 1)
            canWrite = canWrite && canWriteTo(outports[x][ctx_id]);

        // A context is ready to go only if it's been enabled.
        return contextActive[ctx_id] && !contextRunning[ctx_id] && canRead && canWrite;

    endfunction

    function CONTEXT_ID nextReadyContext();
        
        CONTEXT_ID res = 0;

        for (Integer x = 0; x < valueof(NUM_CONTEXTS); x = x + 1)
        begin
            res = contextReady(fromInteger(x)) ? fromInteger(x) : res;
        end
        
        return res;
    
    endfunction

    function Bool someContextReady();
        
        Bool res = False;

        for (Integer x = 0; x < valueof(NUM_CONTEXTS); x = x + 1)
        begin
            res = contextReady(fromInteger(x)) || res;
        end
        
        return res;
    
    endfunction



    function Bool balanced(Integer ctx_id);
        Bool res = True;
        
        // Are the ports all balanced?
        for (Integer x = 0; x < valueOf(n); x = x + 1)
        begin
            res = res && inports[x][ctx_id].balanced();
        end

        for (Integer x = 0; x < valueOf(m); x = x + 1)
        begin
            res = res && outports[x][ctx_id].balanced();
        end

        return res;
    endfunction

    (* descending_urgency="shiftCommand, shiftResponse, checkBalance" *)
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
                contextBalancedSinceQuery <= replicate(True);
            end

            tagged COM_SyncQuery:
            begin
                checkBalanced <= False;
                if (allTrue(contextBalancedSinceQuery))
                    resps.send_to_next(RESP_Balanced);
                else
                    resps.send_to_next(RESP_UnBalanced);
            end

            tagged COM_Step:
            begin

                state <= LC_Stepping;
                Vector#(NUM_CONTEXTS, Bool) context_stepped = newVector();
                for (Integer x = 0; x < valueOf(NUM_CONTEXTS); x = x + 1)
                begin
                   context_stepped[x] = !contextActive[x];
                end
                contextStepped <= context_stepped;
                
            end

            tagged COM_EnableContext .ctx_id:
            begin
                contextActive[ctx_id] <= True;
            end

            tagged COM_DisableContext .ctx_id:
            begin
                contextActive[ctx_id] <= False;
            end
        endcase

        // send it on
        cmds.send_to_next(newcmd);
    endrule
  
    rule checkBalance (checkBalanced);

        Vector#(NUM_CONTEXTS, Bool) new_balanced = contextBalancedSinceQuery;

        for (Integer x = 0; x < valueOf(NUM_CONTEXTS); x = x + 1)
        begin
            new_balanced[x] = new_balanced[x] && balanced(x);
        end
        
        contextBalancedSinceQuery <= new_balanced;

    endrule

    rule shiftResponse (True);
        let resp <- resps.receive_from_prev();
        // Just send it on
        resps.send_to_next(resp);
    endrule

    rule ignoreDisabledContexts (!contextActive[nextContext.value()]);
    
        nextContext.up();
    
    endrule

    rule updateRunning (True);
    
        Vector#(NUM_CONTEXTS, Bool) new_running = contextRunning;

        for (Integer x = 0; x < valueOf(NUM_CONTEXTS); x = x + 1)
        begin
            if (contextRunning[x])
                new_running[x] =  !endCycleW[x];
            else if (startCycleW[x])
                new_running[x] = !endCycleW[x];
            else
                noAction;
        end
        
        contextRunning <= new_running;
    
    endrule

    method ActionValue#(CONTEXT_ID) startModelCycle() if ((state != LC_Idle) && contextReady(nextContext.value()));

        if (state == LC_Stepping)
        begin

            contextStepped[nextContext.value()] <= True;
            if (allTrue(contextStepped))
                state <= LC_Idle;

        end
        startCycleW[nextContext.value()].send();
        nextContext.up();
        return nextContext.value();

    endmethod
/*
    method Bool contextIsActive(CONTEXT_ID ctx_id);
        return contextActive.sub(ctx_id);
    endmethod
*/
    method Action endModelCycle(CONTEXT_ID ctx, Bit#(8) path);
    
        endCycleW[ctx].send();
        pathDoneW[ctx] <= path; // Put the path into the waveform.
    
    endmethod

    method Action contextDone(CONTEXT_ID ctx_id, Bool pf);
        // XXX this should be per-context.
        resps.send_to_next(tagged RESP_DoneRunning pf);
    endmethod
    
endmodule


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


// ni is number of instances to control.
interface LOCAL_CONTROLLER#(type ni);

    method ActionValue#(INSTANCE_ID#(ni)) startModelCycle();
    method Action endModelCycle(INSTANCE_ID#(ni) iid, Bit#(8) path);
    method Action instanceDone(INSTANCE_ID#(ni) iid, Bool passfail);

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

module [HASIM_MODULE] mkLocalController#(Vector#(n, Vector#(ni, PORT_CONTROL)) inports, Vector#(m, Vector#(ni, PORT_CONTROL)) outports)
    //interface:
        (LOCAL_CONTROLLER#(ni));

    Reg#(LC_STATE) state <- mkReg(LC_Idle);
  
    // Vector of active instances
    Reg#(Vector#(ni, Bool)) instanceActive <- mkReg(replicate(False));
    // Vector of running instances
    Reg#(Vector#(ni, Bool)) instanceRunning <- mkReg(replicate(False));
    // Track stepping state.
    Reg#(Vector#(ni, Bool)) instanceStepped <- mkReg(replicate(False));
    // Check balanced state.
    Reg#(Vector#(ni, Bool)) instanceBalancedSinceQuery <- mkReg(replicate(False));
    Reg#(Vector#(ni, Bool)) instanceCheckingBalance <- mkReg(replicate(False));
    
    // Are we checking if the ports have quiesced?
    Reg#(Bool) checkBalanced <- mkReg(False);

    Vector#(ni, PulseWire)    startCycleW <- replicateM(mkPulseWire());
    Vector#(ni, PulseWire)      endCycleW <- replicateM(mkPulseWire());
    Vector#(ni, Wire#(Bit#(8))) pathDoneW <- replicateM(mkWire());
    
    
    // For now this local controller just goes round-robin over the instances.
    // This is guaranteed to be correct accross multiple modules.
    // The performance of this could be improved, but the interaction with time-multiplexed
    // ports needs to be worked out.
    
    COUNTER#(INSTANCE_ID_BITS#(ni)) nextInstance <- mkLCounter(0);
    
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

    // This function will determine the next instance in a non-round-robin manner when we're ready
    // to go that route. Currently this is unused.

    function Bool instanceReady(INSTANCE_ID#(ni) iid);
        
        Bool canRead  = True;
        Bool canWrite = True;

        // Can we read/write all of the ports?
        for (Integer x = 0; x < valueOf(n); x = x + 1)
            canRead = canRead && canReadFrom(inports[x][iid]);

        for (Integer x = 0; x < valueOf(m); x = x + 1)
            canWrite = canWrite && canWriteTo(outports[x][iid]);

        // An instance is ready to go only if it's been enabled.
        return instanceActive[iid] && !instanceRunning[iid] && canRead && canWrite;

    endfunction

    function INSTANCE_ID#(ni) nextReadyInstance();
        
        INSTANCE_ID#(ni) res = 0;

        for (Integer x = 0; x < valueof(ni); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) ? fromInteger(x) : res;
        end
        
        return res;
    
    endfunction

    function Bool someInstanceReady();
        
        Bool res = False;

        for (Integer x = 0; x < valueof(ni); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) || res;
        end
        
        return res;
    
    endfunction



    function Bool balanced(Integer iid);
        Bool res = True;
        
        // Are the ports all balanced?
        for (Integer x = 0; x < valueOf(n); x = x + 1)
        begin
            res = res && inports[x][iid].balanced();
        end

        for (Integer x = 0; x < valueOf(m); x = x + 1)
        begin
            res = res && outports[x][iid].balanced();
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
                instanceBalancedSinceQuery <= replicate(True);
            end

            tagged COM_SyncQuery:
            begin
                checkBalanced <= False;
                if (allTrue(instanceBalancedSinceQuery))
                    resps.send_to_next(RESP_Balanced);
                else
                    resps.send_to_next(RESP_UnBalanced);
            end

            tagged COM_Step:
            begin

                state <= LC_Stepping;
                Vector#(ni, Bool) instance_stepped = newVector();
                for (Integer x = 0; x < valueOf(ni); x = x + 1)
                begin
                   instance_stepped[x] = !instanceActive[x];
                end
                instanceStepped <= instance_stepped;
                
            end

            // TODO: should this be COM_EnableInstance??
            tagged COM_EnableContext .iid:
            begin
                instanceActive[iid] <= True;
            end

            // TODO: should this be COM_DisableInstance??
            tagged COM_DisableContext .iid:
            begin
                instanceActive[iid] <= False;
            end
        endcase

        // send it on
        cmds.send_to_next(newcmd);
    endrule
  
    rule checkBalance (checkBalanced);

        Vector#(ni, Bool) new_balanced = instanceBalancedSinceQuery;

        for (Integer x = 0; x < valueOf(ni); x = x + 1)
        begin
            new_balanced[x] = new_balanced[x] && balanced(x);
        end
        
        instanceBalancedSinceQuery <= new_balanced;

    endrule

    rule shiftResponse (True);
        let resp <- resps.receive_from_prev();
        // Just send it on
        resps.send_to_next(resp);
    endrule

    rule ignoreDisabledInstances (!instanceActive[nextInstance.value()]);
    
        nextInstance.up();
    
    endrule

    rule updateRunning (True);
    
        Vector#(ni, Bool) new_running = instanceRunning;

        for (Integer x = 0; x < valueOf(ni); x = x + 1)
        begin
            if (instanceRunning[x])
                new_running[x] =  !endCycleW[x];
            else if (startCycleW[x])
                new_running[x] = !endCycleW[x];
            else
                noAction;
        end
        
        instanceRunning <= new_running;
    
    endrule

    method ActionValue#(INSTANCE_ID#(ni)) startModelCycle() if ((state != LC_Idle) && instanceReady(nextInstance.value()));

        if (state == LC_Stepping)
        begin

            instanceStepped[nextInstance.value()] <= True;
            if (allTrue(instanceStepped))
                state <= LC_Idle;

        end
        startCycleW[nextInstance.value()].send();
        nextInstance.up();
        return nextInstance.value();

    endmethod
/*
    method Bool instanceIsActive(INSTANCE_ID#(ni) iid);
        return instanceActive.sub(iid);
    endmethod
*/
    method Action endModelCycle(INSTANCE_ID#(ni) iid, Bit#(8) path);
    
        endCycleW[iid].send();
        pathDoneW[iid] <= path; // Put the path into the waveform.
    
    endmethod

    method Action instanceDone(INSTANCE_ID#(ni) iid, Bool pf);
        // XXX this should be per-instance.
        resps.send_to_next(tagged RESP_DoneRunning pf);
    endmethod
    
endmodule


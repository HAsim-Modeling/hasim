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

// Project imports

`include "asim/provides/hasim_common.bsh"


typedef union tagged
{
    // Commands from to local controllers
    void         COM_RunProgram;     // Begin running, allowed to slip
    void         COM_Synchronize;    // Start synchronizing the system
    Bool         COM_SyncQuery;      // Is the system synchronized yet?
                                     // System is synchronized if the message
                                     // makes it around the ring remaining True.
    void         COM_Step;           // Run exactly one model CC.
    Bool         COM_Pause;          // Stop running (True -> send response to host)
    CONTEXT_ID   COM_EnableContext;  // Enable context
    CONTEXT_ID   COM_DisableContext; // Disable context

    // Messages from local controllers
    Bool         LC_DoneRunning;     // Bool is run passed
}
CONTROLLER_MSG
    deriving (Eq, Bits);

                
// t_NUM_INSTANCES is number of instances to control.
interface LOCAL_CONTROLLER#(type t_NUM_INSTANCES);

    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) startModelCycle();
    method Action endModelCycle(INSTANCE_ID#(t_NUM_INSTANCES) iid, Bit#(8) path);
    method Action instanceDone(INSTANCE_ID#(t_NUM_INSTANCES) iid, Bool passfail);

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

module [HASIM_MODULE] mkLocalController

    // parameters:
    #(
    Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls, 
    Vector#(t_NUM_OUTPORTS, INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES)) outctrls
    )
    // interface:
        (LOCAL_CONTROLLER#(t_NUM_INSTANCES));

    Reg#(LC_STATE) state <- mkReg(LC_Idle);
  
    // Vector of active instances
    Reg#(Vector#(t_NUM_INSTANCES, Bool)) instanceActive <- mkReg(replicate(False));
    // Vector of running instances
    Reg#(Vector#(t_NUM_INSTANCES, Bool)) instanceRunning <- mkReg(replicate(False));
    // Track stepping state.
    Reg#(Vector#(t_NUM_INSTANCES, Bool)) instanceStepped <- mkReg(replicate(False));

    // Signalled DONE to the software?
    Reg#(Bool) signalDone <- mkReg(False);

    Vector#(t_NUM_INSTANCES, PulseWire)    startCycleW <- replicateM(mkPulseWire());
    Vector#(t_NUM_INSTANCES, PulseWire)      endCycleW <- replicateM(mkPulseWire());
    Vector#(t_NUM_INSTANCES, Wire#(Bit#(8))) pathDoneW <- replicateM(mkWire());
    
    
    // For now this local controller just goes round-robin over the instances.
    // This is guaranteed to be correct accross multiple modules.
    // The performance of this could be improved, but the interaction with time-multiplexed
    // ports needs to be worked out.
    
    COUNTER#(INSTANCE_ID_BITS#(t_NUM_INSTANCES)) nextInstance <- mkLCounter(0);
    
    Connection_Chain#(CONTROLLER_MSG) link_controllers <- mkConnection_Chain(`RINGID_CONTROLLER_MESSAGES);

    function Bool allTrue(Vector#(k, Bool) v);
        return foldr(\&& , True, v);
    endfunction

    // Can this module read from this Port?
    function Bool canReadFrom(INSTANCE_CONTROL_IN#(t_NUM_INSTANCES) ctrl_in);
        return case (state)
                   LC_Running:        return !ctrl_in.empty();
                   LC_Stepping:       return !ctrl_in.empty();
                   LC_Synchronizing:  return !ctrl_in.light();
                   default:           return False;
               endcase;
    endfunction

    function canWriteTo(INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES) ctrl_out);
        return case (state)
                   LC_Running:        return !ctrl_out.full();
                   LC_Stepping:       return !ctrl_out.full();
                   LC_Synchronizing:  return !ctrl_out.heavy();
                   default:           return False;
               endcase;
    endfunction

    // This function will determine the next instance in a non-round-robin manner when we're ready
    // to go that route. Currently this is unused.

    function Bool instanceReady(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        
        Bool canRead  = True;
        Bool canWrite = True;

        // Can we read/write all of the ports?
        for (Integer x = 0; x < valueOf(t_NUM_INPORTS); x = x + 1)
            canRead = canRead && canReadFrom(inctrls[x]);

        for (Integer x = 0; x < valueOf(t_NUM_OUTPORTS); x = x + 1)
            canWrite = canWrite && canWriteTo(outctrls[x]);

        // An instance is ready to go only if it's been enabled.
        return instanceActive[iid] && !instanceRunning[iid] && canRead && canWrite;

    endfunction

    function Action checkInstanceSanity();
    action
    
        // Verify all of the input ports share the same instance, and 
        // that it's the expected instance.
        for (Integer x = 0; x < valueOf(t_NUM_INPORTS); x = x + 1)
        begin
        
            if (inctrls[x].nextReadyInstance() matches tagged Valid .iid &&&
                iid != nextInstance.value())
            begin

                $display("WARNING: Local controller expected instance id: %0d, found: %0d on port #%0d", nextInstance.value(), iid, fromInteger(x));

            end

        end

    endaction
    endfunction

    function INSTANCE_ID#(t_NUM_INSTANCES) nextReadyInstance();
        
        INSTANCE_ID#(t_NUM_INSTANCES) res = 0;

        for (Integer x = 0; x < valueof(t_NUM_INSTANCES); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) ? fromInteger(x) : res;
        end
        
        return res;
    
    endfunction

    function Bool someInstanceReady();
        
        Bool res = False;

        for (Integer x = 0; x < valueof(t_NUM_INSTANCES); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) || res;
        end
        
        return res;
    
    endfunction



    function Bool balanced();
        Bool res = True;
        
        // Are the ports all balanced?
        for (Integer x = 0; x < valueOf(t_NUM_INPORTS); x = x + 1)
        begin
            res = res && inctrls[x].balanced();
        end

        for (Integer x = 0; x < valueOf(t_NUM_OUTPORTS); x = x + 1)
        begin
            res = res && outctrls[x].balanced();
        end

        return res;
    endfunction



    // ====================================================================
    //
    // Process controller commands and send responses.
    //
    // ====================================================================

    FIFO#(Bool) checkBalanceQ <- mkFIFO();
    FIFO#(CONTROLLER_MSG) newCtrlMsgQ <- mkFIFO1();
    
    rule checkBalance (True);
        checkBalanceQ.deq();
        link_controllers.send_to_next(tagged COM_SyncQuery balanced());
    endrule

    rule newControlMsg (True);
        let cmd = newCtrlMsgQ.first();
        newCtrlMsgQ.deq();
        
        link_controllers.send_to_next(cmd);
    endrule

    (* descending_urgency = "checkBalance, newControlMsg, nextCommand" *)
    rule nextCommand (state != LC_Stepping);
        let newcmd <- link_controllers.receive_from_prev();
        Maybe#(CONTROLLER_MSG) outcmd = tagged Valid newcmd;

        case (newcmd) matches
            tagged COM_RunProgram:
            begin
                state <= LC_Running;
            end

            tagged COM_Synchronize:
            begin
                state <= LC_Synchronizing;
            end

            tagged COM_SyncQuery .all_balanced:
            begin
                // The COM_SyncQuery state will remain True if all controllers
                // are balanced.  If a previous controller is unbalanced then
                // just forward the unbalanced state.  If we need to check
                // the state of this controller then queue the request.
                //
                // The Bluespec scheduler throws an error about being unable
                // to break a cycle if we attempt to check balance here
                // because it can't break the connection between setting
                // state, startModelCycle and changes to balance() while
                // the model runs.
                if (all_balanced)
                begin
                    outcmd = tagged Invalid;
                    checkBalanceQ.enq(?);
                end
            end

            tagged COM_Step:
            begin
                state <= LC_Stepping;
                Vector#(t_NUM_INSTANCES, Bool) instance_stepped = newVector();
                for (Integer x = 0; x < valueOf(t_NUM_INSTANCES); x = x + 1)
                begin
                   instance_stepped[x] = !instanceActive[x];
                end
                instanceStepped <= instance_stepped;
            end

            tagged COM_Pause .send_response:
            begin
                state <= LC_Idle;
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

        // Forward command around the ring
        if (outcmd matches tagged Valid .cmd)
        begin
            link_controllers.send_to_next(cmd);
        end
    endrule



    rule ignoreDisabledInstances (state != LC_Idle && !instanceActive[nextInstance.value()]);
    
        nextInstance.up();
    
    endrule
    
    for (Integer x = 0; x < valueof(t_NUM_INPORTS); x = x + 1)
    begin
    
        rule dropDisabledInstance (inctrls[x].nextReadyInstance() matches tagged Valid .iid &&&
                                   !instanceActive[iid] &&&
                                   state != LC_Idle);
            inctrls[x].drop();
                    
        endrule
    
    end

    rule updateRunning (True);
    
        Vector#(t_NUM_INSTANCES, Bool) new_running = instanceRunning;

        for (Integer x = 0; x < valueOf(t_NUM_INSTANCES); x = x + 1)
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

    //
    // updateStateForStepping --
    //     State update associated with startModelCycle, encoded in a rule
    //     and controlled by a wire in order to set scheduling priority.
    //
    Wire#(Maybe#(Bit#(INSTANCE_ID_BITS#(t_NUM_INSTANCES)))) newModelCycleStarted <- mkDWire(tagged Invalid);

    (* descending_urgency = "updateStateForStepping, nextCommand" *)
    rule updateStateForStepping (state == LC_Stepping &&&
                                 newModelCycleStarted matches tagged Valid .iid);
        instanceStepped[iid] <= True;
        if (allTrue(instanceStepped))
            state <= LC_Idle;
    endrule


    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) startModelCycle() if ((state != LC_Idle) && instanceReady(nextInstance.value()));

        let next_iid = nextInstance.value();

        if (state == LC_Stepping)
        begin
            newModelCycleStarted <= tagged Valid next_iid;
        end
        
        // checkInstanceSanity();
        
        startCycleW[next_iid].send();
        nextInstance.up();
        return next_iid;

    endmethod

    method Action endModelCycle(INSTANCE_ID#(t_NUM_INSTANCES) iid, Bit#(8) path);
    
        endCycleW[iid].send();
        pathDoneW[iid] <= path; // Put the path into the waveform.
    
    endmethod

    method Action instanceDone(INSTANCE_ID#(t_NUM_INSTANCES) iid, Bool pf);
        // XXX this should be per-instance.  For now only allowed to fire once.
        if (! signalDone)
        begin
            newCtrlMsgQ.enq(tagged LC_DoneRunning pf);
            signalDone <= True;
        end
    endmethod
    
endmodule

interface STAGE_CONTROLLER#(numeric type t_NUM_INSTANCES, type t_PIPE_STATE);

    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_PIPE_STATE st);
    
    method ActionValue#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_PIPE_STATE)) nextReadyInstance();

endinterface

interface STAGE_CONTROLLER_VOID#(numeric type t_NUM_INSTANCES);

    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    
    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();

endinterface

module mkStageController 
    // interface:
        (STAGE_CONTROLLER#(t_NUM_INSTANCES, t_PIPE_STATE))
    provisos
        (Bits#(t_PIPE_STATE, t_PIPE_STATE_SZ));

    FIFO#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_PIPE_STATE)) q <- mkSizedFIFO(`STAGE_CONTROLLER_BUFFERING);

    
    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_PIPE_STATE st);
    
        q.enq(tuple2(iid, st));
    
    endmethod
    
    method ActionValue#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_PIPE_STATE)) nextReadyInstance();

        q.deq();
        return q.first();

    endmethod

endmodule


module mkStageControllerVoid
    // interface:
        (STAGE_CONTROLLER_VOID#(t_NUM_INSTANCES));

    STAGE_CONTROLLER#(t_NUM_INSTANCES, Bit#(0)) m <- mkStageController();

    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    
        m.ready(iid, (?));
    
    endmethod
    
    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
    
        match {.iid, .*} <- m.nextReadyInstance();
        
        return iid;
    
    endmethod

endmodule

typedef enum
{

    MC_idle,
    MC_running,
    MC_stepping

}
MC_STATE deriving (Eq, Bits);

// mkMultiplexController

// This acts like a Local Controller in that it sits on the ring and listens for commands.
// However it controls simulation serially by going through the instances in a round-robin fashion.
// The boolean it returns tells the model when they have gone through every available instance.

interface MULTIPLEX_CONTROLLER#(numeric type t_NUM_INSTANCES);
    
    method ActionValue#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Bool)) nextReadyInstance();
    
    method INSTANCE_ID#(t_NUM_INSTANCES) getActiveInstances();
    
    method Bool running();

endinterface

module [HASIM_MODULE] mkMultiplexController 
    // parameters:
    #(
        Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls
    )
    // interface:
        (MULTIPLEX_CONTROLLER#(t_NUM_INSTANCES));

    // Local-controller-like communication.
    Connection_Chain#(CONTROLLER_MSG) link_controllers <- mkConnection_Chain(`RINGID_CONTROLLER_MESSAGES);

    Reg#(MC_STATE) state <- mkReg(MC_idle);

    // Dynamic number of active instances.
    // Note: all code in this module must work when activeInstances is not a power of 2.
    Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) activeInstances <- mkReg(~0); // NOTE: Start at -1 for now. This means we assume at least one instance is active.
    
    // A counter of which virtual instance should be simulated next.
    Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) curInstance <- mkReg(0);
  

    // ******* Rules *******

    // dropDisabledInstance[1..N]
  
    // Rules to drop the initial tokens of disabled instances, so that they
    // are not simulated dynamically.
  
    for (Integer x = 0; x < valueof(t_NUM_INPORTS); x = x + 1)
    begin
    
        rule dropDisabledInstance (inctrls[x].nextReadyInstance() matches tagged Valid .iid &&&
                                   !(iid <= activeInstances) &&&
                                   state != MC_idle);
            inctrls[x].drop();
                    
        endrule
    
    end


    // ====================================================================
    //
    // Process controller commands and send responses.
    //
    // ====================================================================

    rule nextCommand (True);
        let newcmd <- link_controllers.receive_from_prev();
        CONTROLLER_MSG outcmd = newcmd;

        case (newcmd) matches
            tagged COM_RunProgram:
            begin
                state <= MC_running;
            end

            tagged COM_Synchronize:
            begin
                state <= MC_running;
            end

            tagged COM_SyncQuery .all_balanced:
            begin
                // This controller is always balanced and will never pull down
                // all_balanced.
                noAction;
            end

            tagged COM_Step:
            begin
                state <= MC_stepping;
            end

            // TODO: should this be COM_EnableInstance??
            tagged COM_EnableContext .iid:
            begin
                activeInstances <= activeInstances + 1;
            end

            // TODO: should this be COM_DisableInstance??
            tagged COM_DisableContext .iid:
            begin
                activeInstances <= activeInstances - 1;
            end
        endcase

        // Forward command around the ring
        link_controllers.send_to_next(outcmd);
    endrule


    // ******** Methods *******

    // ready

    method INSTANCE_ID#(t_NUM_INSTANCES) getActiveInstances();
    
        return activeInstances;
    
    endmethod
    
    method Bool running();
    
        return state != MC_idle;
    
    endmethod
    
    // nextReadyInstance
    
    // Return the next instance in a round-robin fashion.
    // Boolean indicates if we have simulated every possible context.
    
    method ActionValue#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Bool)) nextReadyInstance() if (state != MC_idle);

        let done = curInstance == activeInstances;
        
        curInstance <= (done) ? 0 : curInstance + 1;
        
        if (state == MC_stepping && done)
        begin

            state <= MC_idle;

        end
        
        return tuple2(curInstance, done);

    endmethod

endmodule

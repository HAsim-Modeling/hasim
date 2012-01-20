//
// Copyright (C) 2011 Intel Corporation
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

// STAGE_CONTROLLER

// A controller for an intermediate stage in a straight-line pipeline.
// Passes intermediate state, and handles multi-instancing.


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

    // Build a controller with a standard 2-entry FIFO
    let sc <- mkSizedStageController(2);
    return sc;

endmodule

module mkBufferedStageController 
    // interface:
        (STAGE_CONTROLLER#(t_NUM_INSTANCES, t_PIPE_STATE))
    provisos
        (Bits#(t_PIPE_STATE, t_PIPE_STATE_SZ));

    // Build a buffered controller default buffer size.  The size should be
    // chosen to cover the typical latency of functional partition and
    // scratchpad memory pipelines.  For now, we use the number of instances
    // since that is typically the maximum parallelism.
    //
    // A number of pipelines deadlock if they fill the memory request read
    // slots without consuming the results, because subsequent writes wind
    // up blocked.  Reducing buffering below the blocking threshold in
    // scratchpads and packed memories eliminates this problem.
    // For now, we limit buffers to 16, until deadlocks in the in-order
    // 64 CPU model with larger buffers are explained.
    //
    let sc <- mkSizedStageController(valueOf(TMin#(t_NUM_INSTANCES, 16)));
    return sc;

endmodule

module mkSizedStageController#(Integer size)
    // interface:
        (STAGE_CONTROLLER#(t_NUM_INSTANCES, t_PIPE_STATE))
    provisos
        (Bits#(t_PIPE_STATE, t_PIPE_STATE_SZ));

    //
    // Pick a FIFO implementation appropriate for the requested size.
    //
    FIFO#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_PIPE_STATE)) q;
    if (size == 1)
        q <- mkFIFO1();
    else if (size == 2)
        q <- mkFIFO();
    else
        q <- mkSizedFIFO(size);

    
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

// MULTIPLEX_CONTROLLER

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

    let m <- mkNamedMultiplexController("[no name]", inctrls);
    return m;

endmodule

module [HASIM_MODULE] mkNamedMultiplexController 
    // parameters:
    #(
        String name,
        Vector#(t_NUM_INPORTS, INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls
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

    // ====================================================================
    //
    // Process controller commands and send responses.
    //
    // ====================================================================

    rule nextCommand (True);
        let newcmd <- link_controllers.recvFromPrev();
        CONTROLLER_MSG outcmd = newcmd;

        case (newcmd) matches
            tagged COM_RunProgram:
            begin

                for (Integer x = 0; x < valueof(t_NUM_INPORTS); x = x + 1)
                begin
                
                    inctrls[x].setMaxRunningInstance(activeInstances);

                end

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
        link_controllers.sendToNext(outcmd);
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


// DEPENDENCE_CONTROLLER

// This is a small controller that can be used by more complex pipelines
// that have multiple dependences. Currently it does not pass intermediate
// state, but just tracks when the "producer" (earlier stage) and the
// "consumer" (later stage) may begin.

interface DEPENDENCE_CONTROLLER#(parameter numeric type t_NUM_INSTANCES);

    method Bool   producerCanStart();
    method Action producerStart();
    method Action producerDone();
    
    method Bool consumerCanStart();
    method Action consumerStart();
    method Action consumerDone();
    
    interface INSTANCE_CONTROL_IN#(t_NUM_INSTANCES) ctrl;

endinterface

module mkDependenceController
    // interface:
        (DEPENDENCE_CONTROLLER#(t_NUM_INSTANCES));

    COUNTER#(TLog#(TAdd#(1, t_NUM_INSTANCES))) producerCredits <- mkLCounter(fromInteger(valueof(t_NUM_INSTANCES)));
    COUNTER#(TLog#(TAdd#(1, t_NUM_INSTANCES))) consumerCredits <- mkLCounter(0);
    Reg#(Bool) initialized <- mkReg(False);
    
    method Bool   producerCanStart() = initialized && (producerCredits.value() != 0);
    method Action producerStart()    = producerCredits.down();
    method Action producerDone()     = consumerCredits.up();
    
    method Bool   consumerCanStart() = initialized && (consumerCredits.value() != 0);
    method Action consumerStart()    = consumerCredits.down();
    method Action consumerDone()     = producerCredits.up();

    interface INSTANCE_CONTROL_IN ctrl;

        method Bool empty() = producerCredits.value() == 0;
        method Bool balanced() = False;
        method Bool light() = False;

        method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
            return tagged Invalid;
        endmethod

        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);
            initialized <= True;
            Bit#(TLog#(TAdd#(1, t_NUM_INSTANCES))) tmp = zeroExtendNP(iid) + 1;
            producerCredits.setC(tmp);
        endmethod
    
    endinterface

endmodule


//
// The standard local controller permits an instance ID space to be larger
// than the CPU ID space known to the function model in order to support extra
// devices such as memory controllers.  This module maps control from a larger
// space to a smaller one.
//
module mkConvertControllerInstances_IN#(INSTANCE_CONTROL_IN#(t_NUM_INSTANCES_SRC) inctrl)
    // Interface:
    (INSTANCE_CONTROL_IN#(t_NUM_INSTANCES_DST));
     
    method Bool empty = inctrl.empty;
    method Bool balanced = inctrl.balanced;
    method Bool light = inctrl.light;

    method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES_DST)) nextReadyInstance;
        if (inctrl.nextReadyInstance matches tagged Valid .iid)
            return tagged Valid zeroExtendNP(iid);
        else
            return tagged Invalid;
    endmethod

    method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES_DST) iid);
        // The number of active instances in the controller space is larger
        // by a constant (the number of non-functional devices, assumed
        // always active).  Remove the always-active devices from the outbound
        // count.
        iid = iid - fromInteger(valueOf(TSub#(t_NUM_INSTANCES_DST,
                                              t_NUM_INSTANCES_SRC)));
        inctrl.setMaxRunningInstance(truncateNP(iid));
    endmethod

endmodule

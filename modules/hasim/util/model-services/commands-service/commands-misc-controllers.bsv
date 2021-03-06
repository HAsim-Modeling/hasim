//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

import DefaultValue::*;


// STAGE_CONTROLLER

// A controller for an intermediate stage in a straight-line pipeline.
// Passes intermediate state, and handles multi-instancing.


interface STAGE_CONTROLLER#(numeric type t_NUM_INSTANCES, type t_PIPE_STATE);

    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_PIPE_STATE st);
    
    method ActionValue#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_PIPE_STATE)) nextReadyInstance();
    method Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_PIPE_STATE) peekReadyInstance();

endinterface

interface STAGE_CONTROLLER_VOID#(numeric type t_NUM_INSTANCES);

    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    
    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
    method INSTANCE_ID#(t_NUM_INSTANCES) peekReadyInstance();

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

    //
    // Build a buffered controller with default buffer size.  The size should
    // be chosen to cover the typical latency of functional partition and
    // scratchpad memory pipelines, without being so large that it wastes
    // space.
    //
    let sc <- mkSizedStageController(min(`STAGE_CONTROLLER_BUF_MAX,
                                         valueOf(t_NUM_INSTANCES)));
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
    begin
        q <- mkFIFO1();
    end
    else if (size == 2)
    begin
        q <- mkFIFO();
    end
    else
    begin
        q <- mkSizedAutoMemFIFO(size, defaultValue);
    end

    
    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_PIPE_STATE st);
        q.enq(tuple2(iid, st));
    endmethod
    
    method ActionValue#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_PIPE_STATE)) nextReadyInstance();
        q.deq();
        return q.first();
    endmethod
    
    method Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_PIPE_STATE) peekReadyInstance();
        return q.first();
    endmethod
endmodule


module mkStageControllerVoid
    // interface:
        (STAGE_CONTROLLER_VOID#(t_NUM_INSTANCES));

    STAGE_CONTROLLER#(t_NUM_INSTANCES, Bit#(0)) _m <- mkStageController();

    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        _m.ready(iid, (?));
    endmethod
    
    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
        match {.iid, .*} <- _m.nextReadyInstance();
        return iid;
    endmethod
    
    method INSTANCE_ID#(t_NUM_INSTANCES) peekReadyInstance();
        match {.iid, .*} = _m.peekReadyInstance();
        return iid;
    endmethod
endmodule


module mkBufferedStageControllerVoid
    // interface:
        (STAGE_CONTROLLER_VOID#(t_NUM_INSTANCES));

    STAGE_CONTROLLER#(t_NUM_INSTANCES, Bit#(0)) _m <- mkBufferedStageController();

    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        _m.ready(iid, (?));
    endmethod
    
    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
        match {.iid, .*} <- _m.nextReadyInstance();
        return iid;
    endmethod

    method INSTANCE_ID#(t_NUM_INSTANCES) peekReadyInstance();
        match {.iid, .*} = _m.peekReadyInstance();
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
    method Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Bool) peekReadyInstance();
    
    method INSTANCE_ID#(t_NUM_INSTANCES) getActiveInstances();
    
    method Bool running();
endinterface

module [HASIM_MODULE] mkMultiplexController 
    // parameters:
    #(
        Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES)) inctrls,
        Vector#(t_NUM_UNPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES)) uncontrolled_ctrls,
        Vector#(t_NUM_OUTPORTS, INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES)) outctrls
    )
    // interface:
        (MULTIPLEX_CONTROLLER#(t_NUM_INSTANCES));

    let m <- mkNamedMultiplexController("[no name]", inctrls, uncontrolled_ctrls, outctrls);
    return m;

endmodule

module [HASIM_MODULE] mkNamedMultiplexController 
    // parameters:
    #(
        String name,
        Vector#(t_NUM_INPORTS, INSTANCE_CONTROL_IN#(t_NUM_INSTANCES)) inctrls,
        Vector#(t_NUM_UNPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES)) uncontrolled_ctrls,
        Vector#(t_NUM_OUTPORTS, INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES)) outctrls
    )
    // interface:
        (MULTIPLEX_CONTROLLER#(t_NUM_INSTANCES));

    // Emit file with data for generating a connection graph
    emitPortGraphFile(name, inctrls, uncontrolled_ctrls, outctrls);

    // Local-controller-like communication.
    CONNECTION_CHAIN#(CONTROLLER_MSG) link_controllers <-
        mkConnectionChain("CONTROLLER_MESSAGES");

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

                for (Integer x = 0; x < valueof(t_NUM_UNPORTS); x = x + 1)
                begin
                    uncontrolled_ctrls[x].setMaxRunningInstance(activeInstances);
                end

                for (Integer x = 0; x < valueof(t_NUM_OUTPORTS); x = x + 1)
                begin
                    outctrls[x].setMaxRunningInstance(activeInstances);
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

    method Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Bool) peekReadyInstance() if (state != MC_idle);
        let done = curInstance == activeInstances;
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

        method List#(PORT_INFO) portInfo() = List::nil;
    endinterface

endmodule


//
// mkConvertControllerAlwaysReady_OUT --
//   Some connected controllers are always ready because of the way they are
//   used.  (E.g. a multiplexed mesh network in which a model pipeline pass
//   both reads and writes all ports.)  For those, there is no point in
//   wasting area controlling them.  However, there is value in connecting
//   them so the name is reported connected to the controller.
//
//   This module eliminates the control structures but maintains the naming.
//
module mkConvertControllerAlwaysReady_OUT#(INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES) outctrl)
    // Interface:
    (INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES));
     
    method Bool full = False;
    method Bool balanced = True;
    method Bool heavy = False;

    method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid) =
        outctrl.setMaxRunningInstance(iid);

    method List#(String) portName() = outctrl.portName;
endmodule


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

endinterface

module mkDependenceController
    // interface:
        (DEPENDENCE_CONTROLLER#(t_NUM_INSTANCES));

    COUNTER#(TLog#(TAdd#(1, t_NUM_INSTANCES))) producerCredits <- mkLCounter(fromInteger(valueof(t_NUM_INSTANCES)));
    COUNTER#(TLog#(TAdd#(1, t_NUM_INSTANCES))) consumerCredits <- mkLCounter(0);
    
    method Bool   producerCanStart() = producerCredits.value() != 0;
    method Action producerStart()    = producerCredits.down();
    method Action producerDone()     = consumerCredits.up();
    
    method Bool   consumerCanStart() = consumerCredits.value() != 0;
    method Action consumerStart()    = consumerCredits.down();
    method Action consumerDone()     = producerCredits.up();

endmodule


typedef enum
{
    LCN_Idle,               // Waiting for a command
    LCN_Running,            // Running, allowing slip
    LCN_Synchronizing,      // Running, attempting to synchronize
    LCN_Stepping            // Run one modelCC
}
LCN_STATE
    deriving (Eq, Bits);

module [HASIM_MODULE] mkLocalControllerPlusN

    // parameters:
    #(
    Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls, 
    Vector#(t_NUM_INPORTS_N,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES_PLUS_N))  inctrlsN, 
    Vector#(t_NUM_OUTPORTS_N,  INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES_PLUS_N))  outctrlsN
    )
    // interface:
        (LOCAL_CONTROLLER#(t_NUM_INSTANCES_PLUS_N))
    provisos
        (Add#(t_N, t_NUM_INSTANCES, t_NUM_INSTANCES_PLUS_N));

    Reg#(LCN_STATE) state <- mkReg(LCN_Idle);
  
    // Counter of active instances. 
    // We start at -1, so we assume at least one instance is active.
    COUNTER#(INSTANCE_ID_BITS#(t_NUM_INSTANCES_PLUS_N)) maxActiveInstance <- mkLCounter(~0);
    // Vector of running instances
    Reg#(Vector#(t_NUM_INSTANCES_PLUS_N, Bool)) instanceRunning <- mkReg(replicate(False));
    // Track stepping state.
    Reg#(Vector#(t_NUM_INSTANCES_PLUS_N, Bool)) instanceStepped <- mkReg(replicate(False));

    // Signalled DONE to the software?
    Reg#(Bool) signalDone <- mkReg(False);

    Vector#(t_NUM_INSTANCES_PLUS_N, PulseWire)    startCycleW <- replicateM(mkPulseWire());
    Vector#(t_NUM_INSTANCES_PLUS_N, PulseWire)      endCycleW <- replicateM(mkPulseWire());
    Vector#(t_NUM_INSTANCES_PLUS_N, Wire#(Bit#(8))) pathDoneW <- replicateM(mkWire());
    
    
    // For now this local controller just goes round-robin over the instances.
    // This is guaranteed to be correct accross multiple modules.
    // The performance of this could be improved, but the interaction with time-multiplexed
    // ports needs to be worked out.
    
    COUNTER#(INSTANCE_ID_BITS#(t_NUM_INSTANCES_PLUS_N)) nextInstance <- mkLCounter(0);
    
    Connection_Chain#(CONTROLLER_MSG) link_controllers <- mkConnection_Chain(`RINGID_CONTROLLER_MESSAGES);

    function Bool allTrue(Vector#(k, Bool) v);
        return foldr(\&& , True, v);
    endfunction

    // Can this module read from this Port? Purposely ignore the non plus-N ports
    function Bool canReadFromN(INSTANCE_CONTROL_IN#(t_NUM_INSTANCES_PLUS_N) ctrl_in);
        return case (state)
                   LCN_Running:        return !ctrl_in.empty();
                   LCN_Stepping:       return !ctrl_in.empty();
                   LCN_Synchronizing:  return !ctrl_in.light();
                   default:            return False;
               endcase;
    endfunction

    // This function will determine the next instance in a non-round-robin manner when we're ready
    // to go that route. Currently this is unused.

    function Bool instanceReady(INSTANCE_ID#(t_NUM_INSTANCES_PLUS_N) iid);
        
        Bool canRead  = True;

        // Can we read/write all of the plus N ports? Disregard normal ports for this.
        for (Integer x = 0; x < valueOf(t_NUM_INPORTS_N); x = x + 1)
            canRead = canRead && canReadFromN(inctrlsN[x]);

        // An instance is ready to go only if it's been enabled.
        return !instanceRunning[iid] && canRead;

    endfunction

    function INSTANCE_ID#(t_NUM_INSTANCES_PLUS_N) nextReadyInstance();
        
        INSTANCE_ID#(t_NUM_INSTANCES_PLUS_N) res = 0;

        for (Integer x = 0; x < valueof(t_NUM_INSTANCES_PLUS_N); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) ? fromInteger(x) : res;
        end
        
        return res;
    
    endfunction

    function Bool someInstanceReady();
        
        Bool res = False;

        for (Integer x = 0; x < valueof(t_NUM_INSTANCES_PLUS_N); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) || res;
        end
        
        return res;
    
    endfunction



    function Bool balanced();
        Bool res = True;
        
        // Are the plus N ports all balanced? Disregard normal ports for this.
        for (Integer x = 0; x < valueOf(t_NUM_INPORTS_N); x = x + 1)
        begin
            res = res && inctrlsN[x].balanced();
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
        link_controllers.sendToNext(tagged COM_SyncQuery balanced());
    endrule

    rule newControlMsg (True);
        let cmd = newCtrlMsgQ.first();
        newCtrlMsgQ.deq();
        
        link_controllers.sendToNext(cmd);
    endrule

    (* descending_urgency = "checkBalance, newControlMsg, nextCommand" *)
    rule nextCommand (state != LCN_Stepping);
        let newcmd <- link_controllers.recvFromPrev();
        Maybe#(CONTROLLER_MSG) outcmd = tagged Valid newcmd;

        case (newcmd) matches
            tagged COM_RunProgram:
            begin
    
                for (Integer x = 0; x < valueof(t_NUM_INPORTS); x = x + 1)
                begin
                
                    // We know this truncation is safe since the button has only been pushed k times, not k+N.
                    inctrls[x].setMaxRunningInstance(truncateNP(maxActiveInstance.value()));

                end

                for (Integer x = 0; x < valueof(t_NUM_INPORTS_N); x = x + 1)
                begin
                
                    inctrlsN[x].setMaxRunningInstance(maxActiveInstance.value() + fromInteger(valueof(t_N)));

                end
                
                for (Integer x = 0; x < valueof(t_NUM_OUTPORTS_N); x = x + 1)
                begin
                
                    outctrlsN[x].setMaxRunningInstance(maxActiveInstance.value() + fromInteger(valueof(t_N)));

                end

                maxActiveInstance.setC(maxActiveInstance.value() + fromInteger(valueof(t_N)));

                state <= LCN_Running;

            end

            tagged COM_Synchronize:
            begin
                state <= LCN_Synchronizing;
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
                state <= LCN_Stepping;
                Vector#(t_NUM_INSTANCES_PLUS_N, Bool) instance_stepped = newVector();
                for (Integer x = 0; x < valueOf(t_NUM_INSTANCES_PLUS_N); x = x + 1)
                begin
                   instance_stepped[x] = False;
                end
                instanceStepped <= instance_stepped;
            end

            tagged COM_Pause .send_response:
            begin
                state <= LCN_Idle;
            end

            // TODO: should this be COM_EnableInstance??
            tagged COM_EnableContext .iid:
            begin
                maxActiveInstance.up();
            end

            // TODO: should this be COM_DisableInstance??
            tagged COM_DisableContext .iid:
            begin
                maxActiveInstance.down();
            end
        endcase

        // Forward command around the ring
        if (outcmd matches tagged Valid .cmd)
        begin
            link_controllers.sendToNext(cmd);
        end
    endrule

    rule updateRunning (True);
    
        Vector#(t_NUM_INSTANCES_PLUS_N, Bool) new_running = instanceRunning;

        for (Integer x = 0; x < valueOf(t_NUM_INSTANCES_PLUS_N); x = x + 1)
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
    Wire#(Maybe#(Bit#(INSTANCE_ID_BITS#(t_NUM_INSTANCES_PLUS_N)))) newModelCycleStarted <- mkDWire(tagged Invalid);

    (* descending_urgency = "updateStateForStepping, nextCommand" *)
    rule updateStateForStepping (state == LCN_Stepping &&&
                                 newModelCycleStarted matches tagged Valid .iid);
        instanceStepped[iid] <= True;
        if (iid == maxActiveInstance.value())
            state <= LCN_Idle;
    endrule

    //
    // ******** Methods *******


    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES_PLUS_N)) startModelCycle() if ((state != LCN_Idle) && instanceReady(nextInstance.value()));

        let next_iid = nextInstance.value();

        if (state == LCN_Stepping)
        begin
            newModelCycleStarted <= tagged Valid next_iid;
        end
        
        // checkInstanceSanity();
        
        startCycleW[next_iid].send();
        
        if (next_iid >= maxActiveInstance.value())
        begin
            nextInstance.setC(0);
        end
        else
        begin
            nextInstance.up();
        end

        return next_iid;

    endmethod

    method Action endModelCycle(INSTANCE_ID#(t_NUM_INSTANCES_PLUS_N) iid, Bit#(8) path);
    
        endCycleW[iid].send();
        pathDoneW[iid] <= path; // Put the path into the waveform.
    
    endmethod

    method Action instanceDone(INSTANCE_ID#(t_NUM_INSTANCES_PLUS_N) iid, Bool pf);
        // XXX this should be per-instance.  For now only allowed to fire once.
        if (! signalDone)
        begin
            newCtrlMsgQ.enq(tagged LC_DoneRunning pf);
            signalDone <= True;
        end
    endmethod
    
endmodule


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

    COUNTER#(TAdd#(1, t_NUM_INSTANCES)) producerCredits <- mkLCounter(fromInteger(valueof(t_NUM_INSTANCES)));
    COUNTER#(TAdd#(1, t_NUM_INSTANCES)) consumerCredits <- mkLCounter(0);
    
    method Bool   producerCanStart() = producerCredits.value() != 0;
    method Action producerStart()    = producerCredits.down();
    method Action producerDone()     = consumerCredits.up();
    
    method Bool   consumerCanStart() = consumerCredits.value() != 0;
    method Action consumerStart()    = consumerCredits.down();
    method Action consumerDone()     = producerCredits.up();

endmodule

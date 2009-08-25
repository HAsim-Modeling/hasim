
import Vector::*;
import FIFOF::*;


`include "asim/provides/pipetest_common.bsh"



typedef Bit#(TLog#(t_NUM_INSTANCES)) INSTANCE_ID#(type t_NUM_INSTANCES);

interface PORT_CONTROL;

    method Bool full();
    method Bool empty();

endinterface

function PORT_CONTROL fifofToPortControl(FIFOF#(a) q);

    return (interface PORT_CONTROL;
               method Bool full() = !q.notFull();
               method Bool empty() = !q.notEmpty();
            endinterface);

endfunction

interface PIPELINE_STAGE_CONTROLLER#(type t_NUM_INSTANCES);

    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid);

endinterface

module mkPipelineStageController#(Vector#(n, Vector#(ni, PORT_CONTROL)) inports, Vector#(m, Vector#(ni, PORT_CONTROL)) outports, Bool initRdy)
    //interface:
        (PIPELINE_STAGE_CONTROLLER#(ni));

    Vector#(ni, PulseWire)    startRunningW <- replicateM(mkPulseWire());
    Vector#(ni, PulseWire)      readyW <- replicateM(mkPulseWire());
    
    
    // Vector of ready instances.
    Reg#(Vector#(ni, Bool)) instanceReadies <- mkReg(replicate(initRdy));

    function Bool allTrue(Vector#(k, Bool) v);
        return foldr(\&& , True, v);
    endfunction

    // This function will determine the next instance in a non-round-robin manner when we're ready
    // to go that route. Currently this is unused.

    function Bool instanceReady(INSTANCE_ID#(ni) iid);
        
        Bool canRead  = True;
        Bool canWrite = True;

        // Can we read/write all of the ports?
        for (Integer x = 0; x < valueOf(n); x = x + 1)
            canRead = canRead && !inports[x][iid].empty();

        for (Integer x = 0; x < valueOf(m); x = x + 1)
            canWrite = canWrite && !outports[x][iid].full();

        // An instance is ready to go only if it's not currently running.
        return instanceReadies[iid] && canRead && canWrite;

    endfunction

    function Bool someInstanceReady();
        
        Bool res = False;

        for (Integer x = 0; x < valueof(ni); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) || res;
        end
        
        return res;
    
    endfunction


    rule updateReadies (True);
    
        Vector#(ni, Bool) new_readies = instanceReadies;

        for (Integer x = 0; x < valueOf(ni); x = x + 1)
        begin
            if (!instanceReadies[x] || startRunningW[x])
                new_readies[x] = readyW[x];
        end
        
        instanceReadies <= new_readies;
    
    endrule

    method ActionValue#(INSTANCE_ID#(ni)) nextReadyInstance() if (someInstanceReady());
    
        INSTANCE_ID#(ni) res = 0;

        for (Integer x = 0; x < valueof(ni); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) ? fromInteger(x) : res;
        end
        
        startRunningW[res].send();

        return res;

    endmethod

    method Action ready(INSTANCE_ID#(ni) iid);
    
        readyW[iid].send();
    
    endmethod
    
endmodule

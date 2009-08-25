

import Vector::*;
import FIFOF::*;


`include "asim/provides/pipetest_common.bsh"


// ========================================================================
//
// Implementation of the pipelines in which a single rule manages all
// pipelines for a given stage.
//
// ========================================================================

module mkPipeTest
    // interface:
    (PIPELINE_TEST#(n_STAGES, n_PARALLEL_PIPES));
    
    //
    // FIFOs
    //
    Vector#(n_STAGES, Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA))) fifos <-
        replicateM(replicateM(mkTestFIFOF()));

    Vector#(n_STAGES, PIPELINE_STAGE_CONTROLLER#(n_PARALLEL_PIPES)) stageCtrl = newVector();

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        
        let inqs  = map(fifofToPortControl, fifos[s]);
        let outqs = map(fifofToPortControl, fifos[s+1]);
        
        stageCtrl[s] <- mkPipelineStageController(cons(inqs, nil), cons(outqs, nil), s == 0);
    
        rule pipeStage (True); // Note: no explicit conditions necessary. Guarded by nextReadyInstance.
            let iid <- stageCtrl[s].nextReadyInstance();
            let d = fifos[s][iid].first();
            fifos[s][iid].deq();

            fifos[s + 1][iid].enq(d);
            stageCtrl[s + 1].ready(iid);
        endrule
    end

    //
    // Methods
    //

    Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA)) pipesLocal = newVector();

    for (Integer p = 0; p < valueOf(n_PARALLEL_PIPES); p = p + 1)
    begin
        pipesLocal[p] =
            interface FIFOF#(PIPE_TEST_DATA);
                method Action enq(PIPE_TEST_DATA d) = fifos[0][p].enq(d);

                method Action deq() = fifos[valueOf(n_STAGES) - 1][p].deq();
                method PIPE_TEST_DATA first() = fifos[valueOf(n_STAGES) - 1][p].first();

                method Bool notFull() = fifos[0][p].notFull();
                method Bool notEmpty() = fifos[valueOf(n_STAGES) - 1][p].notEmpty();

                method Action clear();
                    noAction;
                endmethod
            endinterface;
    end

    interface pipes = pipesLocal;
endmodule

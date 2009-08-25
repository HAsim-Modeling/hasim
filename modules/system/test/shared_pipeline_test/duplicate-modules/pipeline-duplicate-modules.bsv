
import Vector::*;
import FIFOF::*;


`include "asim/provides/pipetest_common.bsh"

// ========================================================================
//
// Implementation of the pipelines in which we just duplicate the pipeline.
//
// ========================================================================

module mkPipeTest
    // interface:
    (PIPELINE_TEST#(n_STAGES, n_PARALLEL_PIPES));
    
    //
    // FIFOs
    //
    Vector#(n_PARALLEL_PIPES, PIPELINE_TEST#(n_STAGES, 1)) dupedPipes <-
        replicateM(mkPipeTestSingle());

    //
    // Methods
    //

    Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA)) pipesLocal = newVector();

    for (Integer p = 0; p < valueOf(n_PARALLEL_PIPES); p = p + 1)
    begin
        pipesLocal[p] =
            interface FIFOF#(PIPE_TEST_DATA);
                method Action enq(PIPE_TEST_DATA d) = dupedPipes[p].pipes[0].enq(d);

                method Action deq() = dupedPipes[p].pipes[0].deq();
                method PIPE_TEST_DATA first() = dupedPipes[p].pipes[0].first();

                method Bool notFull() = dupedPipes[p].pipes[0].notFull();
                method Bool notEmpty() = dupedPipes[p].pipes[0].notEmpty();

                method Action clear();
                    noAction;
                endmethod
            endinterface;
    end

    interface pipes = pipesLocal;

endmodule


module mkPipeTestSingle (PIPELINE_TEST#(n_STAGES, 1));

    //
    // FIFOs
    //

    Vector#(n_STAGES, FIFOF#(PIPE_TEST_DATA)) fifos <- replicateM(mkTestFIFOF());

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        rule pipeStage (fifos[s].notEmpty() && fifos[s + 1].notFull());
            let d = fifos[s].first();
            fifos[s].deq();

            fifos[s + 1].enq(d);
        endrule
    end

    Vector#(1, FIFOF#(PIPE_TEST_DATA)) pipesLocal = newVector();

    //
    // Methods
    //

    pipesLocal[0] =
        interface FIFOF#(PIPE_TEST_DATA);
            method Action enq(PIPE_TEST_DATA d) = fifos[0].enq(d);

            method Action deq() = fifos[valueOf(n_STAGES) - 1].deq();
            method PIPE_TEST_DATA first() = fifos[valueOf(n_STAGES) - 1].first();

            method Bool notFull() = fifos[0].notFull();
            method Bool notEmpty() = fifos[valueOf(n_STAGES) - 1].notEmpty();

            method Action clear();
                noAction;
            endmethod
        endinterface;

    interface pipes = pipesLocal;

endmodule

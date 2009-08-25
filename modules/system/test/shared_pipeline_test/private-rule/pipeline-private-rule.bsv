
import Vector::*;
import FIFOF::*;


`include "asim/provides/pipetest_common.bsh"

// ========================================================================
//
// Implementation of the pipelines in which each pipeline in each stage has
// its own, private, rule.
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

    //
    // Parallel pipelines must be written round-robin.  These control registers
    // manage which of the parallel pipelines are read in each stage.
    //
    Vector#(n_STAGES, Reg#(Bit#(TLog#(n_PARALLEL_PIPES)))) curPipe <- replicateM(mkReg(0));

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        for (Integer p = 0; p < valueOf(n_PARALLEL_PIPES); p = p + 1)
        begin
            rule pipeStage (curPipe[s] == fromInteger(p) && fifos[s][p].notEmpty() && fifos[s + 1][p].notFull());
                let d = fifos[s][p].first();
                fifos[s][p].deq();

                fifos[s + 1][p].enq(d);
                curPipe[s] <= curPipe[s] + 1;
            endrule
        end
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

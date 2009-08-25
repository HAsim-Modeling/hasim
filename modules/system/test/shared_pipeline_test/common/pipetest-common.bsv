import FIFO::*;
import FIFOF::*;
import Vector::*;

`include "asim/provides/librl_bsv_base.bsh"

typedef Bit#(`PIPE_TEST_DATA_BITS) PIPE_TEST_DATA;
typedef Bit#(TLog#(`PIPE_TEST_NUM_PIPES)) PIPELINE_IDX;

function PIPELINE_IDX getPipeIdx(PIPE_TEST_DATA d);

    return truncateNP(d);

endfunction

interface PIPELINE_TEST#(numeric type n_STAGES, numeric type n_PARALLEL_PIPES);
    interface Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA)) pipes;
endinterface

module mkTestFIFOF (FIFOF#(t)) provisos (Bits#(t, t_SZ));

    FIFOF#(t) q <- (`PIPE_TEST_GUARDED_FIFOS != 0) ? mkFIFOF() : mkUGFIFOF();

    return q;

endmodule


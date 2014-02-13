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

import FIFO::*;
import GetPut::*;
import Connectable::*;

// Project imports

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"


// ========================================================================
//
//  Interface idioms
//  
// ========================================================================

//
// All functional interface connections have output buffering sufficient
// for avoiding deadlocks in standard multiplexed models.  The standard
// multiplexed controller allows at most one cycle to be active for each
// context.  One slot is thus sufficient for a machine that is single
// issue.
//

module [CONNECTED_MODULE] mkFUNCPInterfaceServer#(String connectionName)
    // Interface:
    (Connection_Server#(t_REQ, t_RSP))
    provisos (Bits#(t_REQ, t_REQ_SZ),
              Bits#(t_RSP, t_RSP_SZ));
     
    CONNECTION_SERVER#(t_REQ, t_RSP) con <- mkConnectionServer(connectionName);

    //
    // Debugging:  track the number of requests to a server that are in
    //             flight.  This is often a good way to debug deadlocks.
    //
    COUNTER#(8) nInFlight <- mkLCounter(0);

    DEBUG_SCAN_FIELD_LIST dbg_list = List::nil;
    dbg_list <- addDebugScanField(dbg_list, "Requests in flight", nInFlight.value);
    let dbgNode <- mkDebugScanNode("FUNCP REGMGR Service: " + connectionName, dbg_list);


    method Bool   reqNotEmpty() = con.reqNotEmpty();
    method t_REQ  getReq() = con.getReq();

    method Action deq();
        con.deq();
        nInFlight.up();
    endmethod


    method Action makeResp(t_RSP data);
        con.makeRsp(data);
        nInFlight.down();
    endmethod

    method Bool   respNotFull() = con.rspNotFull();
endmodule


// ========================================================================
//
//  Global types
//
// ========================================================================

// UP_TO_TWO

typedef union tagged
{
    a ONE;
    Tuple2#(a, a) TWO;
}
    UP_TO_TWO#(parameter type a)
        deriving (Eq, Bits);

function a getFirst(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return x;
        tagged TWO {.x1, .x2}: return x1;
    endcase

endfunction

function Bool hasSecond(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return False;
        tagged TWO {.x1, .x2}: return True;
    endcase

endfunction

function Maybe#(a) getSecond(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return tagged Invalid;
        tagged TWO {.x1, .x2}: return tagged Valid x2;
    endcase

endfunction

function a getSecondOfTwo(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return ?;
        tagged TWO {.x1, .x2}: return x2;
    endcase

endfunction


//
// Destination registers.  Architectural and physical registers are kept in
// separate lists because some slots get a physical register even when no
// architectural register is allocated.  (E.g. store data.)
//

typedef struct
{
    Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)) ar;
    ISA_INST_DSTS pr;
}
REGMGR_DST_REGS
    deriving (Bits, Eq);

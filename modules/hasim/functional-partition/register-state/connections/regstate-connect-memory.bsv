//
// Copyright (C) 2008 Intel Corporation
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

//
// Connection from the functional register state manager to functional memory.
//

// Library includes.

import FIFO::*;
import FIFOF::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
 
// Functional Partition includes.

`include "asim/provides/funcp_regstate_base_types.bsh"
`include "asim/provides/funcp_memstate_base_types.bsh"

//
// Each functional pipeline that accesses memory gets its own copy of the
// REGSTATE_MEMORY_QUEUE.  The code in this module manages the priority of
// the requests and routes them to the memory subsystem.  Responses are
// routed back through the appropriate subinterfaces.
//
interface REGSTATE_MEMORY_QUEUE;
    method Action makeReq(MEMSTATE_REQ req);

    method MEM_VALUE getResp();
    method Action deq();
endinterface

interface REGSTATE_MEMORY_CONNECTION;
    interface REGSTATE_MEMORY_QUEUE getInstructionQueue;
    interface REGSTATE_MEMORY_QUEUE doLoadsQueue;
    interface REGSTATE_MEMORY_QUEUE doStoresQueue;
    interface REGSTATE_MEMORY_QUEUE commitStoresQueue;
    interface REGSTATE_MEMORY_QUEUE exceptionQueue;
endinterface


module [HASIM_MODULE] mkFUNCP_Regstate_Connect_Memory
    // interface:
    (REGSTATE_MEMORY_CONNECTION);

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_CONN_LOGFILE_PREFIX + "_memory.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Client#(MEMSTATE_REQ, MEMSTATE_RESP) linkToMem <- mkConnection_Client("funcp_memstate");


    // ====================================================================
    //
    //   Local state
    //
    // ====================================================================

    // Individual incoming queues for each pipeline
    FIFO#(MEMSTATE_REQ) mqInstruction <- mkFIFO();
    FIFO#(MEMSTATE_REQ) mqDoLoads <- mkFIFO();
    FIFO#(MEMSTATE_REQ) mqDoStores <- mkFIFO();
    FIFO#(MEMSTATE_REQ) mqCommit <- mkFIFO();
    FIFO#(MEMSTATE_REQ) mqException <- mkFIFO();

    // Response queues for commit and exceptions
    FIFOF#(Bool) respCommit <- mkFIFOF();
    FIFOF#(Bool) respException <- mkFIFOF();

    FIFO#(FUNCP_MEMRESP_SCOREBOARD_ID) releaseStoresQ <- mkFIFO();

    // Response queues with values.  We want these queues large enough that
    // their sizes are too big to put in LUTs.  Once they are in BRAMs it
    // makes sense for them to use a good fraction of an 18kb BRAM slice.
    SCOREBOARD_FIFO#(MAX_FUNCP_INFLIGHT_MEMREFS, MEM_VALUE) respInstruction <- mkBRAMScoreboardFIFO();
    SCOREBOARD_FIFO#(MAX_FUNCP_INFLIGHT_MEMREFS, MEM_VALUE) respDoLoads <- mkBRAMScoreboardFIFO();
    SCOREBOARD_FIFO#(MAX_FUNCP_INFLIGHT_MEMREFS, MEM_VALUE) respDoStores <- mkBRAMScoreboardFIFO();

    
    function Action decorateAndQueueLoad(MEMSTATE_REQ req,
                                         FUNCP_REGSTATE_MEMPATH path,
                                         FUNCP_MEMRESP_SCOREBOARD_ID id);
    action
        if (req matches tagged REQ_LOAD .ld)
        begin
            let tagged_ld = ld;
            tagged_ld.memRefToken = memRefToken(path, id);
            linkToMem.makeReq(tagged REQ_LOAD tagged_ld);
        end
    endaction
    endfunction


    //
    // Process incoming requests, giving priority to later stages of the pipe.
    //
    
    //
    // The memory subsystem makes no response for exception and commit
    // requests.  The code here generates a response when the request
    // is processed to indicate the request now being ordered in the memory
    // subsystem.  This guarantees cross-cycle order is maintained.
    //

    rule handleReqException (True);
        linkToMem.makeReq(mqException.first());
        mqException.deq();
        respException.enq(?);
        debugLog.record($format("Exception REQ"));
    endrule

    rule handleReqCommit (True);
        linkToMem.makeReq(mqCommit.first());
        mqCommit.deq();
        respCommit.enq(?);
        debugLog.record($format("Commit Store REQ"));
    endrule
    

    //
    // The remaining requests get responses from the memory subsystem.
    // The memRefToken passed to the memory subsystem will be returned
    // along with the result and will be used to route the response to
    // the proper pipeline.
    //

    rule handleReqStore (True);
        let req = mqDoStores.first();
        mqDoStores.deq();

        // DoStores may request either a load or a store
        if (req matches tagged REQ_LOAD .ld)
        begin
            // Allocate a slot in the store response FIFO and decorate the
            // request with the details.
            let resp_id <- respDoStores.enq();
            decorateAndQueueLoad(req, FUNCP_REGSTATE_MEMPATH_DOSTORES_LOAD, resp_id);
            debugLog.record($format("Do Stores LOAD REQ, id=%0d", resp_id));
        end
        else
        begin
            // Store has no response from memory subsystem.  That it was queued
            // is sufficient to respond to the store pipeline.
            linkToMem.makeReq(req);

            let resp_id <- respDoStores.enq();
            debugLog.record($format("Do Stores STORE REQ, id=%0d", resp_id));

            // Can't update the output store FIFO this cycle.  Queue request.
            releaseStoresQ.enq(resp_id);
        end
    endrule

    rule handleReqLoad (True);
        let req = mqDoLoads.first();
        mqDoLoads.deq();

        let resp_id <- respDoLoads.enq();
        decorateAndQueueLoad(req, FUNCP_REGSTATE_MEMPATH_DOLOADS, resp_id);

        debugLog.record($format("Do Loads REQ, id=%0d", resp_id));
    endrule

    (* descending_urgency = "handleReqException, handleReqCommit, handleReqStore, handleReqLoad, handleReqInstruction" *)
    rule handleReqInstruction (True);
        let req = mqInstruction.first();
        mqInstruction.deq();

        let resp_id <- respInstruction.enq();
        decorateAndQueueLoad(req, FUNCP_REGSTATE_MEMPATH_GETINST, resp_id);

        debugLog.record($format("Instruction REQ, id=%0d", resp_id));
    endrule
    

    //
    // Process outgoing responses, routing them to the right FIFOs.  Writing
    // the values automatically enables the preallocated FIFO entries.
    //

    rule handleRespInstruction (linkToMem.getResp().memRefToken.memPath == FUNCP_REGSTATE_MEMPATH_GETINST);
        let v = linkToMem.getResp();
        linkToMem.deq();

        respInstruction.setValue(v.memRefToken.entryId, v.value);
        debugLog.record($format("Instruction RESP, id=%0d", v.memRefToken.entryId));
    endrule

    rule handleRespDoLoads (linkToMem.getResp().memRefToken.memPath == FUNCP_REGSTATE_MEMPATH_DOLOADS);
        let v = linkToMem.getResp();
        linkToMem.deq();

        respDoLoads.setValue(v.memRefToken.entryId, v.value);
        debugLog.record($format("Do Loads RESP, id=%0d", v.memRefToken.entryId));
    endrule

    rule handleRespDoStores_Load (linkToMem.getResp().memRefToken.memPath == FUNCP_REGSTATE_MEMPATH_DOSTORES_LOAD);
        let v = linkToMem.getResp();
        linkToMem.deq();

        respDoStores.setValue(v.memRefToken.entryId, v.value);
        debugLog.record($format("Do Stores LOAD RESP, id=%0d", v.memRefToken.entryId));
    endrule

    (* descending_urgency = "handleRespDoStores_Load, handleReleaseStores" *)
    rule handleReleaseStores (True);
        let id = releaseStoresQ.first();
        releaseStoresQ.deq();

        respDoStores.setValue(id, ?);
        debugLog.record($format("Do Stores STORE RESP, id=%0d", id));
    endrule

    
    //
    // Interfaces for getInstruction and doLoads are identical except
    // for the queues to which they push requests.
    //

    interface REGSTATE_MEMORY_QUEUE getInstructionQueue;
        method Action makeReq(MEMSTATE_REQ req);
            mqInstruction.enq(req);
        endmethod

        method MEM_VALUE getResp();
            return respInstruction.first();
        endmethod

        method Action deq();
            respInstruction.deq();
            debugLog.record($format("Instruction DEQ, id=%0d", respInstruction.deqEntryId()));
        endmethod
    endinterface

    interface REGSTATE_MEMORY_QUEUE doLoadsQueue;
        method Action makeReq(MEMSTATE_REQ req);
            mqDoLoads.enq(req);
        endmethod

        method MEM_VALUE getResp();
            return respDoLoads.first();
        endmethod

        method Action deq();
            respDoLoads.deq();
            debugLog.record($format("Do Loads DEQ, id=%0d", respDoLoads.deqEntryId()));
        endmethod
    endinterface

    //
    // DoStore pipe may request a load in the case of a read-modify-write.
    // The memory subsystem has no response for stores, so the response is
    // made up here.
    //
    interface REGSTATE_MEMORY_QUEUE doStoresQueue;
        method Action makeReq(MEMSTATE_REQ req);
            mqDoStores.enq(req);
        endmethod

        method MEM_VALUE getResp();
            return respDoStores.first();
        endmethod

        method Action deq();
            respDoStores.deq();
            debugLog.record($format("Do Stores DEQ, id=%0d", respDoStores.deqEntryId()));
        endmethod
    endinterface


    //
    // commitStores and the exception queue only need to know that their
    // requests have reached memory.  The response is generated inside
    // this module.
    //

    interface REGSTATE_MEMORY_QUEUE commitStoresQueue;
        method Action makeReq(MEMSTATE_REQ req);
            mqCommit.enq(req);
        endmethod

        method MEM_VALUE getResp() if (respCommit.notEmpty());
            return ?;
        endmethod

        method Action deq();
            respCommit.deq();
            debugLog.record($format("Commit Store DEQ"));
        endmethod
    endinterface

    interface REGSTATE_MEMORY_QUEUE exceptionQueue;
        method Action makeReq(MEMSTATE_REQ req);
            mqException.enq(req);
        endmethod

        method MEM_VALUE getResp() if (respException.notEmpty());
            return ?;
        endmethod

        method Action deq();
            respException.deq();
            debugLog.record($format("Exception DEQ"));
        endmethod
    endinterface

endmodule

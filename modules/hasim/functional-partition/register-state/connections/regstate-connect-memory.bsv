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
`include "asim/provides/hasim_modellib.bsh"
 
// Functional Partition includes.

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


//
// REGSTATE_MEMORY_PATH --
//     Internal enumeration to tag requests so responses get routed back
//     to the right place.
//
typedef enum
{
    REGSTATE_MEMORY_PATH_GETINST,
    REGSTATE_MEMORY_PATH_DOLOADS,
    // DoStores pipeline may request a load if doing a read-modify-write
    REGSTATE_MEMORY_PATH_DOSTORES_LOAD,
    REGSTATE_MEMORY_PATH_DOSTORES_STORE
}
REGSTATE_MEMORY_PATH
    deriving (Eq, Bits);


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

    Connection_Client#(MEMSTATE_REQ, MEM_VALUE) linkToMem <- mkConnection_Client("funcp_memstate");


    // ====================================================================
    //
    //   Local state
    //
    // ====================================================================

    // This global queue records where load responses should be sent.
    FIFO#(REGSTATE_MEMORY_PATH) memPathQ <- mkSizedFIFO(16);

    // Individual incoming queues for each pipeline
    FIFOF#(MEMSTATE_REQ) mqInstruction <- mkFIFOF();
    FIFOF#(MEMSTATE_REQ) mqDoLoads <- mkFIFOF();
    FIFOF#(MEMSTATE_REQ) mqDoStores <- mkFIFOF();
    FIFOF#(MEMSTATE_REQ) mqCommit <- mkFIFOF();
    FIFOF#(MEMSTATE_REQ) mqException <- mkFIFOF();

    // Response queues for commit and exceptions
    FIFOF#(Bool) respCommit <- mkFIFOF();
    FIFOF#(Bool) respException <- mkFIFOF();

    
    //
    // Process incoming requests, giving priority to later stages of the pipe.
    //
    rule pick_request (True);
        //
        // The first two incoming queues checked are for exceptions and
        // store commits.  The memory subsystem makes no response for these
        // requests.  The code here generates a response when the request
        // is processed to indicate the request now being ordered in the memory
        // subsystem.  This guarantees cross-cycle order is maintained.
        //
        if (mqException.notEmpty())
        begin
            linkToMem.makeReq(mqException.first());
            mqException.deq();
            respException.enq(?);
            debugLog.record($format("Exception REQ"));
        end
        else if (mqCommit.notEmpty())
        begin
            linkToMem.makeReq(mqCommit.first());
            mqCommit.deq();
            respCommit.enq(?);
            debugLog.record($format("Commit Store REQ"));
        end
        
        //
        // The remaining requests get responses from the memory subsystem.
        // The memPathQ FIFO will be used to route the reponses to the
        // corresponding request interface.
        //
        else if (mqDoStores.notEmpty())
        begin
            let req = mqDoStores.first();
            linkToMem.makeReq(req);
            mqDoStores.deq();
            // DoStores may request either a load or a store
            if (req matches tagged REQ_STORE .s)
            begin
                memPathQ.enq(REGSTATE_MEMORY_PATH_DOSTORES_STORE);
                debugLog.record($format("Do Stores STORE REQ"));
            end
            else
            begin
                memPathQ.enq(REGSTATE_MEMORY_PATH_DOSTORES_LOAD);
                debugLog.record($format("Do Stores LOAD REQ"));
            end
        end
        else if (mqDoLoads.notEmpty())
        begin
            linkToMem.makeReq(mqDoLoads.first());
            mqDoLoads.deq();
            memPathQ.enq(REGSTATE_MEMORY_PATH_DOLOADS);
            debugLog.record($format("Do Loads REQ"));
        end
        else if (mqInstruction.notEmpty())
        begin
            linkToMem.makeReq(mqInstruction.first());
            mqInstruction.deq();
            memPathQ.enq(REGSTATE_MEMORY_PATH_GETINST);
            debugLog.record($format("Instruction REQ"));
        end
    endrule

    
    //
    // Interfaces for getInstruction and doLoads are identical except
    // for the queues to which they push requests.
    //

    interface REGSTATE_MEMORY_QUEUE getInstructionQueue;
        method Action makeReq(MEMSTATE_REQ req);
            mqInstruction.enq(req);
        endmethod

        method MEM_VALUE getResp() if (memPathQ.first() == REGSTATE_MEMORY_PATH_GETINST);
            return linkToMem.getResp();
        endmethod

        method Action deq() if (memPathQ.first() == REGSTATE_MEMORY_PATH_GETINST);
            linkToMem.deq();
            memPathQ.deq();
            debugLog.record($format("Instruction DEQ"));
        endmethod
    endinterface

    interface REGSTATE_MEMORY_QUEUE doLoadsQueue;
        method Action makeReq(MEMSTATE_REQ req);
            mqDoLoads.enq(req);
        endmethod

        method MEM_VALUE getResp() if (memPathQ.first() == REGSTATE_MEMORY_PATH_DOLOADS);
            return linkToMem.getResp();
        endmethod

        method Action deq() if (memPathQ.first() == REGSTATE_MEMORY_PATH_DOLOADS);
            linkToMem.deq();
            memPathQ.deq();
            debugLog.record($format("Do Loads DEQ"));
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

        method MEM_VALUE getResp() if (memPathQ.first() == REGSTATE_MEMORY_PATH_DOSTORES_LOAD ||
                                       memPathQ.first() == REGSTATE_MEMORY_PATH_DOSTORES_STORE);

            if (memPathQ.first == REGSTATE_MEMORY_PATH_DOSTORES_LOAD)
                return linkToMem.getResp();
            else
                return ?;

        endmethod

        method Action deq() if (memPathQ.first() == REGSTATE_MEMORY_PATH_DOSTORES_LOAD ||
                                memPathQ.first() == REGSTATE_MEMORY_PATH_DOSTORES_STORE);

            if (memPathQ.first == REGSTATE_MEMORY_PATH_DOSTORES_LOAD)
            begin
                linkToMem.deq();
                debugLog.record($format("Do Stores LOAD DEQ"));
            end
            else
            begin
                debugLog.record($format("Do Stores STORE DEQ"));
            end

            memPathQ.deq();

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

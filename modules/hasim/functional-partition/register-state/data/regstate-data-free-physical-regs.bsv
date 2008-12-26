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
// The freelist of registers, which can stay allocated indefinitely.
// The list itself is stored in a block ram.
//

// Library imports

import FIFO::*;
import SpecialFIFOs::*;
import Vector::*;


// Project foundation imports

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/provides/hasim_isa.bsh"

// Dictionary includes
`include "asim/dict/ASSERTIONS_REGSTATE_FREELIST.bsh"

// FUNCP_FREELIST

// The interface to the freelist is request/response because of the block ram.

typedef Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) FUNCP_FREELIST_RESP_VEC;

interface FUNCP_FREELIST;
  
    // Request a new register.
    method Action forwardReqMask(Vector#(ISA_MAX_DSTS, Bool) newReqMask);
    // The responses come back in order.
    method ActionValue#(FUNCP_FREELIST_RESP_VEC) forwardResp();
    // Undo the last allocation.
    method Action back();
    // Go back to a specific point (from a snapshot).
    method Action backTo(FUNCP_PHYSICAL_REG_INDEX r);
    // Get the current location (to record it in a snapshot).
    method FUNCP_PHYSICAL_REG_INDEX current();
    // Put a register back onto the freelist.
    method Action free(FUNCP_PHYSICAL_REG_INDEX r);
  
endinterface


// ========================================================================
//
//  Internal types
//
// ========================================================================

typedef struct
{
    // NULL request (no registers) gets special path
    Bool nullReq;

    Bool done;
    ISA_DST_INDEX idx;
    FUNCP_PHYSICAL_REG_INDEX freeListPos;
}
FUNCP_FREELIST_REQ_Q
    deriving (Eq, Bits);


// mkFUNCP_Freelist

// An implementation of the freelist which uses block RAM to store everything.

module [HASIM_MODULE] mkFUNCP_Freelist#(String debugLogPrefix)
    //interface:
                (FUNCP_FREELIST);

    // ******* Debuging State *******

    DEBUG_FILE debugLog <- mkDebugFile(debugLogPrefix + "_freelist.out");

    // ***** Local Functions ***** //
    
    // The initial map is that every architectural register is mapped to 
    // the corresponding physical register. IE 1 == 1, 2 == 2, etc.
    
    function FUNCP_PHYSICAL_REG_INDEX initialMapping(FUNCP_PHYSICAL_REG_INDEX idx);
    
        return idx;
    
    endfunction

    // ***** Local State ***** //

    // The maximum achitectural register.
    ISA_REG_INDEX maxR = maxBound;

    // The architectural registers begin allocated, so the freelist pointer starts at
    // one position beyond that.
    FUNCP_PHYSICAL_REG_INDEX initFL = zeroExtend(pack(maxR)) + 1;

    // The maximum number of physical registers.
    FUNCP_PHYSICAL_REG_INDEX maxFL = maxBound;

    // The actual freelist
    BRAM#(FUNCP_PHYSICAL_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX) fl <- mkBRAMInitializedWith(initialMapping);

    // The read pointer is the next register to allocate.
    COUNTER#(FUNCP_PHYSICAL_REG_INDEX_SIZE) flRead   <- mkLCounter(pack(initFL));

    // The write pointer is the next register to overwrite.
    COUNTER#(FUNCP_PHYSICAL_REG_INDEX_SIZE) flWrite  <- mkLCounter(0); 

    // We are empty if the write equals the read.
    Bool empty = flRead.value() == flWrite.value();

    // We are out of physical registers when the pointers overlap.
    Bool full = flRead.value() + 1 == flWrite.value();

    // Mask of response vector indices still needing a register
    Reg#(Maybe#(Vector#(ISA_MAX_DSTS, Bool))) reqMask <- mkReg(tagged Invalid);
    
    // Aggregated response vector
    Reg#(FUNCP_FREELIST_RESP_VEC) reqPhysRegs <- mkReg(replicate(tagged Invalid));

    // Queues
    FIFO#(FUNCP_FREELIST_REQ_Q) reqQ <- mkFIFO();
    FIFO#(FUNCP_FREELIST_RESP_VEC) respQ <- mkBypassFIFO();


    // ***** Assertion Checkers *****/

    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_REGSTATE_FREELIST__BASE);
    ASSERTION assertEnoughPRegs <- mkAssertionChecker(`ASSERTIONS_REGSTATE_FREELIST_OUT_OF_PREGS, ASSERT_ERROR, assertNode);
    ASSERTION assertAtLeastOneAllocatedRegister <- mkAssertionChecker(`ASSERTIONS_REGSTATE_FREELIST_ILLEGAL_BACKUP, ASSERT_ERROR, assertNode);


    // ***** Rules *****/

    //
    // handleReqs --
    //     Handle the next request for a physical register and load the
    //     next register from BRAM.
    //
    rule handleReqs (reqMask matches tagged Valid .req_mask);
        if (findElem(True, req_mask) matches tagged Valid .x)
        begin
            //
            // Request wants one or more registers.
            //
            ISA_DST_INDEX idx = pack(x);

            // Update request mask
            let new_mask = req_mask;
            new_mask[idx] = False;

            // Assert that we're not out of physical registers.
            assertEnoughPRegs(!full);

            // Read the next entry and update pointer.
            fl.readReq(flRead.value());
            flRead.up();

            let done = (pack(new_mask) == 0);
            reqQ.enq(FUNCP_FREELIST_REQ_Q { nullReq: False,
                                            done: done,
                                            idx: idx,
                                            freeListPos: flRead.value() });

            if (done)
                reqMask <= tagged Invalid;
            else
                reqMask <= tagged Valid new_mask;
        end
        else
        begin
            //
            // NULL request
            //
            reqQ.enq(FUNCP_FREELIST_REQ_Q { nullReq: True,
                                            done: ?,
                                            idx: ?,
                                            freeListPos: ? });

            reqMask <= tagged Invalid;
        end
    endrule


    //
    // updateReqVec --
    //     Receive next PR requested by handleReqs and update the response vector.
    //
    rule updateReqVec (reqQ.first().nullReq == False);
        let req = reqQ.first();
        let idx = req.idx;
        let pos = req.freeListPos;
        let done = req.done;
        reqQ.deq();
        
        let pr <- fl.readRsp();

        // Log it.
        debugLog.record($format("FREELIST: Allocating PR%0d from position %0d", pr, pos));

        let new_reg_vec = reqPhysRegs;
        new_reg_vec[idx] = tagged Valid pr;
        
        if (! done)
        begin
            // Not done.  Accumulate response.
            reqPhysRegs <= new_reg_vec;
        end
        else
        begin
            // Ready to respond
            respQ.enq(new_reg_vec); 
            reqPhysRegs <= replicate(tagged Invalid);
        end
    endrule


    //
    // updateReqNull --
    //     Special path for empty requests.
    //
    rule updateReqNull (reqQ.first().nullReq == True);
        reqQ.deq();

        debugLog.record($format("FREELIST: Queued NULL response"));

        respQ.enq(replicate(tagged Invalid)); 
    endrule


    // ***** Methods *****/

    // When:   Any time.
    // Effect: Look up the next physical register in the block ram.
    //         If we are out of physical registers a simulator exception occurs.

    method Action forwardReqMask(Vector#(ISA_MAX_DSTS, Bool) newReqMask) if (! isValid(reqMask));

        debugLog.record($format("FREELIST: Incoming request mask %b", newReqMask));

        reqMask <= tagged Valid newReqMask;

    endmethod

    // forwardResp

    // When:   Any time.
    // Effect: Return the result vector to the requestor.

    method ActionValue#(FUNCP_FREELIST_RESP_VEC) forwardResp();

        let rsp = respQ.first();
        respQ.deq();

        debugLog.record($format("FREELIST: Forwarding response"));

        // Return the response to the requestor.
        return rsp;

    endmethod

    // free

    // When:   Any time.
    // Effect: Add register r back to the freelist.

    method Action free(FUNCP_PHYSICAL_REG_INDEX r);

        // Add it back to the freelist.
        fl.write(flWrite.value(), r);

        // Update the write pointer.
        flWrite.up();

        // Log it.
        debugLog.record($format("FREELIST: Freeing PR%0d onto position ", r, flWrite.value()));

    endmethod

    // back

    // When:   Any time.
    // Effect: Undo the last allocation.

    method Action back();

        // If the freelist is empty this is an exception.
        assertAtLeastOneAllocatedRegister(!empty);

        // Update the pointer.
        flRead.down();

    endmethod

    // backTo

    // When:   When there are no inflight requests.
    // Effect: Reset the pointer to the given value.

    method Action backTo(FUNCP_PHYSICAL_REG_INDEX r);

        // Log it.
        debugLog.record($format("FREELIST: Going back to position %0d (Current read: %0d, Current write: %0d)", r, flRead.value(), flWrite.value()));

        let rd = flRead.value();
        let wr = flWrite.value() - 1;

        // Check for errors.
        if(rd > wr && r < wr || rd < wr && r < wr && r > rd)
        begin
            debugLog.record($format("ERROR: Backed up the freelist too far! (r = %0d, flRead = %0d, flWrite = %0d)", r, flRead.value(), flWrite.value()));
            $display("ERROR: Backed up the freelist too far! (r = %0d)", r);
        end

        // Update the pointer.
        flRead.setC(r);

    endmethod
  
    // current

    // When:   Any time.
    // Get the current pointer value (for snapshots).

    method FUNCP_PHYSICAL_REG_INDEX current();

        return flRead.value();

    endmethod

endmodule

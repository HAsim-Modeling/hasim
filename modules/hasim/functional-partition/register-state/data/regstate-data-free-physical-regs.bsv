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
`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/hasim_isa.bsh"

// Dictionary includes
`include "asim/dict/ASSERTIONS_REGSTATE_FREELIST.bsh"

// FUNCP_FREELIST

// The interface to the freelist is request/response because of the block ram.

typedef Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) FUNCP_FREELIST_RESP_VEC;

interface FUNCP_FREELIST;
  
    // Request a new register.
    method Action allocateRegs(Vector#(ISA_MAX_DSTS, Bool) newReqMask);
    // The responses come back in order.
    method ActionValue#(FUNCP_FREELIST_RESP_VEC) allocateRsp();
    // Put a register back onto the freelist.
    method Action freeRegs(ISA_INST_DSTS r);
  
endinterface


// ========================================================================
//
//  Internal types
//
// ========================================================================

typedef struct
{
    Bool done;
    ISA_DST_INDEX idx;
}
FUNCP_FREELIST_REQ_Q
    deriving (Eq, Bits);


// mkFUNCP_Freelist

// An implementation of the freelist which uses block RAM to store everything.

module [HASIM_MODULE] mkFUNCP_Freelist#(String debugLogPrefix)
    //interface:
                (FUNCP_FREELIST);

    // ******* Debuging State *******

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_DATA_LOGFILE_PREFIX + "_freelist.out");

    // ***** Local Functions ***** //
    
    // The initial map is that every architectural register is mapped to 
    // the corresponding physical register. IE 1 == 1, 2 == 2, etc.
    
    function FUNCP_PHYSICAL_REG_INDEX initialMapping(FUNCP_PHYSICAL_REG_INDEX idx);
    
        return idx;
    
    endfunction

    // ***** Local State ***** //

    // The maximum achitectural register.
    ISA_REG_INDEX maxAR = maxBound;

    // Each context gets maxAR physical registers.
    FUNCP_PHYSICAL_REG_INDEX maxInitPR = (1 + zeroExtend(pack(maxAR))) * fromInteger(valueof(NUM_CONTEXTS)) - 1;

    // The architectural registers begin allocated, so the freelist pointer starts at
    // one position beyond that.
    FUNCP_PHYSICAL_REG_INDEX initFL = maxInitPR + 1;

    // The maximum number of physical registers.
    FUNCP_PHYSICAL_REG_INDEX maxFL = maxBound;

    // The actual freelist
    BRAM#(FUNCP_PHYSICAL_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX) fl <- mkBRAMInitializedWith(initialMapping);

    // The read pointer is the next register to allocate.
    COUNTER#(`FUNCP_PHYSICAL_REG_INDEX_BITS) flRead   <- mkLCounter(pack(initFL));

    // The write pointer is the next register to overwrite.
    COUNTER#(`FUNCP_PHYSICAL_REG_INDEX_BITS) flWrite  <- mkLCounter(0); 

    // We are empty if the write equals the read.
    Bool empty = flRead.value() == flWrite.value();

    // We are out of physical registers when the pointers overlap.
    Bool full = flRead.value() + 1 == flWrite.value();

    // Mask of response vector indices still needing a register
    Reg#(Vector#(ISA_MAX_DSTS, Bool)) allocateMask <- mkReg(replicate(False));
    
    // We are allocating if any of the allocate mask is true.
    Bool allocatingRegs = pack(allocateMask) != 0;
    
    // Aggregated response vector
    Reg#(FUNCP_FREELIST_RESP_VEC) reqPhysRegs <- mkReg(replicate(tagged Invalid));

    // Mask of registers to free.
    Reg#(ISA_INST_DSTS) regsToFree <- mkReg(replicate(tagged Invalid));
    
    // We are freeing if any of the registers are Valid.
    Bool freeingRegs = any(isValid, regsToFree);

    // Queues
    FIFO#(Maybe#(FUNCP_FREELIST_REQ_Q)) reqQ <- mkFIFO();
    FIFO#(FUNCP_FREELIST_RESP_VEC) respQ <- mkBypassFIFO();


    // ***** Assertion Checkers ***** //

    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_REGSTATE_FREELIST__BASE);
    ASSERTION assertEnoughPRegs <- mkAssertionChecker(`ASSERTIONS_REGSTATE_FREELIST_OUT_OF_PREGS, ASSERT_ERROR, assertNode);
    ASSERTION assertRequestNotActive <- mkAssertionChecker(`ASSERTIONS_REGSTATE_FREELIST_REQUEST_NOT_ACTIVE, ASSERT_ERROR, assertNode);

    // ***** Helper Functions ***** //

    // allocateNextReg
    
    //     Handle the next request for a physical register and load the
    //     next register from BRAM.
    
    function Action allocateNextReg(Vector#(ISA_MAX_DSTS, Bool) req_mask);
    action
    
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

            let done = pack(new_mask) == 0;
            reqQ.enq(tagged Valid FUNCP_FREELIST_REQ_Q 
                {
                    done: done,
                    idx: idx
                });
            
            allocateMask <= new_mask;

        end

    endaction
    endfunction

    // freeNextReg
    
    // Put the next register in the request back on the freelist.

    function Action freeNextReg(ISA_INST_DSTS rs);
    action
        
        // Lookup the next reg to free.
        if (findIndex(isValid, rs) matches tagged Valid .k)
        begin
        
            // Get the reg from the vector. (We know it's valid.)
            let r = validValue(rs[k]);
        
            // Add it back to the freelist.
            fl.write(flWrite.value(), r);

            // Update the write pointer.
            flWrite.up();

            // Log it.
            debugLog.record($format("FREELIST: Freeing PR%0d onto position ", r, flWrite.value()));
            
            // Record what work is left (if any).
            regsToFree <= update(rs, k, tagged Invalid);

        end
    endaction
    endfunction


    // ***** Rules *****/

    // allocateReg
    
    // When: We have to allocate more registers.
    // Effect: Request the next data from the block RAM.

    rule allocateReg (allocatingRegs);
    
        allocateNextReg(allocateMask);

    endrule

    // freeReg
    
    // When: We have to free more registers.
    // Effect: Write the next register back into the block RAM.


    rule freeReg (freeingRegs);
    
        freeNextReg(regsToFree);
    
    endrule

    //
    // updateReqVec --
    //     Receive next PR requested by handleReqs and update the response vector.
    //
    rule updateReqVec (reqQ.first() matches tagged Valid .req);
        let idx = req.idx;
        let done = req.done;
        reqQ.deq();
        
        let pr <- fl.readRsp();

        // Log it.
        debugLog.record($format("FREELIST: Allocating PR%0d.", pr));

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
    rule updateReqNull (reqQ.first() matches tagged Invalid);
        reqQ.deq();

        debugLog.record($format("FREELIST: Queued NULL response"));

        assertRequestNotActive(!any(isValid, reqPhysRegs));

        respQ.enq(replicate(tagged Invalid)); 
    endrule


    // ***** Methods *****/

    // allocateRegs

    // When:   Any time when we're not allocating a multi-register request.
    // Effect: Look up the next physical register in the block ram.
    //         If we are out of physical registers a simulator exception occurs.

    method Action allocateRegs(Vector#(ISA_MAX_DSTS, Bool) newReqMask) if (!allocatingRegs);

        debugLog.record($format("FREELIST: Incoming request mask %b", newReqMask));

        if (findElem(True, newReqMask) matches tagged Valid .x)
        begin
            allocateNextReg(newReqMask);
        end
        else
        begin
            //
            // NULL request
            //
            reqQ.enq(tagged Invalid);

        end
    endmethod

    // allocateRsp

    // When:   Any time.
    // Effect: Return the result vector to the requestor.

    method ActionValue#(FUNCP_FREELIST_RESP_VEC) allocateRsp();

        let rsp = respQ.first();
        respQ.deq();

        debugLog.record($format("FREELIST: Forwarding response"));

        // Return the response to the requestor.
        return rsp;

    endmethod

    // freeRegs

    // When:   Any time when we're not freeing multiple registers.
    // Effect: Add the registers back to the freelist.

    method Action freeRegs(ISA_INST_DSTS rs) if (!freeingRegs);

         freeNextReg(rs);

    endmethod

endmodule

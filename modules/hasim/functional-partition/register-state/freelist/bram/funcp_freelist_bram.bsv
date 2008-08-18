// funcp_freelist_bram

// The freelist of registers, which can stay allocated indefinitely.
// The list itself is stored in a block ram.

// Library imports

import Counter::*;
import RWire::*;

// Project foundation imports

`include "hasim_common.bsh"
`include "fpga_components.bsh"

`include "hasim_isa.bsh"

// Dictionary includes
`include "asim/dict/ASSERTIONS_FREELIST.bsh"

// FUNCP_FREELIST

// The interface to the freelist is request/response because of the block ram.

interface FUNCP_FREELIST;
  
  // Request a new register.
  method Action forwardReq();
  // The responses come back in order.
  method ActionValue#(FUNCP_PHYSICAL_REG_INDEX) forwardResp();
  // Undo the last allocation.
  method Action back();
  // Go back to a specific point (from a snapshot).
  method Action backTo(FUNCP_PHYSICAL_REG_INDEX r);
  // Get the current location (to record it in a snapshot).
  method FUNCP_PHYSICAL_REG_INDEX current();
  // Put a register back onto the freelist.
  method Action free(FUNCP_PHYSICAL_REG_INDEX r);
  
endinterface

// mkFUNCP_Freelist

// An implementation of the freelist which uses block RAM to store everything.

module [HASIM_MODULE] mkFUNCP_Freelist#(DEBUG_FILE debugLog)
    //interface:
                (FUNCP_FREELIST);

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
    Counter#(FUNCP_PHYSICAL_REG_INDEX_SIZE) flRead   <- mkCounter(pack(initFL));

    // The write pointer is the next register to overwrite.
    Counter#(FUNCP_PHYSICAL_REG_INDEX_SIZE) flWrite  <- mkCounter(0); 

    // We are empty if the write equals the read.
    Bool empty = flRead.value() == flWrite.value();

    // We are out of physical registers when the pointers overlap.
    Bool full = flRead.value() + 1 == flWrite.value();

    // ***** Assertion Checkers *****/

    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_FREELIST__BASE);
    ASSERTION assertEnoughPRegs <- mkAssertionChecker(`ASSERTIONS_FREELIST_OUT_OF_PREGS, ASSERT_ERROR, assertNode);
    ASSERTION assertAtLeastOneAllocatedRegister <- mkAssertionChecker(`ASSERTIONS_FREELIST_ILLEGAL_BACKUP, ASSERT_ERROR, assertNode);


    // When:   Any time.
    // Effect: Look up the next physical register in the block ram.
    //         If we are out of physical registers a simulator exception occurs.

    method Action forwardReq();

        // Assert that we're not out of physical registers.
        assertEnoughPRegs(!full);

        // Log it.
        debugLog.record($format("FREELIST: Requesting %0d", flRead.value()));

        // Read the next entry.
        fl.readReq(flRead.value());

        // Update the pointer.
        flRead.up();

    endmethod

    // forwardResp

    // When:   Any time.
    // Effect: Return the result from BRAM to the requestor.

    method ActionValue#(FUNCP_PHYSICAL_REG_INDEX) forwardResp();

        // Get the response from BRAM.
        let rsp <- fl.readRsp();

        // Log it.
        debugLog.record($format("FREELIST: Allocating PR%0d from position %0d", rsp, flRead.value()));

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
        debugLog.record($format("FREELIST: Freeing PR%0d onto position", r, flWrite.value()));

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
        debugLog.record($format("FREELIST: Going back to PR%0d (Current read: %0d, Current write: %0d)", r, flRead.value(), flWrite.value()));

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

// funcp_freelist_bram

// The freelist of registers, which can stay allocated indefinitely.
// The list itself is stored in a block ram.

// Library imports

import Counter::*;

// Project foundation imports

`include "hasim_common.bsh"
`include "fpga_components.bsh"

`include "hasim_isa.bsh"

// Dictionary includes
`include "asim/dict/STREAMS_ASSERTS_FREELIST.bsh"

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
  method PRName current();
  // Put a register back onto the freelist.
  method Action free(FUNCP_PHYSICAL_REG_INDEX r);
  
endinterface

// mkFUNCP_Freelist

// An implementation of the freelist which uses block RAM to store everything.

module [HASim_Module] mkFUNCP_Freelist#(File debug_log, Tick fpga_cc)
    //interface:
                (FUNCP_FREELIST)
    provisos
             (Bits#(FUNCP_PHYSICAL_REG_INDEX, prname_SZ)); // Physical register index size

    // ***** Local State ***** //

    // The maximum achitectural register.
    ISA_REG_INDEX maxR = maxBound;

    // The architectural registers begin allocated, so the freelist pointer starts at
    // one position beyond that.
    Bit#(prname_SZ) minInitFL_bits = zeroExtend(pack(maxR)) + 1;
    PRName initFL = unpack(minInitFL_bits);

    // The maximum number of physical registers.
    PRName maxFL = maxBound;

    // Register to track if we're initializing.
    Reg#(Bool) initializing <- mkReg(True);

    // The actual freelist
    BRAM#(FUNCP_PHYSICAL_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX) fl <- mkBRAM_Full();

    // The read pointer is the next register to allocate.
    Reg#(FUNCP_PHYSICAL_REG_INDEX) fl_read   <- mkReg(initFL);

    // The write pointer is the next register to overwrite.
    Reg#(FUNCP_PHYSICAL_REG_INDEX) fl_write  <- mkReg(0); 

    // The number of requests in flight is used to make sure we do not rewind in an unsure state.
    Counter#(2)                         req_count <- mkCounter(0);

    // We are empty if the write equals the read.
    Bool empty = fl_read == fl_write;

    // We are out of physical registers when the pointers overlap.
    Bool full = fl_read + 1 == fl_write;

    // ***** Assertion Checkers *****/

    Assertion assert_enough_pregs <- mkAssertionChecker(`STREAMS_ASSERTS_FREELIST_OUT_OF_PREGS, ASSERT_ERROR);
    Assertion assert_at_least_one_allocated_register <- mkAssertionChecker(`STREAMS_ASSERTS_FREELIST_ILLEGAL_BACKUP, ASSERT_ERROR);

    // initialize

    // When:   At the beginning of time.
    // Effect: Put every architectural register onto the freelist and update the pointers to match.

    rule initialize (initializing);

        // Add architectural register X to the freelist.
        fl.write(fl_write, fl_write);

        // X = X + 1.
        fl_write <= fl_write + 1;

        // done initializing if X == maxFL.
        if (fl_write == maxFL)
          initializing <= False;

    endrule

    // forwardReq

    // When:   Any time.
    // Effect: Look up the next physical register in the block ram.
    //         If we are out of physical registers a simulator exception occurs.

    method Action forwardReq() if (!initializing);

        // Assert that we're not out of physical registers.
        assert_enough_pregs(!full);

        // Log it.
        $fdisplay(debug_log, "[%d]: FREELIST: Requesting %0d", fpga_cc, fl_read);

        // Read the next entry.
        fl.read_req(fl_read);

        // Update the pointer.
        fl_read <= fl_read + 1;

        // Update the number of in-flight requests.
        req_count.up();

    endmethod

    // forwardResp

    // When:   Any time.
    // Effect: Return the result from BRAM to the requestor.

    method ActionValue#(PRName) forwardResp() if (!initializing);

        // Get the response from BRAM.
        let rsp <- fl.read_resp();

        // Log it.
        $fdisplay(debug_log, "[%d]: FREELIST: Allocating PR%0d %0d", fpga_cc, rsp, fl_read);

        // Update the number of in-flight requests.
        req_count.down();

        // Return the response to the requestor.
        return rsp;

    endmethod

    // free

    // When:   Any time.
    // Effect: Add register r back to the freelist.

    method Action free(FUNCP_PHYSICAL_REG_INDEX r) if (!initializing);

        // Add it back to the freelist.
        fl.write(fl_write, r);
        // Update the write pointer.
        fl_write <= fl_write + 1;

        // Log it.
        $fdisplay(debug_log, "[%d]: FREELIST: Freeing PR%0d %0d", fpga_cc, r, fl_write + 1);

    endmethod

    // back

    // When:   Any time.
    // Effect: Undo the last allocation.

    method Action back() if (!initializing);

        // If the freelist is empty this is an exception.
        assert_at_least_one_allocated_register(!empty);

        // Update the pointer.
        fl_read <= fl_read - 1;

    endmethod

    // backTo

    // When:   When there are no inflight requests.
    // Effect: Reset the pointer to the given value.

    method Action backTo(PRName r) if (!initializing && req_count.value() == 0);

        // Log it.
        $fdisplay(debug_log, "[%d]: FREELIST: Going back to PR%0d", fpga_cc, r);

        // Check for errors.
        if(fl_read > fl_write && r < fl_write || fl_read < fl_write && r < fl_write && r > fl_read)
            $display("ERROR: Backed up the freelist too far! (r = %0d)", r);

        // Update the pointer.
        fl_read <= r;

    endmethod
  
    // current

    // When:   Any time.
    // Get the current pointer value (for snapshots).

    method PRName current();

        return fl_read;

    endmethod

endmodule

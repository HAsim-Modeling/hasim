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

module [HASim_Module] mkFUNCP_Freelist#(File debugLog, Bit#(32) fpgaCC)
    //interface:
                (FUNCP_FREELIST)
    provisos
             (Bits#(ISA_REG_INDEX, rname_SZ),
              Bits#(FUNCP_PHYSICAL_REG_INDEX, prname_SZ)); // Physical register index size

    // ***** Local State ***** //

    // The maximum achitectural register.
    Bit#(rname_SZ) maxR = maxBound;

    // The architectural registers begin allocated, so the freelist pointer starts at
    // one position beyond that.
    Bit#(prname_SZ) minInitFL_bits = zeroExtend(pack(maxR)) + 1;
    FUNCP_PHYSICAL_REG_INDEX initFL = unpack(minInitFL_bits);

    // The maximum number of physical registers.
    FUNCP_PHYSICAL_REG_INDEX maxFL = maxBound;

    // Register to track if we're initializing.
    Reg#(Bool) initializing <- mkReg(True);

    // The actual freelist
    BRAM#(prname_SZ, FUNCP_PHYSICAL_REG_INDEX) fl <- mkBramInitialized(?);

    // The read pointer is the next register to allocate.
    Reg#(FUNCP_PHYSICAL_REG_INDEX) flRead   <- mkReg(initFL);

    // The write pointer is the next register to overwrite.
    Reg#(FUNCP_PHYSICAL_REG_INDEX) flWrite  <- mkReg(0); 

    // The number of requests in flight is used to make sure we do not rewind in an unsure state.
    Counter#(2)                    reqCount <- mkCounter(0);

    // We are empty if the write equals the read.
    Bool empty = flRead == flWrite;

    // We are out of physical registers when the pointers overlap.
    Bool full = flRead + 1 == flWrite;

    // ***** Assertion Checkers *****/

    Assertion assertEnoughPRegs <- mkAssertionChecker(`ASSERTIONS_FREELIST_OUT_OF_PREGS, ASSERT_ERROR);
    Assertion assertAtLeastOneAllocatedRegister <- mkAssertionChecker(`ASSERTIONS_FREELIST_ILLEGAL_BACKUP, ASSERT_ERROR);

    // initialize

    // When:   At the beginning of time.
    // Effect: Put every architectural register onto the freelist and update the pointers to match.

    // Wires for enabling firing of forwardReq, back and backTo (needed to support multiple destinations in regstate_manager
    Wire#(Bool) forwardReqEn <- mkDWire(False);
    Wire#(Bool)       backEn <- mkDWire(False);
    Wire#(Bool)     backToEn <- mkDWire(False);
    Wire#(FUNCP_PHYSICAL_REG_INDEX)   flReadWire <- mkDWire(?);

    rule update_flRead(True);
        if(backToEn)
            flRead <= flReadWire;
        else if(forwardReqEn && !backEn)
            flRead <= flRead + 1;
        else if(!forwardReqEn && backEn)
            flRead <= flRead - 1;
    endrule

    rule initialize (initializing);

        // Add architectural register X to the freelist.
        fl.write(flWrite, flWrite);

        // X = X + 1.
        flWrite <= flWrite + 1;

        // done initializing if X == maxFL.
        if (flWrite == maxFL)
          initializing <= False;

    endrule

    // forwardReq

    // When:   Any time.
    // Effect: Look up the next physical register in the block ram.
    //         If we are out of physical registers a simulator exception occurs.

    method Action forwardReq() if (!initializing);

        // Assert that we're not out of physical registers.
        assertEnoughPRegs(!full);

        // Log it.
        $fdisplay(debugLog, "[%d]: FREELIST: Requesting %0d", fpgaCC, flRead);

        // Read the next entry.
        fl.readReq(flRead);

        forwardReqEn <= True;
        // Update the pointer.
        //flRead <= flRead + 1;

        // Update the number of in-flight requests.
        reqCount.up();

    endmethod

    // forwardResp

    // When:   Any time.
    // Effect: Return the result from BRAM to the requestor.

    method ActionValue#(FUNCP_PHYSICAL_REG_INDEX) forwardResp() if (!initializing);

        // Get the response from BRAM.
        let rsp <- fl.readResp();

        // Log it.
        $fdisplay(debugLog, "[%d]: FREELIST: Allocating PR%0d %0d", fpgaCC, rsp, flRead);

        // Update the number of in-flight requests.
        reqCount.down();

        // Return the response to the requestor.
        return rsp;

    endmethod

    // free

    // When:   Any time.
    // Effect: Add register r back to the freelist.

    method Action free(FUNCP_PHYSICAL_REG_INDEX r) if (!initializing);

        // Add it back to the freelist.
        fl.write(flWrite, r);
        // Update the write pointer.
        flWrite <= flWrite + 1;

        // Log it.
        $fdisplay(debugLog, "[%d]: FREELIST: Freeing PR%0d %0d", fpgaCC, r, flWrite + 1);

    endmethod

    // back

    // When:   Any time.
    // Effect: Undo the last allocation.

    method Action back() if (!initializing);

        // If the freelist is empty this is an exception.
        assertAtLeastOneAllocatedRegister(!empty);

        backEn <= True;
        // Update the pointer.
        //flRead <= flRead - 1;

    endmethod

    // backTo

    // When:   When there are no inflight requests.
    // Effect: Reset the pointer to the given value.

    method Action backTo(FUNCP_PHYSICAL_REG_INDEX r) if (!initializing && reqCount.value() == 0);

        // Log it.
        $fdisplay(debugLog, "[%d]: FREELIST: Going back to PR%0d", fpgaCC, r);

        // Check for errors.
        if(flRead > flWrite && r < flWrite || flRead < flWrite && r < flWrite && r > flRead)
            $display("ERROR: Backed up the freelist too far! (r = %0d)", r);

        // Update the pointer.
        backToEn <= True;
        flReadWire <= r;
        //flRead <= r;

    endmethod
  
    // current

    // When:   Any time.
    // Get the current pointer value (for snapshots).

    method FUNCP_PHYSICAL_REG_INDEX current();

        return flRead;

    endmethod

endmodule

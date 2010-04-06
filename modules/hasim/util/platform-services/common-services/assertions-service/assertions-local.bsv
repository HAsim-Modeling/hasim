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

import Vector::*;
import FIFOF::*;

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/assertions_device.bsh"

`include "asim/dict/RINGID.bsh"
`include "asim/dict/ASSERTIONS.bsh"

// Assertions

// A way to report to the outside world when something has gone wrong.

typedef function Action checkAssert(Bool b) ASSERTION;
  
// ASSERTION_DATA

// Internal datatype for communicating with the assertions controller
typedef struct
{
    ASSERTION_NODE_VECTOR assertions;
    ASSERTIONS_DICT_TYPE baseID;
}
    ASSERTION_DATA
        deriving (Eq, Bits);

interface ASSERTION_NODE;
            
    method Action raiseAssertion(ASSERTIONS_DICT_TYPE myID, ASSERTION_SEVERITY mySeverity);
    
endinterface

//
// mkAssertionNode --
//     An assertion node is a group of assertions sharing a node on the
//     assertion ring.  Each assertion must be a member of a node in order
//     to pass the assertion to software.  Up to ASSERTIONS_PER_NODE assertions
//     may be allocated for a given node.  These assertions must be numerically
//     related to the node by sharing the same base identifier.  This sharing
//     is managed automatically by the dictionary builder.  An assertion named
//     ASSERTIONS.FOO.BAR in the dictionary gets an ID ASSERTIONS_FOO_BAR and
//     belongs to node ID ASSERTIONS_FOO__BASE.
//
module [Connected_Module] mkAssertionNode#(ASSERTIONS_DICT_TYPE baseID)
    // interface:
        (ASSERTION_NODE);

    // *********** Connections ***********

    // Connection to the assertions controller
    Connection_Chain#(ASSERTION_DATA) chain <- mkConnection_Chain(`RINGID_ASSERTS);

    // Wires & registers for individual assertions
    Vector#(`ASSERTIONS_PER_NODE, Wire#(ASSERTION_SEVERITY)) raiseWire <- replicateM(mkDWire(ASSERT_NONE));
    Vector#(`ASSERTIONS_PER_NODE, Reg#(ASSERTION_SEVERITY))  raiseReg  <- replicateM(mkReg(ASSERT_NONE));

    // Queue of assertions to raise.  When an assertion gets raised there are
    // 2 cycles of accurate assertion groupings.  If the FIFO fills, assertions
    // are held in a single register until they can be sent out.  Thus 3 firings
    // of an assertion are guaranteed. Beyond that, multiple firings may be
    // merged into a single firing.
    FIFOF#(ASSERTION_NODE_VECTOR) assertQ <- mkGFIFOF(True, False);
    Reg#(ASSERTION_NODE_VECTOR) pendingAsserts <- mkReg(replicate(ASSERT_NONE));

    // *********** Rules ***********

    //
    // Cycle counter used for printing in simulation.  Will be optimized away
    // in hardware.
    //
    Reg#(Bit#(64)) cycle <- mkReg(0);

    rule countCycle (True);
        cycle <= cycle + 1;
    endrule



    function Bool isSet(ASSERTION_SEVERITY a) = (a != ASSERT_NONE);

    //
    // detectLocal --
    //     For each local assertion record whether assertion was raised this
    //     cycle and store it in a register.  The incoming registers used to
    //     feed directly into the raiseLocal rule, but that became a timing
    //     critical path.
    //
    for (Integer e = 0; e < `ASSERTIONS_PER_NODE; e = e + 1)
    begin

        rule detectLocal (True);
            raiseReg[e] <= raiseWire[e];
        endrule

    end

    (* conflict_free = "raiseLocal, processLocal" *)

    //
    // raiseLocal --
    //     Detect assertions raised last cycle (stored in raiseReg vector
    //     by detectLocal rule).  If possible, queue the assertion(s) for delivery
    //     to the assertions controller.  If the queue is full store the
    //     assertion(s) for later delivery.
    //
    rule raiseLocal (True);

        //
        // Merge new assertions this cycle and any pending assertions not yet
        // queued.
        //
        ASSERTION_NODE_VECTOR a = ?;
        for (Integer e = 0; e < `ASSERTIONS_PER_NODE; e = e + 1)
        begin
            a[e] = unpack(pack(raiseReg[e]) | pack(pendingAsserts[e]));
        end
        
        if (any(isSet, a))
        begin
            if (assertQ.notFull())
            begin
                assertQ.enq(a);
                pendingAsserts <= replicate(ASSERT_NONE);
            end
            else
            begin
                pendingAsserts <= a;
            end
        end
        
    endrule


    //
    // processLocal --
    //     Send local assertions to the controller.
    //
    rule processLocal (assertQ.notEmpty());

        let a = assertQ.first();
        assertQ.deq();

        let ast = ASSERTION_DATA { assertions: a, baseID: baseID };
        chain.sendToNext(ast);

    endrule

    //
    // processCmd --
    //     Forward assertions from other nodes to the controller.
    //
    rule processCmd (! assertQ.notEmpty());

        ASSERTION_DATA ast <- chain.recvFromPrev();
        chain.sendToNext(ast);

    endrule


    // *********** Methods ***********

    method Action raiseAssertion(ASSERTIONS_DICT_TYPE myID, ASSERTION_SEVERITY mySeverity);

        raiseWire[myID - baseID] <= mySeverity;

        String a_type = case (mySeverity)
                            ASSERT_NONE: "NONE";
                            ASSERT_MESSAGE: "MESSAGE";
                            ASSERT_WARNING: "WARNING";
                            ASSERT_ERROR: "ERROR";
                        endcase;

        $display("ASSERTION %s: cycle %0d, %s", a_type, cycle, showASSERTIONS_DICT(myID));

    endmethod

endmodule

            
//
// mkAssertionChecker --
//    Allocate a checker for a single assertion ID, connected to an assertion node.
//
module [CONNECTED_MODULE] mkAssertionChecker#(ASSERTIONS_DICT_TYPE myID, ASSERTION_SEVERITY mySeverity, ASSERTION_NODE myNode)
    // interface:
        (ASSERTION);

    // *********** Methods ***********
  
    // Check the boolean expression and enqueue a pass/fail.
  
    function Action assert_function(Bool b);
    action

        if (!b) // Check the boolean expression
        begin   // Failed. The system is sad. :(
            myNode.raiseAssertion(myID, mySeverity);
        end

    endaction
    endfunction
  
    return assert_function;

endmodule

module [CONNECTED_MODULE] mkAssertionCheckerError#(ASSERTIONS_DICT_TYPE myID, ASSERTION_NODE myNode)
    // interface:
        (ASSERTION);

    let as <- mkAssertionChecker(myID, ASSERT_ERROR, myNode);
    return as;

endmodule

module [CONNECTED_MODULE] mkAssertionCheckerWarning#(ASSERTIONS_DICT_TYPE myID, ASSERTION_NODE myNode)
    // interface:
        (ASSERTION);

    let as <- mkAssertionChecker(myID, ASSERT_WARNING, myNode);
    return as;

endmodule

module [CONNECTED_MODULE] mkAssertionCheckerMessage#(ASSERTIONS_DICT_TYPE myID, ASSERTION_NODE myNode)
    // interface:
        (ASSERTION);

    let as <- mkAssertionChecker(myID, ASSERT_MESSAGE, myNode);
    return as;

endmodule

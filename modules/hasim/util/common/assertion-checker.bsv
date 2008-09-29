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

`include "asim/dict/RINGID.bsh"
`include "asim/dict/ASSERTIONS.bsh"

// Assertions

// A way to report to the outside world when something has gone wrong.


// ASSERTION_SEVERITY

// The severity of an assertion. This could be used to filter things out.

typedef enum
{
    ASSERT_NONE,
    ASSERT_MESSAGE,
    ASSERT_WARNING,
    ASSERT_ERROR
}
    ASSERTION_SEVERITY 
        deriving (Eq, Bits);


instance Ord#(ASSERTION_SEVERITY);

  function Bool \< (ASSERTION_SEVERITY x, ASSERTION_SEVERITY y) = pack(x) < pack(y);

  function Bool \> (ASSERTION_SEVERITY x, ASSERTION_SEVERITY y) = pack(x) > pack(y);

  function Bool \<= (ASSERTION_SEVERITY x, ASSERTION_SEVERITY y) = pack(x) <= pack(y);

  function Bool \>= (ASSERTION_SEVERITY x, ASSERTION_SEVERITY y) = pack(x) >= pack(y);

endinstance


// Assertion

// The interface checks if a boolean expression is true or false.

typedef function Action checkAssert(Bool b) ASSERTION;
  
// ASSERTION_DATA

// Vector of severity values for assertions baseID + index
typedef Vector#(`ASSERTIONS_PER_NODE, ASSERTION_SEVERITY) ASSERTION_NODE_VECTOR;

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

    // DWires from individual assertions
    Vector#(`ASSERTIONS_PER_NODE, Wire#(ASSERTION_SEVERITY)) raise <- replicateM(mkDWire(ASSERT_NONE));

    // Queue of assertions to raise.  When an assertion gets raised there are
    // 2 cycles of accurate assertion groupings.  If the FIFO fills, assertions
    // are held in a single register until they can be sent out.  Thus 3 firings
    // of an assertion are guaranteed. Beyond that, multiple firings may be
    // merged into a single firing.
    FIFOF#(ASSERTION_NODE_VECTOR) assertQ <- mkSizedFIFOF(2);
    Reg#(ASSERTION_NODE_VECTOR) pendingAsserts <- mkReg(replicate(ASSERT_NONE));

    // *********** Rules ***********

    function Bool isSet(ASSERTION_SEVERITY a) = (a != ASSERT_NONE);

    (* conflict_free = "detectLocal, processLocal" *)

    //
    // detectLocal --
    //     Detect raised assertion(s) for a single cycle.  If possible, queue the
    //     assertion(s) for delivery to the assertions controller.  If the queue
    //     is full store the assertion(s) for later delivery.
    //
    rule detectLocal (True);

        //
        // Merge new assertions this cycle and any pending assertions not yet
        // queued.
        //
        ASSERTION_NODE_VECTOR a = ?;
        for (Integer e = 0; e < `ASSERTIONS_PER_NODE; e = e + 1)
        begin
            a[e] = unpack(pack(raise[e]) | pack(pendingAsserts[e]));
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
        chain.send_to_next(ast);

    endrule

    //
    // processCmd --
    //     Forward assertions from other nodes to the controller.
    //
    rule processCmd (! assertQ.notEmpty());

        ASSERTION_DATA ast <- chain.receive_from_prev();
        chain.send_to_next(ast);

    endrule


    // *********** Methods ***********

    method Action raiseAssertion(ASSERTIONS_DICT_TYPE myID, ASSERTION_SEVERITY mySeverity);

        raise[myID - baseID] <= mySeverity;

    endmethod

endmodule

            
//
// mkAssertionChecker --
//    Allocate a checker for a single assertion ID, connected to an assertion node.
//
module [HASIM_MODULE] mkAssertionChecker#(ASSERTIONS_DICT_TYPE myID, ASSERTION_SEVERITY mySeverity, ASSERTION_NODE myNode)
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
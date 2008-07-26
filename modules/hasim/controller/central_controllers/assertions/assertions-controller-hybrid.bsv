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

import FIFO::*;

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"

`include "asim/provides/rrr.bsh"

`include "asim/rrr/service_ids.bsh"
`include "asim/dict/ASSERTIONS.bsh"
`include "asim/dict/RINGID.bsh"

// AssertionsController
//
// Abstracts communication from the main controller to the assertion checkers
// which are distributed throughout the hardware model.  Because assertions
// are delivered on a ring there is no guarantee that all assertions will arrive
// in order.
//

interface ASSERTIONS_CONTROLLER;

endinterface

// mkAssertionsController

// A module which serially passes Assertion failures back to the software.

module [Connected_Module] mkAssertionsController
    // interface:
        (ASSERTIONS_CONTROLLER);

    //***** State Elements *****
  
    // Communication link to the rest of the Assertion checkers
    Connection_Chain#(ASSERTION_DATA) chain <- mkConnection_Chain(`RINGID_ASSERTS);
  
    // Communication link to our RRR Service
    Connection_Send#(RRR_Request) link_rrr <- mkConnection_Send("rrr_client_assertions");
  
    Reg#(Bit#(32)) fpgaCC <- mkReg(0);
  
    // ***** Rules *****
  
    // countCC
  
    rule countCC (True);

        fpgaCC <= fpgaCC + 1;

    endrule
  

    // processResp

    // Process the next response from an individual assertion checker.
    // Pass assertions on to software.  Here we let the software deal with
    // the relatively complicated base ID and assertions vector.

    rule processResp (True);

        let ast <- chain.receive_from_prev();
        link_rrr.send(RRR_Request {serviceID:    `ASSERTIONS_SERVICE_ID,
                                   param0:       0, //unused for now. Reserved for methodID.
                                   param1:       zeroExtend(pack(ast.baseID)),
                                   param2:       fpgaCC,
                                   param3:       zeroExtend(pack(ast.assertions)),
                                   needResponse: False });

    endrule

endmodule

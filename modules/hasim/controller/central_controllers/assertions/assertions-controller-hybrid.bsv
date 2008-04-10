import FIFO::*;

import hasim_common::*;
import soft_connections::*;

`include "asim/provides/rrr.bsh"

`include "asim/rrr/rrr_service_ids.bsh"
`include "asim/dict/ASSERTIONS.bsh"
`include "asim/dict/RINGID.bsh"

// AssertionsController

// Abstracts communication from the main controller to the assertion checkers
// which are distributed throughout the hardware model.

// When an assertion does occur the the main controller can use the getAssertion() method
// to retreive an assertion for reporting. No further assertions are reported because presumably
// the system is in a bad state. 

// Note: this means that the root cause of the problem could occaisionally be occluded.
// This would occur if one failure caused more to happen, but we saw one of the others
// first. 


interface ASSERTIONS_CONTROLLER;

  method Action doCommand(ASSERTIONS_COMMAND com);

endinterface

// AssertsCommand

// The datatype of commands that the Asserts Controller responds to.

typedef enum
{
      ASSERTS_MinSeverity_Message,
      ASSERTS_MinSeverity_Warning,
      ASSERTS_MinSeverity_Error
}
  ASSERTIONS_COMMAND
                 deriving (Eq, Bits);


// mkAssertionsController

// A module which serially passes Assertion failures back to the software.

module [Connected_Module] mkAssertionsController
    //interface:
                (ASSERTIONS_CONTROLLER);

  //***** State Elements *****
  
  // Communication link to the rest of the Assertion checkers
  Connection_Chain#(ASSERTION_DATA) chain <- mkConnection_Chain(`RINGID_ASSERTS);
  
  // Communication link to our RRR Service
  Connection_Send#(RRR_Request) link_rrr <- mkConnection_Send("rrr_client_assertions");
  
  // The minimum severity of assertions we should pass along
  Reg#(ASSERTION_SEVERITY) min_severity <- mkReg(ASSERT_MESSAGE);
  
  Reg#(Bit#(32)) fpgaCC <- mkReg(0);
  
  // ***** Rules *****
  
  // countCC
  
  rule countCC (True);
  
    fpgaCC <= fpgaCC + 1;
  
  endrule
  
  // processResp
  
  // Process the next response from an individual assertion checker.
  // If it failed it gets passed along to the main controller. 
  // If it passed we just discard it.
  
  rule processResp (True);
  
    let ast <- chain.receive_from_prev();
    
    if (ast.severity >= min_severity)
    begin
      link_rrr.send(RRR_Request { serviceID:    `ASSERTIONS_SERVICE_ID,
                                  param0:       0, //unused for now. Reserved for methodID.
                                  param1:       zeroExtend(pack(ast.assertID)),
                                  param2:       fpgaCC,
                                  param3:       zeroExtend(pack(ast.severity)),
                                  needResponse: False });
    end

  endrule
  
  // ***** Methods *****
  
  // doCommand
  
  // The primary way that the outside world tells us what to do.
  
  method Action doCommand(ASSERTIONS_COMMAND com);
    
    case (com)
      ASSERTS_MinSeverity_Message: min_severity <= ASSERT_MESSAGE;
      ASSERTS_MinSeverity_Warning: min_severity <= ASSERT_WARNING;
      ASSERTS_MinSeverity_Error:   min_severity <= ASSERT_ERROR;
    endcase
    
  endmethod


endmodule

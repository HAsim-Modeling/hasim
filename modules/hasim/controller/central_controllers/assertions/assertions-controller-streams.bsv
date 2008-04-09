import FIFO::*;

import hasim_common::*;
import soft_connections::*;

`include "streams.bsh"
`include "asim/dict/STREAMS.bsh"
`include "asim/dict/STREAMID.bsh"
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

module [Connected_Module] mkAssertionsController#(Connection_Send#(STREAMS_REQUEST) link_streams)
    //interface:
                (ASSERTIONS_CONTROLLER);

  //***** State Elements *****
  
  // Communication link to the rest of the Assertion checkers
  Connection_Chain#(ASSERTION_DATA) chain <- mkConnection_Chain(`RINGID_ASSERTS);
    
  // The minimum severity of assertions we should pass along
  Reg#(ASSERTION_SEVERITY) min_severity <- mkReg(ASSERT_MESSAGE);
  
  // ***** Rules *****
  
  // processResp
  
  // Process the next response from an individual assertion checker.
  // If it failed it gets passed along to the main controller. 
  // If it passed we just discard it.
  
  rule processResp (True);
  
    let ast <- chain.receive_from_prev();
    
    if (ast.severity >= min_severity)
    begin
      link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ASSERT,
                                          stringID: ast.stringID,
                                          payload0: zeroExtend(pack(ast.severity)),
                                          payload1: ? });
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

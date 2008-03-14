import FIFO::*;

import hasim_common::*;
import soft_connections::*;

`include "streams.bsh"
`include "asim/dict/STREAMS.bsh"
`include "asim/dict/STREAMID.bsh"
`include "asim/dict/RINGID.bsh"

// AssertionsController

// Abstracts the communication between the main hardware controller and the 
// Event recorders spread throughout the hardware model.

// mkAssertionsController

// A module which serially passes Assertion failures back to the main hardware controller.

module [Connected_Module] mkAssertionsController#(Connection_Send#(STREAMS_REQUEST) link_streams)
    //interface:
                (AssertionsController);

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
  
  method Action doCommand(AssertionsCommand com);
    
    case (com)
      Asserts_MinSeverity_Message: min_severity <= ASSERT_MESSAGE;
      Asserts_MinSeverity_Warning: min_severity <= ASSERT_WARNING;
      Asserts_MinSeverity_Error:   min_severity <= ASSERT_ERROR;
    endcase
    
  endmethod


endmodule

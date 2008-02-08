import FIFO::*;

import hasim_common::*;
import soft_connections::*;

// AssertionsController

// Abstracts the communication between the main hardware controller and the 
// Event recorders spread throughout the hardware model.

`define CHAIN_IDX_ASSERTS 4

typedef ASSERTION_DATA AssertData;

// mkAssertionsController

// A module which serially passes Assertion failures back to the main hardware controller.

module [Connected_Module] mkAssertionsController
    //interface:
                (AssertionsController);

  //***** State Elements *****
  
  // Communication link to the rest of the Assertion checkers
  Connection_Chain#(AssertData) chain <- mkConnection_Chain(`CHAIN_IDX_ASSERTS);
  
  // Output FIFO of failures to pass along
  FIFO#(AssertInfo) failQ <- mkFIFO();
  
  // The current Assert ID we are expecting
  Reg#(Bit#(8))       cur <- mkReg(0);
  
  // The minimum severity of assertions we should pass along
  Reg#(AssertionSeverity) min_severity <- mkReg(ASSERT_MESSAGE);
  
  // ***** Rules *****
  
  // sendReq
  
  // Send the next request to the Assertion checkers.
  // We are continually polling for assertion failures.
  
  rule sendReq (True);
  
    chain.send_to_next(tagged AST_Boundary);

  endrule
  
  // processResp
  
  // Process the next response from an individual assertion checker.
  // If it failed it gets passed along to the main controller. 
  // If it passed we just discard it.
  
  rule processResp (True);
  
    let ast <- chain.receive_from_prev();
    
    case (ast) matches
      tagged AST_Passed:  //No problems to report.
      begin
        cur <= cur + 1;
      end
      tagged AST_Failed .sev: //It failed.
      begin
        // Check the minimum severity. If it's over our threshold pass it along.
        if (sev > min_severity)
        begin
          failQ.enq(AssertInfo { assertStringID: cur, assertSeverity: sev});
        end
        cur <= cur + 1;
      end
      tagged AST_Boundary: //When our boundary gets back to us we've heard from everybody.
      begin
        cur <= 0;
      end
    endcase
     
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

  // getAssertion
  
  // Return the next assertion failure, which will presumably get passed on to outside world.
  
  method ActionValue#(AssertInfo) getAssertion();
  
    let ast = failQ.first();
    failQ.deq();
    return ast;
    
  endmethod
  
endmodule

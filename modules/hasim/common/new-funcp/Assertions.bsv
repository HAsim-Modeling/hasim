import FIFOF::*;

`define CHAIN_IDX_ASSERTS 4

// Assertions

// A way to report to the outside world when something has gone wrong.


// ASSERTION_SEVERITY

// The severity of an assertion. This could be used to filter things out.

typedef enum
{
  ASSERT_MESSAGE,
  ASSERT_WARNING,
  ASSERT_ERROR
}
  ASSERTION_SEVERITY 
                    deriving (Eq, Bits);

instance Ord#(AssertionSeverity);

  function Bool \< (AssertionSeverity x, AssertionSeverity y) = pack(x) < pack(y);

  function Bool \> (AssertionSeverity x, AssertionSeverity y) = pack(x) > pack(y);

  function Bool \<= (AssertionSeverity x, AssertionSeverity y) = pack(x) <= pack(y);

  function Bool \>= (AssertionSeverity x, AssertionSeverity y) = pack(x) >= pack(y);

endinstance

// Assertion

// The interface checks if a boolean expression is true or false.

typedef function Action checkAssert(Bool b) Assertion;
  
// ASSERTION_DATA

// Internal datatype for communicating with the AssertionsController


typedef union tagged
{
  void               AST_Boundary;
  void               AST_Passed;
  ASSERTION_SEVERITY AST_Failed;
}
  ASSERTION_DATA
             deriving (Eq, Bits);

// mkAssertionChecker

// Make a module which checks one assertion.
// The assert() method should be called with the condition to check.

module [Connected_Module] mkAssertionChecker#(String assertmessage, ASSERTION_SEVERITY severity)
  // interface:
               (Assertion);

  // *********** Connections ***********

  // Connection to the assertions controller
  Connection_Chain#(ASSERTION_DATA) chain <- mkConnection_Chain(`CHAIN_IDX_ASSERTS);
  
  // *********** Local State ***********

  // Local data queue for sending on to the controller.
  FIFOF#(ASSERTION_DATA)  localQ <- mkFIFOF();
  
  //When we get a boundary message from the controller it's time for us to send our own data.
  Reg#(Bool)         stall <- mkReg(False);
  
  // *********** Rules ***********

  // processCmd
  
  // Process a message from the controller
  
  rule processCmd (!stall);
  
    ASSERTION_DATA ast <- chain.receive_from_prev();

    case (ast) matches 
      tagged AST_Boundary .t: stall     <= True;
      default:                noAction;
    endcase

    chain.send_to_next(ast);
  endrule
  
  // respond

  // Give our response to the Controller.
  
  // Note: If the queue is empty, we assume the assertion passed.
  //       This might obscure when exactly an assertion fails.
  //       IE "imprecise exceptions".
  
  rule respond (stall);
  
    if (localQ.notEmpty())
    begin
      chain.send_to_next(localQ.first());
      localQ.deq();
    end
    else
    begin
      chain.send_to_next(tagged AST_Passed);
    end
    
    stall <= False;
  
  endrule
  
  // *********** Methods ***********
  
  // assert
  
  // Check the boolean expression and enqueue a pass/fail.
  
  function Action assert_function(Bool b);
  action
  
    if (!b) // Check the boolean expression
    begin   // Failed. The system is sad. :(
      localQ.enq(tagged AST_Failed severity);
      // In simulation we can display the problem right away.
      String sevmessage = case (severity)
                              ASSERT_MESSAGE: "Message";
                              ASSERT_WARNING: "Warning";
                              ASSERT_ERROR:   "Error";
                          endcase;
      $display("Assertion Failed: %s (Severity: %s)", assertmessage, sevmessage);
    end

  endaction
  endfunction
  
  return assert_function;

endmodule

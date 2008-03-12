import FIFOF::*;

`include "asim/dict/RINGID.bsh"
`include "asim/dict/STREAMS.bsh"

// Assertions

// A way to report to the outside world when something has gone wrong.


// AssertionSeverity

// The severity of an assertion. This could be used to filter things out.

typedef enum
{
  ASSERT_Message,
  ASSERT_Warning,
  ASSERT_Error
}
  AssertionSeverity 
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
  
// AssertData

// Internal datatype for communicating with the AssertionsController


typedef struct
{
   STREAMS_DICT_TYPE  stringID;
   AssertionSeverity severity;
}
  AssertData
             deriving (Eq, Bits);

// mkAssertionChecker

// Make a module which checks one assertion.
// The assert() method should be called with the condition to check.

module [Connected_Module] mkAssertionChecker#(STREAMS_DICT_TYPE myID, AssertionSeverity my_severity)
  // interface:
               (Assertion);

  // *********** Connections ***********

  // Connection to the assertions controller
  Connection_Chain#(AssertData)  chain  <- mkConnection_Chain(`RINGID_ASSERTS);
  
  // *********** Rules ***********

  // processCmd
  
  // Process a message from the controller
  
  rule processCmd (True);
  
    AssertData ast <- chain.receive_from_prev();
    chain.send_to_next(ast);

  endrule
    
  // *********** Methods ***********
  
  // assert
  
  // Check the boolean expression and enqueue a pass/fail.
  
  function Action assert_function(Bool b);
  action
  
    if (!b) //Check the boolean expression
    begin //Failed. The system is sad. :(
      chain.send_to_next(AssertData {stringID: myID, severity: my_severity});
    end

  endaction
  endfunction
  
  return assert_function;

endmodule

import hasim_common::*;

// AssertionsController

// Abstracts communication from the main controller to the assertion checkers
// which are distributed throughout the hardware model.

// When an assertion does occur the the main controller can use the getAssertion() method
// to retreive an assertion for reporting. No further assertions are reported because presumably
// the system is in a bad state. 

// Note: this means that the root cause of the problem could occaisionally be occluded.
// This would occur if one failure caused more to happen, but we saw one of the others
// first. 


interface AssertionsController;

  method Action doCommand(AssertionsCommand com);

endinterface

// AssertsCommand

// The datatype of commands that the Asserts Controller responds to.

typedef enum
{
      Asserts_MinSeverity_Message,
      Asserts_MinSeverity_Warning,
      Asserts_MinSeverity_Error
}
  AssertionsCommand
                 deriving (Eq, Bits);


// EventInfo

// The datatype of Events that the Events Controller returns

typedef struct
{
        Bit#(8)            assertStringID; // Which Assertion is it?
        AssertionSeverity  assertSeverity; // Message / Warning / Error
}
  AssertInfo 
            deriving 
                     (Eq, Bits);


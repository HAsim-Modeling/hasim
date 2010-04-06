
module [CONNECTED_MODULE] mkAssertionsService#(ASSERTIONS assertionsDevice)
    // interface:
        ();


    //***** State Elements *****
  
    // Communication link to the rest of the Assertion checkers
    Connection_Chain#(ASSERTION_DATA) chain <- mkConnection_Chain(`RINGID_ASSERTS);
  
    // ***** Rules *****
  

    // processResp

    // Process the next response from an individual assertion checker.
    // Pass assertions on to software.  Here we let the software deal with
    // the relatively complicated base ID and assertions vector.

    rule processResp (True);

        let ast <- chain.recvFromPrev();
        assertionsDevice.assertionNodeValues(ast.baseID, ast.assertions);

    endrule


endmodule

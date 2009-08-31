
// SOFT_PARAMS_STATE

// An internal datatype to track the state of the params controller

typedef enum
{
    PCS_Idle,           // Not executing any commands
    PCS_High32,         // Send the high 32 bits
    PCS_Low32,          // Send the low 32 bits
    PCS_Waiting         // Wait for the ring response to ACK parameter.
}
    SOFT_PARAMS_STATE
               deriving (Eq, Bits);

// mkParamsController

// Abstracts all communication from the main controller to individual stat counters.

module [CONNECTED_MODULE] mkDynamicParametersService#(DYNAMIC_PARAMETERS dynParam)
    //interface:
                ();

    // ****** State Elements ******

    // Communication link to the Params themselves
    Connection_Chain#(PARAM_DATA) chain <- mkConnection_Chain(`RINGID_PARAMS);
 
    // Our internal state
    Reg#(SOFT_PARAMS_STATE) state <- mkReg(PCS_Idle);
    
    // ****** Rules ******
    
    // waitForParam
    //    
    // Wait for parameter update request
    
    rule waitForParam (state == PCS_Idle);

        DYN_PARAM p = dynParam.getParameter();

        //
        // The first message on the chain is the parameter ID
        //
        PARAM_DATA msg = tagged PARAM_ID p.paramID;
        chain.send_to_next(msg);

        state <= PCS_High32;

    endrule
    
    // send
    //
    // State machine for sending parameter values in two 32 bit chunks
    //
    rule sendHigh (state == PCS_High32);
  
        PARAM_DATA msg;
        let v = dynParam.getParameter().value;
        //
        // Send the high 32 bits first.
        //

        msg = tagged PARAM_High32 v[63:32];
        chain.send_to_next(msg);
        state <= PCS_Low32;

    endrule
    
    rule sendLow (state == PCS_Low32);
    
        PARAM_DATA msg;
        let v = dynParam.getParameter().value;
        msg = tagged PARAM_Low32 v[31:0];
        chain.send_to_next(msg);
        state <= PCS_Waiting;
  
    endrule
    
    // receive
    //
    // Receive messages that completed their journey around the ring.  Drop
    // them.  Send an ACK for PARAMS_NULL.  The software side sends NULL last
    // and waits for the ACK to know that all parameters have been received.
    //
    rule receive (state == PCS_Waiting);
  
        PARAM_DATA msg <- chain.receive_from_prev();

        if (msg matches tagged PARAM_ID .id)
        begin
            dynParam.nextParameter();
            state <= PCS_Idle;
        end
  
    endrule

endmodule

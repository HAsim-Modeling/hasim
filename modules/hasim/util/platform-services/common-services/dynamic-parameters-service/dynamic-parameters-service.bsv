//
// Copyright (C) 2009 Intel Corporation
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


//
// Dynamic parameters are passed from the software during the startup phase.
// The ring protocol breaks messages into 3 parts:  token ID, high half of
// the value and low half of the value.  There is no synchronization ACK
// returned to the software since the local code to read the value of a parameter
// blocks until it has been initialized.
//


// SOFT_PARAMS_STATE

// An internal datatype to track the state of the params controller

typedef enum
{
    PCS_IDLE,           // Not executing any commands
    PCS_HIGH32,         // Send the high 32 bits
    PCS_LOW32          // Send the low 32 bits
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
    Reg#(SOFT_PARAMS_STATE) state <- mkReg(PCS_IDLE);
    
    // ****** Rules ******
    
    // waitForParam
    //    
    // Wait for parameter update request
    
    rule waitForParam (state == PCS_IDLE);
        DYN_PARAM c = dynParam.peekCmd();

        //
        // The first message on the chain is the parameter ID
        //
        PARAM_DATA msg = tagged PARAM_ID c.paramID;
        chain.sendToNext(msg);

        state <= PCS_HIGH32;
    endrule
    
    // send
    //
    // State machine for sending parameter values in two 32 bit chunks
    //
    rule sendHigh (state == PCS_HIGH32);
        let c = dynParam.peekCmd();

        //
        // Send the high 32 bits first.
        //
        PARAM_DATA msg = tagged PARAM_High32 c.value[63:32];
        chain.sendToNext(msg);
        state <= PCS_LOW32;
    endrule
    
    rule sendLow (state == PCS_LOW32);
        let c <- dynParam.getCmd();
        dynParam.finishCmd();

        PARAM_DATA msg = tagged PARAM_Low32 c.value[31:0];
        chain.sendToNext(msg);
        state <= PCS_IDLE;
    endrule
    
    // receive
    //
    // Sink messages coming around the ring.
    //
    rule receive (True);
        PARAM_DATA msg <- chain.recvFromPrev();
    endrule
endmodule

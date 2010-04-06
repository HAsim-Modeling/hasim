//
// Copyright (C) 2008 Intel Corporation
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
// @file dynamic-paramters.bsv
// @brief Dynamic parameters.  The size of the parameter is an argument to
//        the declaration.  Parameters may be at most 64 bits.
//
// @author Michael Adler
//

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/dynamic_parameters_device.bsh"

`include "asim/dict/RINGID.bsh"
`include "asim/dict/PARAMS.bsh"

//
// PARAMETER_NODE
//
// Interface to mkDynamicParameterNode.  Individual parameters constantly
// snoop to see whether their value has arrived on the ring.  They must
// look every cycle since the ring stop guarantees to export a value for only
// a cycle before a new ID might arrive.
//
interface PARAMETER_NODE;
    
    method Maybe#(Bit#(64)) checkForNewValue(PARAMS_DICT_TYPE myID);

endinterface


//
// Param
//
// Interface to a single parameter.  The read method blocks until the parameter
// is initialized by the controller.
//
interface Param#(numeric type bits);

    method Bit#(bits) _read();

endinterface

//
// PARAM_DATA is the message sent along the dynamic parameter ring.  It comes
// in three parts to save wires:  first the parameter ID, then the high 32 bits,
// then the low 32 bits.
//
typedef union tagged
{
    PARAMS_DICT_TYPE PARAM_ID;
    Bit#(32) PARAM_High32;
    Bit#(32) PARAM_Low32;
}
    PARAM_DATA
           deriving (Eq, Bits);


//
// mkDynamicParameterNode --
//
// Every module with dynamic parameters must allocate at least one node to
// receive values.  The node is just a temporary holding point as values
// pass through.  Each individual parameter (see mkDynamicParameter below)
// connects to a ring stop and snoops for incoming updates.
//
module [Connected_Module] mkDynamicParameterNode
    //interface:
        (PARAMETER_NODE);

    // Ring connections
    Connection_Chain#(PARAM_DATA) chain <- mkConnection_Chain(`RINGID_PARAMS);
    Reg#(Bool) receiving <- mkReg(False);

    // Most recent param that came in on the ring
    Reg#(Bool) validParam <- mkReg(False);
    Reg#(PARAMS_DICT_TYPE) id <- mkReg(?);
    Reg#(Bit#(64)) value <- mkReg(?);
 
    // shift
    //
    // Normal rule for passing messages through the ring.  Look for the
    // right ID and switch to receiving mode if found.
    //
    rule shift (! receiving);
  
        PARAM_DATA param <- chain.recvFromPrev();
        chain.sendToNext(param);

        if (param matches tagged PARAM_ID .new_id)
        begin
            receiving <= True;
            id <= new_id;
            // validParam will be set true after the param value arrives
            validParam <= False;
        end

    endrule
  
    // getParam
    //
    // Get parameter, in parts.  When PARAM_Low32 comes through drop out of
    // receiving mode.
    //
    rule getParam (receiving);
  
        PARAM_DATA param <- chain.recvFromPrev();

        // Forward the data around the ring in case there are multiple readers
        // of the same parameter.
        chain.sendToNext(param);

        case (param) matches
            tagged PARAM_High32 .high32:
            begin
                // High part comes first.
                value[63:32] <= high32;
            end

            tagged PARAM_Low32 .low32:
            begin
                // Low part comes last.  Param is now valid.
                value[31:0] <= low32;
                validParam <= True;
                receiving <= False;
            end
        endcase

    endrule

    // checkForNewValue
    //
    // Individual parameters invoke this method every cycle to receive param
    // values.
    //
    method Maybe#(Bit#(64)) checkForNewValue(PARAMS_DICT_TYPE myID);
        if (validParam && id == myID)
            return tagged Valid value;
        else
            return tagged Invalid;
    endmethod

endmodule


//
// mkDynamicParameter --
//
// Object for an individual parameter.
//
module [Connected_Module] mkDynamicParameter#(PARAMS_DICT_TYPE myID, PARAMETER_NODE paramNode)
    //interface:
        (Param#(bits)) provisos (Add#(a__, bits, 64));

    Reg#(Maybe#(Bit#(bits))) value <- mkReg(tagged Invalid);

    // setValue
    //
    // Monitor the parameter node and update the parameter when it comes through.
    //
    rule setValue(True);
        if (paramNode.checkForNewValue(myID) matches tagged Valid .v)
        begin
            value <= tagged Valid truncate(v);
        end
    endrule

    method Bit#(bits) _read() if (value matches tagged Valid .v);
        return v;
    endmethod

endmodule

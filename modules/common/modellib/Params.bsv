//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
//

//
// @file Params.bsv
// @brief Dynamic parameters.  The size of the parameter is an argument to
//        the declaration.  Parameters may be at most 64 bits.
//
// @author Michael Adler
//

`include "asim/dict/RINGID.bsh"
`include "asim/dict/PARAMS.bsh"

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


module [Connected_Module] mkDynamicParameter#(PARAMS_DICT_TYPE myID)
  //interface:
              (Param#(bits)) provisos (Add#(a__, bits, 64));

    Connection_Chain#(PARAM_DATA) chain <- mkConnection_Chain(`RINGID_PARAMS);
    Reg#(Bool) receiving <- mkReg(False);
    Reg#(Bit#(bits)) value <- mkReg(0);
 
    // shift
    //
    // Normal rule for passing messages through the ring.  Look for the
    // right ID and switch to receiving mode if found.
    //
    rule shift (! receiving);
  
        PARAM_DATA param <- chain.receive_from_prev();
        chain.send_to_next(param);

        if (param matches tagged PARAM_ID .id &&& id == myID)
        begin
            receiving <= True;
        end

    endrule
  
    // getParam
    //
    // Get parameter, in parts.  When PARAM_Low32 comes through drop out of
    // receiving mode.
    //
    rule getParam (receiving);
  
        PARAM_DATA param <- chain.receive_from_prev();

        // Forward the data around the ring in case there are multiple readers
        // of the same parameter.
        chain.send_to_next(param);

        case (param) matches
            tagged PARAM_High32 .high32:
            begin
                // High part comes first.  Clear low part at the same time.
                let val = {high32, 32'b0};
                value <= truncate(val);
            end

            tagged PARAM_Low32 .low32:
            begin
                // Preserve high part.  Low part is guaranteed to be 0, so or
                // is safe.
                Bit#(64) val = {32'b0, low32};
                val = val | zeroExtend(value);
                value <= truncate(val);
                receiving <= False;
            end
        endcase

    endrule

    method Bit#(bits) _read() = value;

endmodule

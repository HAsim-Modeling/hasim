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
// Data structure declarations for communication with an ISA-specific ALU.
// Each ISA must have an implementation of an ALU passing these types.
//
// The functional partition's register state manager communicates with the
// ISA-specific ALU as a client of the "isa_datapath" soft connection.
//

import Vector::*;

// FUNCP_ISA_DATAPATH_EXCEPTIONS

typedef enum
{
    FUNCP_ISA_EXCEPT_NONE,
    FUNCP_ISA_EXCEPT_ILLEGAL_INSTR
}
    FUNCP_ISA_DATAPATH_EXCEPTIONS
        deriving (Eq, Bits);


// FUNCP_ISA_DATAPATH_REQ

typedef struct
{
    TOKEN             token;
    ISA_INSTRUCTION   instruction;
    ISA_ADDRESS       instAddress;
    ISA_INST_DSTS     instDstPhysRegs;    // Destination physical registers
}
FUNCP_ISA_DATAPATH_REQ
    deriving (Eq, Bits);

function FUNCP_ISA_DATAPATH_REQ initISADatapathReq(TOKEN tok,
                                                   ISA_INSTRUCTION i,
                                                   ISA_ADDRESS pc,
                                                   ISA_INST_DSTS dstPrs);
    return FUNCP_ISA_DATAPATH_REQ
            {
                token: tok,
                instruction: i,
                instAddress: pc,
                instDstPhysRegs: dstPrs
            };
endfunction


// FUNCP_ISA_DATAPATH_SRCVALS

typedef struct
{
    ISA_SOURCE_VALUES srcValues;
}
    FUNCP_ISA_DATAPATH_SRCVALS
        deriving (Eq, Bits);

function FUNCP_ISA_DATAPATH_SRCVALS initISADatapathSrcVals(ISA_SOURCE_VALUES sr);

    return FUNCP_ISA_DATAPATH_SRCVALS
            {
                srcValues:   sr
            };

endfunction



// FUNCP_ISA_EXECUTION_RESULT

// A struct of possible execution results that the timing model should know about.
// Returned by the getResults() operation, which obtains it from the ISA datapath.

typedef union tagged
{
    ISA_ADDRESS RBranchTaken;    //Branch was taken to this Addr
    ISA_ADDRESS RBranchNotTaken; //Branch was not taken
    ISA_ADDRESS REffectiveAddr;  //Load/Store effective address for DCache
    void        RNop;            //ALU op with no interesting data
    Bool        RTerminate;      //End the run if this instruction commits. Bool is pass/fail.
}
FUNCP_ISA_EXECUTION_RESULT 
    deriving (Eq, Bits);


//
// FUNCP_ISA_DATAPATH_RSP --
//     Response from the ISA datapath describes exceptions and control flow
//     for the timing partition.  It does not include register writebacks!
//     These are returned on a separate pipeline, allowing the timing model
//     to procede even when datapath computation is slow.  The functional
//     model will block on register use.
//
typedef struct
{
    FUNCP_ISA_DATAPATH_EXCEPTIONS except;       // Exceptions
    FUNCP_ISA_EXECUTION_RESULT    timepResult;  // Result to give to the timing partition.
}
FUNCP_ISA_DATAPATH_RSP
    deriving (Eq, Bits);

function FUNCP_ISA_DATAPATH_RSP initISADatapathRsp(FUNCP_ISA_DATAPATH_EXCEPTIONS except,
                                                   FUNCP_ISA_EXECUTION_RESULT r);
    return FUNCP_ISA_DATAPATH_RSP { except: except, timepResult: r };
endfunction

function FUNCP_ISA_DATAPATH_RSP initISADatapathRspOp(FUNCP_ISA_EXECUTION_RESULT r);
    return initISADatapathRsp(FUNCP_ISA_EXCEPT_NONE, r);
endfunction

function FUNCP_ISA_DATAPATH_RSP initISADatapathRspException(FUNCP_ISA_DATAPATH_EXCEPTIONS except);
    return initISADatapathRsp(except, tagged RNop);
endfunction

function FUNCP_ISA_DATAPATH_RSP initISADatapathRspNop();
    return initISADatapathRsp(FUNCP_ISA_EXCEPT_NONE, tagged RNop);
endfunction


//
// FUNCP_ISA_WRITEBACK
//
// Register writes are flow from the ISA datapath to the register state manager
// in a separate pipeline from the execution server request/response.
// Writebacks are tagged with the token and physical register target so no
// bookkeeping is required in the register state manager.
//
// The physical register has a valid bit so a "tokDone" message can be sent even
// when an instruction writes no registers.
//
typedef struct
{
    TOKEN token;
    Maybe#(FUNCP_PHYSICAL_REG_INDEX) physDst;
    ISA_VALUE value;
    Bool tokDone;               // Last write for token
}
FUNCP_ISA_WRITEBACK
    deriving (Eq, Bits);

function FUNCP_ISA_WRITEBACK initISAWriteback(TOKEN tok,
                                              Maybe#(FUNCP_PHYSICAL_REG_INDEX) pr,
                                              ISA_VALUE val,
                                              Bool done);
    return FUNCP_ISA_WRITEBACK { token: tok, physDst: pr, value: val, tokDone: done };
endfunction

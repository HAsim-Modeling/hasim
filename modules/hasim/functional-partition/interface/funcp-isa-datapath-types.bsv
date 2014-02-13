//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
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

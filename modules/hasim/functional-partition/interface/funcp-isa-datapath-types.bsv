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
    ISA_INSTRUCTION   instruction;
    ISA_ADDRESS       instAddress;
    ISA_SOURCE_VALUES srcValues;
}
    FUNCP_ISA_DATAPATH_REQ
        deriving (Eq, Bits);

function FUNCP_ISA_DATAPATH_REQ initISADatapathReq(ISA_INSTRUCTION i, ISA_ADDRESS pc, ISA_SOURCE_VALUES sr);

    return FUNCP_ISA_DATAPATH_REQ
            {
                instruction: i,
                instAddress: pc,
                srcValues:   sr
            };

endfunction

// FUNCP_ISA_DATAPATH_RSP

typedef struct
{
    FUNCP_ISA_DATAPATH_EXCEPTIONS except;       // Exceptions
    FUNCP_ISA_EXECUTION_RESULT    timepResult;  // Result to give to the timing partition.
    ISA_ADDRESS                   memAddress;   // Address for Loads/Stores
    Bool                          isStore;      // If it's a store, writebacks[0] should be the store value.
    ISA_RESULT_VALUES             writebacks;   // Values to write back to the registers.
}
    FUNCP_ISA_DATAPATH_RSP
        deriving (Eq, Bits);

function FUNCP_ISA_DATAPATH_RSP initISADatapathRsp(FUNCP_ISA_DATAPATH_EXCEPTIONS except,
                                                   FUNCP_ISA_EXECUTION_RESULT r,
                                                   ISA_ADDRESS a,
                                                   Bool isSt,
                                                   ISA_RESULT_VALUES wr);

    return FUNCP_ISA_DATAPATH_RSP
            {
                except: except,
                timepResult: r,
                memAddress:  a,
                isStore:     isSt,
                writebacks:  wr
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
    deriving 
            (Eq, Bits);
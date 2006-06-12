///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// ToyMIPS.bsv                                                               //
//                                                                           //
// Top-level datatypes for the ToyMIPS example ISA.                          //
//                                                                           //
// This file will be included by most ToyMIPs-specific modules.              //
//                                                                           //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

import GetPut::*;
import ClientServer::*;
import RegFile::*;

//************ Basic Datatypes ************//

typedef Bit#(8)  Token;
typedef Bit#(32) Tick;
typedef Bit#(8)  Addr;
typedef Bit#(5)  RName;
typedef Bit#(6)  PRName;
typedef Bit#(32) Value;
typedef Bit#(4)  SnapshotPtr;

//For convenience

RName r0 = 0;
RName r1 = 1;
RName r2 = 2; 
RName r3 = 3;
RName r4 = 4;
RName r5 = 5;
RName r6 = 6;
RName r7 = 7;
RName r8 = 8;
RName r9 = 9;



/************* Functional Partition Datatypes *************/


//Unpacked ISA representation

//Fetch-->Decode
typedef union tagged 
{
  struct {RName dest; RName src1; RName src2;     } IAdd;
  struct {RName dest; RName src1; RName src2;     } ISub;
  struct {RName cond; RName addr;                 } IBz;
  struct {RName dest; RName idx;  Bit#(5) offset; } ILoad;
  struct {RName dest; Bit#(10)  imm;              } ILoadImm;
  struct {RName src;  RName idx;  Bit#(5) offset; } IStore;
  void                                              ITerminate;
}
  Inst 
      deriving
              (Eq, Bits);

//Decoded Instruction

//Decode-->Exec               
typedef union tagged 
{
  struct {PRName pdest;  PRName opdest; PRName op1;     PRName op2;     } DAdd;
  struct {PRName pdest;  PRName opdest; PRName op1;     PRName op2;     } DSub;
  struct {PRName opdest; PRName cond;	PRName addr;	   	        } DBz;     
  struct {PRName pdest;  PRName opdest; PRName idx;     Bit#(6) offset; } DLoad;
  struct {PRName pdest;  PRName opdest; Bit#(12) value;		        } DLoadImm;
  struct {PRName value;  PRName opdest; PRName idx;     Bit#(6) offset; } DStore;
  void                                                                    DTerminate;
}
  DecodedInst 
    deriving 
            (Eq, Bits);

//Executed Instruction

//Possibly should include branch info if Functional Partition has branch predictor

//Exec-->Mem-->LCom-->GCom
typedef union tagged 
{
  struct {PRName pdest; PRName opdest;                             } EWB;
  struct {PRName opdest;					   } ENop;
  struct {PRName idx; Bit#(6) offset; PRName pdest; PRName opdest; } ELoad;
  struct {PRName idx; Bit#(6) offset; PRName val;   PRName opdest; } EStore;
  void                                                               ETerminate;
}
  ExecedInst
     deriving 
             (Eq, Bits);



/************* Timing Partition Datatypes *************/


//Dependency info for Timing Partition

//FP Decode-->TP
typedef struct 
{
  Maybe#(Tuple2#(RName, PRName)) dep_dest;
  Maybe#(Tuple2#(RName, PRName)) dep_src1;  	 
  Maybe#(Tuple2#(RName, PRName)) dep_src2;
}
  DepInfo 
    deriving
            (Eq, Bits);

//Result of executing an instruction

//FP Exec-->TP
typedef union tagged
{
  Addr RBranchTaken;
  void RBranchNotTaken; // Possibly should also include address. 
  void RNop;
  void RTerminate;
}
  InstResult 
     deriving 
             (Eq, Bits);



typedef union tagged
{
  void         COM_LoadState;
  void         COM_RunProgram;  //Perhaps pass in global tick here
  void         COM_CheckResult;
  ModelCommand COM_Other;
}
  Command 
                deriving 
		        (Eq, Bits);

		
typedef union tagged
{
  void                                            RESP_DoneLoading;
  void                                            RESP_DoneRunning; //Perhaps return local tick here	 
  struct {Addr addr; Value exp_v; Value found_v;} RESP_Failure;
  void                                            RESP_CheckPassed;
  void                                            RESP_CheckFailed;
  ModelResponse                                   RESP_Other;
}
  Response
                 deriving
		         (Eq, Bits);

//Commands specific to this model

typedef void ModelCommand;
typedef void ModelResponse;

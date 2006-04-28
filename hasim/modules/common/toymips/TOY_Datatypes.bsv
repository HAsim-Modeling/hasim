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

/************* Basic Datatypes *************/

typedef Bit#(8)  TOY_Token;
typedef Bit#(32) TOY_Tick;
typedef Bit#(8)  TOY_Addr;
typedef Bit#(5)  TOY_RName;
typedef Bit#(6)  TOY_PRName;
typedef Bit#(32) TOY_Value;
typedef Bit#(4)  TOY_SnapshotPtr;

//For convenience

TOY_RName r0 = 0;
TOY_RName r1 = 1;
TOY_RName r2 = 2; 
TOY_RName r3 = 3;
TOY_RName r4 = 4;
TOY_RName r5 = 5;
TOY_RName r6 = 6;
TOY_RName r7 = 7;
TOY_RName r8 = 8;
TOY_RName r9 = 9;



/************* Functional Partition Datatypes *************/


//Unpacked ISA representation

//Fetch-->Decode
typedef union tagged 
{
  struct {TOY_RName dest; TOY_RName src1; TOY_RName src2;} IAdd;
  struct {TOY_RName dest; TOY_RName src1; TOY_RName src2;} ISub;
  struct {TOY_RName cond; TOY_RName addr;                } IBz;
  struct {TOY_RName dest; TOY_RName idx;  Bit#(5) offset;} ILoad;
  struct {TOY_RName dest; Bit#(10)  imm;                 } ILoadImm;
  struct {TOY_RName src;  TOY_RName idx;  Bit#(5) offset;} IStore;
  void                                                     ITerminate;
}
  TOY_Inst 
    deriving
            (Eq, Bits);

//Decoded Instruction

//Decode-->Exec               
typedef union tagged 
{
  struct {TOY_PRName pdest;  TOY_PRName opdest; TOY_PRName op1;   TOY_PRName op2;} DAdd;
  struct {TOY_PRName pdest;  TOY_PRName opdest; TOY_PRName op1;   TOY_PRName op2;} DSub;
  struct {TOY_PRName opdest; TOY_PRName cond;	TOY_PRName addr;		 } DBz;     
  struct {TOY_PRName pdest;  TOY_PRName opdest; TOY_PRName idx;   Bit#(6) offset;} DLoad;
  struct {TOY_PRName pdest;  TOY_PRName opdest; Bit#(12)   value;		 } DLoadImm;
  struct {TOY_PRName value;  TOY_PRName opdest; TOY_PRName idx;   Bit#(6) offset;} DStore;
  void                                                                             DTerminate;
}
  TOY_DecodedInst 
    deriving 
            (Eq, Bits);

//Executed Instruction

//Possibly should include branch info if Functional Partition has branch predictor

//Exec-->Mem-->LCom-->GCom
typedef union tagged 
{
  struct {TOY_PRName pdest; TOY_PRName opdest;                                } EWB;
  struct {TOY_PRName opdest;						      } ENop;
  struct {TOY_PRName idx; Bit#(6) offset; TOY_PRName pdest; TOY_PRName opdest;} ELoad;
  struct {TOY_PRName idx; Bit#(6) offset; TOY_PRName val;   TOY_PRName opdest;} EStore;
  void                                                                          ETerminate;
}
  TOY_ExecedInst
    deriving 
            (Eq, Bits);



/************* Timing Partition Datatypes *************/


//Dependency info for Timing Partition

//FP Decode-->TP
typedef struct 
{
  Maybe#(Tuple2#(TOY_RName, TOY_PRName)) dep_dest;
  Maybe#(Tuple2#(TOY_RName, TOY_PRName)) dep_src1;  	 
  Maybe#(Tuple2#(TOY_RName, TOY_PRName)) dep_src2;
}
  TOY_DepInfo 
    deriving
            (Eq, Bits);

//Result of executing an instruction

//FP Exec-->TP
typedef union tagged
{
  TOY_Addr RBranchTaken;
  void     RBranchNotTaken; // Possibly should also include address. 
  void     RNop;
  void     RTerminate;
}
  TOY_InstResult 
    deriving 
            (Eq, Bits);


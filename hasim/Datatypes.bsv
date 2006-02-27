
import GetPut::*;
import ClientServer::*;
import RegFile::*;

//Basic datatypes

typedef Bit#(8) Token;
typedef Bit#(64) Tick;
typedef Bit#(8)  Addr;
typedef Bit#(5)   RName;
typedef Bit#(6)   PRName;
typedef Bit#(32)  Value;

//Instructions

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

//Fetch-->Decode
typedef union tagged {
 struct {RName dest; RName src1; RName src2;}    IAdd;
 struct {RName dest; RName src1; RName src2;}    ISub;
 struct {RName cond; RName addr;}                IBz;
 struct {RName dest; RName idx; Bit#(5) offset;} ILoad;
 struct {RName dest; Bit#(10) imm;}              ILoadImm;
 struct {RName src;  RName idx; Bit#(5) offset;} IStore;
 RName                                           ITerminate; //Terminate and return value in RName
}
 Inst deriving (Eq,Bits);

//Decode-->Exec               
typedef union tagged {
  struct       {PRName pdest; PRName opdest; PRName op1; PRName op2;  }   DAdd;
  struct       {PRName pdest; PRName opdest; PRName op1; PRName op2;  }   DSub;
  struct       {              PRName opdest; PRName cond;PRName addr; }   DBz;     
  struct       {PRName pdest; PRName opdest; PRName idx; Bit#(6) offset;} DLoad;
  struct       {PRName pdest; PRName opdest;   Bit#(12)  value;}          DLoadImm;
  struct       {PRName value; PRName opdest; PRName idx; Bit#(6) offset;} DStore;
  PRName                                                                  DTerminate;
}
  DecodedInst deriving (Eq,Bits);

//Exec-->Mem-->LCom-->GCom
typedef union tagged {
  struct {                            PRName pdest; PRName opdest;}  EWB;
  struct {                                          PRName opdest;}  ENop;
  struct { PRName idx; Bit#(6) offset;PRName pdest; PRName opdest;}  ELoad;
  struct { PRName idx; Bit#(6) offset;PRName val;   PRName opdest;}  EStore;
  PRName                                                             ETerminate;
}
  ExecedInst deriving (Eq,Bits);

//Decode-->TP
typedef struct {
  Maybe#(Tuple2#(RName, PRName))  dest;
  Maybe#(Tuple2#(RName, PRName))  src1;           
  Maybe#(Tuple2#(RName, PRName))  src2;
}
  DepInfo deriving(Eq,Bits);
       
//Exec-->TP
typedef union tagged {
  Addr RBranchTaken;
  void RBranchNotTaken;
  void RNop;
  void RTerminate;
}
  InstResult deriving (Eq, Bits);

//Memory

typedef union tagged {
  struct {             addr_T addr; token_T token;} Ld;
  struct {value_T val; addr_T addr; token_T token;} St;
}
  MemReq#(parameter type addr_T,
          parameter type token_T,
	  parameter type value_T) 
    deriving
            (Eq,Bits);

typedef union tagged {
  value_T LdResp;
  void  StResp;
}
  MemResp#(parameter type value_T) 
    deriving
            (Eq, Bits);

interface Memory#(type addr_T,  type inst_T, type value_T, type token_T);

  interface Server#(addr_T, inst_T) imem;
  interface Server#(MemReq#(addr_T, token_T, value_T), MemResp#(value_T)) dmem;
  
  method Action        commit(token_T token);
  method Action        killRange(token_T lb, token_T ub); 
  
  //Magic link for the test harness to load the program
  interface RegFile#(addr_T, inst_T) magic_imem;
  interface RegFile#(addr_T, value_T) magic_dmem;

endinterface

//Functional Partition

interface FP_Unit#(type tick_T, type token_T,  type init_T, type req_T, type resp_T, type next_T);

  interface Put#(Tuple2#(token_T, init_T)) in;
  
  interface Server#(Tuple3#(token_T, req_T, tick_T), Tuple2#(token_T, resp_T)) server;
 
  interface Get#(Tuple2#(token_T, next_T)) out;
 
  method Action                                     killToken(token_T t);

endinterface

typedef Server#(Tuple3#(token_T, init_T, req_T), Tuple3#(token_T, resp_T, next_T))
        Unit#(type token_T, type init_T, type req_T, type resp_T, type next_T);

interface BypassUnit#(type vreg_T, type preg_T, type value_T, type token_T);
  // first is new pointer, second is old. if no new pointer, the first will be undefined
  method ActionValue#(Tuple2#(preg_T,preg_T)) makeMapping(Maybe#(vreg_T) x, token_T tok,Bool snapshot); //token is the ref name
  method preg_T lookup1(vreg_T v);
  method preg_T lookup2(vreg_T v);

  method Maybe#(value_T) read1(preg_T i);
  method Maybe#(value_T) read2(preg_T i);
  method Maybe#(value_T) read3(preg_T i);
  method Maybe#(value_T) read4(preg_T i);

  method Action write1(preg_T i, value_T v);
  method Action write2(preg_T i, value_T v);

  method Action freePReg(token_T tok, preg_T x);
  method Action rewindtoToken(token_T tok); // exception
endinterface



interface FunctionalPartition#(type tick_T, type token_T,
                               type fet_req_T, type fet_resp_T,
			       type dec_req_T, type dec_resp_T,
			       type exe_req_T, type exe_resp_T,
			       type mem_req_T, type mem_resp_T,
			       type lcm_req_T, type lcm_resp_T,
			       type gcm_req_T, type gcm_resp_T);

  interface Server#(Tuple3#(token_T, void, tick_T), Tuple2#(token_T, void))            tokgen;
  interface Server#(Tuple3#(token_T, fet_req_T, tick_T), Tuple2#(token_T, fet_resp_T)) fetch;
  interface Server#(Tuple3#(token_T, dec_req_T, tick_T), Tuple2#(token_T, dec_resp_T)) decode;
  interface Server#(Tuple3#(token_T, exe_req_T, tick_T), Tuple2#(token_T, exe_resp_T)) execute;
  interface Server#(Tuple3#(token_T, mem_req_T, tick_T), Tuple2#(token_T, mem_resp_T)) memory;
  interface Server#(Tuple3#(token_T, lcm_req_T, tick_T), Tuple2#(token_T, lcm_resp_T)) local_commit;
  interface Server#(Tuple3#(token_T, gcm_req_T, tick_T), Tuple2#(token_T, gcm_resp_T)) global_commit;
  
  method Action killToken(token_T t);
  
endinterface

//Timing Partition

interface TimingPartition;

  method Action start();
  method Bool done();

endinterface

//CPU

interface CPU;

  method Action start();
  method Bool done();

endinterface

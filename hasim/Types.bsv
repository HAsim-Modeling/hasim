
typedef UInt#(8) Token;
typedef UInt#(64) Tick;
typedef Bit#(32)  Addr;
typedef Bit#(5)   RName;
typedef Bit#(6)   PRName;
typedef Bit#(32)  Value;

typedef union tagged {
 struct {RName dest; RName src1; RName src2;}    IAdd;
 struct {RName dest; RName src1; RName src2;}    ISub;
 struct {RName cond; RName addr;}                IBz;
 struct {RName dest; RName idx; Bit#(5) offset;} ILoad;
 struct {RName dest; Bit#(10) imm;}              ILoadImm;
 struct {RName src;  RName idx; Bit#(5) offset;} IStore;
}
 Inst deriving (Eq,Bits);
                     
typedef union tagged {
  struct       {PRName pdest; PRName opdest; PRName op1; PRName op2;  } DAdd;
  struct       {PRName pdest; PRName opdest; PRName op1; PRName op2;  } DSub;
  struct       {              PRName opdest; PRName cond;PRName addr; } DBz;     
  struct       {PRName pdest; PRName opdest; PRName idx; Bit#(6) offset;} DLoad;
  struct       {PRName pdest; PRName opdest;   Bit#(12)  value;} DLoadImm;
  struct       {PRName value; PRName opdest; PRName idx; Bit#(6) offset;} DStore;
}
  DecodedInst deriving (Eq,Bits);

typedef struct {
  Maybe#(Tuple2#(RName, PRName))  dest;
  Maybe#(Tuple2#(RName, PRName))  src1;           
  Maybe#(Tuple2#(RName, PRName))  src2;
}
  DepInfo deriving(Eq,Bits);
       
typedef union tagged {
  struct {                            PRName pdest; PRName opdest;}  EWB;
  struct {                                          PRName opdest;}  ENop;
  struct { PRName idx; Bit#(6) offset;PRName pdest; PRName opdest;}  ELoad;
  struct { PRName idx; Bit#(6) offset;PRName val;   PRName opdest;}  EStore;
}
  ExecedInst deriving (Eq,Bits);

typedef union tagged {
  Addr RBranchTaken;
  void RBranchNotTaken;
  void RNop;
}
  InstResult deriving (Eq, Bits);

typedef union tagged {
  struct {           Addr addr; Token token;} Ld;
  struct {Value val; Addr addr; Token token;} St;
}
  MemReq deriving(Eq,Bits);

typedef union tagged {
  Value LdResp;
  void  StResp;
}
  MemResp deriving(Eq,Bits);

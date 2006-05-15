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

typedef Bit#(8)  SMIPS_Token;
typedef Bit#(32) SMIPS_Tick;
typedef Bit#(32) SMIPS_Addr;
typedef Bit#(5)  SMIPS_RName;
typedef Bit#(6)  SMIPS_PRName;
typedef Bit#(32) SMIPS_Value;
typedef Bit#(4)  SMIPS_SnapshotPtr;
typedef Bit#(16) SMIPS_SImm;
typedef Bit#(16) SMIPS_ZImm;
typedef Bit#(5)  SMIPS_ShAmt;
typedef Bit#(26) SMIPS_Target;
typedef Bit#(5)  SMIPS_CP0Index;

//For convenience

SMIPS_RName r0 = 0;
SMIPS_RName r1 = 1;
SMIPS_RName r2 = 2; 
SMIPS_RName r3 = 3;
SMIPS_RName r4 = 4;
SMIPS_RName r5 = 5;
SMIPS_RName r6 = 6;
SMIPS_RName r7 = 7;
SMIPS_RName r8 = 8;
SMIPS_RName r9 = 9;



/************* Functional Partition Datatypes *************/


//Unpacked ISA representation

typedef union tagged                
{

  struct { SMIPS_RName rbase; SMIPS_RName rdst;  SMIPS_SImm offset;  } LW;
  struct { SMIPS_RName rbase; SMIPS_RName rsrc;  SMIPS_SImm offset;  } SW; 

  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_SImm imm;     } ADDIU;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_SImm imm;     } SLTI;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_SImm imm;     } SLTIU;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_ZImm imm;     } ANDI;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_ZImm imm;     } ORI;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_ZImm imm;     } XORI;
  struct {                    SMIPS_RName rdst;  SMIPS_ZImm imm;     } LUI;

  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_ShAmt shamt;  } SLL;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_ShAmt shamt;  } SRL;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_ShAmt shamt;  } SRA;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_RName rshamt; } SLLV;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_RName rshamt; } SRLV;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;  SMIPS_RName rshamt; } SRAV;
  struct { SMIPS_RName rsrc1; SMIPS_RName rsrc2; SMIPS_RName rdst;   } ADDU;
  struct { SMIPS_RName rsrc1; SMIPS_RName rsrc2; SMIPS_RName rdst;   } SUBU;
  struct { SMIPS_RName rsrc1; SMIPS_RName rsrc2; SMIPS_RName rdst;   } AND;
  struct { SMIPS_RName rsrc1; SMIPS_RName rsrc2; SMIPS_RName rdst;   } OR;
  struct { SMIPS_RName rsrc1; SMIPS_RName rsrc2; SMIPS_RName rdst;   } XOR;
  struct { SMIPS_RName rsrc1; SMIPS_RName rsrc2; SMIPS_RName rdst;   } NOR;
  struct { SMIPS_RName rsrc1; SMIPS_RName rsrc2; SMIPS_RName rdst;   } SLT;
  struct { SMIPS_RName rsrc1; SMIPS_RName rsrc2; SMIPS_RName rdst;   } SLTU;

  struct { SMIPS_Target target;                                      } J;
  struct { SMIPS_Target target;                                      } JAL;
  struct { SMIPS_RName rsrc;                                         } JR;
  struct { SMIPS_RName rsrc;  SMIPS_RName rdst;                      } JALR;
  struct { SMIPS_RName rsrc1; SMIPS_RName rsrc2; SMIPS_SImm offset;  } BEQ;
  struct { SMIPS_RName rsrc1; SMIPS_RName rsrc2; SMIPS_SImm offset;  } BNE;
  struct { SMIPS_RName rsrc;  SMIPS_SImm offset;                     } BLEZ;
  struct { SMIPS_RName rsrc;  SMIPS_SImm offset;                     } BGTZ;
  struct { SMIPS_RName rsrc;  SMIPS_SImm offset;                     } BLTZ;
  struct { SMIPS_RName rsrc;  SMIPS_SImm offset;                     } BGEZ;

  //struct { SMIPS_RName rdst;  SMIPS_CP0Index cop0src;                } MFC0;
  //struct { SMIPS_RName rsrc;  SMIPS_CP0Index cop0dst;                } MTC0; 

  void                                                                 ILLEGAL;

}
  SMIPS_Inst
    deriving (Eq);

Bit#(6) opFUNC  = 6'b000000;  Bit#(6) fcSLL   = 6'b000000;
Bit#(6) opRT    = 6'b000001;  Bit#(6) fcSRL   = 6'b000010;
Bit#(6) opRS    = 6'b010000;  Bit#(6) fcSRA   = 6'b000011;
                              Bit#(6) fcSLLV  = 6'b000100;
Bit#(6) opLW    = 6'b100011;  Bit#(6) fcSRLV  = 6'b000110;
Bit#(6) opSW    = 6'b101011;  Bit#(6) fcSRAV  = 6'b000111;
                              Bit#(6) fcADDU  = 6'b100001;
Bit#(6) opADDIU = 6'b001001;  Bit#(6) fcSUBU  = 6'b100011;
Bit#(6) opSLTI  = 6'b001010;  Bit#(6) fcAND   = 6'b100100;
Bit#(6) opSLTIU = 6'b001011;  Bit#(6) fcOR    = 6'b100101;
Bit#(6) opANDI  = 6'b001100;  Bit#(6) fcXOR   = 6'b100110;
Bit#(6) opORI   = 6'b001101;  Bit#(6) fcNOR   = 6'b100111;
Bit#(6) opXORI  = 6'b001110;  Bit#(6) fcSLT   = 6'b101010;
Bit#(6) opLUI   = 6'b001111;  Bit#(6) fcSLTU  = 6'b101011;

Bit#(6) opJ     = 6'b000010;
Bit#(6) opJAL   = 6'b000011;
Bit#(6) fcJR    = 6'b001000;
Bit#(6) fcJALR  = 6'b001001;
Bit#(6) opBEQ   = 6'b000100;
Bit#(6) opBNE   = 6'b000101;
Bit#(6) opBLEZ  = 6'b000110;
Bit#(6) opBGTZ  = 6'b000111;
Bit#(5) rtBLTZ  = 5'b00000;
Bit#(5) rtBGEZ  = 5'b00001;

Bit#(5) rsMFC0  = 5'b00000;
Bit#(5) rsMTC0  = 5'b00100;

instance Bits#(SMIPS_Inst,32);

  // Pack Function

  function Bit#(32) pack( SMIPS_Inst instr );

    case ( instr ) matches

      tagged LW    .it : return { opLW,    it.rbase, it.rdst,  it.offset };
      tagged SW    .it : return { opSW,    it.rbase, it.rsrc,  it.offset };

      tagged ADDIU .it : return { opADDIU, it.rsrc,  it.rdst,  it.imm                      }; 
      tagged SLTI  .it : return { opSLTI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged SLTIU .it : return { opSLTIU, it.rsrc,  it.rdst,  it.imm                      }; 
      tagged ANDI  .it : return { opANDI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged ORI   .it : return { opORI,   it.rsrc,  it.rdst,  it.imm                      }; 
      tagged XORI  .it : return { opXORI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged LUI   .it : return { opLUI,   5'b0,     it.rdst,  it.imm                      };

      tagged SLL   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSLL  }; 
      tagged SRL   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSRL  }; 
      tagged SRA   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSRA  }; 

      tagged SLLV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSLLV }; 
      tagged SRLV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSRLV }; 
      tagged SRAV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSRAV }; 

      tagged ADDU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcADDU }; 
      tagged SUBU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSUBU }; 
      tagged AND   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcAND  }; 
      tagged OR    .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcOR   }; 
      tagged XOR   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcXOR  }; 
      tagged NOR   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcNOR  }; 
      tagged SLT   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSLT  }; 
      tagged SLTU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSLTU }; 

      tagged J     .it : return { opJ,     it.target                                       }; 
      tagged JAL   .it : return { opJAL,   it.target                                       }; 
      tagged JR    .it : return { opFUNC,  it.rsrc,  5'b0,     5'b0,      5'b0,     fcJR   };
      tagged JALR  .it : return { opFUNC,  it.rsrc,  5'b0,     it.rdst,   5'b0,     fcJALR };
      tagged BEQ   .it : return { opBEQ,   it.rsrc1, it.rsrc2, it.offset                   }; 
      tagged BNE   .it : return { opBNE,   it.rsrc1, it.rsrc2, it.offset                   }; 
      tagged BLEZ  .it : return { opBLEZ,  it.rsrc,  5'b0,     it.offset                   }; 
      tagged BGTZ  .it : return { opBGTZ,  it.rsrc,  5'b0,     it.offset                   }; 
      tagged BLTZ  .it : return { opRT,    it.rsrc,  rtBLTZ,   it.offset                   }; 
      tagged BGEZ  .it : return { opRT,    it.rsrc,  rtBGEZ,   it.offset                   }; 

      //tagged MFC0  .it : return { opRS,    rsMFC0,   it.rdst,  it.cop0src, 11'b0           }; 
      //tagged MTC0  .it : return { opRS,    rsMTC0,   it.rsrc,  it.cop0dst, 11'b0           };  

    endcase

  endfunction

  // Unpack Function

  function SMIPS_Inst unpack( Bit#(32) instrBits );

    let opcode = instrBits[ 31 : 26 ];
    let rs     = instrBits[ 25 : 21 ];
    let rt     = instrBits[ 20 : 16 ];
    let rd     = instrBits[ 15 : 11 ];
    let shamt  = instrBits[ 10 :  6 ];
    let funct  = instrBits[  5 :  0 ];
    let imm    = instrBits[ 15 :  0 ];
    let target = instrBits[ 25 :  0 ];

    case ( opcode )

      opLW        : return LW    { rbase:rs, rdst:rt,  offset:imm  };
      opSW        : return SW    { rbase:rs, rsrc:rt,  offset:imm  };
      opADDIU     : return ADDIU { rsrc:rs,  rdst:rt,  imm:imm     };
      opSLTI      : return SLTI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opSLTIU     : return SLTIU { rsrc:rs,  rdst:rt,  imm:imm     };
      opANDI      : return ANDI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opORI       : return ORI   { rsrc:rs,  rdst:rt,  imm:imm     };
      opXORI      : return XORI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opLUI       : return LUI   {           rdst:rt,  imm:imm     };
      opJ         : return J     { target:target                   };
      opJAL       : return JAL   { target:target                   };
      opBEQ       : return BEQ   { rsrc1:rs, rsrc2:rt, offset:imm  };
      opBNE       : return BNE   { rsrc1:rs, rsrc2:rt, offset:imm  };
      opBLEZ      : return BLEZ  { rsrc:rs,  offset:imm            };
      opBGTZ      : return BGTZ  { rsrc:rs,  offset:imm            };

      opFUNC  : 
        case ( funct )
          fcSLL   : return SLL   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSRL   : return SRL   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSRA   : return SRA   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSLLV  : return SLLV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcSRLV  : return SRLV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcSRAV  : return SRAV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcADDU  : return ADDU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcSUBU  : return SUBU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcAND   : return AND   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcOR    : return OR    { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcXOR   : return XOR   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcNOR   : return NOR   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcSLT   : return SLT   { rsrc1:rs, rsrc2:rt, rdst:rd     }; 
          fcSLTU  : return SLTU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcJR    : return JR    { rsrc:rs                         };
          fcJALR  : return JALR  { rsrc:rs,  rdst:rd               };
          default : return ILLEGAL;
        endcase

      opRT : 
        case ( rt )
          rtBLTZ  : return BLTZ  { rsrc:rs,  offset:imm            };
          rtBGEZ  : return BGEZ  { rsrc:rs,  offset:imm            };
          default : return ILLEGAL;
        endcase

      opRS : 
        case ( rs )
          //rsMFC0  : return MFC0  { rdst:rt,  cop0src:rd            };
          //rsMTC0  : return MTC0  { rsrc:rt,  cop0dst:rd            };
          default : return ILLEGAL;
        endcase

      default : return ILLEGAL;
      
    endcase

  endfunction

endinstance


//Decoded Instruction
typedef union tagged                
{

  struct { SMIPS_PRName pbase; SMIPS_PRName pdst;  SMIPS_SImm offset;	SMIPS_PRName opdst; } DLW;
  struct { SMIPS_PRName pbase; SMIPS_PRName psrc;  SMIPS_SImm offset;	SMIPS_PRName opdst; } DSW; 

  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_SImm imm;	SMIPS_PRName opdst; } DADDIU;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_SImm imm;	SMIPS_PRName opdst; } DSLTI;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_SImm imm;	SMIPS_PRName opdst; } DSLTIU;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_ZImm imm;	SMIPS_PRName opdst; } DANDI;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_ZImm imm;	SMIPS_PRName opdst; } DORI;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_ZImm imm;	SMIPS_PRName opdst; } DXORI;
  struct { 		       SMIPS_PRName pdst;  SMIPS_ZImm imm;	SMIPS_PRName opdst; } DLUI;

  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_ShAmt shamt;	SMIPS_PRName opdst; } DSLL;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_ShAmt shamt;	SMIPS_PRName opdst; } DSRL;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_ShAmt shamt;	SMIPS_PRName opdst; } DSRA;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_PRName pshamt; SMIPS_PRName opdst; } DSLLV;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_PRName pshamt; SMIPS_PRName opdst; } DSRLV;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;  SMIPS_PRName pshamt; SMIPS_PRName opdst; } DSRAV;
  struct { SMIPS_PRName psrc1; SMIPS_PRName psrc2; SMIPS_PRName pdst;	SMIPS_PRName opdst; } DADDU;
  struct { SMIPS_PRName psrc1; SMIPS_PRName psrc2; SMIPS_PRName pdst;	SMIPS_PRName opdst; } DSUBU;
  struct { SMIPS_PRName psrc1; SMIPS_PRName psrc2; SMIPS_PRName pdst;	SMIPS_PRName opdst; } DAND;
  struct { SMIPS_PRName psrc1; SMIPS_PRName psrc2; SMIPS_PRName pdst;	SMIPS_PRName opdst; } DOR;
  struct { SMIPS_PRName psrc1; SMIPS_PRName psrc2; SMIPS_PRName pdst;	SMIPS_PRName opdst; } DXOR;
  struct { SMIPS_PRName psrc1; SMIPS_PRName psrc2; SMIPS_PRName pdst;	SMIPS_PRName opdst; } DNOR;
  struct { SMIPS_PRName psrc1; SMIPS_PRName psrc2; SMIPS_PRName pdst;	SMIPS_PRName opdst; } DSLT;
  struct { SMIPS_PRName psrc1; SMIPS_PRName psrc2; SMIPS_PRName pdst;	SMIPS_PRName opdst; } DSLTU;

  struct { SMIPS_Target target; 					SMIPS_PRName opdst; } DJ;
  struct { SMIPS_PRName pdst;  SMIPS_Target target;			SMIPS_PRName opdst; } DJAL;
  struct { SMIPS_PRName psrc;						SMIPS_PRName opdst; } DJR;
  struct { SMIPS_PRName psrc;  SMIPS_PRName pdst;			SMIPS_PRName opdst; } DJALR;
  struct { SMIPS_PRName psrc1; SMIPS_PRName psrc2; SMIPS_SImm offset;	SMIPS_PRName opdst; } DBEQ;
  struct { SMIPS_PRName psrc1; SMIPS_PRName psrc2; SMIPS_SImm offset;	SMIPS_PRName opdst; } DBNE;
  struct { SMIPS_PRName psrc;  SMIPS_SImm offset;			SMIPS_PRName opdst; } DBLEZ;
  struct { SMIPS_PRName psrc;  SMIPS_SImm offset;			SMIPS_PRName opdst; } DBGTZ;
  struct { SMIPS_PRName psrc;  SMIPS_SImm offset;			SMIPS_PRName opdst; } DBLTZ;
  struct { SMIPS_PRName psrc;  SMIPS_SImm offset;			SMIPS_PRName opdst; } DBGEZ;

  //struct { SMIPS_PRName pdst;  SMIPS_CP0Index cop0src;  		SMIPS_PRName opdst; } DMFC0;
  //struct { SMIPS_PRName rsrc;  SMIPS_CP0Index cop0dst;  		SMIPS_PRName opdst; } DMTC0; 

  void                                                                                        DILLEGAL;

}
  SMIPS_DecodedInst
    deriving 
            (Eq, Bits);

//Executed Instruction

//Possibly should include branch info if Functional Partition has branch predictor

//Exec-->Mem-->LCom-->GCom
typedef union tagged 
{
  struct { SMIPS_Value val; SMIPS_PRName pdst; 	    SMIPS_PRName opdst; } EWB;
  //struct { SMIPS_Value val; SMIPS_CP0Index cop0dst; SMIPS_PRName opdst; } ECoProc;
  struct { SMIPS_Addr addr; SMIPS_PRName pdst;      SMIPS_PRName opdst; } ELoad;
  struct { SMIPS_Addr addr; SMIPS_Value  val;	    SMIPS_PRName opdst; } EStore;
  struct {		    		            SMIPS_PRName opdst; } ENop;
}
  SMIPS_ExecedInst
    deriving 
            (Eq, Bits);



/************* Timing Partition Datatypes *************/


//Dependency info for Timing Partition

//FP Decode-->TP
typedef struct 
{
  Maybe#(Tuple2#(SMIPS_RName, SMIPS_PRName)) dep_dest;
  Maybe#(Tuple2#(SMIPS_RName, SMIPS_PRName)) dep_src1;  	 
  Maybe#(Tuple2#(SMIPS_RName, SMIPS_PRName)) dep_src2;
}
  SMIPS_DepInfo 
    deriving
            (Eq, Bits);

//Result of executing an instruction

//FP Exec-->TP
typedef union tagged
{
  SMIPS_Addr RBranchTaken;
  void     RBranchNotTaken; // Possibly should also include address. 
  void     RNop;
}
  SMIPS_InstResult 
    deriving 
            (Eq, Bits);


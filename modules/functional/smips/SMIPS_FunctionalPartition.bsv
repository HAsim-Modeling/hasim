import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import Mem::*;
import Links::*;
import BypassUnit::*;
import FunctionalPartition::*;
import Debug::*;

import SMIPS_Datatypes::*;

`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"

// ToyMIPS is an extremely simple ISA designed to work as a proof of concept.
    

//-------------------------------------------------------------------------//
// Fetch Unit                                                              //
//-------------------------------------------------------------------------//

//mkSMIPS_Fetch :: IMem Link -> FP_Unit

module [Module] mkSMIPS_Fetch ();

  FIFO#(Tuple2#(SMIPS_Token, SMIPS_Addr)) waitingQ <- mkFIFO();

  //Links
  Link_Server#(Tuple3#(SMIPS_Token, void, SMIPS_Addr), 
               Tuple3#(SMIPS_Token, SMIPS_Inst, Tuple2#(SMIPS_Addr, SMIPS_Inst))) link_fet <- mkLink_Server("link_fet");
  
  Link_Client#(SMIPS_Addr, SMIPS_Inst) link_to_imem <- mkLink_Client("link_to_imem");

  //handleReq
  
  //Just pass the request on to the IMem

  rule handleFetch (True);
      
    let tup <- link_fet.getReq();
    match {.t, .*, .a} = tup;
    
    link_to_imem.makeReq(a);
    waitingQ.enq(tuple2(t, a));
    
  endrule

  //getMemResp
  
  //Just pass the response back from the IMem

  rule getMemResp (True);
  
    SMIPS_Inst resp <- link_to_imem.getResp();
    
    match {.tok, .addr} = waitingQ.first();
    waitingQ.deq();
    
    link_fet.makeResp(tuple3(tok, resp, tuple2(addr, resp)));
  endrule

endmodule


//-------------------------------------------------------------------------//
// Decode Stage                                                            //
//-------------------------------------------------------------------------//

// Also lookup physical register from BypassUnit

// mkSMIPS_Decode :: BypassUnit -> FP_Unit

module [Module] mkSMIPS_Decode#(BypassUnit#(SMIPS_RName, SMIPS_PRName, SMIPS_Value, SMIPS_Token, SMIPS_SnapshotPtr) bypass) ();
  
  //Links
  Link_Server#(Tuple3#(SMIPS_Token, Tuple2#(SMIPS_Addr, SMIPS_Inst), void), 
               Tuple3#(SMIPS_Token, SMIPS_DepInfo, Tuple2#(SMIPS_Addr, SMIPS_DecodedInst))) link_dec <- mkLink_Server("link_dec");
  
  FIFO#(Tuple3#(SMIPS_Token, SMIPS_DepInfo, Tuple2#(SMIPS_Addr, SMIPS_DecodedInst))) respQ <- mkFIFO();
  
  //Helper functions
  
  /*
  function Maybe#(SMIPS_RName) getOp1(Instr i);
     return case ( i ) matches

      // -- Memory Ops ------------------------------------------------      

      tagged LW .it : return Valid it.rbase;

      tagged SW .it : return Valid it.rbase;

      // -- Simple Ops ------------------------------------------------      

      tagged ADDIU .it : return Valid it.rsrc;
      tagged SLTI  .it : return Valid it.rsrc;
      tagged SLTIU .it : return Valid it.rsrc;
      tagged ANDI  .it : return Valid it.rsrc;
      tagged ORI   .it : return Valid it.rsrc;
      tagged XORI  .it : return Valid it.rsrc;
      tagged LUI   .it : return Invalid;

      tagged SLL   .it : return Valid it.rsrc;
      tagged SRL   .it : return Valid it.rsrc;
      tagged SRA   .it : return Valid it.rsrc;
      tagged SLLV  .it : return Valid it.rsrc;
      tagged SRLV  .it : return Valid it.rsrc;
      tagged SRAV  .it : return Valid it.rsrc;
      tagged ADDU  .it : return Valid it.rsrc1;
      tagged SUBU  .it : return Valid it.rsrc1;
      tagged AND   .it : return Valid it.rsrc1;
      tagged OR    .it : return Valid it.rsrc1;
      tagged XOR   .it : return Valid it.rsrc1;
      tagged NOR   .it : return Valid it.rsrc1;
      tagged SLT   .it : return Valid it.rsrc1;
      tagged SLTU  .it : return Valid it.rsrc1;

      tagged MTC0  .it : return Valid it.rsrc;
      tagged MFC0  .it : return Invalid;

      // -- Branches --------------------------------------------------

      tagged BLEZ  .it : return Valid it.rsrc;

      tagged BGTZ  .it : return Valid it.rsrc;

      tagged BLTZ  .it : return Valid it.rsrc;

      tagged BGEZ  .it : return Valid it.rsrc;

      tagged BEQ   .it : return Valid it.rsrc1;

      tagged BNE   .it : return Valid it.rsrc1;
      
      // -- Jumps -----------------------------------------------------
      
      tagged J     .it : return Invalid;
      
      tagged JR    .it : return Invalid;

      tagged JAL   .it : return Invalid;

      tagged JALR  .it : return Valid it.rsrc;
      default:           return Invalid;
    endcase;
  endfunction
  
  function Maybe#(Rindx) getOp2(Instr i);
    return case ( i ) matches

      tagged SW    .it : return Valid it.rsrc;
      tagged ADDU  .it : return Valid it.rsrc2;
      tagged SUBU  .it : return Valid it.rsrc2;
      tagged AND   .it : return Valid it.rsrc2;
      tagged OR    .it : return Valid it.rsrc2;
      tagged XOR   .it : return Valid it.rsrc2;
      tagged NOR   .it : return Valid it.rsrc2;
      tagged SLT   .it : return Valid it.rsrc2;
      tagged SLTU  .it : return Valid it.rsrc2;

      tagged BEQ   .it : return Valid it.rsrc2;

      tagged BNE   .it : return Valid it.rsrc2;
      default:           return Invalid;
    endcase;
  endfunction
 */
  //handleDecode
  
  //Handles the actual decoding and register allocation
  
  rule handleDecode (True);
      
    Tuple3#(SMIPS_Token, Tuple2#(SMIPS_Addr, SMIPS_Inst), void) tup <- link_dec.getReq();
    
    match {.t, {.a, .inst}, .*} = tup;
    
    SMIPS_DepInfo depinfo = ?;
    SMIPS_DecodedInst decinst = ?;

    //Actually do the decode
    case (inst) matches
      // -- Memory Ops ------------------------------------------------      
      
      //Load Word
      tagged LW {rbase: .rb, rdst:.rd, offset: .off}: 
	begin
	  let prb  = bypass.lookup1(rb);

          let rtup <- bypass.makeMapping(Valid rd, t, False);
          match {.prd, .oprd} = rtup;

          decinst = DLW 
	            {
		      opdst:  oprd, 
		      pbase:  prb, 
		      pdst:   prd, 
		      offset: off
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rb, prb), 
		      dep_src2: Invalid
		    };

	end
	
      //Store Word
      tagged SW {rbase: .rb, rsrc: .rs, offset: .off}: 
        begin
	  let prb = bypass.lookup1(rb);
	  let prs = bypass.lookup2(rs);
	  
          let rtup <- bypass.makeMapping(Invalid, t, False);
          match {.prd, .oprd} = rtup;
	  
          decinst = DSW
	            {
		      opdst:  oprd, 
		      pbase:  prb, 
		      psrc:   prs, 
		      offset: off
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rb, prb), 
		      dep_src2: Valid tuple2(rs, prs)
		    };
        end

      // -- Simple Ops ------------------------------------------------      

      //Add Immediate Signed (Not a Typo)
      tagged ADDIU {rsrc: .rs, rdst: .rd, imm: .simm}:
        begin
	  
	  let prs  = bypass.lookup1(rs);
	  
          let rtup <- bypass.makeMapping(Valid rd, t, False);
          match {.prd, .oprd} = rtup;
	  
	  
          decinst = DADDIU 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   simm
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end
	
      //Set Less Than Immediate (Signed)
      tagged SLTI {rsrc: .rs, rdst: .rd, imm: .simm}:
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLTI 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   simm
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Set Less Than Immediate Unsigned 
      tagged SLTIU {rsrc: .rs, rdst: .rd, imm:.simm}:
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLTIU
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   simm //Immediate is still sign extended
		                              //Not a typo: Exec handles it differently
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //And Immediate
      tagged ANDI {rsrc: .rs, rdst: .rd, imm:.zimm}:
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DANDI 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   zimm
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Or Immediate
      tagged ORI {rsrc: .rs, rdst: .rd, imm:.zimm}:
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DORI 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   zimm
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //XOR Immediate
      tagged XORI {rsrc: .rs, rdst: .rd, imm:.zimm}:
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DXORI 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   zimm
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Load Unsigned Immediate (Really is unsigned)
      tagged LUI {rdst: .rd, imm:.zimm}:
        begin
	  	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DLUI 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      imm:   zimm
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Invalid,
		      dep_src2: Invalid
		    };
	end
	
      //Shift Left Logical (Immediate)
      tagged SLL {rsrc: .rs, rdst: .rd, shamt:.sha}: 
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLL 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      shamt: sha
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Shift Right Logical (Immediate)
      tagged SRL {rsrc: .rs, rdst: .rd, shamt:.sha}: 
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	 
          decinst = DSRL
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      shamt: sha
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Shift Right Arithmatic (Immediate)
      tagged SRA {rsrc: .rs, rdst: .rd, shamt:.sha}: 
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSRA
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      shamt: sha
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Shift Left Logical Variable
      tagged SLLV {rsrc: .rs, rdst: .rd, rshamt:.rsha}: 
        begin
	  
	  let prs   = bypass.lookup1(rs);
	  let prsha = bypass.lookup2(rsha);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLLV 
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc:   prs, 
		      pshamt: prsha
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Valid tuple2(rsha, prsha)
		    };
	end
	
      //Shift Right Logical Variable
      tagged SRLV {rsrc: .rs, rdst: .rd, rshamt:.rsha}: 
        begin
	  
	  let prs   = bypass.lookup1(rs);
	  let prsha = bypass.lookup2(rsha);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSRLV 
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc:   prs, 
		      pshamt: prsha
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Valid tuple2(rsha, prsha)
		    };
	end
	
      //Shift Right Arithmatic Variable
      tagged SRAV {rsrc: .rs, rdst: .rd, rshamt:.rsha}: 
        begin
	  
	  let prs   = bypass.lookup1(rs);
	  let prsha = bypass.lookup2(rsha);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSRAV 
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc:   prs, 
		      pshamt: prsha
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Valid tuple2(rsha, prsha)
		    };
	end
	
      //Add Unsigned
      tagged ADDU {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}: 
        begin
	  
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DADDU
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end

      //Subtract Unsigned
      tagged SUBU {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}:
        begin
	  
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSUBU 
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end
	
      //And
      tagged AND {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}:
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DAND
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end
      
      //OR
      tagged OR {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}:
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DOR
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end
	
      //XOR
      tagged XOR {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}:
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DXOR
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end

      //NOR
      tagged NOR {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}:
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DNOR
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end

      //Set Less Than
      tagged SLT {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}: 
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLT
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end
      
      //Set Less Than Unsigned
      tagged SLTU {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}: 
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLTU
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end
/*
      //Move To Coprocessor 0
      tagged MTC0 .rs .op: 
        begin
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DMTC0
	            {
		      opdst:   oprd, 
		      psrc:    prs,
		      cop0src: op
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
      
      //Move From Coprocessor 0
      tagged MFC0 .rd .op:
        begin 
	
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DMFC0
	            {
		      opdst:   oprd, 
		      pdst:    prd,
		      cop0dst: op
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Invalid, 
		      dep_src2: Invalid
		    };
	end
*/

      // -- Branches --------------------------------------------------
      
      //Branch if Less-Than or Equal to Zero
      tagged BLEZ {rsrc: .rs, offset: .off}:
        begin
	
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBLEZ
	            {
		      opdst:  oprd, 
		      psrc:   prs,
		      offset: off
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Greater Than Zero
      tagged BGTZ {rsrc: .rs, offset: .off}: 
        begin
	
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBGTZ
	            {
		      opdst:  oprd, 
		      psrc:   prs,
		      offset: off
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Less Than Zero
      tagged BLTZ {rsrc: .rs, offset: .off}: 
        begin
	
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBLTZ
	            {
		      opdst:  oprd, 
		      psrc:   prs,
		      offset: off
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Greater than or Equal to Zero
      tagged BGEZ {rsrc: .rs, offset: .off}: 
        begin
	
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBGEZ
	            {
		      opdst:  oprd, 
		      psrc:   prs,
		      offset: off
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Equal
      tagged BEQ {rsrc1: .rs1, rsrc2: .rs2, offset: .off}: 
        begin
	
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup1(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBEQ
	            {
		      opdst:  oprd, 
		      psrc1:  prs1,
		      psrc2:  prs2,
		      offset: off
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
        end

      //Branch if Not Equal
      tagged BNE {rsrc1: .rs1, rsrc2: .rs2, offset: .off}: 
        begin
	
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup1(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBNE
	            {
		      opdst:  oprd, 
		      psrc1:  prs1,
		      psrc2:  prs2,
		      offset: off
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
        end
      
      // -- Jumps -----------------------------------------------------

      //Jump
      tagged J {target: .targ}: 
        begin
		  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DJ
	            {
		      opdst:  oprd, 
		      target: targ
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Invalid, 
		      dep_src2: Invalid
		    };
        end
      
      //Jump Register
      tagged JR {rsrc: .rs}:
        begin
	
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DJR
	            {
		      opdst:  oprd, 
		      psrc:   prs
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end
      //Jump and Link (into archictectural register 31)
      tagged JAL {target: .targ}: 
        begin
		 
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid 5'd31, t, False);
	  
          decinst = DJAL
	            {
		      opdst:  oprd,
		      pdst:   prd, 
		      target: targ
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(5'd31, prd), 
		      dep_src1: Invalid, 
		      dep_src2: Invalid
		    };
        end

      //Jump and Link into Register
      tagged JALR {rsrc: .rs, rdst: .rd}: 
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DJALR
	            {
		      opdst:  oprd,
		      pdst:   prd,
		      psrc:   prs
		    };
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end

       // -- Illegal ---------------------------------------------------
 
      default: 
        begin
          decinst = DILLEGAL;
		    
          depinfo = SMIPS_DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Invalid, 
		      dep_src2: Invalid
		    };
        end

    endcase

    link_dec.makeResp(tuple3(t, depinfo, tuple2(a, decinst)));
    
  endrule
  
endmodule


//-------------------------------------------------------------------------//
// Execute Unit                                                            //
//-------------------------------------------------------------------------//
// 

module [Module] mkSMIPS_Execute#(BypassUnit#(SMIPS_RName, SMIPS_PRName, SMIPS_Value, SMIPS_Token, SMIPS_SnapshotPtr) bypass) ();

  //Links
  Link_Server#(Tuple3#(SMIPS_Token, Tuple2#(SMIPS_Addr, SMIPS_DecodedInst), void),
               Tuple3#(SMIPS_Token, SMIPS_InstResult, SMIPS_ExecedInst)) link_exe <- mkLink_Server("link_exe");
  
  //State elements
  FIFO#(Tuple3#(SMIPS_Token, Tuple2#(SMIPS_Addr, SMIPS_DecodedInst), void)) waitingQ <- mkFIFO();
  
  //handleExec
  
  //We can't always exec right away, since our operands may not be available.
   
  rule handleExec (True);
  
    debug_rule("handleExec");

    let tup <- link_exe.getReq();
    waitingQ.enq(tup);

  endrule
  
  //execute

  rule execute (True);
  
    debug_rule("execute");

    match {.t, {.addr, .dec}, .*} = waitingQ.first();

    SMIPS_InstResult res = ?;
    SMIPS_ExecedInst einst = ?;
    Bool done = False;
    Maybe#(SMIPS_Addr) branchResult = Invalid;

    //Actually do the execute
    case (dec) matches
      // -- Memory Ops ------------------------------------------------      
      
      //Load Word
      tagged DLW {pbase: .rb, pdst: .rd, offset: .off, opdst: .opd}: 
	begin
	
	  Maybe#(SMIPS_Value) mvb = bypass.read1(rb);

          done  = isJust(mvb);
	  res   = RNop;
	  einst = ELoad 
	          {
		    addr:   unJust(mvb) + signExtend(off), 
		    pdst:   rd, 
		    opdst:  opd
		  };
	end
	
      //Store Word
      tagged DSW {pbase: .rb, psrc: .rs, offset: .off, opdst: .opd}: 
	begin
	
	  Maybe#(SMIPS_Value) mvb = bypass.read1(rb);
	  Maybe#(SMIPS_Value) mvs = bypass.read2(rs);

          done  = isJust(mvb) && isJust(mvs);
	  res   = RNop;
	  einst = EStore
	          {
		    addr:   unJust(mvb) + signExtend(off), 
		    val:    unJust(mvs), 
		    opdst:  opd
		  };
	end

      // -- Simple Ops ------------------------------------------------      

      //Add Immediate Unsigned 
      //Actually the numbers are sign extended, it just can't overflow
      tagged DADDIU {psrc: .rs, pdst: .rd, imm:.simm, opdst: .opd}: 
	begin
	
	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          done  = isJust(mvs);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs) + signExtend(simm),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Set Less Than Immediate (Signed)
      tagged DSLTI {psrc: .rs, pdst: .rd, imm:.simm, opdst: .opd}: 
	begin
	
	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          done  = isJust(mvs);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   zeroExtend(pack(signedLT(unJust(mvs), signExtend(simm)))),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Set Less Than Immediate Unsigned 
      tagged DSLTIU {psrc: .rs, pdst: .rd, imm:.simm, opdst: .opd}: 
	begin
	
	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          done  = isJust(mvs);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   zeroExtend(pack(unJust(mvs) < signExtend(simm))),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //And Immediate
      tagged DANDI {psrc: .rs, pdst: .rd, imm:.zimm, opdst: .opd}: 
	begin
	
	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          done  = isJust(mvs);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs) & zeroExtend(zimm),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Or Immediate
      tagged DORI {psrc: .rs, pdst: .rd, imm:.zimm, opdst: .opd}: 
	begin
	
	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          done  = isJust(mvs);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs) | zeroExtend(zimm),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //XOR Immediate
      tagged DXORI {psrc: .rs, pdst: .rd, imm:.zimm, opdst: .opd}: 
	begin
	
	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          done  = isJust(mvs);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs) ^ zeroExtend(zimm),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
	
      //Load Unsigned Immediate (Really is unsigned)
      tagged DLUI {pdst: .rd, imm:.zimm, opdst: .opd}: 
	begin

          done  = True;
	  res   = RNop;
	  einst = EWB
	          {
		    val:   zeroExtend(zimm) << 16,
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
	
      //Shift Left Logical (Immediate)
      tagged DSLL {psrc: .rs, pdst: .rd, shamt:.sha, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          done  = isJust(mvs);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs) << zeroExtend(sha),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Shift Right Logical (Immediate)
      tagged DSRL {psrc: .rs, pdst: .rd, shamt:.sha, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          done  = isJust(mvs);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs) >> zeroExtend(sha),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Shift Right Arithmatic (Immediate)
      tagged DSRA {psrc: .rs, pdst: .rd, shamt:.sha, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          done  = isJust(mvs);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   signedShiftRight(unJust(mvs), zeroExtend(sha)),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Shift Left Logical Variable
      tagged DSLLV {psrc: .rs, pdst: .rd, pshamt:.rsha, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs   = bypass.read1(rs);
	  Maybe#(SMIPS_Value) mvsha = bypass.read2(rsha);

          done  = isJust(mvs) && isJust(mvsha);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs) << zeroExtend(unJust(mvsha)[4:0]),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Shift Right Logical Variable
      tagged DSRLV {psrc: .rs, pdst: .rd, pshamt:.rsha, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs   = bypass.read1(rs);
	  Maybe#(SMIPS_Value) mvsha = bypass.read2(rsha);

          done  = isJust(mvs) && isJust(mvsha);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs) >> zeroExtend(unJust(mvsha)[4:0]),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Shift Right Arithmatic Variable
      tagged DSRAV {psrc: .rs, pdst: .rd, pshamt:.rsha, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs   = bypass.read1(rs);
	  Maybe#(SMIPS_Value) mvsha = bypass.read2(rsha);

          done  = isJust(mvs) && isJust(mvsha);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   signedShiftRight(unJust(mvs), zeroExtend(unJust(mvsha)[4:0])),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Add Unsigned
      tagged DADDU {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs1  = bypass.read1(rs1);
	  Maybe#(SMIPS_Value) mvs2  = bypass.read2(rs2);

          done  = isJust(mvs1) && isJust(mvs2);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs1) + unJust(mvs2),
		    pdst:  rd,
		    opdst: opd
		  };
	end

      //Subtract Unsigned
      tagged DSUBU {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs1 = bypass.read1(rs1);
	  Maybe#(SMIPS_Value) mvs2 = bypass.read2(rs2);

          done  = isJust(mvs1) && isJust(mvs2);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs1) - unJust(mvs2),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //And
      tagged DAND {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs1 = bypass.read1(rs1);
	  Maybe#(SMIPS_Value) mvs2 = bypass.read2(rs2);

          done  = isJust(mvs1) && isJust(mvs2);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs1) & unJust(mvs2),
		    pdst:  rd,
		    opdst: opd
		  };
	end
      
      //OR
      tagged DOR {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs1 = bypass.read1(rs1);
	  Maybe#(SMIPS_Value) mvs2 = bypass.read2(rs2);

          done  = isJust(mvs1) && isJust(mvs2);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs1) | unJust(mvs2),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //XOR
      tagged DXOR {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs1 = bypass.read1(rs1);
	  Maybe#(SMIPS_Value) mvs2 = bypass.read2(rs2);

          done  = isJust(mvs1) && isJust(mvs2);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mvs1) ^ unJust(mvs2),
		    pdst:  rd,
		    opdst: opd
		  };
	end

      //NOR
      tagged DNOR {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs1 = bypass.read1(rs1);
	  Maybe#(SMIPS_Value) mvs2 = bypass.read2(rs2);

          done  = isJust(mvs1) && isJust(mvs2);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   ~(unJust(mvs1) | unJust(mvs2)),
		    pdst:  rd,
		    opdst: opd
		  };
	end

      //Set Less Than
      tagged DSLT {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs1 = bypass.read1(rs1);
	  Maybe#(SMIPS_Value) mvs2 = bypass.read2(rs2);

          done  = isJust(mvs1) && isJust(mvs2);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   zeroExtend(pack(signedLT(unJust(mvs1), unJust(mvs2)))),
		    pdst:  rd,
		    opdst: opd
		  };
	end
      
      //Set Less Than Unsigned
      tagged DSLTU {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs1 = bypass.read1(rs1);
	  Maybe#(SMIPS_Value) mvs2 = bypass.read2(rs2);

          done  = isJust(mvs1) && isJust(mvs2);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   zeroExtend(pack(unJust(mvs1) < unJust(mvs2))),
		    pdst:  rd,
		    opdst: opd
		  };
	end


      // -- Branches --------------------------------------------------
      
      //Branch if Less-Than or Equal to Zero
      tagged DBLEZ {psrc: .rs, offset: .off, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          Bool taken = signedLE(unJust(mvs), 0);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mvs);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Branch if Greater Than Zero
      tagged DBGTZ {psrc: .rs, offset: .off, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          Bool taken = signedGT(unJust(mvs), 0);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mvs);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Branch if Less Than Zero
      tagged DBLTZ {psrc: .rs, offset: .off, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          Bool taken = signedLT(unJust(mvs), 0);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mvs);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Branch if Greater than or Equal to Zero
      tagged DBGEZ {psrc: .rs, offset: .off, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          Bool taken = signedGE(unJust(mvs), 0);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mvs);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Branch if Equal
      tagged DBEQ {psrc1: .rs1, psrc2: .rs2, offset: .off, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs1 = bypass.read1(rs1);
	  Maybe#(SMIPS_Value) mvs2 = bypass.read2(rs2);

          Bool taken = unJust(mvs1) == unJust(mvs2);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mvs1) && isJust(mvs2);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Branch if Not Equal
      tagged DBNE {psrc1: .rs1, psrc2: .rs2, offset: .off, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs1 = bypass.read1(rs1);
	  Maybe#(SMIPS_Value) mvs2 = bypass.read2(rs2);

          Bool taken = unJust(mvs1) != unJust(mvs2);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mvs1) && isJust(mvs2);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end
      
      // -- Jumps -----------------------------------------------------

      //Jump
      tagged DJ {target: .targ, opdst: .opd}: 
	begin

	  SMIPS_Addr dest  = {addr[31:28], targ, 2'b0};

          done  = True;
	  res   = RBranchTaken dest;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end
      
      //Jump Register
      tagged DJR {psrc: .rs, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);

          SMIPS_Addr dest = unJust(mvs);

          done  = isJust(mvs);
	  res   = RBranchTaken dest;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Jump and Link (into archictectural register 31)
      tagged DJAL {target: .targ, pdst: .rd, opdst: .opd}: 
	begin

	  SMIPS_Addr dest  = {addr[31:28], targ, 2'b0};
	  
          done  = True;
	  res   = RBranchTaken dest;
	  einst = EWB
	          {
		    val:   addr,
		    pdst:  rd,
		    opdst: opd
		  };
	end


      //Jump and Link into Register
      tagged DJALR {psrc: .rs, pdst: .rd, opdst: .opd}: 
	begin

	  Maybe#(SMIPS_Value) mvs = bypass.read1(rs);
	  
	  SMIPS_Addr dest  = unJust(mvs);
	  
          done  = True;
	  res   = RBranchTaken dest;
	  einst = EWB
	          {
		    val:   addr,
		    pdst:  rd,
		    opdst: opd
		  };
	end

       // -- Illegal ---------------------------------------------------
 
      default: 
        begin
	
	  done = True;
	  res = RNop;
	  einst = ENop {opdst: ?};
	  
	  $display("ERROR: EXECUTING ILLEGAL INSTRUCTION");
	  
        end
    endcase
      
    if (done)
      begin
	link_exe.makeResp(tuple3(t, res, einst));
	waitingQ.deq();
      end

  endrule

  
endmodule 


//-------------------------------------------------------------------------//
// Memory Unit                                                             //
//-------------------------------------------------------------------------//

// 

module [Module] mkSMIPS_Mem#(BypassUnit#(SMIPS_RName, SMIPS_PRName, SMIPS_Value, SMIPS_Token, SMIPS_SnapshotPtr) bypass) ();
  
  //Links
  Link_Server#(Tuple3#(SMIPS_Token, SMIPS_ExecedInst, void),
               Tuple3#(SMIPS_Token, void, SMIPS_ExecedInst)) link_mem <- mkLink_Server("link_mem");
	  
  Link_Client#(MemReq#(SMIPS_Token, SMIPS_Addr, SMIPS_Value), 
               MemResp#(SMIPS_Value)) link_to_dmem <- mkLink_Client("link_to_dmem");

  FIFO#(Tuple2#(SMIPS_Token, SMIPS_ExecedInst)) waitingQ <- mkFIFO();

  //doReq

  rule doReq (True);
       
    match {.t, .i, .*} <- link_mem.getReq();

    waitingQ.enq(tuple2(t, i));
    
    case (i) matches
      tagged ELoad {addr: .a, pdst: .prd, opdst: .oprd}:
	begin

          link_to_dmem.makeReq(Ld {token: t, addr: a});

	end
      tagged EStore{opdst: .oprd, val: .v, addr: .a}:
        begin
	
          link_to_dmem.makeReq(St {val: v, addr: a, token: t});
	  
        end
      default: // push
        noAction;
    endcase
  endrule

  //getResp

  rule getResp(True);
  
    match {.tok, .i} = waitingQ.first();
    
    case (i) matches
      tagged ELoad {addr: .a, pdst: .prd, opdst: .oprd}:
        begin
	  
          let resp <- link_to_dmem.getResp();
          waitingQ.deq();
	  
          SMIPS_Value v = case (resp) matches
                      tagged LdResp .val: return val;
                      tagged StResp .*  : return 0; // impossible
                    endcase;
		    
          link_mem.makeResp(tuple3(tok, ?, EWB{val: v, pdst: prd, opdst: oprd}));
          bypass.write1(prd, v);
	  
        end
      tagged EStore {opdst: .oprd, val: .*, addr: .*}:
        begin
	
          let resp <- link_to_dmem.getResp();
          waitingQ.deq();
	  
          link_mem.makeResp(tuple3(tok, ?, ENop {opdst: oprd}));
	  
        end
      tagged EWB {val: .v, pdst: .prd, opdst: .opd}:
        begin
	  
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok, ?, i));
	  
        end
      default:
        begin
	  
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok, ?, i));
	  
        end
    endcase
  endrule

  
endmodule


//-------------------------------------------------------------------------//
// Local Commit Unit                                                       //
//-------------------------------------------------------------------------//

//

module [Module] mkSMIPS_LocalCommit#(BypassUnit#(SMIPS_RName, SMIPS_PRName, SMIPS_Value, SMIPS_Token, SMIPS_SnapshotPtr) bypass) ();
  
  Link_Server#(Tuple3#(SMIPS_Token, SMIPS_ExecedInst, void),
               Tuple3#(SMIPS_Token, void, SMIPS_ExecedInst)) link_lco <- mkLink_Server("link_lco");
  
  rule handleLCO (True);
  
    match {.t, .ei, .*} <- link_lco.getReq();
    
    SMIPS_PRName p = case (ei) matches
                 tagged ENop    .x: return(x.opdst);
		 tagged EWB     .x: return(x.opdst);
		 tagged ELoad   .x: return(x.opdst);
		 tagged EStore  .x: return(x.opdst);
	       endcase;

    bypass.freePReg(t, p);

    link_lco.makeResp(tuple3(t, ?, ?));

  endrule
  
endmodule


//-------------------------------------------------------------------------//
// Global Commit Unit                                                      //
//-------------------------------------------------------------------------//

module [Module] mkSMIPS_GlobalCommit ();

  Link_Send#(SMIPS_Token) link_mem_commit <- mkLink_Send("link_mem_commit");
  
  Link_Server#(Tuple3#(SMIPS_Token, SMIPS_ExecedInst, void),
               Tuple3#(SMIPS_Token, void, void)) link_gco <- mkLink_Server("link_gco");
  
  rule handleGCO (True);
  
    match {.tok, .*, .*} <- link_gco.getReq();
    
    link_mem_commit.send(tok);
    
    link_gco.makeResp(tuple3(tok, ?, ?));
  
  endrule
  
endmodule

//-------------------------------------------------------------------------//
// Functional Partition                                                    //
//-------------------------------------------------------------------------//

interface FP_Stage_Link#(type tick_T, 
                	 type token_T,
			 type init_T,
			 type req_T,
			 type resp_T,
			 type next_T);

endinterface

module [Module] mkFP_Stage_Link#(String stagename,
                                 String linkname, 
                                 String servername,
				 String prevname,
				 String nextname,
				 Integer sz) 
    //interface:
               (FP_Stage_Link#(tick_T, 
	                       token_T, 
			       init_T, 
			       req_T, 
			       resp_T, 
			       next_T))
        provisos
          (Bits#(token_T, token_SZ), 
	   Bounded#(token_T),
	   Eq#(token_T),
	   Literal#(token_T),
           Bits#(tick_T, tick_SZ), 
           Bits#(init_T, init_SZ),
           Bits#(req_T, req_SZ),
           Bits#(resp_T, resp_SZ),
           Bits#(next_T, next_SZ));

  //Local definitions
  token_T tableMin = minBound;
  token_T tableMax = fromInteger(sz - 1);

  //Links
  Link_Client#(Tuple3#(token_T, init_T, req_T),
               Tuple3#(token_T, resp_T, next_T)) link_to_unit <- mkLink_Client(linkname);
  
  Link_Server#(Tuple3#(token_T, tick_T, req_T),
               Tuple2#(token_T, resp_T))         link_from_tp <- mkLink_Server(servername);
  
  Link_Receive#(Tuple2#(token_T, init_T))        link_from_prev <- mkLink_Receive(prevname);
  
  Link_Send#(Tuple2#(token_T, next_T))           link_to_next <- mkLink_Send(nextname);
  
  Link_Receive#(token_T)                         link_killToken <- mkLink_Receive("link_killToken");

  		
  //SRAM tables
  RegFile#(token_T, init_T)		  values    <- mkRegFile(tableMin, tableMax);
  RegFile#(token_T, Bool)		  valids    <- mkRegFile(tableMin, tableMax); 
  RegFile#(token_T, Bool)		  dones     <- mkRegFile(tableMin, tableMax); 

  //Rules
  
  //insert

  rule insert (True);
  
    match {.tok,.iVal} <- link_from_prev.receive();
    
    Bool valid = valids.sub(tok);
    
    if (valid)
      begin        
	$display("%s ERROR: reinserting allocated token %h", stagename, tok);
      end
    else
      begin
	//Set valid to true and done to false
	valids.upd(tok,True);
	dones.upd(tok,False);
	values.upd(tok, iVal);
      end
  
  endrule


  //handleReq
  
  rule handleReq (True);

    match {.tok, .tick, .req} <- link_from_tp.getReq();
   
    Bool done   =  dones.sub(tok);
    Bool valid  =  valids.sub(tok);  

    init_T iVal = values.sub(tok);

    if (!valid)
       $display("%s ERROR: requesting unallocated token %h", stagename, tok);
     else if (done)
       $display("%s ERROR: re-requesting finished token %h", stagename, tok);            
     else // !done
       link_to_unit.makeReq(tuple3(tok, iVal, req));
  endrule

  //getResponse
  
  rule getResponse (True);
  
    match {.tok, .resp, .next} <- link_to_unit.getResp();
    
    Bool valid = valids.sub(tok);
    
    if (valid) // don't insert if it was killed
      begin
        dones.upd(tok, True);
	link_from_tp.makeResp(tuple2(tok, resp));
	link_to_next.send(tuple2(tok, next));
      end
      
  endrule
  
  //killToken
  
  rule killToken (True);
    
    let tok <- link_killToken.receive();
  
    valids.upd(tok, False);
  
  endrule

endmodule

module [Module] mkSMIPS_TOK_Stage
    //interface:
                (FP_Stage_Link#(SMIPS_Tick,   //Tick type
		                SMIPS_Token,  //Token type
			        void,	  //Type from previous stage
			        void,	  //Request Type
			        void,	  //Response Type
			        void));    //Type to next stage

  Reg#(SMIPS_Token) r_first <- mkReg(minBound);
  Reg#(SMIPS_Token) r_free <- mkReg(minBound);
  
  //Links
  Link_Server#(Tuple3#(SMIPS_Token, SMIPS_Tick, void),
               Tuple2#(SMIPS_Token, void))           link_from_tp <- mkLink_Server("tok_server");
  
  Link_Receive#(Tuple2#(SMIPS_Token, void))          link_from_prev <- mkLink_Receive("gco_to_tok");
  
  Link_Send#(Tuple2#(SMIPS_Token, void))             link_to_next <- mkLink_Send("tok_to_fet");
  
  Link_Receive#(SMIPS_Token)                         link_killToken <- mkLink_Receive("link_killToken");


  //handleReq
  
  rule handleReq (True);
  
    match {.*, .*, .tick} <- link_from_tp.getReq();

    //allocate a new token
    r_free <= r_free + 1;

    link_from_tp.makeResp(tuple2(r_free, ?));
    link_to_next.send(tuple2(r_free, ?));
    
  
  endrule
  
 
  //recycle
 
  rule recycle (True);

    match {.t, .*} <- link_from_prev.receive();

    //complete token t

    if (r_first != t) 
      $display("TGen ERROR: tokens completing out of order");

    r_first <= r_first + 1;

  endrule
   
  //killToken
  
  rule killToken (True);
    
    let tok <- link_killToken.receive();
    
    //free tok and all tokens after it
    r_free <= tok;
    
  endrule
  
endmodule
              

module [Module] mkSMIPS_FET_Stage 
    //interface:
                (FP_Stage_Link#(SMIPS_Tick, 
	                	SMIPS_Token, 
				void, 
				SMIPS_Addr, 
				SMIPS_Inst, 
				Tuple2#(SMIPS_Addr, SMIPS_Inst)));

  let s <- mkFP_Stage_Link("FET", 
                           "link_fet",
			   "fet_server",
			   "tok_to_fet",
			   "fet_to_dec", 
			   8);
  
  return s;

endmodule

module [Module] mkSMIPS_DEC_Stage 
    //interface:
                (FP_Stage_Link#(SMIPS_Tick, 
	                	SMIPS_Token, 
				Tuple2#(SMIPS_Addr, SMIPS_Inst), 
				void, 
				SMIPS_DepInfo, 
				Tuple2#(SMIPS_Addr, SMIPS_DecodedInst)));

  let s <- mkFP_Stage_Link("DEC", 
                           "link_dec",
			   "dec_server",
			   "fet_to_dec",
			   "dec_to_exe", 
			   8);
  
  return s;

endmodule

module [Module] mkSMIPS_EXE_Stage 
    //interface:
                (FP_Stage_Link#(SMIPS_Tick, 
	                	SMIPS_Token, 
				Tuple2#(SMIPS_Addr, SMIPS_DecodedInst), 
				void, 
				SMIPS_InstResult, 
				SMIPS_ExecedInst));

  let s <- mkFP_Stage_Link("EXE", 
                           "link_exe",
			   "exe_server",
			   "dec_to_exe",
			   "exe_to_mem", 
			   8);
  
  return s;

endmodule

module [Module] mkSMIPS_MEM_Stage 
    //interface:
                (FP_Stage_Link#(SMIPS_Tick, 
	                	SMIPS_Token, 
				SMIPS_ExecedInst, 
				void, 
				void, 
				SMIPS_ExecedInst));

  let s <- mkFP_Stage_Link("MEM", 
                           "link_mem",
			   "mem_server",
			   "exe_to_mem",
			   "mem_to_lco", 
			   8);
  
  return s;

endmodule

module [Module] mkSMIPS_LCO_Stage 
    //interface:
                (FP_Stage_Link#(SMIPS_Tick, 
	                	SMIPS_Token, 
				SMIPS_ExecedInst, 
				void, 
				void, 
				SMIPS_ExecedInst));

  let s <- mkFP_Stage_Link("LCO", 
                           "link_lco",
			   "lco_server",
			   "mem_to_lco",
			   "lco_to_gco", 
			   8);
  
  return s;

endmodule

module [Module] mkSMIPS_GCO_Stage 
    //interface:
                (FP_Stage_Link#(SMIPS_Tick, 
	                	SMIPS_Token, 
				SMIPS_ExecedInst, 
				void, 
				void, 
				void));

  let s <- mkFP_Stage_Link("GCO", 
                           "link_gco",
			   "gco_server",
			   "lco_to_gco",
			   "gco_to_tok", 
			   8);
  
  return s;

endmodule

module [Module] mkSMIPS_FP (); 

  BypassUnit#(SMIPS_RName, SMIPS_PRName, SMIPS_Value, SMIPS_Token, SMIPS_SnapshotPtr) bypass <- mkBypassUnit();
  
  Empty fet <- mkSMIPS_Fetch();
  Empty dec <- mkSMIPS_Decode(bypass);
  Empty exe <- mkSMIPS_Execute(bypass);
  Empty mem <- mkSMIPS_Mem(bypass);
  Empty lco <- mkSMIPS_LocalCommit(bypass);
  Empty gco <- mkSMIPS_GlobalCommit();

  
  FP_Stage_Link#(SMIPS_Tick, 
		 SMIPS_Token,
		 void, 
		 void, 
		 void, 
		 void) tok_stage <- mkSMIPS_TOK_Stage();
		  
  let fet_stage <- mkSMIPS_FET_Stage();
  let dec_stage <- mkSMIPS_DEC_Stage();
  let exe_stage <- mkSMIPS_EXE_Stage();
  let mem_stage <- mkSMIPS_MEM_Stage();
  let lco_stage <- mkSMIPS_LCO_Stage();
  FP_Stage_Link#(SMIPS_Tick, 
	         SMIPS_Token, 
		 SMIPS_ExecedInst, 
		 void, 
		 void, 
		 void) gco_stage <- mkSMIPS_GCO_Stage();
  
endmodule

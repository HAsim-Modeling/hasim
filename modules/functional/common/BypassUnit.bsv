import HASim::*;
import FunctionalPartition::*;

import GetPut::*;
import RegFile::*;
import FIFO::*;
import Vector::*;
import ConfigReg::*;


//----------------------------------------------------------------------------------
// Physical Register File
//----------------------------------------------------------------------------------

interface RFile_4_2#(type addr_T, type value_T);
  method Maybe#(value_T) read1(addr_T a);
  method Maybe#(value_T) read2(addr_T a);
  method Maybe#(value_T) read3(addr_T a);
  method Maybe#(value_T) read4(addr_T a);


  method Action write1(addr_T a, value_T v);
  method Action write2(addr_T a, value_T v);

  method Action alloc(addr_T a);
endinterface

module [Module] mkRFile_4_2
    //interface:
                (RFile_4_2#(prname_T, value_T)) 
    provisos
            (Bits#(prname_T, prname_SZ),
	     Bits#(value_T,  value_SZ),
	     PrimIndex#(prname_T, prname_DY),
	     Bounded#(prname_T),
	     Literal#(value_T),
	     Literal#(prname_T),
	     Ord#(prname_T),
	     Eq#(prname_T));

  //RegisterFile

  Vector#(TExp#(prname_SZ), Reg#(value_T)) rf_regs <- mapM(compose(mkConfigReg,fromInteger), genVector);

  function initiallyValid(Integer x);
    prname_T rmax = maxBound ;
    return(fromInteger(x) < rmax);
  endfunction

  Vector#(TExp#(prname_SZ), Reg#(Bool))           rf_valids <-
            mapM(compose(mkConfigReg, initiallyValid), genVector);
//            replicateM(mkConfigReg, mapM(initiallyValid, genVector));

  function Maybe#(value_T) read(prname_T x);
     return (select(rf_valids, x)._read()) ? Just((select(rf_regs, x))._read()) : Nothing;
  endfunction

  function Action write(prname_T x, value_T v);
    action 
      (select(rf_valids, x)) <= True;
      (select(rf_regs, x))   <= v;
    endaction
  endfunction

  function Action allocF(prname_T x);
    action
      (select(rf_valids, x)) <= False;
    endaction
  endfunction

  method read1 = read;
  method read2 = read;
  method read3 = read;
  method read4 = read;

  method write1 = write;
  method write2 = write;

  method alloc = allocF;

endmodule

//----------------------------------------------------------------------------------
// Bypass
//----------------------------------------------------------------------------------

module [Module] mkBypassUnit
    //interface:
                (BypassUnit#(rname_T,        //Register Name type
		             prname_T,       //Physical Register Name type
			     value_T,        //Value type
			     token_T,        //Token type
			     snapshotptr_T)) //SnapShot Ptr type
    provisos
            (Bits#(rname_T, rname_SZ), 
	     Eq#(rname_T), 
	     Bounded#(rname_T),
	     Literal#(rname_T),
	     PrimIndex#(rname_T, rname_DY),
	     Bits#(prname_T, prname_SZ),
	     Bounded#(prname_T),
	     Ord#(prname_T),
	     Eq#(prname_T),
	     Arith#(prname_T),
	     Literal#(prname_T),
	     PrimIndex#(prname_T, prname_DY),
	     Bits#(value_T, value_SZ),
	     Literal#(value_T),
	     Bits#(token_T, token_SZ),
	     Eq#(token_T),
	     Ord#(token_T),
	     Arith#(token_T),
             Bits#(snapshotptr_T, snapshotptr_SZ),
	     PrimIndex#(snapshotptr_T, snapshotptr_DY),
	     Bounded#(snapshotptr_T),
	     Literal#(snapshotptr_T),
	     Eq#(snapshotptr_T),
	     Add#(rname_SZ, rdiff_TMP, prname_SZ),
	     Add#(snapshotptr_SZ, sdiff_TMP, token_SZ));

  RFile_4_2#(prname_T, value_T) prf <- mkRFile_4_2();

  //map table 

  rname_T maxR = maxBound;
  Bit#(prname_SZ) minInitFL_bits = zeroExtend(pack(maxR)) + 1;
  prname_T minInitFL = unpack(minInitFL_bits);
  prname_T maxInitFL = maxBound;
  
  Vector#(TExp#(rname_SZ), prname_T) initmap = map(fromInteger, genVector);
  Reg#(Vector#(TExp#(rname_SZ), prname_T)) maptbl <- mkReg(initmap); // init with [0 .. max]

  function prname_T lookup(rname_T r);
     return select(maptbl._read(), r);
  endfunction 

  // XXX initialized with max+1 to maxBound ..
  RegFile#(prname_T, prname_T)  freelist  <- mkRegFileFullLoad("freelist.hex");
  Reg #(prname_T)               fl_read   <- mkReg(minInitFL);
  Reg #(prname_T)               fl_write  <- mkReg(maxInitFL); 

  //rob                                   old
  RegFile#(prname_T, Tuple3#(token_T, Maybe#(rname_T), prname_T)) rob      <- mkRegFileFull();
  Reg#(prname_T)   						  rob_old  <- mkReg(0);
  Reg#(prname_T) 						  rob_new  <- mkReg(0);

  Reg#(Vector#(TExp#(snapshotptr_SZ), Bool))     snap_valids        <- mkReg(unpack(0));
  Reg#(Vector#(TExp#(snapshotptr_SZ), token_T))  snap_ids           <- mkRegU();
  RegFile#(snapshotptr_T, prname_T)              snap_flreadptrs    <- mkRegFileFull();
  RegFile#(snapshotptr_T, prname_T)              snap_robnewptrs    <- mkRegFileFull(); 
  RegFile#(snapshotptr_T, Vector#(TExp#(rname_SZ), prname_T)) snaps <- mkRegFileFull();


  Reg#(Bool)  busy <- mkReg(False);
  Reg#(token_T) stopToken <- mkRegU();

  Bool free_ss = pack(snap_valids) != ~0;

  //unBusy
  
  rule unBusy (busy);
  
    //if newest token is stopToken or we ran out unbusy
    match {.tok, .mx, .oldp} = rob.sub(rob_new);
    
    if (tok == stopToken || (rob_new == rob_old + 1))
       busy <= False;
       
   //back up maptable
   if (isJust(mx))
     maptbl <= update(maptbl, unJust(mx), oldp);

   //back up (freelist)
   fl_read <= fl_read - 1;
 
   //backup (rob)
   rob_new <= rob_new - 1;
   
  endrule


  //makeMapping (used by Decode)

  //                          new    old
  method ActionValue#(Tuple2#(prname_T, prname_T)) makeMapping(Maybe#(rname_T) mx, token_T tok, Bool ss)
         //guard:
                 if (!busy && (fl_read + 1 != fl_write));

    prname_T oldPReg; 
    prname_T newPReg;

    if (isJust(mx))
      begin
	oldPReg = select(maptbl, unJust(mx));
	newPReg = freelist.sub(fl_read);
      end
    else
      begin
	oldPReg = freelist.sub(fl_read);
	newPReg = ?;
       end

    //take off freelist
    fl_read <= fl_read + 1;

    // update map
    if (isJust(mx))
      begin
	Vector#(TExp#(rname_SZ), prname_T) new_map = maptbl;
	new_map = update(new_map, unJust(mx), newPReg);
	maptbl <= new_map;
      end

    // write rob
    rob_new <= rob_new + 1;
    rob.upd(rob_new, tuple3(tok, mx, newPReg));

    // make snapshot if needed
    Maybe#(snapshotptr_T) midx = Nothing;

  //INDEX sequence
    Bit#(snapshotptr_SZ) ti_bits = truncate(pack(tok));
    snapshotptr_T ti = unpack(ti_bits);
    if(!select(snap_valids, ti))
      midx = Just(ti);

    //for(Integer i = 0; i < valueOf(TExp#(ssz)); i = i + 1) // 
    //  if(!snap_valids[i])
    //    midx = Just(fromInteger(i));
  //END INDEX sequence


    //snapshot only when it makes sense

    midx = (ss) ? midx : Nothing;

    if (isJust(midx))
      begin
	let idx = unJust(midx);
	snap_valids     <= update(snap_valids, idx, True);
	snap_ids        <= update(snap_ids   , idx, tok);
	snap_flreadptrs.upd(idx, fl_read);
	snap_robnewptrs.upd(idx, rob_new);
                  snaps.upd(idx, maptbl);
      end
    
    // return value
    return(tuple2(newPReg, oldPReg));
    
  endmethod
  
  //lookup{1, 2} used by Decode

  method lookup1 = lookup;
  method lookup2 = lookup;

  //read{1,2,3,4} 
  //{1,2} used by Exec
  //{3,4} used by Mem

  method read1 = prf.read1;
  method read2 = prf.read2;
  method read3 = prf.read3;
  method read4 = prf.read4;

  //write{1,2}
  //1 used by Execute
  //2 used by Mem

  method write1 = prf.write1;
  method write2 = prf.write2;

  //freePReg (used by Local Commit)

  method Action freePReg(token_T tok,prname_T x) if (!busy);
    freelist.upd(fl_write,x);
    fl_write <= fl_write + 1;

    rob_old <= rob_old + 1;

    function f(s,t) = s && (t != tok);

    let newvals = zipWith(f, snap_valids, snap_ids);

    snap_valids <= newvals;
    
  endmethod

  //rewindToToken (used by FunctionalPartition.killToken)

  method Action rewindtoToken(token_T tok) if (!busy);
    //NO!!!!!
     
    //see if token is snapshotted
  //INDEX function 
    Maybe#(snapshotptr_T) midx = Nothing;

    Bit#(snapshotptr_SZ) idx_bits = truncate(pack(tok));
    snapshotptr_T idx = unpack(idx_bits);
    
    if (select(snap_valids, idx))
       midx = Just(idx);

    //for(Integer i = 0; i < valueOf(TExp#(ssz)); i = i + 1) // 
    //  if(!snap_valids[i] && (snap_ids[i] == tok))
    //    midx = Just(fromInteger(i));
  //END INDEX function

    case (midx) matches
      tagged Just .i:
        begin
          maptbl  <=           snaps.sub(i);
          fl_read <= snap_flreadptrs.sub(i);
          rob_new <= snap_robnewptrs.sub(i);
        end
      tagged Nothing:   //if not write busy and record token
        begin
          busy <= True;
          stopToken <= tok;
        end
    endcase

    //flatten dead snaps
    match {.oldTok, .*, .*} = rob.sub(rob_old);

    function Bool flatten(Bool x, token_T t) = x && (tok-oldTok > t - oldTok); //valid and older than tok stay
     
    snap_valids <= zipWith(flatten, snap_valids, snap_ids);

  endmethod

endmodule


import Datatypes::*;

import GetPut::*;
import RegFile::*;
import FIFO::*;
import Vector::*;
import ConfigReg::*;


typedef Bit#(4) SnapShotPtr;

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

(* synthesize *)
module [Module] mkRFile_4_2(RFile_4_2#(PRName, Value)) provisos (Bits#(PRName, psz));

  //RegisterFile

  Vector#(TExp#(psz), Reg#(Value))            rf_regs <- replicateM(mkConfigRegU);
  Vector#(TExp#(psz), Reg#(Bool))           rf_valids <- replicateM(mkConfigReg(False));

  function Maybe#(Value) read(PRName x);
     return (rf_valids[x]._read) ? Just((rf_regs[x])._read): Nothing;
  endfunction

  function Action write(PRName x, Value v);
    action 
      (rf_valids[x]) <= True;
      (rf_regs[x])   <= v;
    endaction
  endfunction

  function Action allocF(PRName x);
    action
      (rf_valids[x]) <= False;
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

(* synthesize *)
module [Module] mkBypassUnit(BypassUnit#(RName, PRName, Value, Token))
  provisos(
    Bits#(RName, rsz), Eq#(RName), Bounded#(RName), 
    Bits#(PRName,psz),
    Bits#(SnapShotPtr, ssz) 
    );

  RFile_4_2#(PRName, Value)                         prf <- mkRFile_4_2();

  //rob

  //map table 

  RName maxR = maxBound;
  PRName minInitFL = zeroExtend(maxR) + 1;
  PRName maxInitFL = maxBound;

  Vector#(TExp#(rsz), PRName) initmap = map(fromInteger,genVector);
  Reg#(Vector#(TExp#(rsz), PRName)) map <- mkReg(initmap); // init with [0 .. max]

  function PRName lookup(RName r);
     return(map[r]);
  endfunction 

  // XXX initialized with max+1 to maxBound ..
  RegFile#(PRName,PRName)                      freelist <- mkRegFileFull(); //XXX
  Reg #(PRName)                                 fl_read <- mkReg(minInitFL);
  Reg #(PRName)                                fl_write <- mkReg(maxInitFL); 

  //rob                                   old
  RegFile#(PRName, Tuple3#(Token, Maybe#(RName), PRName)) rob <- mkRegFileFull();
  Reg #(PRName)                                 rob_old <- mkReg(0);
  Reg #(PRName)                                rob_new  <- mkReg(0);

  Reg#(Vector#(TExp#(ssz), Bool))              snap_valids <- mkReg(unpack(0));
  Reg#(Vector#(TExp#(ssz), Token))                snap_ids <- mkRegU();
  Reg#(Vector#(TExp#(ssz), PRName))        snap_flreadptrs <- mkRegU();
  RegFile#(SnapShotPtr, Vector#(TExp#(rsz), PRName)) snaps <- mkRegFileFull();
  
  Reg#(Bool) 		                              busy <- mkReg(False);
  Reg#(Token) 	  				 stopToken <- mkRegU();

  Bool free_ss = pack(snap_valids) != ~0;

  rule unBusy(busy);
    //if newest token is stopToken or we ran out unbusy
    match {.tok,.mx,.oldp} = rob.sub(rob_new);
    if (tok == stopToken || (rob_new == rob_old + 1))
       busy <= False;
   //back up maptable
   if (isJust(mx))
     map <= update(map, unJust(mx), oldp);

   //back up (freelist)
   fl_read <= fl_read -1;
 
   //backup (rob)
   rob_new <= rob_new - 1;
  endrule

  //                          new    old
  method ActionValue#(Tuple2#(PRName,PRName)) makeMapping(Maybe#(RName) mx, Token tok, Bool ss)
    if (!busy && (fl_read + 1 != fl_write));
  
  PRName oldPReg; 
  PRName newPReg;

  if (isJust(mx))
    begin
      oldPReg = map[unJust(mx)];
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
      Vector#(TExp#(rsz), PRName) new_map = map;
      new_map[unJust(mx)] = newPReg;
      map <= new_map;
    end
 
  // write rob
  rob_new <= rob_new + 1;
  rob.upd(rob_new, tuple3(tok, mx, newPReg));

  // make snapshot if needed
  if (ss && free_ss()) // can and should make snapshot
    begin
      PRName idx = 0;
      for(Integer i = 0; i < valueOf(TExp#(ssz)); i = i + 1) // 
        if(!snap_valids[i])
          idx = fromInteger(i);

      snap_valids     <= update(snap_valids, idx, True);
      snap_ids        <= update(snap_ids   , idx, tok);
      snap_flreadptrs <= update(snap_flreadptrs, idx, fl_read);
    end
    
    // return value
    return(tuple2(newPReg,oldPReg));
  endmethod

  method lookup1 = lookup;
  method lookup2 = lookup;

  method read1 = prf.read1;
  method read2 = prf.read2;
  method read3 = prf.read3;
  method read4 = prf.read4;

  method write1 = prf.write1;
  method write2 = prf.write2;

  method Action freePReg(Token tok,PRName x) if (!busy);
    freelist.upd(fl_write,x);
    fl_write <= fl_write + 1;

    rob_old <= rob_old + 1;

    function f(s,t) = s && (t != tok);

    let newvals = zipWith(f, snap_valids, snap_ids);

    snap_valids <= newvals;
    
  endmethod

  method Action rewindtoToken(Token tok) if (!busy);
    //NO!!!!!
     
    //see if token is snapshotted
    Maybe#(SnapShotPtr) midx = Nothing;
    for(Integer i = 0; i < valueOf(TExp#(ssz)); i = i + 1) // 
      if(!snap_valids[i] && (snap_ids[i] == tok))
        midx = Just(fromInteger(i));

    case (midx) matches
      tagged Just .idx:
        begin
          map <= snaps.sub(idx);
          fl_read <= snap_flreadptrs[idx];
        end
      tagged Nothing:   //if not write busy and record token
        begin
          busy <= True;
          stopToken <= tok;
        end
    endcase

    //flatten dead snaps
    match {.oldTok, .*, .*} = rob.sub(rob_old);

    function flatten(x, t) = x && (tok-oldTok > t - oldTok); //valid and older than tok
     
    snap_valids <= zipWith(flatten, snap_valids, snap_ids);

  endmethod

endmodule



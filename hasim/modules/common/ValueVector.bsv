import Vector::*;
import RWire::*;



interface ValueVector#(type value_T, type index_T);
  method value_T read1(index_T i);
  method value_T read2(index_T i);  
  method value_T read3(index_T i);
  method value_T read4(index_T i);

  method Action write1(index_T i, value_T v);
  method Action write2(index_T i, value_T v);
endinterface   

module [Module] mkBoolVector(ValueVector#(Bool, index_T))
   provisos
     (Bits#(index_T, isz), PrimIndex#(index_T),Eq#(index_T), Literal#(index_T));

  Reg#(Bit#(TExp#(isz)))                       is <- mkReg(0);

  RWire#(Tuple2#(index_T,Bool))               rw1 <- mkRWire();
  RWire#(Tuple2#(index_T,Bool))               rw2 <- mkRWire();  

  rule write(True);

    Bool d1 = isJust(rw1.wget);
    Bool d2 = isJust(rw2.wget);
  
    match {.i1, .v1} = unJust(rw1.wget);
    match {.i2, .v2} = unJust(rw2.wget);   

    Bit#(TExp#(isz)) orig = is;

    Bit#(TExp#(isz)) post1 = d1 ? (v1 ? orig | (1 << toIndex(i1)) : orig & ~(1 << toIndex(i1)))
                                 : orig;
    Bit#(TExp#(isz)) post2 = d2 ? (v2 ? post1 | (1 << toIndex(i2)) : post1 & ~(1 << toIndex(i2)))
                                 : post1;
    
    is <= post2;
  endrule
   
  method read1(i);
    return (is[i] == 1'b1);
  endmethod
    
  method read2(i);
    return (is[i] == 1'b1);
  endmethod
    
  method read3(i);
    return (is[i] == 1'b1);
  endmethod

  method read4(i);
    return (is[i] == 1'b1);
  endmethod
    
  method Action write1(i,v);
    rw1.wset(tuple2(i,v));
  endmethod   

  method Action write2(i,v);
    rw2.wset(tuple2(i,v));    
  endmethod 

endmodule
















module [Module] mkBoolVector1(ValueVector#(Bool, index_T))
   provisos
     (Bits#(index_T, isz), PrimIndex#(index_T),Eq#(index_T));

  Vector#(TExp#(isz),Reg#(Bool))               is <- replicateM(mkReg(False));
  RWire#(Tuple2#(index_T,Bool))               rw1 <- mkRWire();
  RWire#(Tuple2#(index_T,Bool))               rw2 <- mkRWire();  

  rule write(True);

    Bool d1 = isJust(rw1.wget);
    Bool d2 = isJust(rw2.wget);
  
    match {.i1, .v1} = unJust(rw1.wget);
    match {.i2, .v2} = unJust(rw2.wget);   

    if (d2)
      (is[i2]) <= v2;
    if (d1 && (!d2 || (i1 != i2)))
      (is[i1]) <= v1;

  endrule
   
  method read1(i);
    return ((is[i])._read);
  endmethod
    
  method read2(i);
    return ((is[i])._read);
  endmethod
    
  method read3(i);
    return ((is[i])._read);
  endmethod

  method Action write1(i,v);
    rw1.wset(tuple2(i,v));
  endmethod   

  method Action write2(i,v);
    rw2.wset(tuple2(i,v));    
  endmethod 
endmodule
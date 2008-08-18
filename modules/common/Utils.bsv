import Vector::*;
import FShow::*;

typedef Bit#(1) VOID;


function Vector#(TExp#(width), dataT) updateRange(dataT data, Bit#(width) lo, Bit#(width) hi, Vector#(TExp#(width), dataT) oldVec);
    Vector#(TExp#(width), dataT) newVec = newVector();
    for(Integer i = 0; i < valueOf(TExp#(width)); i = i + 1)
    begin
        if(lo < hi)
            newVec[i] = (fromInteger(i) > lo && fromInteger(i) < hi)? data: oldVec[i];
        else
            newVec[i] = (fromInteger(i) > lo || fromInteger(i) < hi)? data: oldVec[i];
    end
    return newVec;
endfunction

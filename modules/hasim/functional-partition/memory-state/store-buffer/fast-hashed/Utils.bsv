import Vector::*;
import FShow::*;

instance FShow#(Bit#(t));
    function Fmt fshow(Bit#(t) d);
        return $format("%x", d);
    endfunction
endinstance

typedef struct {
    Bool valid;
    dataT value;
} May#(type dataT) deriving (Bits, Eq);

function May#(dataT) makeInvalid();
    return May{valid: False, value: ?};
endfunction

function May#(dataT) makeValid(dataT val);
    return May{valid: True, value: val};
endfunction

instance FShow#(May#(dataT)) provisos(FShow#(dataT));
    function Fmt fshow(May#(dataT) data);
        return $format("valid: %b value: ", data.valid) + fshow(data.value);
    endfunction
endinstance

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

interface DebugFile;
    method Action _write (Fmt fmt);
endinterface

module mkDebugFile#(String fname)
        (DebugFile);

    Reg#(Bit#(32))  cycle <- mkReg(0);

    Reg#(Maybe#(File)) fileHandle <- mkReg(Invalid);

    rule open (fileHandle == Invalid);
        let fh <- $fopen(fname, "w");
        fileHandle <= Valid(fh);
    endrule

    rule inc(True);
        cycle <= cycle + 1;
    endrule

    method Action _write (Fmt fmt) if (fileHandle matches tagged Valid .fh);
        $fdisplay(fh, $format("[%d]: ", cycle) + fmt);
    endmethod
endmodule

import FShow::*;

interface FpgaDebugFile;
    method Action _write (Fmt fmt);
endinterface

module mkFpgaDebugFile#(String fname)
        (FpgaDebugFile);

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

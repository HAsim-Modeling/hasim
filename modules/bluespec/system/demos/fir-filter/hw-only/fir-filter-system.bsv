import Vector::*;
import RegFile::*;
import FixedPoint::*;

`include "asim/provides/virtual_platform.bsh"

typedef UInt#(TLog#(`INPUT_SIZE)) INPUT_IDX;
typedef UInt#(TLog#(`N_TAPS))     TAP_IDX;
typedef FixedPoint#(16, 16)      FIR_VAL;

module mkApplication#(VIRTUAL_PLATFORM vp) ();

    RegFile#(INPUT_IDX, Int#(32)) inputVals <- mkRegFileLoad(`INPUT_FILE, 0, `INPUT_SIZE - 1);
    Reg#(File) outFile <- mkReg(InvalidFile);
    
    FIR_FILTER fir <- mkFIRFilterSequential();

    Reg#(Bool) initializingCoeffs <- mkReg(True);
    Reg#(INPUT_IDX) curInput <- mkReg(0);
    
    rule initializeCoeffs (initializingCoeffs);
    
        for (Integer x = 0; x < `N_TAPS; x = x + 1)
        begin
            fir.setCoeff(fromInteger(x),  fromReal(1.0 / fromInteger(x + 1)));
        end
        
        initializingCoeffs <= False;
    
    endrule

    rule openOutputFile (outFile == InvalidFile);
    
        let fd <- $fopen(`OUTPUT_FILE);
        if (fd == InvalidFile)
        begin

            $display("ERROR: Could not open output file %s for writing.", `OUTPUT_FILE);
            $finish(1);

        end

        outFile <= fd;
        
    endrule

    rule process (!initializingCoeffs);

        FIR_VAL v = FixedPoint { fxpt: signExtend(inputVals.sub(curInput)) };
        fir.put(v);
        curInput <= curInput + 1;
        
        let res <- fir.get();
        if (curInput > `N_TAPS)
        begin
            $fdisplay(outFile, "[%0d]: %0d.%0d", curInput, fxptGetInt(res), fxptGetFrac(res));
        end
        
        if (curInput + 1 == `INPUT_SIZE)
        begin
            $finish(0);
        end
        
    endrule

endmodule


interface FIR_FILTER;

    method Action setCoeff(TAP_IDX k, FIR_VAL v);
    method Action put(FIR_VAL v);
    method ActionValue#(FIR_VAL) get();

endinterface

module mkFIRFilterSequential (FIR_FILTER);

    Vector#(`N_TAPS, Reg#(FIR_VAL)) rs <- replicateM(mkRegU());
    Vector#(`N_TAPS, Reg#(FIR_VAL)) coeffs <- replicateM(mkRegU());
    
    method Action setCoeff(TAP_IDX k, FIR_VAL v);
        coeffs[k] <= v;
    endmethod
    
    method Action put(FIR_VAL v);
        rs[0] <= v;
    endmethod
    
    method ActionValue#(FIR_VAL) get();

        for (Integer x = 0; x < (`N_TAPS-1); x = x + 1)
        begin
            rs[x+1] <= rs[x] + (rs[x] * coeffs[x]);
        end
        
        return rs[`N_TAPS-1] + (rs[`N_TAPS-1] * coeffs[`N_TAPS-1]);

    endmethod

endmodule

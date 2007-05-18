interface ChannelIO;
    method Bit#(32) read();
    method Action   write(Bit#(32) data);
endinterface

import "BDPI" function Bit#(8)  cio_open(Bit#(8) programID);
import "BDPI" function Bit#(32) cio_read(Bit#(8) handle);
import "BDPI" function Action   cio_write(Bit#(8) handle, Bit#(32) data);

module mkChannelIO(ChannelIO);

    Reg#(Bit#(8))   handle  <- mkReg(0);
    Reg#(Bit#(1))   ready   <- mkReg(0);

    rule initialize(ready == 0);
        handle <= cio_open(0);
        ready  <= 1;
    endrule

    method Bit#(32) read() if (ready == 1);
        return cio_read(handle);
    endmethod

    method Action write(Bit#(32) data) if (ready == 1);
        cio_write(handle, data);
    endmethod

endmodule

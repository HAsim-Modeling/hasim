import channelio::*;
import toplevel_wires::*;

interface FrontPanel;
    method Bit#(9)  readSwitches();
    method Action   writeLEDs(Bit#(4) data);
endinterface

module mkFrontPanel#(TopLevelWires wires) (FrontPanel);
    // maintain input and output caches
    Reg#(Bit#(32))  inputCache  <- mkReg(0);
    Reg#(Bit#(32))  outputCache <- mkReg(0);

    // we assume that the model has been properly configured
    // and we have a channel io to a UNIX process
    // open a channel to a hasim-front-panel process (ID = 0)
    ChannelIO       channel     <- mkChannelIO();

    // we want readSwitches() to be a pure value method (to provide
    // the illusion of a wire coming from a physical switch.
    // Therefore we cannot probe the channel and update our
    // internal cache in this method; we do this in a separate
    // rule
    rule updateInputCache (True);
        inputCache <= fromMaybe(inputCache, channel.read());
    endrule

    // the actual readSwitches() method
    method Bit#(9) readSwitches();
        return inputCache[8:0];
    endmethod

    method Action writeLEDs(Bit#(4) data);
        // write to channel only if state has changed
        Bit#(32) ext = zeroExtend(data);
        if (ext != outputCache)
        begin
            outputCache <= ext;
            channel.write(ext);
        end
    endmethod

endmodule

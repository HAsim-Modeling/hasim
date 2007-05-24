import channelio::*;

interface FrontPanel;
    method Bit#(32) readSwitch();
    method Action   writeLED(Bit#(32) data);
endinterface

module mkFrontPanel(FrontPanel);
    // maintain input and output caches
    Reg#(Bit#(32))  inputCache  <- mkReg(0);
    Reg#(Bit#(32))  outputCache <- mkReg(0);

    // we assume that the model has been properly configured
    // and we have a channel io to a UNIX process
    // open a channel to a hasim-front-panel process (ID = 0)
    ChannelIO       channel     <- mkChannelIO();

    // we want readSwitch() to be a pure value method (to provide
    // the illusion of a wire coming from a physical switch.
    // Therefore we cannot probe the channel and update our
    // internal cache in this method; we do this in a separate
    // rule
    rule updateInputCache (True);
        inputCache <= fromMaybe(inputCache, channel.read());
    endrule

    // the actual readSwitch() method
    method Bit#(32) readSwitch();
        return inputCache;
    endmethod

    method Action writeLED(Bit#(32) data);
        // write to channel only if state has changed
        if (data != outputCache)
        begin
            outputCache <= data;
            channel.write(data);
        end
    endmethod

endmodule

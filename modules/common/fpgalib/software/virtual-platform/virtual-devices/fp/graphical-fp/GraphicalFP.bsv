import channelio::*;

interface FrontPanel;
    method Bit#(32) readSwitch();
    method Action   writeLED(Bit#(32) data);
endinterface

module mkFrontPanel(FrontPanel);

    // we assume that the model has been properly configured
    // and we have a channel io to a UNIX process
    // open a channel to a hasim-front-panel process (ID = 0)
    ChannelIO channel <- mkChannelIO();

    method Bit#(32) readSwitch();
        return channel.read();
    endmethod

    method Action writeLED(Bit#(32) data);
        channel.write(data);
    endmethod

endmodule

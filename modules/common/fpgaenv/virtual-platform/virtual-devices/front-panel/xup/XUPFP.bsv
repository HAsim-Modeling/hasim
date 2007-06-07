import toplevel_wires::*;

interface FrontPanel;
    method Bit#(9)  readSwitches();
    method Action   writeLEDs(Bit#(4) data);
endinterface

module mkFrontPanel#(TopLevelWires wires) (FrontPanel);

    method Bit#(9) readSwitches();
        // read from toplevel wires
        Bit#(9) all_inputs;

        all_inputs[3:0] = wires.getSwitches();
        all_inputs[4]   = wires.getButtonUp();
        all_inputs[5]   = wires.getButtonLeft();
        all_inputs[6]   = wires.getButtonCenter();
        all_inputs[7]   = wires.getButtonRight();
        all_inputs[8]   = wires.getButtonDown();

        return all_inputs;
    endmethod

    method Action writeLEDs(Bit#(4) data);
        // write to toplevel wires
        wires.setLEDs(data);
    endmethod

endmodule

import FPGA_Common::*;


module mkLED_Controller (LED_Controller);

  method setLeds (Bit#(4) newval);
  
    $display("LEDs: %0b", newval);
  
  endmethod

endmodule

module mkSwitch_Controller (Switch_Controller);

  method Bit#(4) readSwitches();
  
    return 0;
  
  endmethod

endmodule


module mkButton_Controller (Button_Controller);

  method Bit#(1) button_up() = 0;
  method Bit#(1) button_right() = 0;
  method Bit#(1) button_left() = 0;
  method Bit#(1) button_down() = 0;
  method Bit#(1) button_center() = 0;

endmodule

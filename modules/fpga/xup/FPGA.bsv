import FPGA_Common::*;


import "BVI" prim_led =
  module mkPrim_LED(Bit#(4) v, Empty ifc);
    port led_in = v;
  endmodule

module mkLED_Controller (LED_Controller);

  Reg#(Bit#(4)) val <- mkReg(0);
  
  Empty leds <- mkPrim_LED(val);

  method Action setLEDs (Bit#(4) newval);
    
    val <= newval;
  endmethod

endmodule


import "BVI" prim_switch =
  module mkSwitch_Controller(Switch_Controller ifc);
    method switch readSwitches();
  endmodule

import "BVI" prim_button =
  module mkButton_Controller(Button_Controller ifc);
    method button_up button_up();
    method button_right button_right();
    method button_left button_left();
    method button_down button_down();
    method button_center button_center();
  endmodule


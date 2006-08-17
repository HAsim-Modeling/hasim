
interface LED_Controller;

  method Action setLEDs(Bit#(4) newleds);

endinterface

interface Switch_Controller;

  method Bit#(4) readSwitches();

endinterface

interface Button_Controller;

  method Bit#(1) button_up();
  method Bit#(1) button_right();
  method Bit#(1) button_left();
  method Bit#(1) button_down();
  method Bit#(1) button_center();

endinterface


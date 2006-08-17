
module prim_led(RST_N,
		CLK,
		CLK_GATE,
		led_in,
		led);
  input  CLK;
  input  RST_N;
  input  CLK_GATE;
  input  [3:0] led_in;

  output [3:0] led;

  assign led = ~led_in;

endmodule 

module prim_switch(RST_N,
		   CLK,
		   CLK_GATE,
		   switch);
  input  CLK;
  input  RST_N;
  input  CLK_GATE;

  output [3:0] switch;

endmodule 

module prim_button(RST_N,
		   CLK,
		   CLK_GATE,
		   button_up,
		   button_down,
		   button_left,
		   button_right,
		   button_center);
  input  CLK;
  input  RST_N;
  input  CLK_GATE;
  
  output button_up;
  output button_down;
  output button_left;
  output button_right;
  output button_center;

endmodule 

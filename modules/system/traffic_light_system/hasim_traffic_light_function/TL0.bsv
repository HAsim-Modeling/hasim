// Copyright 2000--2006 Bluespec, Inc.  All rights reserved.

`include "soft_connections.bsh"
`include "front_panel_service.bsh"
`include "clocks_device.bsh"
`include "soft_clocks.bsh"
`include "fpga_components.bsh"

// Simple model of a traffic light
// (modeled after the light at the intersection of Rte 16 and Broadway
//  on the border between Arlington, MA and Somerville, MA)

// Version 0: just model the normal cycle of states
  
typedef enum {
   GreenNS, AmberNS, RedAfterNS,
   GreenE, AmberE, RedAfterE,
   GreenW, AmberW, RedAfterW} TLstates deriving (Eq, Bits);

module [CONNECTED_MODULE] mk_traffic_light();
  UserClock domain <- mkSoftClock(40);
  let tl0 <- mk_traffic_light_domain(clocked_by domain.clk, reset_by domain.rst);
endmodule

module [CONNECTED_MODULE] mk_traffic_light_domain();
   Reg#(TLstates) state <- mkReg(RedAfterW);
   
   Connection_Send#(FRONTP_MASKED_LEDS) link_leds <- mkConnection_Send("fpga_leds");
   Connection_Receive#(FRONTP_SWITCHES) link_switches <- mkConnection_Receive("fpga_switches");
   Connection_Receive#(FRONTP_BUTTON_INFO) link_buttons <- mkConnection_Receive("fpga_buttons");
   
   Reg#(Bit#(32)) waitCount <- mkReg(`MAX_WAIT);
   Reg#(FRONTP_MASKED_LEDS) leds <- mkReg(FRONTP_MASKED_LEDS {state: 0, mask: 'b1111});
   
   rule waiting (waitCount != 0);
      waitCount <= waitCount - 1;
   endrule
   
   rule fromGreenNS (state == GreenNS && waitCount == 0);
      state <= AmberNS;
      waitCount <= `MAX_WAIT;
   endrule
   
   rule fromAmberNS (state == AmberNS && waitCount == 0);
      state <= RedAfterNS;
      waitCount <= `MAX_WAIT;
   endrule

   rule fromRedAfterNS (state == RedAfterNS && waitCount == 0);
      state <= GreenE;
      leds.state <= 'b0100;
      link_leds.send(leds);
      waitCount <= `MAX_WAIT;
   endrule

   rule fromGreenE (state == GreenE && waitCount == 0);
      state <= AmberE;
      waitCount <= `MAX_WAIT;
   endrule

   rule fromAmberE (state == AmberE && waitCount == 0);
      state <= RedAfterE;
      waitCount <= `MAX_WAIT;
   endrule


   rule fromRedAfterE (state == RedAfterE && waitCount == 0);
      state <= GreenW;
      leds.state <= 'b0001;
      link_leds.send(leds);
      waitCount <= `MAX_WAIT;
   endrule

   rule fromGreenW (state == GreenW && waitCount == 0);
      state <= AmberW;
      waitCount <= `MAX_WAIT;
   endrule

   rule fromAmberW (state == AmberW && waitCount == 0);
      state <= RedAfterW;
      waitCount <= `MAX_WAIT;
   endrule

   rule fromRedAfterW (state == RedAfterW && waitCount == 0);
      state <= GreenNS;
      leds.state <= 'b1010;
      link_leds.send(leds);
      waitCount <= `MAX_WAIT;
   endrule

endmodule

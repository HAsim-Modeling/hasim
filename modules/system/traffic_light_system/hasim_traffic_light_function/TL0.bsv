// Copyright 2000--2006 Bluespec, Inc.  All rights reserved.

package TL0;

import HASim::*;

// Simple model of a traffic light
// (modeled after the light at the intersection of Rte 16 and Broadway
//  on the border between Arlington, MA and Somerville, MA)

// Version 0: just model the normal cycle of states

`define MAX_WAIT 100

// An empty interface:
interface TL;
endinterface: TL

typedef enum {
   GreenNS, AmberNS, RedAfterNS,
   GreenE, AmberE, RedAfterE,
   GreenW, AmberW, RedAfterW} TLstates deriving (Eq, Bits);

module [HASim_Module] sysTL(TL);
   Reg#(TLstates) state <- mkReg(RedAfterW);
   
   Connection_Send#(Bit#(4)) link_leds <- mkConnection_Send("fpga_leds");
   Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
   Connection_Receive#(ButtonInfo) link_buttons <- mkConnection_Receive("fpga_buttons");
   
   Reg#(Bit#(32)) waitCount <- mkReg(`MAX_WAIT);
   
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
      link_leds.send(4'b0100);
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
      link_leds.send(4'b0001);
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
      link_leds.send(4'b1010);
      waitCount <= `MAX_WAIT;
   endrule

endmodule

endpackage


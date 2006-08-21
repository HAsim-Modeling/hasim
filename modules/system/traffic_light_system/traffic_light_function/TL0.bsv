// Copyright 2000--2006 Bluespec, Inc.  All rights reserved.

package TL0;

import HASim::*;

// Simple model of a traffic light
// (modeled after the light at the intersection of Rte 16 and Broadway
//  on the border between Arlington, MA and Somerville, MA)

// Version 0: just model the normal cycle of states

typedef enum {
   GreenNS, AmberNS, RedAfterNS,
   GreenE, AmberE, RedAfterE,
   GreenW, AmberW, RedAfterW} TLstates deriving (Eq, Bits);

module [HASim_Module] sysTL ();
   Reg#(TLstates) state <- mkReg(RedAfterW);
   Connection_Send#(Bit#(4)) link_leds <- mkConnection_Send("fpga_leds");
   Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
   Connection_Receive#(ButtonInfo) link_buttons <- mkConnection_Receive("fpga_buttons");
   
   rule fromGreenNS (state == GreenNS);
      state <= AmberNS;
   endrule
   
   rule fromAmberNS (state == AmberNS);
      state <= RedAfterNS;
   endrule

   rule fromRedAfterNS (state == RedAfterNS);
      state <= GreenE;
      link_leds.send(4'b0100);
   endrule

   rule fromGreenE (state == GreenE);
      state <= AmberE;
   endrule

   rule fromAmberE (state == AmberE);
      state <= RedAfterE;
   endrule

   rule fromRedAfterE (state == RedAfterE);
      state <= GreenW;
      link_leds.send(4'b0001);
   endrule: fromRedAfterE

   rule fromGreenW (state == GreenW);
      state <= AmberW;
   endrule

   rule fromAmberW (state == AmberW);
      state <= RedAfterW;
   endrule

   rule fromRedAfterW (state == RedAfterW);
      state <= GreenNS;
      link_leds.send(4'b1010);
   endrule

endmodule

endpackage


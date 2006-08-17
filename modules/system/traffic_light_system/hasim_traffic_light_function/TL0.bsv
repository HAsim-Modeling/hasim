// Copyright 2000--2006 Bluespec, Inc.  All rights reserved.

package TL0;

import HASim::*;
import FPGA_Common::*;
import FPGA::*;

// Simple model of a traffic light
// (modeled after the light at the intersection of Rte 16 and Broadway
//  on the border between Arlington, MA and Somerville, MA)

// Version 0: just model the normal cycle of states

// An empty interface:
interface TL;
endinterface: TL

typedef enum {
   GreenNS, AmberNS, RedAfterNS,
   GreenE, AmberE, RedAfterE,
   GreenW, AmberW, RedAfterW} TLstates deriving (Eq, Bits);

module [HASim_Module] sysTL(TL);
   Reg#(TLstates) state <- mkReg(RedAfterW);
   LED_Controller leds <- mkLED_Controller();
   
   rule fromGreenNS (state == GreenNS);
      state <= AmberNS;
   endrule: fromGreenNS
   
   rule fromAmberNS (state == AmberNS);
      state <= RedAfterNS;
   endrule: fromAmberNS

   rule fromRedAfterNS (state == RedAfterNS);
      state <= GreenE;
      leds.setLEDs(4'b0100);
   endrule: fromRedAfterNS

   rule fromGreenE (state == GreenE);
      state <= AmberE;
   endrule: fromGreenE

   rule fromAmberE (state == AmberE);
      state <= RedAfterE;
   endrule: fromAmberE

   rule fromRedAfterE (state == RedAfterE);
      state <= GreenW;
      leds.setLEDs(4'b0001);
   endrule: fromRedAfterE

   rule fromGreenW (state == GreenW);
      state <= AmberW;
   endrule: fromGreenW

   rule fromAmberW (state == AmberW);
      state <= RedAfterW;
   endrule: fromAmberW

   rule fromRedAfterW (state == RedAfterW);
      state <= GreenNS;
      leds.setLEDs(4'b1010);
   endrule: fromRedAfterW

endmodule: sysTL

endpackage: TL0


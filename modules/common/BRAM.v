//////////////////////////////////////////////////////////////////////
//
// BRAM.v
// Copyright (C) 2005 Carnegie Mellon University
//
// Description:
// Dual-ported BRAM Verilog model
//
// Revision History
// File created
// 5-18-2006, Eric Chung
//
//
// Triple read port, single write port synchronous Block RAM (synthesizable onto Xilinx block rams)
// Created on 5-17-2006
// Eric S. Chung 
//


module BRAM(CLK, RST_N,
            WR_ADDR,  WR_VAL, WR_EN,
            RD1_ADDR, RD1_EN,
	    RD2_ADDR, RD2_EN,
	    RD3_ADDR, RD3_EN,
	    DOUT1, DOUT1_RDY,
	    DOUT2, DOUT2_RDY,
	    DOUT3, DOUT3_RDY);

   // synopsys template   
   parameter                   addr_width = 1;
   parameter                   data_width = 1;
   parameter                   lo = 0;
   parameter                   hi = 1;
   
   input                       CLK;
   input		       RST_N;   

   // Write port
   input [addr_width - 1 : 0]  WR_ADDR;
   input [data_width - 1 : 0]  WR_VAL;
   input                       WR_EN;

   // Read port #1
   // req
   input		       RD1_EN;
   input [addr_width - 1 : 0]  RD1_ADDR;
   // resp
   output [data_width - 1 : 0] DOUT1;
   output		       DOUT1_RDY;

   // Read port #2
   // req
   input		       RD2_EN;
   input [addr_width - 1 : 0]  RD2_ADDR;
   // resp
   output [data_width - 1 : 0] DOUT2;
   output		       DOUT2_RDY;


   // Read port #3
   // req
   input		       RD3_EN;
   input [addr_width - 1 : 0]  RD3_ADDR;
   // resp
   output [data_width - 1 : 0] DOUT3;
   output		       DOUT3_RDY;

 
   reg [data_width - 1 : 0]    arr[lo:hi];
   
   reg [data_width - 1 : 0]    DOUT1;
   reg 			       DOUT1_RDY;

   reg [data_width - 1 : 0]    DOUT2;
   reg 			       DOUT2_RDY; 

   reg [data_width - 1 : 0]    DOUT3;
   reg 			       DOUT3_RDY;
   
   integer x;
 

   always@(posedge CLK)
     begin

       
       if (!RST_N) 
         begin
	   for (x = lo; x < hi; x = x + 1)
	   begin
	     arr[x] <= 0;
	   end

           DOUT1_RDY <= 1'b0;
	   DOUT2_RDY <= 1'b0;
	   DOUT3_RDY <= 1'b0;
         end
       else 
         begin

           DOUT1_RDY <= RD1_EN; 
	   DOUT2_RDY <= RD2_EN;
	   DOUT3_RDY <= RD3_EN;

	   if (RD1_EN) begin
             DOUT1 <= arr[RD1_ADDR];
	   end

	   if (RD2_EN) begin
             DOUT2 <= arr[RD2_ADDR];
	   end

	   if (RD3_EN) begin
             DOUT3 <= arr[RD3_ADDR];
	   end


	   if (WR_EN)
             arr[WR_ADDR] <= WR_VAL;
         end
     end // always@ (posedge CLK)

endmodule


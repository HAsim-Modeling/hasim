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
            RD1_ADDR, RD1_EN, RD1_RDY,
	    RD2_ADDR, RD2_EN, RD2_RDY,
	    RD3_ADDR, RD3_EN, RD3_RDY,
	    RES1, RES1_RDY, RES1_EN, 
	    RES2, RES2_RDY, RES2_EN,
	    RES3, RES3_RDY, RES3_EN);

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
   output                      RD1_RDY;
   // resp
   output [data_width - 1 : 0] RES1;
   output                      RES1_RDY;
   input                       RES1_EN;


   // Read port #2
   // req
   input		       RD2_EN;
   input [addr_width - 1 : 0]  RD2_ADDR;
   output                      RD2_RDY;
   // resp
   output [data_width - 1 : 0] RES2;
   output                      RES2_RDY;
   input                       RES2_EN;


   // Read port #3
   // req
   input		       RD3_EN;
   input [addr_width - 1 : 0]  RD3_ADDR;
   output                      RD3_RDY;
   // resp
   output [data_width - 1 : 0] RES3;
   output                      RES3_RDY;
   input                       RES3_EN;

 
   reg [data_width - 1 : 0]    arr[lo:hi];
   
   reg REQ_MADE1;
   reg REQ_MADE2;
   reg REQ_MADE3;
   
   reg [data_width - 1 : 0]    DOUT1;

   reg [data_width - 1 : 0]    DOUT2;

   reg [data_width - 1 : 0]    DOUT3;
   
   integer x;

   
   FIFO2#(.width(data_width), .guarded(1)) q1(.RST_N(RST_N),
					      .CLK(CLK),
					      .D_IN(DOUT1),
					      .ENQ(REQ_MADE1),
					      .DEQ(RES1_EN),
					      .CLR(1'b0),
					      .D_OUT(RES1),
					      .FULL_N(RD1_RDY),
					      .EMPTY_N(RES1_RDY));

   FIFO2#(.width(data_width), .guarded(1)) q2(.RST_N(RST_N),
					      .CLK(CLK),
					      .D_IN(DOUT2),
					      .ENQ(REQ_MADE2),
					      .DEQ(RES2_EN),
					      .CLR(1'b0),
					      .D_OUT(RES2),
					      .FULL_N(RD2_RDY),
					      .EMPTY_N(RES2_RDY));

   FIFO2#(.width(data_width), .guarded(1)) q3(.RST_N(RST_N),
					      .CLK(CLK),
					      .D_IN(DOUT3),
					      .ENQ(REQ_MADE3),
					      .DEQ(RES3_EN),
					      .CLR(1'b0),
					      .D_OUT(RES3),
					      .FULL_N(RD3_RDY),
					      .EMPTY_N(RES3_RDY));
   always@(posedge CLK)
     begin

       
       if (!RST_N) 
         begin
           // synopsys translate_off
	   for (x = lo; x < hi; x = x + 1)
	   begin
	     arr[x] <= 0;
	   end
           // synopsys translate_on
         end
       else 
         begin 
	   REQ_MADE1 <= RD1_EN;
	   REQ_MADE2 <= RD2_EN;
	   REQ_MADE3 <= RD3_EN;
 

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


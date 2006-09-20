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
            WR_ADDR, D_IN, WE,
            RD_ADDR, D_OUT, RE, RD_RDY,
	    RD_ADDR2, D_OUT2, RE2, RD_RDY2,
	    RD_ADDR3, D_OUT3, RE3, RD_RDY3);

   // synopsys template   
   parameter                   addr_width = 1;
   parameter                   data_width = 1;
   parameter                   lo = 0;
   parameter                   hi = 1;
   
   input                       CLK;
   input		       RST_N;   

   // Write port
   input [addr_width - 1 : 0]  WR_ADDR;
   input [data_width - 1 : 0]  D_IN;
   input                       WE;

   // Read port #1
   input		       RE;
   input [addr_width - 1 : 0]  RD_ADDR;
   output [data_width - 1 : 0] D_OUT;
   output		       RD_RDY; 

   // Read port #2
   input		       RE2;
   input [addr_width - 1 : 0]  RD_ADDR2;
   output [data_width - 1 : 0] D_OUT2;
   output		       RD_RDY2; 


   // Read port #3
   input		       RE3;
   input [addr_width - 1 : 0]  RD_ADDR3;
   output [data_width - 1 : 0] D_OUT3;
   output		       RD_RDY3; 
 

 
   reg [data_width - 1 : 0]    arr[lo:hi];
   
   reg [data_width - 1 : 0]    D_OUT;
   //reg 			       RD_RDY; 

   reg [data_width - 1 : 0]    D_OUT2;
   //reg 			       RD_RDY2; 

   reg [data_width - 1 : 0]    D_OUT3;
   //reg 			       RD_RDY3; 
 

   assign RD_RDY = 1'b1;
   assign RD_RDY2 = 1'b1;
   assign RD_RDY3 = 1'b1;

   always@(posedge CLK)
     begin

       /*
       if (!RST_N) begin
         RD_RDY <= 1'b0;
	 RD_RDY2 <= 1'b0;
	 RD_RDY3 <= 1'b0;
       end

       else begin
         RD_RDY <= RE; 
	 RD_RDY2 <= RE2;
	 RD_RDY3 <= RE3;
       end
       */
       if (RE) begin
         D_OUT <= arr[RD_ADDR];       
       end

       if (RE2) begin
         D_OUT2 <= arr[RD_ADDR2];
       end

       if (RE3) begin
         D_OUT3 <= arr[RD_ADDR3];
       end
 
	
       if (WE)
         arr[WR_ADDR] <= D_IN;

     end // always@ (posedge CLK)

endmodule


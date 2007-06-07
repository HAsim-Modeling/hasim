
import hasim_common::*;

module [HASim_Module] mkSystem ();

   BRAM#(Bit#(8), Bit#(32)) br <- mkBRAM_Full();
   Reg#(Bool) started <- mkReg(False);
   Reg#(Bool) finishing <- mkReg(False);
   Reg#(Bit#(8)) counter <- mkReg(0);

   Connection_Send#(Bit#(4))       link_leds     <- mkConnection_Send("fpga_leds");
   Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
   Connection_Receive#(ButtonInfo) link_buttons  <- mkConnection_Receive("fpga_buttons");

   rule initial_req (!started);
   
     br.read_req(0);
   
   endrule
   
   rule initial_resp (!started);
     
     let x <- br.read_resp();
     
     if (x != 0)
     begin
       started <= True;
       link_leds.send(4'b0001);
     end
   
   endrule
   
   rule process (started && !finishing);
   
     let n <- br.read_resp();
     
     if (counter != 0)
     begin
       br.write(counter, n + 1);
       counter <= counter + 1;
     end
     
     if (counter < 255)
       br.read_req(counter + 1);
     else
       finishing <= True;
   
   endrule
   
   rule finishUp (started && finishing);
      br.write(0, 0);
      started <= False;
      finishing <= False;
      link_leds.send(4'b0011);
   endrule
   
endmodule


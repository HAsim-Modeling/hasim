import hasim_common::*;

//An example DME that simply loads a bunch of addresses, adds 1 to the data, and writes them back

/*
Mnemonic     Position	Range	Width	Description   
---------------------------------------------------
reserve	         74:71		4	1111	   
ADDR	         70:38	35:3	33	Address	   
TID	         37:26	11:0	12	The TID is used by clusters to associate responses with requests. This field is set only by the original requestor. Forwarding agents should mantain the same ID and should not change them. When generating a response a cluster should use the same TID that came along with the associated request.
REQ/RSP	         25	1	1	Request   : 0   Response : 1	   
R/W	         24	1	1	Read : 0   Write : 1	   
INV	         23	1	1	Invalidate	   
INTR	         22:20	2:0	3	No Interrupt : 000
				        INT0   : 001
				        INT1   : 010
				        INT2   : 011
				        INT3   : 100 
LEN	         19:17	2:0	3	Length of data associated with the request. 
                                        0-8Bytes     : 000 (Byte Enables active)
                                        16B	       : 001		
                                        32B	       : 010
                                        64B	       : 011
                                        Reserved    : 1XX	   
BE	         16:9	7:0	8	Byte Enables for partial reads/writes of length 0 - 8 bytes	   
RSP_REQD	 8	1	1	Response Required : Setting this bit indicates that the requesting cluster requires a response to be sent back . The response indicates that the request has completed. The response is synthesized by the cluster that completes the request .	   
DATA_PR	         7	1	1	Is there data associated with the command?
                                        No   : 0
                                        Yes : 1  
UPDT_CACHE
(Reserved)	 6	1	1	Update the cache with the data associated with this command e.g. Write.
                                        OR 
                                        Update the cache with the data that comes with the response for this command e.g. Read
                                        No cache update    : 0
                                        Update cache	     : 1	   
DATA_SRC	 5:3	2:0	3	Data buffer that holds the data associated with this command. 
                                        SPL data buffer            : 010
DATA_DST	 2:0	2:0	3	Destination data buffer into which the data should be stored.
                                        SPL data buffer  : 010
                                        SVD              : Others    
Total # of bits	 	 	75	 	 
*/

function Bit#(36) fsb_cmd_addr(Bit#(75) cmd);
  return {cmd[70:38], 3'b000};
endfunction

function Bit#(3) fsb_cmd_len(Bit#(75) cmd);
  return cmd[19:17];
endfunction

function Bit#(75) fsb_cmd_new();
  return
    {
      4'b1111,           //reserved
      33'b0,             //addr
      12'b0,             //tid
      1'b0,              // == A request
      1'b0,              // == A read
      1'b0,              // == Not an Invalidate
      3'b000,            // == No Interrupt
      3'b0,              //len
      8'b11111111,       // == all bytes
      1'b0,              // == no response required
      1'b0,              // == no data associated
      1'b0,              // == no cache update
      3'b010,            // == SPL/DME
      3'b010             // == SPL/DME
    };
endfunction

function Bit#(75) fsb_cmd_load(Bit#(36) addr, Bit#(3) len);

  let cmd = fsb_cmd_new();
  cmd[70:38] = addr[35:3]; //Set the address
  cmd[19:17] = len;        //Set the length
  return cmd;

endfunction

function Bit#(75) fsb_cmd_store(Bit#(36) addr, Bit#(3) len);

  let cmd = fsb_cmd_new();
  cmd[70:38] = addr[35:3]; //Set the address
  cmd[19:17] = len;        //Set the length
  cmd[24]    = 1'b1;       // == A Store
  cmd[7]     = 1'b1;       // == Data is associated with this cmd
  return cmd;

endfunction

module [HASIM_MODULE] mkDME_Alg ();
   
  //Connection_Client#(Bit#(256), Bit#(256)) link_dme <- mkConnection_Server("dme_to_afu");
  Connection_Send#(Tuple3#(Bit#(256), Bool, Bool))     link_to_fsb_data      <- mkConnection_Send("dme_to_fsb_data");
  Connection_Receive#(Tuple3#(Bit#(256), Bool, Bool))  link_from_fsb_data    <- mkConnection_Receive("fsb_to_dme_data");
  Connection_Send#(Bit#(75))                           link_to_fsb_cmd       <- mkConnection_Send("dme_to_fsb_cmd");
  Connection_Receive#(Bit#(75))                        link_from_fsb_cmd     <- mkConnection_Receive("fsb_to_dme_cmd");

  Reg#(Bit#(36)) cur <- mkReg(0);
  
  (* descending_urgency= "reqLoad, reqStore" *)
  
  rule reqLoad (cur[35:3] < 255);
  
    let cmd = fsb_cmd_load(cur, 3'b010); //Load 32B == 256 bits == 1 data response
    cur <= cur + zeroExtend(4'b1000);
    link_to_fsb_cmd.send(cmd);
    
  endrule
  
  rule reqStore (True);
  
    let rsp = link_from_fsb_cmd.receive();
    link_from_fsb_cmd.deq();
    
    match {.d, .b, .e}   = link_from_fsb_data.receive();
    link_from_fsb_data.receive();
    
    let cmd = fsb_cmd_store(fsb_cmd_addr(rsp), 3'b010);
    link_to_fsb_cmd.send(cmd);
    link_to_fsb_data.send(tuple3(d + 1, True, True));
  
  endrule
  
endmodule

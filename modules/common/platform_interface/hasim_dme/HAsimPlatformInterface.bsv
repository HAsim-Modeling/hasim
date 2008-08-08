import hasim_common::*;
import front_panel::*;
import physical_platform::*;

module [HASIM_MODULE] mkPlatformInterface(TOP_LEVEL_WIRES);

    // instantiate connections
    Connection_Send#(Tuple3#(Bit#(256), Bool, Bool))     link_to_dme_data      <- mkConnection_Send("fsb_to_dme_data");
    Connection_Receive#(Tuple3#(Bit#(256), Bool, Bool))  link_from_dme_data    <- mkConnection_Receive("dme_to_fsb_data");
    Connection_Send#(Bit#(75))                           link_to_dme_cmd       <- mkConnection_Send("fsb_to_dme_cmd");
    Connection_Receive#(Bit#(75))                        link_from_dme_cmd     <- mkConnection_Receive("dme_to_fsb_cmd");
    
    // instantiate top-level wires
    TopLevelWiresDriver             wires           <- mkTopLevelWiresDriver();

    // instantiate virtual devices
    FrontPanel                      frontPanel      <- mkFrontPanel(wires);

    //rules
    rule moveDataFromFSB (True);
    
      let d <- frontPanel.getData();
      link_to_dme_data.send(d);
    
    endrule

    rule moveCmdFromFSB (True);
    
      let c <- frontPanel.getCmd();
      link_to_dme_cmd.send(c);
    
    endrule

    rule moveDataToFSB (True);
    
      match {.d, .b, .e} = link_from_dme_data.receive();
      link_from_dme_data.deq();
      
      frontPanel.putData(d, b, e);
    
    endrule

    rule moveCmdToFSB (True);
    
      let c = link_from_dme_cmd.receive();
      link_from_dme_cmd.deq();

      frontPanel.putCmd(c);
    
    endrule

    // return interface to top-level wires
    return wires.wires_out;

endmodule

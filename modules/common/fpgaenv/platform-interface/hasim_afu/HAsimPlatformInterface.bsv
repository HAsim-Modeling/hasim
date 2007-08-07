import hasim_common::*;
import front_panel::*;
import toplevel_wires::*;

module [HASim_Module] mkPlatformInterface(TopLevelWires);

    // instantiate connections
    Connection_Client#(Bit#(256), Bit#(256))    link_afu       <- mkConnection_Client("dme_to_afu");

    // instantiate top-level wires
    TopLevelWiresDriver             wires           <- mkTopLevelWiresDriver();

    // instantiate virtual devices
    FrontPanel                      frontPanel      <- mkFrontPanel(wires);

    //rules
    rule moveReq (True);
    
      let d <- frontPanel.getRequest();
      link_afu.makeReq(d);
    
    endrule

    rule moveRsp (True);
    
      let d = link_afu.getResp();
      link_afu.deq();

      frontPanel.makeResponse(d);
    
    endrule

    // return interface to top-level wires
    return wires.wires_out;

endmodule

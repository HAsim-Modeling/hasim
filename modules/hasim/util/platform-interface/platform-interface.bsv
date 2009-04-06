//
// Copyright (C) 2008 Massachusetts Institute of Technology
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//
// Tokens are the main way for HAsim to track data across simulator      
// partitions. The token type includes an index for token tables, epochs,
// and scratchpads which partitions can use as they see fit.             

import Vector::*;

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/front_panel.bsh"
`include "asim/provides/physical_platform.bsh"
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/low_level_platform_interface.bsh"
`include "asim/provides/rrr.bsh"
`include "asim/provides/scratchpad_memory.bsh"
`include "asim/provides/central_cache.bsh"
`include "asim/provides/streams.bsh"

`include "asim/rrr/server_connections.bsh"
`include "asim/rrr/client_connections.bsh"

`include "asim/dict/ASSERTIONS_PLATFORM_INTERFACE.bsh"
`include "asim/dict/PARAMS_PLATFORM_INTERFACE.bsh"

typedef struct
{
    Bit#(1) b_up;
    Bit#(1) b_down;
    Bit#(1) b_left;
    Bit#(1) b_right;
    Bit#(1) b_center;
}
ButtonInfo
    deriving (Eq, Bits);


module [HASIM_MODULE] mkPlatformInterface#(Clock topLevelClock, Reset topLevelReset)
    // interface
        (TOP_LEVEL_WIRES);

    // instantiate connections
    Connection_Receive#(FRONTP_MASKED_LEDS) link_leds <- mkConnection_Receive("fpga_leds");
    Connection_Send#(FRONTP_SWITCHES) link_switches   <- mkConnection_Send("fpga_switches");
    Connection_Send#(ButtonInfo)      link_buttons    <- mkConnection_Send("fpga_buttons");

    // soft reset
    Connection_Send#(Bool) link_reset <- mkConnection_Send("soft_reset");

    // ***** Dynamic parameters *****
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();

    Param#(1) enableCentralCacheParam <- mkDynamicParameter(`PARAMS_PLATFORM_INTERFACE_CENTRAL_CACHE_ENABLE, paramNode);
    function Bool enableCentralCache() = (enableCentralCacheParam == 1);

    Param#(1) writeBackCentralCacheParam <- mkDynamicParameter(`PARAMS_PLATFORM_INTERFACE_CENTRAL_CACHE_WRITE_BACK, paramNode);
    function Bool centralCacheIsWriteBack() = (writeBackCentralCacheParam == 1);

    // ***** Assertion Checkers *****
    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_PLATFORM_INTERFACE__BASE);
    ASSERTION assertScratchpadSpace <- mkAssertionChecker(`ASSERTIONS_PLATFORM_INTERFACE_SCRATCHPAD_FULL, ASSERT_ERROR, assertNode);

    // currently only one user can read and write memory
    Vector#(SCRATCHPAD_N_CLIENTS, Connection_Server#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_MEM_VALUE)) link_memory = newVector();
    for (Integer p = 0; p < valueOf(SCRATCHPAD_N_CLIENTS); p = p + 1)
    begin
        link_memory[p] <- mkConnection_Server("vdev_memory_" + integerToString(p));
    end

    // other virtual devices
    Connection_Receive#(STREAMS_REQUEST) link_streams <- mkConnection_Receive("vdev_streams");

    // instantiate low-level platform interface
    LowLevelPlatformInterface llpint <- mkLowLevelPlatformInterface(topLevelClock, topLevelReset);

    // instantiate virtual devices
    FrontPanel frontPanel <- mkFrontPanel(llpint);
    CENTRAL_CACHE_IFC central_cache <- mkCentralCache(llpint);
    SCRATCHPAD_MEMORY_IFC memory <- mkMemoryVirtualDevice(llpint, central_cache);
    Streams streams <- mkStreams(llpint);

    // connection terminus
    let t <- mkConnectionTerminus();

    // auto-generated submodules for RRR connections
    let rrr_server_links <- mkServerConnections(llpint.rrrServer);
    let rrr_client_links <- mkClientConnections(llpint.rrrClient);
    
    // Initialization
    Reg#(Bool) initialized <- mkReg(False);
    rule doInit (! initialized);
        central_cache.init(enableCentralCache, centralCacheIsWriteBack);
        initialized <= True;
    endrule

    // rules
    rule set_leds (True);
        let newval = link_leds.receive();
        link_leds.deq();

        // ask front panel to display my current LED state
        frontPanel.writeLEDs(newval.state, newval.mask);
    endrule
  
    rule send_switches (True);
        // read in switch state from front panel
        FRONTP_SWITCHES sstate = frontPanel.readSwitches();

        // send switch info over the connection
        link_switches.send(sstate);
    endrule

    rule send_buttons (True);
        // read in button state from front panel
        FRONTP_BUTTONS bstate = frontPanel.readButtons();
        ButtonInfo bi = ButtonInfo {
                            b_up: bstate[0],
                            b_down: bstate[4], 
                            b_left: bstate[1],
                            b_right: bstate[3],
                            b_center: bstate[2]
                        };

        // send button info over the connection
        link_buttons.send(bi);
    endrule
    
    rule send_reset (True);
        // accept soft reset request and send it out to whoever cares
        llpint.physicalDrivers.soft_reset();
        link_reset.send(?);
    endrule

    rule send_streams_req (True);

        // read in streams request and send it to device
        let sreq = link_streams.receive();
        link_streams.deq();
        streams.makeRequest(sreq.streamID,
                            sreq.stringID,
                            sreq.payload0,
                            sreq.payload1);

    endrule

    //
    // Scratchpad connections.  One connection for each individual port.
    // Some implementations of scratchpad funnel all ports through the same
    // FIFO and, thus, have conflicts between the ports.  The code below
    // forces a static priority among the ports.
    //
    Rules send_req = emptyRules;
    for (Integer p = 0; p < valueOf(SCRATCHPAD_N_CLIENTS); p = p + 1)
    begin
        let r =
            (rules
                rule sendScratchpadReq (True);
                    let req = link_memory[p].getReq();
                    link_memory[p].deq();

                    case (req) matches
                        tagged SCRATCHPAD_MEM_INIT .allocLastWordIdx:
                        begin
                            let s <- memory.ports[p].init(allocLastWordIdx);
                            assertScratchpadSpace(s);
                        end

                        tagged SCRATCHPAD_MEM_READ .addr:
                        begin
                            memory.ports[p].mem.readReq(addr);
                        end

                        tagged SCRATCHPAD_MEM_WRITE .wr_info:
                        begin
                            memory.ports[p].mem.write(wr_info.addr, wr_info.val);
                        end
                    endcase
                endrule
            endrules);

        send_req = rJoinDescendingUrgency(send_req, r);
    
        rule sendScratchpadResp (True);
            let d <- memory.ports[p].mem.readRsp();
            link_memory[p].makeResp(d);
        endrule
    end

    addRules(send_req);
    
    // return interface to top-level wires
    return llpint.topLevelWires;

endmodule

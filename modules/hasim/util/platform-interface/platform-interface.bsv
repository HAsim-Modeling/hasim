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
`include "asim/provides/clocks_device.bsh"

`include "asim/rrr/server_connections.bsh"
`include "asim/rrr/client_connections.bsh"

`include "asim/dict/ASSERTIONS_PLATFORM_INTERFACE.bsh"
`include "asim/dict/PARAMS_PLATFORM_INTERFACE.bsh"
`include "asim/dict/DEBUG_SCAN_PLATFORM_INTERFACE.bsh"

`include "asim/dict/VDEV.bsh"

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

//
// PLATFORM_INTERFACE: the interface itself is a replica of the LLPI interface
//

interface PLATFORM_INTERFACE;
    
    interface PHYSICAL_DRIVERS physicalDrivers;
    interface TOP_LEVEL_WIRES  topLevelWires;
        
endinterface

//
// mkPlatformInterface: For code cleanliness, we split this module into a raw
//                      raw "un-clocked" module and a real clocked module
//

module [HASIM_MODULE] mkPlatformInterface
    // interface
        (PLATFORM_INTERFACE);

    // Platform Interface is instantiated inside a NULL clock domain, so
    // the first thing we should do is instantiate the LLPI and obtain
    // a clock and reset from it.

    // instantiate low-level platform interface
    LowLevelPlatformInterface llpint <- mkLowLevelPlatformInterface();

    Clock clk = llpint.physicalDrivers.clocksDriver.clock;
    Reset rst = llpint.physicalDrivers.clocksDriver.reset;
    
    // instantiate the real platform interface using our new clock and reset
    let pi <- mkClockedPlatformInterface(llpint, clocked_by clk, reset_by rst);
    
    // export LLPI's drivers and wires to the top level
    interface physicalDrivers = llpint.physicalDrivers;
    interface topLevelWires   = llpint.topLevelWires;
    
endmodule

//
// mkClockedPlatformInterface: The real clocked platform interface
//

module [HASIM_MODULE] mkClockedPlatformInterface#(LowLevelPlatformInterface llpint)
    // interface
        ();

    // instantiate connections
    Connection_Receive#(FRONTP_MASKED_LEDS) link_leds <- mkConnection_Receive("fpga_leds");
    Connection_Send#(FRONTP_SWITCHES) link_switches   <- mkConnection_Send("fpga_switches");
    Connection_Send#(ButtonInfo)      link_buttons    <- mkConnection_Send("fpga_buttons");

    DEBUG_FILE debugLog <- mkDebugFile("platform_interface.out");

    // ***** Dynamic parameters *****
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();

    Param#(3) centralCacheMode <- mkDynamicParameter(`PARAMS_PLATFORM_INTERFACE_CENTRAL_CACHE_MODE, paramNode);

    // ***** Assertion Checkers *****
    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_PLATFORM_INTERFACE__BASE);
    ASSERTION assertScratchpadSpace <- mkAssertionChecker(`ASSERTIONS_PLATFORM_INTERFACE_SCRATCHPAD_FULL, ASSERT_ERROR, assertNode);

    // other virtual devices
    Connection_Receive#(STREAMS_REQUEST) link_streams <- mkConnection_Receive("vdev_streams");

    // instantiate virtual devices
    FrontPanel frontPanel <- mkFrontPanel(llpint);
    let centralCacheStats <- mkCentralCacheStats();
    CENTRAL_CACHE_IFC centralCache <- mkCentralCache(llpint, centralCacheStats);
    SCRATCHPAD_MEMORY_IFC memory <- mkMemoryVirtualDevice(llpint, centralCache);
    Streams streams <- mkStreams(llpint);

    // connection terminus
    let t <- mkConnectionTerminus();

    // auto-generated submodules for RRR connections
    let rrr_server_links <- mkServerConnections(llpint.rrrServer);
    let rrr_client_links <- mkClientConnections(llpint.rrrClient);
    
    // Initialization
    Reg#(Bool) initialized <- mkReg(False);
    rule doInit (! initialized);
        //
        // Initialize central cache.  The first argument controls the mode
        // of the set-associative cache.  The second argument controls whether
        // to cache local memory between the set-associative cache and local
        // memory.
        //
        centralCache.init(unpack(centralCacheMode[1:0]),
                          ! unpack(centralCacheMode[2]));
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
    
    rule send_streams_req (True);

        // read in streams request and send it to device
        let sreq = link_streams.receive();
        link_streams.deq();
        streams.makeRequest(sreq.streamID,
                            sreq.stringID,
                            sreq.payload0,
                            sreq.payload1);

    endrule


    // ====================================================================
    //
    // Scratchpad connections.  One soft connection for each individual
    // port. All the soft connections funnel down into a single
    // memory-style interface to the scratchpad here.
    //
    // ====================================================================

    Vector#(SCRATCHPAD_N_CLIENTS, Connection_Server#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_READ_RESP)) link_memory = newVector();

    Rules mem_send_req = emptyRules;
    for (Integer p = 0; p < valueOf(SCRATCHPAD_N_CLIENTS); p = p + 1)
    begin
        link_memory[p] <- mkConnection_Server("vdev_memory_" + integerToString(p));

        let r =
            (rules
                rule sendScratchpadReq (True);
                    let req = link_memory[p].getReq();
                    link_memory[p].deq();

                    case (req) matches
                        tagged SCRATCHPAD_MEM_INIT .allocLastWordIdx:
                        begin
                            let s <- memory.init(allocLastWordIdx, fromInteger(p));
                            assertScratchpadSpace(s);
                        end

                        tagged SCRATCHPAD_MEM_READ .r_req:
                        begin
                            let ref_info = SCRATCHPAD_REF_INFO { portNum: fromInteger(p),
                                                                 clientRefInfo: r_req.clientRefInfo };
                            memory.readReq(r_req.addr, ref_info);
                        end

                        tagged SCRATCHPAD_MEM_WRITE .w_req:
                        begin
                            memory.write(w_req.addr, w_req.val, fromInteger(p));
                        end
                    endcase
                endrule
            endrules);

        mem_send_req = rJoinDescendingUrgency(mem_send_req, r);
    end
    
    //
    // All read responses from the scratchpad are retrieved here and forwarded
    // to the appropriate soft connection to the client.
    //
    if (valueOf(SCRATCHPAD_N_CLIENTS) > 0)
    begin
        rule sendScratchpadResp (True);
            let r <- memory.readRsp();
            link_memory[r.refInfo.portNum].makeResp(SCRATCHPAD_READ_RESP { val: r.val,
                                                                           addr: r.addr,
                                                                           clientRefInfo: r.refInfo.clientRefInfo });
        endrule
    end

    addRules(mem_send_req);
    


    // ====================================================================
    //
    // Central cache connections.  Two soft connections for each individual
    // port.  One connection is for requests to the cache.  The other
    // is for requests from the cache to backing storage provided by the
    // client.
    //
    // ====================================================================

    Vector#(CENTRAL_CACHE_N_CLIENTS, Connection_Server#(CENTRAL_CACHE_REQ, CENTRAL_CACHE_RESP)) link_cache = newVector();
    Vector#(CENTRAL_CACHE_N_CLIENTS, Connection_Client#(CENTRAL_CACHE_BACKING_REQ, CENTRAL_CACHE_BACKING_RESP)) link_cache_backing = newVector();

    for (Integer p = 0; p < valueOf(CENTRAL_CACHE_N_CLIENTS); p = p + 1)
    begin
        //
        // The central cache may have clients that are inside the virtual platform,
        // such as scratchpad memory.  Internal clients tag themselves by
        // setting their dictionary string to "platform".  Do not build soft
        // connections for internal clients.
        //
        if (showVDEV_CACHE_DICT(fromInteger(p + `VDEV_CACHE__BASE)) != "platform")
        begin
            link_cache[p] <- mkConnection_Server("vdev_cache_" + integerToString(p));

            //
            // Forward requests to the central cache.
            //
            rule sendCentralCacheReq (True);
                let req = link_cache[p].getReq();
                link_cache[p].deq();

                centralCache.clientPorts[p].newReq(req);
            endrule

            //
            // Return responses from the central cache.
            //
            let resp_data =
                (rules
                    // Return the requested word of the line fetched from
                    // the cache.
                    rule recvCentralCacheData (True);
                        let d <- centralCache.clientPorts[p].readResp();
                        link_cache[p].makeResp(tagged CENTRAL_CACHE_READ d);
                    endrule
                endrules);

            let resp_flush_ack =
                (rules
                    // Flush or invalidate ACK response
                    rule recvCentralCacheFlushAck (True);
                        let d <- centralCache.clientPorts[p].invalOrFlushWait();
                        link_cache[p].makeResp(tagged CENTRAL_CACHE_FLUSH_ACK False);
                    endrule
                endrules);

            addRules(rJoinDescendingUrgency(resp_flush_ack, resp_data));


            //
            // Backing storage communication.  Requests come from the cache
            // back to the client.
            //

            link_cache_backing[p] <- mkConnection_Client("vdev_cache_backing_" + integerToString(p));

            //
            // Forward requests to the central cache.
            //
            let back_rules =
                (rules
                    rule sendCentralCacheBackingReadReq (True);
                        let r <- centralCache.backingPorts[p].getReadReq();
                        link_cache_backing[p].makeReq(tagged CENTRAL_CACHE_BACK_READ r);
                    endrule
                endrules);

            let back_wreq =
                (rules
                    rule sendCentralCacheBackingWriteReq (True);
                        let r <- centralCache.backingPorts[p].getWriteReq();
                        link_cache_backing[p].makeReq(tagged CENTRAL_CACHE_BACK_WREQ r);
                    endrule
                endrules);

            back_rules = rJoinDescendingUrgency(back_rules, back_wreq);

            let back_wdata =
                (rules
                    rule sendCentralCacheBackingWriteData (True);
                        let d <- centralCache.backingPorts[p].getWriteData();
                        link_cache_backing[p].makeReq(tagged CENTRAL_CACHE_BACK_WDATA d);
                    endrule
                endrules);

            back_rules = rJoinDescendingUrgency(back_rules, back_wdata);
            addRules(back_rules);

            //
            // Backing storage responses
            //
            rule recvCentralCacheBackingResp (True);
                let resp = link_cache_backing[p].getResp();
                link_cache_backing[p].deq();

                case (resp) matches
                    tagged CENTRAL_CACHE_BACK_READ .r:
                    begin
                        centralCache.backingPorts[p].sendReadResp(r);
                    end

                    tagged CENTRAL_CACHE_BACK_WACK .dummy:
                    begin
                        centralCache.backingPorts[p].sendWriteAck();
                    end
                endcase
            endrule
        end
    end


    // ====================================================================
    //
    // DEBUG_SCAN state
    //
    // ====================================================================

    //
    // Debug state that can be scanned out:
    //
    //     Bits 5-0: cacheReadsInFlight counter
    //
    Wire#(CENTRAL_CACHE_DEBUG_SCAN) debugScanData <- mkBypassWire();
    DEBUG_SCAN#(CENTRAL_CACHE_DEBUG_SCAN) debugScan <- mkDebugScanNode(`DEBUG_SCAN_PLATFORM_INTERFACE_CENTRAL_CACHE, debugScanData);

    (* no_implicit_conditions *)
    rule updateCentralCacheDebugScanState (True);
        debugScanData <= centralCache.debugScanState();
    endrule

endmodule

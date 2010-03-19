//
// Copyright (C) 2009 Intel Corporation
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

import Arbiter::*;

`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/scratchpad_memory.bsh"

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"

`include "asim/dict/ASSERTIONS_SCRATCHPAD_MEMORY_SERVICE.bsh"
`include "asim/dict/DEBUG_SCAN_SCRATCHPAD_MEMORY_SERVICE.bsh"


module [CONNECTED_MODULE] mkScratchpadMemoryService#(VIRTUAL_DEVICES vdevs)
    // interface:
    ()
    provisos (Max#(SCRATCHPAD_N_CLIENTS, 1, n_SCRATCHPAD_CLIENTS_NONZERO));
    
    let memory = vdevs.scratchpadMemory;

    // ***** Assertion Checkers *****
    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_SCRATCHPAD_MEMORY_SERVICE__BASE);
    ASSERTION assertScratchpadSpace <- mkAssertionChecker(`ASSERTIONS_SCRATCHPAD_MEMORY_SERVICE_FULL, ASSERT_ERROR, assertNode);

    // ====================================================================
    //
    // Scratchpad connections.  One soft connection for each individual
    // port. All the soft connections funnel down into a single
    // memory-style interface to the scratchpad here.
    //
    // ====================================================================

    Vector#(SCRATCHPAD_N_CLIENTS, Connection_Server#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_READ_RESP)) link_memory = newVector();

    // Only one scratchpad may send a request at a time.
    Arbiter_IFC#(n_SCRATCHPAD_CLIENTS_NONZERO) arbiter <- mkArbiter(False);

    Rules mem_send_req = emptyRules;
    for (Integer p = 0; p < valueOf(SCRATCHPAD_N_CLIENTS); p = p + 1)
    begin
        link_memory[p] <- mkConnectionServerOptional("vdev_memory_" + integerToString(p));

        //
        // sendScratchpadTryReq --
        //     Send a request to the arbiter for every incoming connection that
        //     has a pending request.
        //
        rule sendScratchpadTryReq (link_memory[p].reqNotEmpty());
            arbiter.clients[p].request();
        endrule

        let r =
            (rules
                //
                // sendScratchpadReq --
                //     Forward a scratchpad client's request to the scratchpad
                //     device.
                //
                //     At most one instance of this rule will fire, based on the
                //     arbiter.
                //
                rule sendScratchpadReq (arbiter.clients[p].grant());
                    let req = link_memory[p].getReq();

                    link_memory[p].deq();

                    case (req) matches
                        tagged SCRATCHPAD_MEM_INIT .init:
                        begin
                            let s <- memory.init(init.allocLastWordIdx,
                                                 fromInteger(p),
                                                 init.cached);
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

                        tagged SCRATCHPAD_MEM_WRITE_MASKED .w_req:
                        begin
                            memory.writeMasked(w_req.addr,
                                               w_req.val,
                                               w_req.byteWriteMask,
                                               fromInteger(p));

                        end
                    endcase
                endrule
            endrules);

        // The Bluespec scheduler can't detect that the arbiter grants access
        // to at most one client.  Assert the lack of conflicts....
        mem_send_req = rJoinConflictFree(mem_send_req, r);
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
    // DEBUG_SCAN state
    //
    // ====================================================================

    //
    // Scan data for debugging deadlocks.
    //
    Wire#(SCRATCHPAD_MEMORY_DEBUG_SCAN) debugScanData <- mkBypassWire();
    DEBUG_SCAN#(SCRATCHPAD_MEMORY_DEBUG_SCAN) debugScan <- mkDebugScanNode(`DEBUG_SCAN_SCRATCHPAD_MEMORY_SERVICE_DATA, debugScanData);

    (* fire_when_enabled *)
    (* no_implicit_conditions *)
    rule updateDebugScanState (True);
        debugScanData <= memory.debugScanState();
    endrule
endmodule

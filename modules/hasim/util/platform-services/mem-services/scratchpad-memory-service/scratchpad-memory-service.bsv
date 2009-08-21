
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/scratchpad_memory.bsh"

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"

`include "asim/dict/ASSERTIONS_SCRATCHPAD_MEMORY_SERVICE.bsh"

module [CONNECTED_MODULE] mkScratchpadMemoryService#(VIRTUAL_DEVICES vdevs)
    // interface:
        ();
    
    let memory = vdevs.scratchpadMemory;

    // ***** Assertion Checkers *****
    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_SCRATCHPAD_MEMORY_SERVICE__BASE);
    ASSERTION assertScratchpadSpace <- mkAssertionChecker(`ASSERTIONS_SCRATCHPAD_MEMORY_SERVICE_FULL, assertNode);

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
        link_memory[p] <- mkConnectionServerOptional("vdev_memory_" + integerToString(p));

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

endmodule

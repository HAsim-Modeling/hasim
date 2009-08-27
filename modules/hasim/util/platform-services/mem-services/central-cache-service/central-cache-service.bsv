import Vector::*;

`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/central_cache.bsh"

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"


`include "asim/dict/STATS_CENTRAL_CACHE_SERVICE.bsh"
`include "asim/dict/PARAMS_CENTRAL_CACHE_SERVICE.bsh"
`include "asim/dict/DEBUG_SCAN_CENTRAL_CACHE_SERVICE.bsh"

module [CONNECTED_MODULE] mkCentralCacheService#(VIRTUAL_DEVICES vdevs)
    // interface:
        ();

    let centralCache = vdevs.centralCache;
    
    // ***** Stats *****
    let cacheStats <- mkCentralCacheStats(centralCache.stats);

    // ***** Dynamic parameters *****
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();

    Param#(3) centralCacheMode <- mkDynamicParameter(`PARAMS_CENTRAL_CACHE_SERVICE_CENTRAL_CACHE_MODE, paramNode);

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
`ifdef VDEV_CACHE__BASE
        if (showVDEV_CACHE_DICT(fromInteger(p + `VDEV_CACHE__BASE)) != "platform")
        begin
            link_cache[p] <- mkConnectionServerOptional("vdev_cache_" + integerToString(p));

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

            link_cache_backing[p] <- mkConnectionClientOptional("vdev_cache_backing_" + integerToString(p));

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
`endif
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
    DEBUG_SCAN#(CENTRAL_CACHE_DEBUG_SCAN) debugScan <- mkDebugScanNode(`DEBUG_SCAN_CENTRAL_CACHE_SERVICE_DATA, debugScanData);

    (* no_implicit_conditions *)
    rule updateCentralCacheDebugScanState (True);
        debugScanData <= centralCache.debugScanState();
    endrule

endmodule

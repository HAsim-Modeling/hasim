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



// initRoutingTable

// Given information about all sends and recvs local to a station, and all the children,
// build a routing table that will guide the station in moving messages.

// Also return information that will be passed to the parent of this station.

// This is a module rather than a function so we can do messageMs for debugging or type errors.

module initRoutingTable#(List#(PHYSICAL_STATION_INFO) children_infos) 
    // interface:
        (PHYSICAL_STATION_INFO);

    // Some local indices        
    LOCAL_DST cur_dst = 0;    
    
    // Group the receives together.
    LOGICAL_MAP logical_map <- initLogicalMap(children_infos);
    LOGICAL_RECV_MAP logical_recv_map = logical_map.recvMap;
    List#(MULTICAST_INFO) from_multi = logical_map.fromMulti;

    // Track which routes we've matched so we don't pass them to our parent.
    // Initially we haven't matched anything.
    List#(Bool) matched_routes = List::replicate(List::length(logical_recv_map), False);

    // Start indexing outgoing connections.
    LOCAL_SRC cur_src = 0;
    MULTICAST_IDX cur_multi = fromInteger(length(from_multi));
    
    // Now deal with the children's sends. This will build the from_children list.
    List#(List#(ROUTING_DECISION)) from_children  = List::nil;
    List#(LOGICAL_SEND_INFO) outgoing_infos = List::nil;

    for (Integer x = 0; x < List::length(children_infos); x = x + 1)
    begin
        
        // For each child we build a routing decision for each outgoing connection.
        let child_info = children_infos[x];
        List#(ROUTING_DECISION) from_child = List::nil;

        // Loop on each outgoing connection.
        for (Integer y = 0; y < List::length(child_info.outgoingInfo); y = y + 1)
        begin
        
            let send_info = child_info.outgoingInfo[y];
        
            // Check if we know about this logical connection.    
            let m_route = lookupLogicalRecv(logical_recv_map, send_info.logicalName);

            case (m_route) matches
                tagged Invalid:
                begin

                    // We don't know about this logical connection, so send up to parent.
                    let route = tagged ROUTE_parent cur_src;

                    // Bump the outgoing index.
                    cur_src = cur_src + 1;

                    // Record the information for our parent.
                    outgoing_infos = List::cons(send_info, outgoing_infos);

                    // Add the decision to the routing table.
                    from_child = List::cons(route, from_child);

                end
                tagged Valid .idx:
                begin
    
                    // We do! Note it as being matched so it won't be given to our parent.
                    matched_routes[idx] = True;

                    // One of our children needs it... but is it a multicast?
                    if (send_info.oneToMany)
                    begin
                    
                        // Yes, so we need to send to our parent, in case they have more listeners.
                        
                        match {.recv_info, .existing_route} = logical_recv_map[idx];
                        
                        // Is the existing route already a multicast?
                        case (existing_route) matches
                            tagged ROUTE_child {.c, .d}:
                            begin

                                // Nope, so we gotta make it a multicast route.
                                let multi_info = initMulticastInfo(length(children_infos));

                                // Mark the existing child as a listener of this multicast.
                                multi_info.childrenNeed[c] = tagged Valid d;

                                // Mark the multicast as going to the parent as well.
                                multi_info.parentNeed = tagged Valid cur_src;
                                
                                // Bump the outgoing index.
                                cur_src = cur_src + 1;
                                
                                // Record all the updated routing decisions.
                                outgoing_infos = List::cons(send_info, outgoing_infos);
                                from_multi = List::append(from_multi, cons(multi_info, nil));
                                let route = tagged ROUTE_multicast cur_multi;
                                cur_multi = cur_multi + 1;
                                from_child = List::cons(route, from_child);
                            
                            end
                            tagged ROUTE_multicast .midx:
                            begin

                                // Yes... Just mark this multicast as being sent to our parent.
                                let multi_info = from_multi[midx];
                                multi_info.parentNeed = tagged Valid cur_src;
                                from_multi[midx] = multi_info;
                                
                                // Bump the outgoing index.
                                cur_src = cur_src + 1;
                                
                                // Record all the updated routing decisions.
                                outgoing_infos = List::cons(send_info, outgoing_infos);
                                let route = tagged ROUTE_multicast midx;
                                from_child = List::cons(route, from_child);
                            end
                        endcase
                    
                    end
                    else
                    begin
                    
                        // Nope just a normal child route, so we can use the match.

                        // Use the predetermined routing decision in the routing table.
                        match {.recv_info, .route} = logical_recv_map[idx];
                        
                        // As the least-common ancestor, typecheck this connection.
                        checkConnectionTypes(send_info, recv_info);
                        
                        // Record the routing decision.
                        from_child = List::cons(route, from_child);

                    end

                end
            endcase
        end
        
        // Add this child's info to the routing table.
        // Note: we need to reverse this list since we built it as a stack.
        from_children = cons(List::reverse(from_child), from_children);
    end

    // We need to reverse the final lists since we built them as stacks.
    from_children = List::reverse(from_children);
    outgoing_infos = List::reverse(outgoing_infos);

    // Now we need to remove all the matched receives that we don't send up to our parent.
    LOGICAL_RECV_MAP unmatched_recvs = removeMatchedRoutes(logical_recv_map, matched_routes);    

    // Every receive we do expose to our parent we need to learn how to route.
    List#(LOGICAL_RECV_INFO) incoming_infos = List::nil;
    List#(ROUTING_DECISION) from_parent = List::nil;

    for (Integer x = 0; x < List::length(unmatched_recvs); x = x + 1)
    begin

        match {.cur_info, .cur_route} = unmatched_recvs[x];
        
        // Expose the unmatched recv to our parent.
        incoming_infos = List::cons(cur_info, incoming_infos);
        
        // If our parent sends to this recv, route it to the right place.
        from_parent = List::cons(cur_route, from_parent);

    end

    // We need to reverse the final list since we built it as a stack.
    incoming_infos = List::reverse(incoming_infos);
    from_parent = List::reverse(from_parent);

    // Build the final routing table.
    let route_table = 
        ROUTING_TABLE
        {
            fromChild:  from_children,
            fromParent: from_parent,
            fromMulti:  from_multi
        };

    // Build the final result.
    let station_info = 
        PHYSICAL_STATION_INFO
        {
            routingTable: route_table,
            outgoingInfo: outgoing_infos,
            incomingInfo: incoming_infos
        };
    
    return station_info;

endmodule



// initLogicalMap

// Groups the logical receives of children stations together.
// Logical receives with more than one physical receiver become multicast.

module initLogicalMap#(List#(PHYSICAL_STATION_INFO) children_infos) (LOGICAL_MAP);
    
    // Start the lists at nil and build them as we go.
    LOGICAL_RECV_MAP logical_recv_map = List::nil;
    List#(MULTICAST_INFO) from_multi = List::nil;

    Integer cur_multi = 0;

    // Loop over all the children.
    for (Integer x = 0; x < List::length(children_infos); x = x + 1)
    begin

        let child_info = children_infos[x];
        
        // Loop over this child's receives.
        for (Integer y = 0; y < List::length(child_info.incomingInfo); y = y + 1)
        begin
            
            let recv_info = child_info.incomingInfo[y];

            // Do we already know about this logical name?
            let m_route = lookupLogicalRecv(logical_recv_map, recv_info.logicalName);

            case (m_route) matches
                tagged Invalid:
                begin
                    // No, so just add it to the list.
                    let recv_route = tagged ROUTE_child tuple2(fromInteger(x), fromInteger(y));
                    logical_recv_map = cons(tuple2(recv_info, recv_route), logical_recv_map);
                end
                tagged Valid .idx:
                begin

                    // Yes... But is it a multicast already?
                    match {.existing_info, .existing_route} = logical_recv_map[idx];
                    checkMultiConnectionTypes(existing_info, recv_info);

                    case (existing_route) matches
                        tagged ROUTE_multicast .midx:
                        begin

                            // It is. Just mark this child as a receiver of this multicast.
                            let multi_info = from_multi[midx];
                            multi_info.childrenNeed[x] = tagged Valid fromInteger(y);
                            from_multi[midx] = multi_info;

                        end
                        tagged ROUTE_child {.child_idx, .dst}:
                        begin
                           
                            // It's not. First change it to multicast. 
                            let multi_info = initMulticastInfo(length(children_infos));

                            // Mark the old receiver as a receiver of this multicast.
                            multi_info.childrenNeed[child_idx] = tagged Valid dst;

                            // Mark the new guy as well.
                            multi_info.childrenNeed[x] = tagged Valid fromInteger(y);
                            
                            // Update all the maps.
                            logical_recv_map[idx] = tuple2(existing_info, tagged ROUTE_multicast fromInteger(cur_multi));
                            cur_multi = cur_multi + 1;
                            from_multi = append(from_multi, cons(multi_info, nil));

                        end
                    endcase
                end
            endcase
        end

    end

    // Build and return the final result.
    let res =
        LOGICAL_MAP
        {
            recvMap: logical_recv_map,
            fromMulti: from_multi
        };

    return res;

endmodule


// initRoutingTableLeaf

// A convenience function... A leaf station is one whose children are all physical INs or OUTs.
// These we can route easily by pretending they are very simple stations with only one send/recv.
// Then we can use our normal routing table function.

module initRoutingTableLeaf#(List#(LOGICAL_RECV_INFO) recvs,
                             List#(LOGICAL_SEND_INFO) sends) (PHYSICAL_STATION_INFO);


    List#(PHYSICAL_STATION_INFO) stations = List::nil;

    for (Integer x = 0; x < length(recvs); x = x + 1)
    begin

        let rinfo = 
            PHYSICAL_STATION_INFO
            {
                outgoingInfo: nil,
                incomingInfo: cons(recvs[x], nil),
                routingTable: ?
            };
        stations = append(stations, cons(rinfo, nil));
    end

    for (Integer x = 0; x < length(sends); x = x + 1)
    begin

        let sinfo = 
            PHYSICAL_STATION_INFO
            {
                outgoingInfo: cons(sends[x], nil),
                incomingInfo: nil,
                routingTable: ?
            };
        stations = append(stations, cons(sinfo, nil));
    end

    let res <- initRoutingTable(stations);

    return res;

endmodule

module  mkConnStationWrappers#(List#(LOGICAL_RECV_INFO) recvs, List#(LOGICAL_SEND_INFO) sends) (List#(PHYSICAL_STATION));

    List#(PHYSICAL_STATION) stations = List::nil;

    for (Integer x = 0; x < length(recvs); x = x + 1)
    begin
        let station <- mkRecvStationWrapper(recvs[x].incoming);
        stations = append(stations, cons(station, nil));
    end

    for (Integer x = 0; x < length(sends); x = x + 1)
    begin
        let station <- mkSendStationWrapper(sends[x].outgoing);
        stations = append(stations, cons(station, nil));
    end

    return stations;
    
endmodule

module mkSendStationWrapper#(PHYSICAL_CONNECTION_OUT physical_send)
    // interface:
        (PHYSICAL_STATION);

    interface PHYSICAL_STATION_IN incoming;

        method Action enq(MESSAGE_DOWN msg);
            noAction;
        endmethod

    endinterface

    interface PHYSICAL_STATION_OUT outgoing;

       method MESSAGE_UP first();

            let msg =
                MESSAGE_UP
                {
                    origin: 0,
                    payload: truncate(physical_send.first())
                };

            return msg;

       endmethod

       method Bool notEmpty() = physical_send.notEmpty();
       method Action deq() = physical_send.deq();

    endinterface

endmodule

module mkRecvStationWrapper#(PHYSICAL_CONNECTION_IN physical_recv)
    // interface:
        (PHYSICAL_STATION);

    FIFOF#(PHYSICAL_PAYLOAD) q <- mkFIFOF();


    rule try (q.notEmpty);
        physical_recv.try(zeroExtend(q.first));
    endrule
    
    rule success (physical_recv.success);
        q.deq();
    endrule

    interface PHYSICAL_STATION_IN incoming;

        method Action enq(MESSAGE_DOWN msg);
            q.enq(msg.payload);
        endmethod

    endinterface

    interface PHYSICAL_STATION_OUT outgoing;

       method MESSAGE_UP first() if (False) = ?;
       method Bool notEmpty() = False;
       method Action deq() = noAction;

    endinterface

endmodule

// lookupLogicalRecv

// Given a name, see if there's a receive of that name in the map.
// If so, return the index.

function Maybe#(Integer) lookupLogicalRecv(LOGICAL_RECV_MAP rmap, String logicalName);

    Maybe#(Integer) res = tagged Invalid;

    for (Integer x = 0; x < length(rmap); x = x + 1)
    begin
        match {.recv_info, .route} = rmap[x];
        if (recv_info.logicalName == logicalName)
        begin
            res = tagged Valid x;
        end
    end
    
    return res;

endfunction


// removeMatchedRoutes

// When a station is the least-common ancestor of a logical connection, there is no need
// expose that connection to our parent. So lets remove those from the list.

function LOGICAL_RECV_MAP removeMatchedRoutes(LOGICAL_RECV_MAP rmap, List#(Bool) match_info);
    LOGICAL_RECV_MAP res = List::nil;
    
    for (Integer x = 0; x < length(rmap); x = x + 1)
    begin
    
        if (!match_info[x])
        begin
            res = List::cons(rmap[x],res);
        end

    end
    
    return reverse(res);
  
endfunction


// initMulticastInfo

// Create a fresh multicast info that does not send to any child, nor to the parent.

function MULTICAST_INFO initMulticastInfo(Integer num_children);

    return
        MULTICAST_INFO
        {
            childrenNeed: List::replicate(num_children, tagged Invalid),
            parentNeed: Invalid
        };

endfunction



// connectStationsTree

// Find the root station, then connect all of its children together,
// building the routing tables as we go.

module [ConnectedModule] connectStationsTree#(Clock c) ();

    let root_name <- getRootStationName();
    List#(STATION_INFO) sts <- getParentlessStations();
    // First of all, all parentless stations become
    // children of the actual root.
    while (!List::isNull(sts))
    begin
        let cur = List::head(sts);
        sts = List::tail(sts);
        if (cur.stationName != root_name)
            registerChildToStation(root_name, cur.stationName);
    end
    
    // Now, see if there's an actual tree to connect.
    if (root_name != "InvalidRootStation")
    begin
        let root_info <- findStationInfo(root_name);
        let soft_reset <- getSoftReset();
        // Recursively connect all the stations together in a tree.
        match {.r, .r_info} <- mkStationTree(root_info, clocked_by c, reset_by soft_reset);
        // Add an empty root that drops everything routed to it.
        // This allows the existing root node to not be a special case
        // whe it comes to routing broadcasts and the like.
        mkEmptyRoot(r);    
    end
    
endmodule


// mkStationTree

// Convert a logical station into a physical tree node. Return that node, along with a set of info
// describing all connections attached it, and all of its children. These infos allow us to build
// a routing table at each node.

module [ConnectedModule] mkStationTree#(STATION_INFO info) (Tuple2#(PHYSICAL_STATION, PHYSICAL_STATION_INFO));

    // Examine the current node's children.
    List#(String) children = info.childrenNames;
    if (List::isNull(children))
    begin
        // Current node is s a leaf.
        let phys_station_info <- initRoutingTableLeaf(info.registeredRecvs, info.registeredSends);
        messageM("Creating Physical Station: " + info.stationName + "(Leaf).");
        for (Integer x = 0; x < List::length(info.registeredRecvs); x = x + 1)
        begin
            messageM("    Registering Recv: " + info.registeredRecvs[x].logicalName);
        end
        for (Integer x = 0; x < List::length(info.registeredSends); x = x + 1)
        begin
            messageM("    Registering Send: " + info.registeredSends[x].logicalName);
        end
        printStationInfo(phys_station_info);
        // Make wrappers for the sends and receives to make them look like mini-stations.
        // This simplifies the code since there's only one type of thing to deal with.
        let wrappers <- mkConnStationWrappers(info.registeredRecvs, info.registeredSends);
        // Instantiate a physical station based on the routing table and the wrappers.
        let m <- mkPhysicalStation(wrappers, phys_station_info.routingTable);
        return tuple2(m, phys_station_info);
    end
    else
    begin
        // Current node is not a leaf.
        
        // Make some lists for recursing down to our children.
        List#(PHYSICAL_STATION) phys_children = List::nil;
        List#(PHYSICAL_STATION_INFO) phys_children_info = List::nil;

        // Recursively deal with every child.
        while (!List::isNull(children))
        begin
            let cur_child = List::head(children);
            children = List::tail(children);

            let child_info <- findStationInfo(cur_child);
            match {.c, .c_info} <- mkStationTree(child_info);
            phys_children = List::cons(c, phys_children);
            phys_children_info = List::cons(c_info, phys_children_info);
        end

        messageM("Creating Physical Station: " + info.stationName);

        for (Integer x = 0; x < List::length(info.childrenNames); x = x + 1)
        begin
            messageM("    Registering Child: " + info.childrenNames[x]);
        end

        for (Integer x = 0; x < List::length(info.registeredRecvs); x = x + 1)
        begin
            messageM("    Registering Recv: " + info.registeredRecvs[x].logicalName);
            let recv_info = 
                PHYSICAL_STATION_INFO
                {
                    outgoingInfo: nil,
                    incomingInfo: cons(info.registeredRecvs[x], nil),
                    routingTable: ?
                };
            phys_children_info = List::cons(recv_info, phys_children_info);
            let wrapper <- mkRecvStationWrapper(info.registeredRecvs[x].incoming);
            phys_children = List::cons(wrapper, phys_children);
        end

        for (Integer x = 0; x < List::length(info.registeredSends); x = x + 1)
        begin
            messageM("    Registering Send: " + info.registeredSends[x].logicalName);
            let send_info = 
                PHYSICAL_STATION_INFO
                {
                    outgoingInfo: cons(info.registeredSends[x], nil),
                    incomingInfo: nil,
                    routingTable: ?
                };
            phys_children_info = List::cons(send_info, phys_children_info);
            let wrapper <- mkSendStationWrapper(info.registeredSends[x].outgoing);
            phys_children = List::cons(wrapper, phys_children);
        end

        // Restore the list to the original indices.
        phys_children = List::reverse(phys_children);
        phys_children_info = List::reverse(phys_children_info);        

        // Based on all of our children, make a routing table for this node.
        let phys_station_info <- initRoutingTable(phys_children_info);
        printStationInfo(phys_station_info);
        // Make a physical station for this node, including FIFOs and whatnot.
        let m <- mkPhysicalStation(phys_children, phys_station_info.routingTable);
        return tuple2(m, phys_station_info);

    end
    
endmodule

// Helper function to find parentless stations that are not connected
// to the root. (Perhaps because of synthesis boundaries.)

function Bool isParentlessStation(List#(STATION_INFO) sts, STATION_INFO st);

    if (List::isNull(sts)) 
        return True;
    else
    begin
        let cur = List::head(sts);
        List#(String) childs = cur.childrenNames;
        Bool found = False;
        while (!List::isNull(childs))
        begin
            let cur_child = List::head(childs);
            childs = List::tail(childs);
            if (st.stationName == cur_child)
                found = True;
        end
        return found ? False : isParentlessStation(List::tail(sts), st);
    end

endfunction

// Helper function to filter for all parentless stations.

module [ConnectedModule] getParentlessStations (List#(STATION_INFO));

    let sts <- getStationInfos();
    return List::filter(isParentlessStation(sts), sts);

endmodule

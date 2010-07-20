import Clocks::*;
// ****** Connection Functions ******


// register{Send,Recv}

// Adds a new send/recv. First try to find existing matches, 
// then either connect it or add it to the unmatched list.

module [ConnectedModule] registerSend#(LOGICAL_SEND_INFO new_send) ();

    if (!new_send.oneToMany)
    begin
        // See if we can find our partner.
        let recvs <- getUnmatchedRecvs();
        let m_match <- findMatchingRecv(new_send.logicalName, recvs);

        case (m_match) matches
            tagged Invalid:   
            begin

                // No match. Add the send to the list.
                addUnmatchedSend(new_send);

            end
            tagged Valid .recv:
            begin
                
                if (!recv.manyToOne)
                begin
                    // A 1-to-1 match! Attempt to connect them.
                    connect(new_send, recv);

                    // The receive is no longer unmatched, so remove it from the list.
                    removeUnmatchedRecv(recv.logicalName);
                end
                else
                begin
                    // Defer connecting until the partner is found.
                    addUnmatchedSend(new_send);
                end

            end
        endcase
    end
    else
    begin
        // Defer one to many send to the very top level.
        addUnmatchedSend(new_send);
    end

endmodule

module [ConnectedModule] registerRecv#(LOGICAL_RECV_INFO new_recv) ();

    if (!new_recv.manyToOne)
    begin
        // See if we can find our partner.
        let sends <- getUnmatchedSends();
        let m_match <- findMatchingSend(new_recv.logicalName, sends);

        case (m_match) matches
            tagged Invalid:   
            begin

                // No match. Add the recv to the list.
                addUnmatchedRecv(new_recv);

            end
            tagged Valid .send:
            begin
                if (!send.oneToMany)
                begin
                    // A match! Attempt to connect them.
                    connect(send, new_recv);

                    // The send is no longer unmatched, so remove it from the list.
                    removeUnmatchedSend(send.logicalName);
                end
                else
                begin
                    // Defer connecting until the partner is found.
                    addUnmatchedRecv(new_recv);
                end

            end
        endcase
    end
    else
    begin
        // Defer many to one recvs to the very top level.
        addUnmatchedRecv(new_recv);
    end

endmodule

// BACKWARDS COMPATABILITY: Connection Chains

// These will eventually be subsumed by manyToOne/oneToMany logical connections
// that are then divorced from their physical topology. Until that day arrives
// a Logical and Physical chain is the same thing.

module [ConnectedModule] registerChain#(LOGICAL_CHAIN_INFO new_link) ();

    // See what existing links are out there.
    let idx = new_link.logicalIdx;
    let existing_links <- getChain(idx);

    if (List::isNull(existing_links))
    begin

        // It's the only member of the chain, so just add it.
        messageM("Adding Initial Link for Chain: [" + integerToString(idx) + "]");
        putChain(idx, List::cons(new_link, existing_links));

    end
    else
    begin

        // There are other links already, so make sure the types
        // are consistent.
        let latest_link = List::head(existing_links);
   
        // Make sure connections match or consumer is receiving it as void
        if (new_link.logicalType != latest_link.logicalType && new_link.logicalType != "" && latest_link.logicalType != "")
        begin

            // Error out
            messageM("Detected existing chain type: " + latest_link.logicalType);
            messageM("Detected new link type: " + new_link.logicalType);
            error("ERROR: data types for Connection Chain #" + integerToString(idx) + " do not match.");

        end
        else
        begin  
           
            // Actually do the connection
            messageM("Adding Link to Chain: [" + integerToString(idx) + "]");
            connectOutToIn(new_link.outgoing, latest_link.incoming);

        end

        // Add the new link to the list.
        putChain(idx, List::cons(new_link, existing_links));

    end

endmodule

// Logical stations can have sends and receives associated with them.
// These functions handle that.

module [ConnectedModule] registerSendToStation#(LOGICAL_SEND_INFO new_send, String station_name) ();

    // Get the station info.
    let st_info <- findStationInfo(station_name);
    
    // Check to see if the counterpart recv is already at this station.
    // If so let's just make them point-to-point.

    let m_match <- findMatchingRecv(new_send.logicalName, st_info.registeredRecvs);
    
    if (m_match matches tagged Valid .recv &&& !new_send.oneToMany &&& !recv.manyToOne)
    begin
    
        // A match! Attempt to connect them.
        connect(new_send, recv);
        
        // The send is no longer unmatched, so remove it from the list.
        removeUnmatchedRecv(new_send.logicalName);
        
    end
    else
    begin

        // No match. Add the send to the station's list.
        st_info.registeredSends = List::cons(new_send, st_info.registeredSends);

        // Update the registry.
        updateStationInfo(station_name, st_info);

    end

endmodule

module [ConnectedModule] registerRecvToStation#(LOGICAL_RECV_INFO new_recv, String station_name) ();

    // Get the station info.
    let st_info <- findStationInfo(station_name);
    
    // Check to see if the counterpart recv is already at this station.
    // If so let's just make them point-to-point.

    let m_match <- findMatchingSend(new_recv.logicalName, st_info.registeredSends);
    
    if (m_match matches tagged Valid .send &&& !send.oneToMany &&& !new_recv.manyToOne)
    begin
        
        // A match! Attempt to connect them.
        connect(send, new_recv);
        
        // The send is no longer unmatched, so remove it from the list.
        removeUnmatchedSend(new_recv.logicalName);
    
    end
    else
    begin
    
        // No match. Add the recv to the station's list.
        st_info.registeredRecvs = List::cons(new_recv, st_info.registeredRecvs);

        // Update the registry.
        updateStationInfo(station_name, st_info);
    
    end

endmodule

// Register a new logical station. Initially it has now children.
// Currently network type and station type are just strings.
// Later they could be more complex data types.

module [ConnectedModule] registerStation#(String station_name, String network_name, String station_type) ();
    
    // Make a fresh station info.
    let new_st_info = STATION_INFO 
                      {
                           stationName:     station_name,
                           networkName:     network_name,
                           stationType:     station_type,
                           childrenNames:   List::nil,
                           registeredSends: List::nil, 
                           registeredRecvs: List::nil
                      };
    
    let st_infos <- getStationInfos();
    putStationInfos(List::cons(new_st_info, st_infos));

endmodule

// Add a child station to an existing station. IE for trees and so forth.

module [ConnectedModule] registerChildToStation#(String parentName, String childName) ();

    let parent_info <- findStationInfo(parentName);
        
    parent_info.childrenNames = List::cons(childName, parent_info.childrenNames);
    
    updateStationInfo(parentName, parent_info);
    
endmodule

// findMatching{Recv/Send}

// Find the match for the given name. If there's more than one match 
// then this is a serious error that should end the world.

module [ConnectedModule] findMatchingRecv#(String sname, List#(LOGICAL_RECV_INFO) recvs) (Maybe#(LOGICAL_RECV_INFO));

  let recv_matches = List::filter(recvNameMatches(sname), recvs);
  

  // The list should have exactly zero or one element in it....

  case (List::length(recv_matches))
      0:
      begin

          return tagged Invalid;

      end
      1:
      begin

          return tagged Valid List::head(recv_matches);

      end
      default:
      begin

          error("ERROR: Found multiple ConnectionRecv named: " + sname);
          return tagged Invalid;

      end
  endcase

endmodule

module [ConnectedModule] findMatchingSend#(String rname, List#(LOGICAL_SEND_INFO) sends) (Maybe#(LOGICAL_SEND_INFO));

  let send_matches = List::filter(sendNameMatches(rname), sends);
  
  // The list should have exactly zero or one element in it....

  case (List::length(send_matches))
      0:
      begin

          return tagged Invalid;

      end
      1:
      begin

          return tagged Valid List::head(send_matches);

      end
      default:
      begin

          error("ERROR: Found multiple ConnectionSend named: " + rname);
          return tagged Invalid;

      end
  endcase

endmodule

// findAllMatching{Recv/Send}s

// Find all matches for the given name and remove them from the list.
// Useful for manyToOne/oneToMany connections.

module [ConnectedModule] findAllMatchingRecvs#(String sname) (List#(LOGICAL_RECV_INFO));

  let recvs <- getUnmatchedRecvs();

  let recv_matches = List::filter(recvNameMatches(sname), recvs);
  let remaining = List::filter(recvNameDoesNotMatch(sname), recvs);
  putUnmatchedRecvs(remaining);

  return recv_matches;

endmodule

module [ConnectedModule] findAllMatchingSends#(String rname) (List#(LOGICAL_SEND_INFO));

  let sends <- getUnmatchedSends();

  let send_matches = List::filter(sendNameMatches(rname), sends);
  let remaining = List::filter(sendNameDoesNotMatch(rname), sends);
  putUnmatchedSends(remaining);

  return send_matches;

endmodule


// connect

// Do the actual business of connecting a 1-to-1 send to a receive.

module [ConnectedModule] connect#(LOGICAL_SEND_INFO csend, LOGICAL_RECV_INFO crecv) ();

   
    // Make sure connections match or consumer is receiving it as void
    if (csend.logicalType != crecv.logicalType && crecv.logicalType != "")
    begin

        messageM("Detected send type: " + csend.logicalType);
        messageM("Detected receive type: " + crecv.logicalType);
        error("ERROR: data types for Connection " + csend.logicalName + " do not match.");

    end
    else
    begin  //Actually do the connection
  
        messageM("Connecting: " + csend.logicalName);
        connectOutToIn(csend.outgoing, crecv.incoming);

    end

endmodule


// connectOutToIn

// This is the module that actually performs the connection between two
// physical endpoints. This is for 1-to-1 communication only.

module connectOutToIn#(PHYSICAL_CONNECTION_OUT cout, PHYSICAL_CONNECTION_IN cin) ();
  
  if(sameFamily(cin.clock,cout.clock))
  begin
      rule trySend (True);
          // Try to move the data
          let x = cout.first();
          cin.try(x);
      endrule

      rule success (cin.success());
          // We succeeded in moving the data
          cout.deq();
    
      endrule
  end
  else
  begin
      messageM("CrossDomain@ Found");

      // choose a size large enough to cover latency of fifo
      let domainFIFO <- mkSyncFIFO(8,
                                   cout.clock, 
                                   cout.reset,
                                   cin.clock);

      rule receive;
          let x = cout.first();
          domainFIFO.enq(x);
          cout.deq();
      endrule
  
      rule trySend;
          cin.try(domainFIFO.first());
      endrule

      rule succeedSend(cin.success());
          domainFIFO.deq;
      endrule
  end
endmodule

import Vector::*;
import ModuleContext::*;
import List::*;
import HList::*;
import FIFOF::*;

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/physical_interconnect.bsh"
`include "asim/provides/soft_connections_common.bsh"
`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_services_lib.bsh"


// connectMulticasts

// One-to-many and Many-to-ones are deffered. This actually connects them.

module [SoftConnectionModule] connectMulticasts#(Clock c) ();

    List#(LOGICAL_SEND_INFO) multi_sends <- getMulticastSends();
    let soft_reset <- getSoftReset();
    
    while (!List::isNull(multi_sends))
    begin
        let cur_send = List::head(multi_sends);
        multi_sends = List::tail(multi_sends);
        
        let matching_recvs <- findAllMatchingRecvs(cur_send.logicalName);

        if (List::length(matching_recvs) == 0 && !cur_send.optional)
        begin
            error("Unmatched broadcast: " + cur_send.logicalName);
        end
        
        connectBroadcast(cur_send, matching_recvs, clocked_by c, reset_by soft_reset);        
        
    end
    
    List#(LOGICAL_RECV_INFO) multi_recvs <- getMulticastRecvs();
    
    while (!List::isNull(multi_recvs))
    begin
        let cur_recv = List::head(multi_recvs);
        multi_recvs = List::tail(multi_recvs);
        
        let matching_sends <- findAllMatchingSends(cur_recv.logicalName);

        if (List::length(matching_sends) == 0 && !cur_recv.optional)
        begin
            error("Unmatched listener: " + cur_recv.logicalName);
        end
        
        connectListener(cur_recv, matching_sends, clocked_by c, reset_by soft_reset);
        
    end
    
endmodule


// connectBroadcast

// Do the actual business of connect a broadcast to many receivers.

module [SoftConnectionModule] connectBroadcast#(LOGICAL_SEND_INFO csend, List#(LOGICAL_RECV_INFO) crecvs) ();

    List#(Reg#(Bool)) recvs = List::nil;
    List#(FIFOF#(PHYSICAL_CONNECTION_DATA)) qs = List::nil;

    for (Integer x = 0; x < List::length(crecvs); x = x + 1)
    begin
        
        let cur_recv = crecvs[x];
    
        // Make sure connection types match or consumer is receiving it as void.
        if (csend.logicalType != cur_recv.logicalType && cur_recv.logicalType != "")
        begin

            messageM("Detected send type: " + csend.logicalType);
            messageM("Detected receive type: " + cur_recv.logicalType);
            error("ERROR: data types for broadcast Connection " + csend.logicalName + " do not match.");

        end

        // Make a bit to show whether each receiver has received the broadcast.
        Reg#(Bool) recv <- mkReg(False);
        recvs = List::cons(recv, recvs);

        // Make a FIFO for each receiver for better throughput.
        let q <- mkFIFOF();
        qs = List::cons(q, qs);
        
        // Try to move messages from the individual FIFO to the receiver.
        rule sendTry (q.notEmpty());
            
            cur_recv.incoming.try(q.first());
            
        endrule

        rule sendAck (q.notEmpty() && cur_recv.incoming.success());
            
            q.deq();
            
        endrule
        
    end
        
    messageM("Connecting Broadcast " + csend.logicalName + " to " + integerToString(List::length(crecvs)) + " receives.");

    // This rule transfers an incoming message to all outgoing queues.
    // Once all outgoing queues have the message, we are done with it.

    rule transfer (csend.outgoing.notEmpty());
    
        // Temporary variable to update list of who's gotten the message.
        List#(Bool) new_recvs = List::replicate(List::length(crecvs), False);

        for (Integer x = 0; x < List::length(crecvs); x = x + 1)
        begin
        
            if (!recvs[x])
            begin
                qs[x].enq(csend.outgoing.first());
                new_recvs[x] = True;
            end
            else
            begin
                new_recvs[x] = True;
            end
        
        end
        
        if (List::all(\== (True), new_recvs))
        begin
            
            // Everyone's gotten the message. We're done.
            for (Integer x = 0; x < List::length(crecvs); x = x + 1)
            begin
                recvs[x] <= False;
            end
            
            csend.outgoing.deq();

        end
        else
        begin

            // Record who we managed to get the message to.
            for (Integer x = 0; x < List::length(crecvs); x = x + 1)
            begin
                recvs[x] <= new_recvs[x];
            end
            
        end
    
    endrule
    
    
endmodule


// connectListener

// Connect many sends to a many-to-1 receive.

module [SoftConnectionModule] connectListener#(LOGICAL_RECV_INFO crecv, List#(LOGICAL_SEND_INFO) csends) ();

    for (Integer x = 0; x < List::length(csends); x = x + 1)
    begin
        
        // For every sender, make a wire that indicates whether or not
        // they are trying to send data.
        let cur_send = csends[x];
        PulseWire tryW <- mkPulseWire();
    
        // Make sure connection types match or consumer is receiving it as void.
        if (crecv.logicalType != cur_send.logicalType && crecv.logicalType != "")
        begin

            messageM("Detected send type: " + cur_send.logicalType);
            messageM("Detected receive type: " + crecv.logicalType);
            error("ERROR: data types for listener Connection " + cur_send.logicalName + " do not match.");

        end

        rule sendTry (cur_send.outgoing.notEmpty);
            
            // TODO: add tag here based on x.
            crecv.incoming.try(cur_send.outgoing.first());
            tryW.send();
            
        endrule

        rule sendAck (tryW && crecv.incoming.success());
            
            cur_send.outgoing.deq();
            
        endrule
        
    end

    messageM("Connecting Listener " + crecv.logicalName + " to " + integerToString(List::length(csends)) + " sends.");

endmodule

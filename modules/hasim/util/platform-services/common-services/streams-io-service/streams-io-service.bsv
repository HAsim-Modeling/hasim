
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/streams_io.bsh"


module [CONNECTED_MODULE] mkStreamsIOService#(STREAMS_IO streamsIO)
    // interface:
        ();

    
    Connection_Receive#(STREAMS_REQUEST)    linkStreams  <- mkConnectionRecvOptional("vdev_streams");

    rule send_streams_req (True);

        // read in streams request and send it to device
        let sreq = linkStreams.receive();
        linkStreams.deq();
        streamsIO.makeRequest(sreq.streamID,
                              sreq.stringID,
                              sreq.payload0,
                              sreq.payload1);

    endrule

endmodule

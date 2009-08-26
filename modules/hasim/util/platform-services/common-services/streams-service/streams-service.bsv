
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/streams_device.bsh"


module [CONNECTED_MODULE] mkStreamsService#(STREAMS streams)
    // interface:
        ();

    
    Connection_Receive#(STREAMS_REQUEST)    linkStreams  <- mkConnectionRecvOptional("vdev_streams");

    rule send_streams_req (True);

        // read in streams request and send it to device
        let sreq = linkStreams.receive();
        linkStreams.deq();
        streams.makeRequest(sreq.streamID,
                            sreq.stringID,
                            sreq.payload0,
                            sreq.payload1);

    endrule

endmodule

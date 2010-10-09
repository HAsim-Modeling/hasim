
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




// Connect soft connections as normal, but dangling connections are not an error
// Instead they're exposed as a WithConnections interface and messages are entered
// into the compilation log recording their address index.
// Connection Chains are not "tied off" but exposed as head and tail

//toWithConnections :: [ConnectionData] -> Module WithConnections


module [Module] toWithConnections#(LOGICAL_CONNECTION_INFO ctx)       (WITH_CONNECTIONS#(numIn, numOut));

  let outs     <- exposeDanglingSends(ctx.unmatchedSends);
  let ins      <- exposeDanglingRecvs(ctx.unmatchedRecvs);

  Vector#(CON_NUM_CHAINS, PHYSICAL_CONNECTION_INOUT) chns = newVector();
  
  // For every chain, we must expose it to the next level.
  for (Integer x = 0; x < valueof(CON_NUM_CHAINS); x = x + 1)
  begin
     let links = ctx.chains[x];
     if (List::isNull(links))
     begin

        // If a particular chain is empty for this synthesis boundary
        // make a dummy link that is just a pass-through.
        messageM("Exposing Chain: [" + integerToString(x) + "] as Pass-Through.");
        let dummy <- mkPassThrough();
        chns[x] = (interface PHYSICAL_CONNECTION_INOUT;
                       interface incoming = dummy.incoming;
                       interface outgoing = dummy.outgoing;
                   endinterface);

     end
     else
     begin

        // For non-empty chains, we connect to the head of the first link
        // and the tail of the last link. (These could be the same link if
        // there was only one.)
        messageM("Exposing Chain: [" + integerToString(x) + "]");
        let latest_link = List::head(links);
        let earliest_link = List::last(links);
        chns[x] = (interface PHYSICAL_CONNECTION_INOUT;
                       interface incoming = latest_link.incoming;
                       interface outgoing = earliest_link.outgoing;
                   endinterface);

     end
  end
  
  interface outgoing = outs;
  interface incoming = ins;
  interface chains = chns;
  
endmodule  

instance ExposableContext#(LOGICAL_CONNECTION_INFO,WITH_CONNECTIONS#(numIn, numOut));

module [Module] exposeContext#(LOGICAL_CONNECTION_INFO ctx) (WITH_CONNECTIONS#(numIn, numOut));

  let rst <- exposeCurrentReset();
  let clk <- exposeCurrentClock();

  // TODO: hasim-connect doesn't know about multicasts or stations yet.
  // Therefore for now we just connect them at every synthesis boundary
  // as if it were the toplevel.
  
  // In the future hasim-connect will know about these and we'll do something
  // different - more akin to how Chains are today.
  
  // Until then this allows them to work within a synthesis boundary, but not
  // across synthesis boundaries.
  match {.new_context2, .m4} <- runWithContext(ctx, connectMulticasts(clk));

  match {.final_context, .m5} <- runWithContext(new_context2, connectStationsTree(clk));
  
  let x <- toWithConnections(ctx);
  return x;

endmodule
endinstance


// Expose dangling sends to other synthesis boundaries via compilation messages

// exposeDangingSends :: [LOGICAL_SEND_INFO] -> Module [PHYSICAL_CONNECTION_OUT]

module exposeDanglingSends#(List#(LOGICAL_SEND_INFO) dsends) (Vector#(n, PHYSICAL_CONNECTION_OUT));

  Vector#(n, PHYSICAL_CONNECTION_OUT) res = newVector();
  Integer cur_out = 0;

  // Output a compilation message and tie it to the next free outport
  for (Integer x = 0; x < length(dsends); x = x + 1)
  begin
    if (cur_out >= valueof(n))
      error("ERROR: Too many dangling Send Connections (max " + integerToString(valueof(n)) + "). Increase the numOut parameter to WithConnections.");

    let cur = dsends[x];
    messageM("Dangling Send {" + cur.logicalType + "} [" + integerToString(cur_out) +  "]: " + cur.logicalName);
    res[cur_out] = cur.outgoing;
    cur_out = cur_out + 1;
  end
  
  // Zero out unused dangling sends
  for (Integer x = cur_out; x < valueOf(n); x = x + 1)
    res[x] = PHYSICAL_CONNECTION_OUT{clock:noClock,
                                     reset:noReset,
                                     deq:?,
                                     first:?,
                                     notEmpty:?
                                    };
  
  return res;
  
endmodule

// Expose dangling receives to other synthesis boundaries via compilation messages

// exposeDangingRecvs :: [LOGICAL_RECV_INFO] -> Module [PHYSICAL_CONNECTION_IN]

module exposeDanglingRecvs#(List#(LOGICAL_RECV_INFO) drecvs) (Vector#(n, PHYSICAL_CONNECTION_IN));

  Vector#(n, PHYSICAL_CONNECTION_IN) res = newVector();
  Integer cur_in = 0;
  
  //Output a compilation message and tie it to the next free inport
  for (Integer x = 0; x < length(drecvs); x = x + 1)
  begin
    if (cur_in >= valueof(n))
      error("ERROR: Too many dangling Receive Connections (max " + integerToString(valueof(n)) + "). Increase the numIn parameter to WithConnections.");

    let cur = drecvs[x];
    messageM("Dangling Rec {" + cur.logicalType + "} [" + integerToString(cur_in) + "]: " + cur.logicalName);
    res[cur_in] = cur.incoming;
    cur_in = cur_in + 1;
  end
  
  //Zero out unused dangling recvs
  for (Integer x = cur_in; x < valueOf(n); x = x + 1)
    res[x] = PHYSICAL_CONNECTION_IN{clock:noClock,
                                    reset:noReset,
                                    success: ?,
                                    try: ? };
  
  return res;

endmodule
  
  

//If there are no links then it's just a pass-through queue
module mkPassThrough
    //interface:
                (PHYSICAL_CONNECTION_INOUT);

  // Local Clock and reset
  Clock localClock <- exposeCurrentClock();
  Reset localReset <- exposeCurrentReset();

  FIFOF#(PHYSICAL_CONNECTION_DATA) passQ <- mkFIFOF();
  PulseWire enW <- mkPulseWire();
  
  interface PHYSICAL_CONNECTION_IN incoming;

    method Action try(PHYSICAL_CONNECTION_DATA d);
      passQ.enq(d);
      enW.send();
    endmethod

    method Bool   success();
      return enW;
    endmethod

    interface Clock clock = localClock;
    interface Reset reset = localReset; 

  endinterface

  // A physical outgoing connection
  interface PHYSICAL_CONNECTION_OUT outgoing;

    method Bool notEmpty() = passQ.notEmpty();
    method PHYSICAL_CONNECTION_DATA first() = passQ.first();
    method Action deq() = passQ.deq();

    interface Clock clock = localClock;
    interface Reset reset = localReset; 

  endinterface

endmodule

import FIFOF::*;
import Clocks::*;
import ModuleContext::*;
import HList::*;

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/physical_interconnect.bsh"
`include "asim/provides/soft_connections_common.bsh"
`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_services_lib.bsh"


// instatiateWithConnections
// Connect all remaining connections at the top-level. 
// This includes all one-to-many and many-to-one connections,
// and all shared interconnects.

// For backwards compatability we also still handle chains.

instance FinalizableContext#(LOGICAL_CONNECTION_INFO);

  module [Module] finalizeContext#(LOGICAL_CONNECTION_INFO m) (Empty);
    
   let finalModule <- finalizeSoftConnection(m);   
 
  endmodule

endinstance


module [Module] finalizeSoftConnection#(LOGICAL_CONNECTION_INFO m) (Empty);
  // Build context of Connected_Module

  Clock clk <- exposeCurrentClock();

  // Backwards compatability: Connect all chains in the resulting context.
  match {.new_context2, .m3} <- runWithContext(m, connectChains(clk)); 
  printRecvs(new_context2.unmatchedRecvs);
  printSends(new_context2.unmatchedSends);
  // Connect all broadcasts in the resulting context.
  match {.new_context3, .m4} <- runWithContext(new_context2, connectMulticasts(clk));
  printRecvs(new_context2.unmatchedRecvs);
  printSends(new_context2.unmatchedSends);
  // Connect all trees in the resulting context.
  // Eventually more physical interconnects will be supported.
  match {.final_context, .m5} <- runWithContext(new_context3, connectStationsTree(clk));
  printRecvs(new_context2.unmatchedRecvs);
  printSends(new_context2.unmatchedSends);
  // Error out if there are dangling connections
  Bool error_occurred = False;
  // Final Dangling sends
  for (Integer x = 0; x < List::length(final_context.unmatchedSends); x = x + 1)
  begin
    let cur = final_context.unmatchedSends[x];
    if (!cur.optional)
      begin
        messageM("ERROR: Unmatched logical send: " +  cur.logicalName);
        error_occurred = True;
      end
  end

  // Final Dangling recvs
  for (Integer x = 0; x < List::length(final_context.unmatchedRecvs); x = x + 1)
  begin
    let cur = final_context.unmatchedRecvs[x];
    if (!cur.optional)
      begin
        messageM("ERROR: Unmatched logical receive: " + cur.logicalName);
        error_occurred = True;
      end
  end

  if (error_occurred)
    error("Error: Unmatched logical connections at top level.");

endmodule


// connectChains

// Backwards Compatability: Connection Chains

module [SoftConnectionModule] connectChains#(Clock c) ();

    for (Integer x = 0; x < valueof(CON_NUM_CHAINS); x = x + 1)
    begin
		
        // Iterate through the chains.
        let chn <- getChain(x);
        
        // Close non-nil chains off.
        if (!List::isNull(chn))
        begin

            let latest_link = List::head(chn);
            let earliest_link = List::last(chn);
            // This is the reverse of the non-top level way, because we are
            // closing the chain.
            messageM("Closing Chain: [" + integerToString(x) + "]");
            connectOutToIn(earliest_link.outgoing, latest_link.incoming);

        end
        else
        begin
            messageM("Skipping Empty Chain: [" + integerToString(x) + "].");
        end

    end
    
endmodule



// Need to provide some code for exposing connections not at the top level. 
// this should at some point be merged with the above. \


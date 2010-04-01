import FIFOF::*;
import Clocks::*;
import ModuleContext::*;

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/physical_interconnect.bsh"

// instatiateWithConnections
// Connect all remaining connections at the top-level. 
// This includes all one-to-many and many-to-one connections,
// and all shared interconnects.

// For backwards compatability we also still handle chains.


module [Module] instantiateWithConnections#(Connected_Module#(t_IFC) m) (t_IFC);

  // Get a fresh context.
  let ctx = freshContext;
  
  // Gotta set the initial soft reset to an actual reset line 
  // in order to keep things happy.
  let clk <- exposeCurrentClock();
  let rst <- exposeCurrentReset();
  ctx.softReset = rst;

  // Instantiate the module and get the resulting context.
  match {.new_context, .m2} <- runWithContext(ctx, m);
  
  // Backwards compatability: Connect all chains in the resulting context.
  match {.new_context2, .m3} <- runWithContext(new_context, connectChains(clk));

  // Connect all broadcasts in the resulting context.
  match {.new_context3, .m4} <- runWithContext(new_context2, connectMulticasts(clk));

  // Connect all trees in the resulting context.
  // Eventually more physical interconnects will be supported.
  match {.final_context, .m5} <- runWithContext(new_context3, connectStationsTree(clk));
  
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

  return m2;

endmodule


// connectChains

// Backwards Compatability: Connection Chains

module [ConnectedModule] connectChains#(Clock c) ();

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




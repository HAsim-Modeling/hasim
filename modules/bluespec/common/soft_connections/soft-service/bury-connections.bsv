import List::*;
import Vector::*;
import ModuleContext::*;

`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_connections_common.bsh"

// Forwards-compatability

typedef Tuple3#(String, String, Integer) ConMap;

//Add the parsed information back is as normal connections

instance BuriableContext#(SoftServicesModule,WithConnections#(numIn, numOut));

module [SoftServicesModule] buryContext#(WithConnections#(numIn, numOut) mod) ();
  // some kind soul (hasim-connect) has already put the external metadata
  // into the environment
endmodule 

endinstance


//Add the parsed information back is as normal connections

module [ConnectedModule]  addConnections#(SoftServicesSynthesisInterface#(numIn, numOut) mod, List#(ConMap) sends, List#(ConMap) recs) ();
  liftSCM(addConnectionsSC(extractWithConnections(mod),sends,recs));
endmodule


module [SoftConnectionModule] addConnectionsSC#(WithConnections#(numIn, numOut) mod, List#(ConMap) sends, List#(ConMap) recs) ();
   
   //Add Sends
   
   let nSends = length(sends);
   for (Integer x = 0; x < nSends; x = x + 1)
   begin
     match {.nm, .contype, .idx} = sends[x];
     let inf = LOGICAL_SEND_INFO {logicalName: nm, logicalType: contype, optional: False, oneToMany: False, outgoing: mod.outgoing[idx]};
     registerSend(inf);
   end

   //Add Recs

   let nRecs = length(recs);
   for (Integer x = 0; x < nRecs; x = x + 1)
   begin
     match {.nm, .contype, .idx} = recs[x];
     let inf = LOGICAL_RECV_INFO {logicalName:nm, logicalType: contype, optional: False, manyToOne: False, incoming: mod.incoming[idx]};
     registerRecv(inf);
   end
   
   //Add Chains
   for (Integer x = 0; x < valueof(CON_NUM_CHAINS); x = x + 1)
   begin
    // Collect up our info.
    let info = 
        LOGICAL_CHAIN_INFO 
        {
            logicalIdx: x, 
            logicalType: "", 
            incoming: mod.chains[x].incoming,
            outgoing: mod.chains[x].outgoing
        };

    // Register the chain
    registerChain(info);
   end

endmodule


/*   
module [SoftConnectionModule] buryContextSC#(WithConnections#(numIn, numOut) mod) ();   
   let ctxtIn <- getContext();
   let sends = ctxtIn.unmatchedSends;
   let recs  = ctxtIn.unmatchedRecvs;

   // ditch old, partial context after extracting the useful bit
   putContext(initializeContext());

   //Add Sends
   
   let nSends = length(sends);
   for (Integer x = 0; x < nSends; x = x + 1)
   begin
     match {.nm, .contype, .idx} = sends[x];
     let inf = LOGICAL_SEND_INFO {logicalName: sends[x].nm, logicalType: sends[x].contype, optional: False, oneToMany: False, outgoing: mod.outgoing[idx]};
     registerSend(inf);
   end

   //Add Recs

   let nRecs = length(recs);
   for (Integer x = 0; x < nRecs; x = x + 1)
   begin
     match {.nm, .contype, .idx} = recs[x];
     let inf = LOGICAL_RECV_INFO {logicalName:nm, logicalType: contype, optional: False, manyToOne: False, incoming: mod.incoming[idx]};
     registerRecv(inf);
   end
   
   //Add Chains
   for (Integer x = 0; x < valueof(CON_NUM_CHAINS); x = x + 1)
   begin
    // Collect up our info.
    let info = 
        LOGICAL_CHAIN_INFO 
        {
            logicalIdx: x, 
            logicalType: "", 
            incoming: mod.chains[x].incoming,
            outgoing: mod.chains[x].outgoing
        };

    // Register the chain
    registerChain(info);
   end


 
endmodule
*/
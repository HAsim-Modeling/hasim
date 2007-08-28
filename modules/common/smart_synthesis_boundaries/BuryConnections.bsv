

//Re-bury connections which have been exposed at synthesis boundaries

//Connection map from conname to local address

typedef Tuple2#(String, Integer) ConMap;

//Add the parsed information back is as normal connections

module [Connected_Module] addConnections#(WithConnections mod, List#(ConMap) sends, List#(ConMap) recs) ();
   
   //Add Sends
   
   let nSends = length(sends);
   for (Integer x = 0; x < nSends; x = x + 1)
   begin
     match {.nm, .idx} = sends[x];
     let inf = CSend_Info {cname: nm, ctype: "", conn: mod.outgoing[idx]};
     addToCollection(tagged LSend inf);
   end

   //Add Recs

   let nRecs = length(recs);
   for (Integer x = 0; x < nRecs; x = x + 1)
   begin
     match {.nm, .idx} = recs[x];
     let inf = CRecv_Info {cname:nm, ctype: "", conn: mod.incoming[idx]};
     addToCollection(tagged LRecv inf);
   end
   
   //Add Chains
   for (Integer x = 0; x < valueof(CON_NumChains); x = x + 1)
   begin
     let inf = CChain_Info {cnum: x, ctype: "", conn: mod.chains[x]};
     addToCollection(tagged LChain inf);
   end
   
endmodule

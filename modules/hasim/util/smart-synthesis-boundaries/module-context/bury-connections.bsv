//
// Copyright (C) 2008 Massachusetts Institute of Technology
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

//Re-bury connections which have been exposed at synthesis boundaries

// Forwards-compatability
/*

typedef struct 
{
    String logicalName;
    String logicalType;
    Bool oneToMany;
    Bool optional;
    CON_Out outgoing;
} 
    LOGICAL_SEND_INFO;

//Data about logical connections
typedef struct 
{
    String logicalName;
    String logicalType;
    Bool manyToOne;
    Bool optional;
    CON_In incoming;
} 
    LOGICAL_RECV_INFO;

module [Connected_Module] registerSend#(LOGICAL_SEND_INFO inf) ();

    let inf2 = LOGICAL_SEND_INFO {cname: inf.logicalName, ctype: inf.logicalType, conn: inf.outgoing};
    addToCollection(tagged LSend inf2);

endmodule


module [Connected_Module] registerRecv#(LOGICAL_RECV_INFO inf) ();

    let inf2 = CRecv_Info {cname:inf.logicalName, ctype: inf.logicalType, conn: inf.incoming};
    addToCollection(tagged LRecv inf2);

endmodule

module [Connected_Module] registerChains#(WITH_CONNECTIONS#(numIn, numOut, inter_T) mod) ();

    for (Integer x = 0; x < valueof(CON_NumChains); x = x + 1)
    begin
      let inf = CChain_Info {cnum: x, ctype: "", conn: mod.chains[x]};
      addToCollection(tagged LChain inf);
    end

endmodule
*/

typedef Tuple3#(String, String, Integer) ConMap;

//Add the parsed information back is as normal connections

module [Connected_Module] addConnections#(WithConnections#(numIn, numOut) mod, List#(ConMap) sends, List#(ConMap) recs) ();
   
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

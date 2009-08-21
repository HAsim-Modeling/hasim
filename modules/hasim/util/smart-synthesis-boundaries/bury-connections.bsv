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

//Connection map from conname to local address

typedef Tuple3#(String, String, Integer) ConMap;

//Add the parsed information back is as normal connections

module [Connected_Module] addConnections#(WithConnections#(numIn, numOut) mod, List#(ConMap) sends, List#(ConMap) recs) ();
   
   //Add Sends
   
   let nSends = length(sends);
   for (Integer x = 0; x < nSends; x = x + 1)
   begin
     match {.nm, .contype, .idx} = sends[x];
     let inf = CSend_Info {cname: nm, ctype: contype, optional: False, conn: mod.outgoing[idx]};
     addToCollection(tagged LSend inf);
   end

   //Add Recs

   let nRecs = length(recs);
   for (Integer x = 0; x < nRecs; x = x + 1)
   begin
     match {.nm, .contype, .idx} = recs[x];
     let inf = CRecv_Info {cname:nm, ctype: contype, optional: False, conn: mod.incoming[idx]};
     addToCollection(tagged LRecv inf);
   end
   
   //Add Chains
   for (Integer x = 0; x < valueof(CON_NumChains); x = x + 1)
   begin
     let inf = CChain_Info {cnum: x, ctype: "", conn: mod.chains[x]};
     addToCollection(tagged LChain inf);
   end
   
endmodule

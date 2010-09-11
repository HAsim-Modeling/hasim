///
// Copyright (C) 2010 Intel Corporation
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

// Should I really be including these?
// I probably just want stats....
`include "asim/provides/stats_service.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/librl_bsv_cache.bsh"

// SCRATCHPAD_CACHE_CONSTRUCTOR

// A function to instantiate an RL_DM_CACHE, given an RL_DM_CACHE_SOURCE_DATA
// of the appropriate types.

// Passed to the multi-cached-memory modules below.

typedef function CONNECTED_MODULE#(RL_DM_CACHE#(Bit#(t_CONTAINER_ADDR_SZ), 
                                                SCRATCHPAD_MEM_VALUE, 
                                                t_REF_INFO)) 
                       f(RL_DM_CACHE_SOURCE_DATA#(Bit#(t_CONTAINER_ADDR_SZ), 
                                                  SCRATCHPAD_MEM_VALUE, 
                                                  t_REF_INFO) source) 
                            SCRATCHPAD_CACHE_CONSTRUCTOR#(type t_CONTAINER_ADDR_SZ,
                                                          type t_REF_INFO);

// SCRATCHPAD_STATS_CONSTRUCTOR

// A function to instantiate a stat tracker. Passed to the multi-cached-memory
// modules below.

typedef function CONNECTED_MODULE#(Empty) f(RL_CACHE_STATS stats) SCRATCHPAD_STATS_CONSTRUCTOR;


// This file contains some useful utilities for building scratchpads

module [CONNECTED_MODULE] mkBasicScratchpadCacheStats#(
                            STATS_DICT_TYPE idLoadHit,
                            STATS_DICT_TYPE idLoadMiss,
                            STATS_DICT_TYPE idWriteHit,
                            STATS_DICT_TYPE idWriteMiss,
                            RL_CACHE_STATS stats)
    // interface:
    ();

    STAT statLoadHit <- mkStatCounter(idLoadHit);
    STAT statLoadMiss <- mkStatCounter(idLoadMiss);
    STAT statWriteHit <- mkStatCounter(idWriteHit);
    STAT statWriteMiss <- mkStatCounter(idWriteMiss);
    
    rule readHit (stats.readHit());
      statLoadHit.incr();
    endrule

    rule readMiss (stats.readMiss());
      statLoadMiss.incr();
    endrule

    rule writeHit (stats.writeHit());
      statWriteHit.incr();
    endrule

    rule writeMiss (stats.writeMiss());
      statWriteMiss.incr();
    endrule
endmodule


module [CONNECTED_MODULE] mkNullScratchpadCacheStats#(RL_CACHE_STATS stats)
    // interface:
    ();


endmodule
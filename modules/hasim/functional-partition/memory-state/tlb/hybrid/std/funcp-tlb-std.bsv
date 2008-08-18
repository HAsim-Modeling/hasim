//
// Copyright (C) 2008 Intel Corporation
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

//
// Standard hybrid TLB
//

// Library includes.

import FIFO::*;
import Vector::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/provides/hasim_isa.bsh"
`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/dict/STATS_FUNCP_TLB.bsh"


//
// TLB type (instruction or data)
//
typedef enum
{
  FUNCP_ITLB,
  FUNCP_DTLB
}
  FUNCP_TLB_TYPE
      deriving (Eq, Bits);


//
// Query passed to TLB server
//
typedef Tuple2#(TOKEN, ISA_ADDRESS) FUNCP_TLB_QUERY;


interface FUNCP_TLB;
    
    //
    // quickTranslateVA may return a translation within the same cycle if it
    // finds a hit in a small set of LUTs.  If the method returns false then
    // the caller should resort to making a request to the "funcp_tlb" server.
    //
    method Maybe#(MEM_ADDRESS) quickTranslateVA(FUNCP_TLB_QUERY query);
    
endinterface


`define QUICK_CACHE_ENTRIES 8


typedef union tagged
{
  MEM_ADDRESS SB_Hit;
  void SB_Miss;
}
  RESP_PATH
        deriving (Eq, Bits);    


module [HASIM_MODULE] mkFUNCP_TLB#(FUNCP_TLB_TYPE tlbType, String serverConn)
    // Interface:
        (FUNCP_TLB);
   
    // ***** Local State *****
    
    Connection_Server#(FUNCP_TLB_QUERY, Maybe#(MEM_ADDRESS)) link_regstate <- mkConnection_Server(serverConn);

    // Connection to memory translation service
    let i_or_d = (tlbType == FUNCP_ITLB) ? "I" : "D";
    let memory_link_name = strConcat("funcp_memory_VtoP_", i_or_d);
    Connection_Client#(ISA_ADDRESS, MEM_ADDRESS) link_memory <- mkConnection_Client(memory_link_name);

    FIFO#(ISA_ADDRESS) translateQ <- mkFIFO();
    FIFO#(RESP_PATH)   pathQ <- mkSizedFIFO(8);

    Vector#(`QUICK_CACHE_ENTRIES, Reg#(Maybe#(ISA_ADDRESS))) lastVA <- replicateM(mkReg(tagged Invalid));
    Vector#(`QUICK_CACHE_ENTRIES, Reg#(MEM_ADDRESS)) lastPA <- replicateM(mkRegU());
    Reg#(Bit#(TLog#(`QUICK_CACHE_ENTRIES))) nextQuickEntry <- mkReg(0);

    Reg#(Maybe#(ISA_ADDRESS)) victimVA <- mkReg(tagged Invalid);
    Reg#(MEM_ADDRESS) victimPA <- mkRegU();


    // ***** Statistics *****

    let miss_stat_id = (tlbType == FUNCP_ITLB) ? `STATS_FUNCP_TLB_I_MISS : `STATS_FUNCP_TLB_D_MISS;
    Stat statTLBMiss <- mkStatCounter(miss_stat_id);


    // ******* Debuging State *******

    // Fake register to hold our debugging file descriptor.
    let debugLog         <- mkReg(InvalidFile);
    Reg#(Bool) debugInit <- mkReg(False);

    // The current FPGA clock cycle
    Reg#(Bit#(32)) fpgaCC <- mkReg(0);

    rule currentCC (True);

        fpgaCC <= fpgaCC + 1;

    endrule

        //Open the debug logs. (First time only. Afterwards it is not InvalidFile.)
    rule debugDoInit (! debugInit);

        debugInit <= True;

        let fd <- $fopen(`FUNCP_TLB_LOGFILE_NAME, "w");
        if (fd == InvalidFile)
        begin
            $display(strConcat("Error opening FUNCP TLB logfile ", `FUNCP_TLB_LOGFILE_NAME));
            $finish(1);
        end

        debugLog <= fd;

    endrule

    function Action printDebug(Action a);
    action

        $fwrite(debugLog, "[%d]: ", fpgaCC);
        a;
        $fwrite(debugLog, "\n");

    endaction
    endfunction

    // ***** Internal functions *****
    
    function ISA_ADDRESS pageMask();
    
        FUNCP_PAGE page = maxBound;
        ISA_ADDRESS mask = zeroExtend(page);
        return ~mask;

    endfunction

    function MEM_ADDRESS pageOffset(ISA_ADDRESS va);
    
        return truncate(va & ~pageMask());
    
    endfunction


    function ISA_ADDRESS pageAlign(ISA_ADDRESS va);
    
        return va & pageMask();

    endfunction


    function Maybe#(MEM_ADDRESS) doQuickTranslateVA(ISA_ADDRESS va);
    
        Bool hit = False;
        MEM_ADDRESS hitPA = 0;

        // Hit in the recent translation cache?
        ISA_ADDRESS alignedVA = pageAlign(va);

        for (Integer i = 0; i < `QUICK_CACHE_ENTRIES; i = i + 1)
        begin
            if (lastVA[i] matches tagged Valid .last_va &&& alignedVA == last_va)
            begin
                hit = True;
                hitPA = lastPA[i];
            end
        end

        if (hit)
        begin
            MEM_ADDRESS pa = hitPA | pageOffset(va);
            return tagged Valid pa;
        end
        else
        begin
            return tagged Invalid;
        end

    endfunction


    // ***** Rules *****

    (* descending_urgency= "translate_VtoP_request, translate_VtoP_response" *)

    rule translate_VtoP_request (True);

        // pop a request from the link
        match { .tok, .va } = link_regstate.getReq();
        link_regstate.deq();

        if (doQuickTranslateVA(va) matches tagged Valid .pa)
        begin
            //
            // Quick path hit.  Simply return the translation now.
            //
            printDebug($fwrite(debugLog, "Quick hit: VA 0x%x -> PA 0x%x", va, pa));

            pathQ.enq(tagged SB_Hit pa);
        end
        else if (victimVA matches tagged Valid .last_va &&& pageAlign(va) == last_va)
        begin
            //
            // Hit in the victim cache.  Put victim back in cache and return it.
            //
            victimVA <= lastVA[nextQuickEntry];
            victimPA <= lastPA[nextQuickEntry];

            lastVA[nextQuickEntry] <= victimVA;
            lastPA[nextQuickEntry] <= victimPA;
            nextQuickEntry <= nextQuickEntry + 1;

            MEM_ADDRESS pa = victimPA | pageOffset(va);

            printDebug($fwrite(debugLog, "Victim hit: VA 0x%x -> PA 0x%x", va, pa));

            pathQ.enq(tagged SB_Hit pa);
        end
        else
        begin
            //
            // Ask the memory service for a translation.
            //
            ISA_ADDRESS alignedVA = pageAlign(va);
            link_memory.makeReq(alignedVA);
            translateQ.enq(va);
            pathQ.enq(tagged SB_Miss);
        end

    endrule

    rule translate_VtoP_response (pathQ.first() matches tagged SB_Miss);

        let va = translateQ.first();
        translateQ.deq();
        pathQ.deq();

        // pop a request from the link
        MEM_ADDRESS pa = link_memory.getResp();
        link_memory.deq();

        statTLBMiss.incr();

        // Sending a bit over RRR is difficult.  Instead, use the low bit
        // to signal whether the translation is valid.
        if (pa[0] == 0)
        begin
            // Valid translation
            
            // Keep a one entry victim cache since the replacement is round robin
            victimVA <= lastVA[nextQuickEntry];
            victimPA <= lastPA[nextQuickEntry];

            // Store the translation in the small cache
            lastVA[nextQuickEntry] <= tagged Valid pageAlign(va);
            lastPA[nextQuickEntry] <= pa;
            nextQuickEntry <= nextQuickEntry + 1;

            pa = pa | pageOffset(va);

            printDebug($fwrite(debugLog, "VtoP response: VA 0x%x -> PA 0x%x", va, pa));

            link_regstate.makeResp(tagged Valid pa);
        end
        else
        begin
            printDebug($fwrite(debugLog, "Translation failed: VA 0x%x", va));

            link_regstate.makeResp(tagged Invalid);
        end

    endrule

    rule give_hit_response (pathQ.first() matches tagged SB_Hit .pa);

        pathQ.deq();

        link_regstate.makeResp(tagged Valid pa);

    endrule
    // ***** Methods *****

    method Maybe#(MEM_ADDRESS) quickTranslateVA(FUNCP_TLB_QUERY query);
    
        match { .tok, .va } = query;
        return doQuickTranslateVA(va);

    endmethod

endmodule

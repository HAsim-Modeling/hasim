//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
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

interface FUNCP_TLB;
    
    //
    // quickTranslateVA may return a translation within the same cycle if it
    // finds a hit in a small set of LUTs.  If the method returns false then
    // the caller should resort to making a request to the "funcp_tlb" server.
    //
    method Maybe#(MEM_ADDRESS) quickTranslateVA(ISA_ADDRESS va);
    
endinterface


`define QUICK_CACHE_ENTRIES 8


module [HASIM_MODULE] mkFUNCP_TLB
    // Interface:
        (FUNCP_TLB)
    provisos
        (Bits#(ISA_ADDRESS, isa_address_SZ),
         Bits#(MEM_ADDRESS, mem_address_SZ));
   
    // ***** Local State *****
    
    Connection_Server#(ISA_ADDRESS, Maybe#(MEM_ADDRESS)) link_regstate <- mkConnection_Server("funcp_tlb");

    // Connection to memory translation service
    Connection_Client#(ISA_ADDRESS, MEM_ADDRESS) link_memory <- mkConnection_Client("funcp_memory_VtoP");

    FIFO#(ISA_ADDRESS) translateQ <- mkFIFO();

    Vector#(`QUICK_CACHE_ENTRIES, Reg#(Maybe#(ISA_ADDRESS))) lastVA <- replicateM(mkReg(tagged Invalid));
    Vector#(`QUICK_CACHE_ENTRIES, Reg#(MEM_ADDRESS)) lastPA <- replicateM(mkRegU());
    Reg#(Bit#(TLog#(`QUICK_CACHE_ENTRIES))) nextQuickEntry <- mkReg(0);

    Reg#(Maybe#(ISA_ADDRESS)) victimVA <- mkReg(tagged Invalid);
    Reg#(MEM_ADDRESS) victimPA <- mkRegU();


    // ***** Statistics *****

    Stat statTLBMiss <- mkStatCounter(`STATS_FUNCP_TLB_MISS);


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

    function ISA_ADDRESS pageOffset(ISA_ADDRESS va);
    
        return va & ~pageMask();
    
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
        ISA_ADDRESS va = link_regstate.getReq();
        link_regstate.deq();

        if (doQuickTranslateVA(va) matches tagged Valid .pa)
        begin
            //
            // Quick path hit.  Simply return the translation now.
            //
            printDebug($fwrite(debugLog, "Quick hit: VA 0x%x -> PA 0x%x", va, pa));

            link_regstate.makeResp(tagged Valid pa);
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

            link_regstate.makeResp(tagged Valid pa);
        end
        else
        begin
            //
            // Ask the memory service for a translation.
            //
            ISA_ADDRESS alignedVA = pageAlign(va);
            link_memory.makeReq(alignedVA);
            translateQ.enq(va);
        end

    endrule

    rule translate_VtoP_response (True);

        let va = translateQ.first();
        translateQ.deq();

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

    // ***** Methods *****

    method Maybe#(MEM_ADDRESS) quickTranslateVA(ISA_ADDRESS va);
    
        return doQuickTranslateVA(va);

    endmethod

endmodule

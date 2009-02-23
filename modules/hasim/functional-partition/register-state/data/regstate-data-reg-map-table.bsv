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
// Architectural to physical register mapping.  Also stores details needed
// to recover older mapping when killing a token.
//
//   Separate interfaces are provided for individual pipeline stages.
//

// Library includes.

import FIFO::*;
import FIFOF::*;
import RWire::*;
import Vector::*;
import FShow::*;
import SpecialFIFOs::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
 
// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"

// ISA includes

`include "asim/provides/hasim_isa.bsh"
`include "asim/provides/hasim_isa_datapath.bsh"
 

// ========================================================================
//
//  Public types
//
// ========================================================================

//
// Description of new register mappings requested during rewind.
//
typedef struct
{
    CONTEXT_ID context_id;
    Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING)) mappings;
}
REGSTATE_NEW_MAPPINGS
    deriving (Eq, Bits);

//
// Rewind info describes how to roll back side effects of a token.
//
typedef struct
{
    ISA_INST_DSTS regsToFree;
}
REGSTATE_REWIND_INFO
    deriving (Eq, Bits);


// ========================================================================
//
//  Interfaces
//
// ========================================================================

//
// REGSTATE_REG_MAPPING_NEWINFLIGHT --
//   Interface for new token allocation.
//
interface REGSTATE_REG_MAPPING_NEWINFLIGHT;
    method Action initToken(TOKEN tok);
endinterface

//
// REGSTATE_REG_MAPPING_GETDEPENDENCIES -- Interface used by getDependencies
//   pipeline for reading current mappings for register sources and
//   updating mappings for destinations.
//
interface REGSTATE_REG_MAPPING_GETDEPENDENCIES;
    method Action decodeStage1(TOKEN tok,
                               Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)) ar_srcs,
                               Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)) ar_dsts);

    method Action decodeStage2(TOKEN tok,
                               Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_dsts);

    method ActionValue#(Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) decodeRsp();
endinterface

//
// REGSTATE_REG_MAPPING_GETRESULTS --
//   Get results pipeline interface.  Methods only read state.
//
interface REGSTATE_REG_MAPPING_GETRESULTS;
    method Action readMapReq(CONTEXT_ID ctx_id, ISA_REG_INDEX ar);
    method ActionValue#(FUNCP_PHYSICAL_REG_INDEX) readMapRsp();
    
    method Action readRewindReq(TOKEN_INDEX tokIdx);
    method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp();
endinterface

//
// REGSTATE_REG_MAPPING_COMMITRESULTS --
//   Commit pipeline interface.
//
interface REGSTATE_REG_MAPPING_COMMITRESULTS;
    method Action readRewindReq(TOKEN tok);
    method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp();
endinterface

//
// REGSTATE_REG_MAPPING_EXCEPTION --
//   Exception pipeline interface.
//
interface REGSTATE_REG_MAPPING_EXCEPTION;
    method Action readRewindReq(TOKEN_INDEX tokIdx);
    method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp();

    method Action updateMap(REGSTATE_NEW_MAPPINGS map_dsts);
endinterface


interface REGSTATE_REG_MAPPING;
    interface REGSTATE_REG_MAPPING_NEWINFLIGHT     newInFlight;
    interface REGSTATE_REG_MAPPING_GETDEPENDENCIES getDependencies;
    interface REGSTATE_REG_MAPPING_GETRESULTS      getResults;
    interface REGSTATE_REG_MAPPING_COMMITRESULTS   commitResults;
    interface REGSTATE_REG_MAPPING_EXCEPTION       exceptionQueue;
endinterface


// ========================================================================
//
//   Internal data structures
//
// ========================================================================

// 
// MAPTABLE_CONSUMER manages responses for multiple consumers of map table
// BRAM.
//
typedef enum
{
    REGSTATE_MAPT_DECODE,
    REGSTATE_MAPT_EXCEPTION,
    REGSTATE_MAPT_READMAP
}
MAPTABLE_CONSUMER
    deriving (Eq, Bits);

// 
// REGSTATE_REWIND_READER manages responses for multiple readers of rewind
// BRAM.
//
typedef enum
{
    REGSTATE_REWR_GETRESULTS,
    REGSTATE_REWR_COMMITRESULTS,
    REGSTATE_REWR_EXCEPTION
}
REGSTATE_REWIND_READER
    deriving (Eq, Bits);


// ========================================================================
//
//   The busy vector tracks active map table read requests.  Only one
//   access may be in flight for a given context, though multiple contexts
//   may be in flight.  Only one may be in flight for a context because
//   exception (rewind) requests must be given priority over other queued
//   requests.
//
// ========================================================================

interface MAP_BUSY_VECTOR;
    method Bool notBusy(CONTEXT_ID ctxId);
    method Action set(CONTEXT_ID ctxId);
    method Action clear(CONTEXT_ID ctxId);
endinterface

module mkMapBusyVector
    // interface:
    (MAP_BUSY_VECTOR);
    
    Reg#(Bit#(NUM_CONTEXTS)) busy <- mkReg(0);
    
    RWire#(CONTEXT_ID) setW <- mkRWire();
    RWire#(CONTEXT_ID) clearW <- mkRWire();

    //
    // One rule manages updates to the busy vector using incoming wires
    // in order to permit one set and one clear per cycle.
    //
    (* fire_when_enabled *)
    rule updateBusyVec (True);
        let b = busy;
        
        if (setW.wget() matches tagged Valid .s)
            b[s] = 1;

        if (clearW.wget() matches tagged Valid .c)
            b[c] = 0;
        
        busy <= b;
    endrule

    method Bool notBusy(CONTEXT_ID ctxId);
        return busy[ctxId] == 0;
    endmethod

    method Action set(CONTEXT_ID ctxId);
        setW.wset(ctxId);
    endmethod

    method Action clear(CONTEXT_ID ctxId);
        clearW.wset(ctxId);
    endmethod
endmodule


// ========================================================================
//
//   Map table
//
// ========================================================================

//
// Physical register file container.  The number of read ports is configurable,
// trading area for simulator performance.
//
module [HASIM_MODULE] mkFUNCP_Regstate_RegMapping
    // interface:
    (REGSTATE_REG_MAPPING);

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_DATA_LOGFILE_PREFIX + "_regmap.out");


    // ====================================================================
    //
    //   Local state
    //
    // ====================================================================

    //
    // Initial architectural to physical register map
    // AR 0 of context 0 -> PR 0
    // AR N of context 0 -> PR N
    // AR 0 of context 1 -> PR N+1
    // etc
    Vector#(NUM_CONTEXTS, Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) initMap = newVector();
    for (Integer y = 0; y < valueOf(NUM_CONTEXTS); y = y + 1)
    begin
        for (Integer x  = 0; x < valueOf(ISA_NUM_REGS); x = x + 1)
        begin
            initMap[y][x] = fromInteger(x+(y*valueOf(ISA_NUM_REGS)));
        end
    end

    function Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) initMapFunc(CONTEXT_ID x);
        return initMap[x];
    endfunction

    // The true map table
    BRAM#(CONTEXT_ID, Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) mapTable <- mkBRAMInitializedWith(initMapFunc);
    
    // Queue to track proper consumer of read data coming from map table BRAM
    FIFO#(MAPTABLE_CONSUMER) mapTableConsumerQ <- mkFIFO();
    
    // Queue to rate limit requests coming in to the map table.  No new read
    // requests may be queued if a request is already in flight for the same
    // context.  Read requests for different contexts may be in flight at the
    // same time.  The bypass FIFO saves a pipeline stage.
    FIFO#(CONTEXT_ID) newMapTableReqQ_DEC <- mkBypassFIFO();
    FIFO#(CONTEXT_ID) newMapTableReqQ_READ <- mkBypassFIFO();
    FIFO#(CONTEXT_ID) newMapTableReqQ_EXC <- mkBypassFIFO();

    // mapTableBusy tracks which context are in flight.
    MAP_BUSY_VECTOR mapTableBusy <- mkMapBusyVector();

    //
    // Rewind information
    //
    BRAM#(TOKEN_INDEX, Maybe#(REGSTATE_REWIND_INFO)) rewindInfo <- mkBRAM();
    FIFO#(REGSTATE_REWIND_READER) rewindReaderQ <- mkFIFO();

    // Incoming request queues
    FIFO#(Tuple2#(Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)),
                  Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)))) decodeStage1InQ <- mkFIFO();
    FIFO#(Tuple2#(TOKEN,
                  Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)))) decodeStage2InQ <- mkFIFO();

    FIFO#(Tuple2#(CONTEXT_ID, ISA_REG_INDEX)) getResultsReqInQ <- mkFIFO();
    FIFO#(REGSTATE_NEW_MAPPINGS) exceptInQ <- mkFIFO();

    // Incoming token rewind info requests
    FIFO#(TOKEN) rewInitInQ <- mkFIFO();
    FIFOF#(TOKEN_INDEX) rewGetResultsReadInQ <- mkFIFOF();
    FIFOF#(TOKEN) rewCommitReadInQ <- mkFIFOF();
    FIFOF#(TOKEN_INDEX) rewExceptionReadInQ <- mkFIFOF();

    // Outgoing queues
    FIFO#(Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) mapDecodeOutQ <- mkFIFO();


    // ====================================================================
    //
    //   Map table logic
    //
    // ====================================================================

    //
    // newMapTableReq --
    //   Consume all possible read requests for the map table.  New requests
    //   may start BRAM reads only if no other operations for the context
    //   are in flight.
    //

    rule newMapTableReq_DEC (mapTableBusy.notBusy(newMapTableReqQ_DEC.first()));
        let ctx_id = newMapTableReqQ_DEC.first();
        newMapTableReqQ_DEC.deq();

        mapTableBusy.set(ctx_id);
        mapTable.readReq(ctx_id);
        mapTableConsumerQ.enq(REGSTATE_MAPT_DECODE);

        debugLog.record($format("MAP: Table read context %0d, consumer DECODE", ctx_id));
    endrule

    rule newMapTableReq_READ (mapTableBusy.notBusy(newMapTableReqQ_READ.first()));
        let ctx_id = newMapTableReqQ_READ.first();
        newMapTableReqQ_READ.deq();

        mapTableBusy.set(ctx_id);
        mapTable.readReq(ctx_id);
        mapTableConsumerQ.enq(REGSTATE_MAPT_READMAP);

        debugLog.record($format("MAP: Table read context %0d, consumer READ", ctx_id));
    endrule

    (* descending_urgency = "newMapTableReq_EXC, newMapTableReq_READ, newMapTableReq_DEC" *)
    rule newMapTableReq_EXC (mapTableBusy.notBusy(newMapTableReqQ_EXC.first()));
        let ctx_id = newMapTableReqQ_EXC.first();
        newMapTableReqQ_EXC.deq();

        mapTableBusy.set(ctx_id);
        mapTable.readReq(ctx_id);
        mapTableConsumerQ.enq(REGSTATE_MAPT_EXCEPTION);

        debugLog.record($format("MAP: Table read context %0d, consumer EXC", ctx_id));
    endrule


    //
    // doMapDecode --
    //   Three separate requests rendezvous here:
    //     1.  The mapTable for the token's context.
    //     2.  The source and destination architectural register vectors from
    //         decodeStage1.
    //     3.  The new physical registers for the token, from decodeStage2.
    //
    //   From this the rule:
    //     1.  Updates the map table for the new register pairs written by
    //         the token.
    //     2.  Updates rewind information for the token.
    //     3.  Returns a vector of physical registers corresponding to the
    //         architectural source registers.
    //
    rule doMapDecode (mapTableConsumerQ.first() == REGSTATE_MAPT_DECODE);
        match {.ar_srcs, .ar_dsts} = decodeStage1InQ.first();
        decodeStage1InQ.deq();

        match {.tok, .phy_dsts} = decodeStage2InQ.first();
        decodeStage2InQ.deq();

        let map <- mapTable.readRsp();
        mapTableConsumerQ.deq();
        mapTableBusy.clear(tokContextId(tok));

        //
        // Compute physical registers that are freed when this token commits.
        // These free registers are the physical registers previously mapped
        // to the token's architectural destinations.
        //
        Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) old_phy_dsts;
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            old_phy_dsts[x] = case (ar_dsts[x]) matches
                                  tagged Valid .ar: tagged Valid map[pack(ar)];
                                  tagged Invalid: tagged Invalid;
                              endcase;

            if (old_phy_dsts[x] matches tagged Valid .pr)
                debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Slot #%0d AR %0d will free PR %0d", x, validValue(ar_dsts[x]), pr));
        end
        
        // For instruction with no true destination free the dummy physical reg.
        if (! isValid(ar_dsts[0]))
        begin
            old_phy_dsts[0] = phy_dsts[0];

            if (old_phy_dsts[0] matches tagged Valid .pr)
                debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Slot #0 (hidden) will free PR %0d", pr));
        end
            
        // Store rewind info
        rewindInfo.write(tok.index,
                         tagged Valid REGSTATE_REWIND_INFO { regsToFree: old_phy_dsts });


        //
        // Update mappings for destinations.
        //
        Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) updated_map = map;
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            if (ar_dsts[x] matches tagged Valid .ar &&&
                phy_dsts[x] matches tagged Valid .pr)
            begin
                updated_map[pack(ar)] = pr;
                debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Slot #%0d dest AR %0d -> PR %0d", x, ar, pr));
            end
        end


        //
        // Compute source mappings.
        //
        Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_srcs;
        for (Integer x = 0; x < valueOf(ISA_MAX_SRCS); x = x + 1)
        begin
            phy_srcs[x] = case (ar_srcs[x]) matches
                                  tagged Valid .ar: tagged Valid map[pack(ar)];
                                  tagged Invalid: tagged Invalid;
                          endcase;

            if (phy_srcs[x] matches tagged Valid .pr)
                debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Slot #%0d source AR %0d is PR %0d", x, validValue(ar_srcs[x]), pr));
        end
        

        mapTable.write(tokContextId(tok), updated_map);
        mapDecodeOutQ.enq(phy_srcs);
    endrule


    //
    // doExceptionUpdates --
    //   Update map table during exception rewind.
    //
    (* descending_urgency = "doExceptionUpdates, doMapDecode" *)
    rule doExceptionUpdates (mapTableConsumerQ.first() == REGSTATE_MAPT_EXCEPTION);
        let new_maps = exceptInQ.first();
        exceptInQ.deq();

        let map <- mapTable.readRsp();
        mapTableConsumerQ.deq();
        mapTableBusy.clear(new_maps.context_id);

        Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) updated_map = map;

        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            if (new_maps.mappings[x] matches tagged Valid { .ar, .pr })
            begin
                updated_map[pack(ar)] = pr;
                debugLog.record($format("MAP: Update: context %0d, AR %0d -> PR %0d", new_maps.context_id, ar, pr));
            end
        end
        
        mapTable.write(new_maps.context_id, updated_map);
    endrule


    // ====================================================================
    //
    //   Rewind info logic
    //
    // ====================================================================

    //
    // doRewindInit --
    //   Mark token's rewind info invalid.   
    //
    //
    (* descending_urgency = "doRewindInit, doMapDecode" *)
    rule doRewindInit (True);
        let tok = rewInitInQ.first();
        rewInitInQ.deq();

        rewindInfo.write(tok.index, tagged Invalid);
        debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Init REWIND"));
    endrule


    //
    // doRewindReads --
    //   Handle all incoming requests to read the rewind information,
    //   including arbitration between pipelines.
    //
    rule doRewindReads (True);
        //
        // The commit queue is the only pipeline that reads rewind information
        // on its normal path, so there should be no need to add extra read
        // ports.
        //

        //
        // Some rules pass in TOKEN, some TOKEN_INDEX.  TOKEN index is more useful,
        // so it is used when available.  Some clients don't have the full token
        // and are allowed to use the index instead.
        //

        if (rewGetResultsReadInQ.notEmpty())
        begin
            //
            // Only used during instruction emulation.
            //
            let tok_idx = rewGetResultsReadInQ.first();
            rewGetResultsReadInQ.deq();

            rewindReaderQ.enq(REGSTATE_REWR_GETRESULTS);
            rewindInfo.readReq(tok_idx);

            debugLog.record($format("MAP: ") + fshow(tok_idx) + $format(": Request REWIND info for GETRESULTS"));
        end
        else if (rewCommitReadInQ.notEmpty())
        begin
            //
            // Normal commit path.
            //
            let tok = rewCommitReadInQ.first();
            rewCommitReadInQ.deq();

            rewindReaderQ.enq(REGSTATE_REWR_COMMITRESULTS);
            rewindInfo.readReq(tok.index);

            debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Request REWIND info for COMMIT"));
        end
        else if (rewExceptionReadInQ.notEmpty())
        begin
            let tok_idx = rewExceptionReadInQ.first();
            rewExceptionReadInQ.deq();

            rewindReaderQ.enq(REGSTATE_REWR_EXCEPTION);
            rewindInfo.readReq(tok_idx);

            debugLog.record($format("MAP: ") + fshow(tok_idx) + $format(": Request REWIND info for EXCEPT"));
        end
    endrule


    // ====================================================================
    //
    //   Interfaces
    //
    // ====================================================================


    interface REGSTATE_REG_MAPPING_NEWINFLIGHT newInFlight;

        method Action initToken(TOKEN tok);
            rewInitInQ.enq(tok);
        endmethod

    endinterface


    interface REGSTATE_REG_MAPPING_GETDEPENDENCIES getDependencies;

        method Action decodeStage1(TOKEN tok,
                                   Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)) ar_srcs,
                                   Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)) ar_dsts);

            decodeStage1InQ.enq(tuple2(ar_srcs, ar_dsts));
            debugLog.record($format("MAP: ") + fshow(tok.index) + $format(":decodeStage1"));
        endmethod

        method Action decodeStage2(TOKEN tok,
                                   Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_dsts);

            newMapTableReqQ_DEC.enq(tokContextId(tok));

            decodeStage2InQ.enq(tuple2(tok, phy_dsts));
            debugLog.record($format("MAP: ") + fshow(tok.index) + $format(":decodeStage2"));
        endmethod

        method ActionValue#(Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) decodeRsp();
            let r = mapDecodeOutQ.first();
            mapDecodeOutQ.deq();
            return r;
        endmethod

    endinterface


    interface REGSTATE_REG_MAPPING_GETRESULTS getResults;

        method Action readMapReq(CONTEXT_ID ctx_id, ISA_REG_INDEX ar);
            getResultsReqInQ.enq(tuple2(ctx_id, ar));
            newMapTableReqQ_READ.enq(ctx_id);

            debugLog.record($format("MAP: Request context %0d, AR %0d for RSLT", ctx_id, ar));
        endmethod

        method ActionValue#(FUNCP_PHYSICAL_REG_INDEX) readMapRsp() if (mapTableConsumerQ.first() == REGSTATE_MAPT_READMAP);
            match { .ctx_id, .req_ar } = getResultsReqInQ.first();
            getResultsReqInQ.deq();

            let map <- mapTable.readRsp();
            mapTableConsumerQ.deq();
            mapTableBusy.clear(ctx_id);

            let pr = map[pack(req_ar)];
            debugLog.record($format("MAP: Resp: context %0d, AR %0d is PR %0d", ctx_id, req_ar, pr));

            return pr;
        endmethod

        method Action readRewindReq(TOKEN_INDEX tokIdx);
            rewGetResultsReadInQ.enq(tokIdx);
        endmethod

        method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp() if (rewindReaderQ.first() == REGSTATE_REWR_GETRESULTS);
            rewindReaderQ.deq();
            let r <- rewindInfo.readRsp();
            return r;
        endmethod

    endinterface


    interface REGSTATE_REG_MAPPING_COMMITRESULTS commitResults;

        method Action readRewindReq(TOKEN tok);
            rewCommitReadInQ.enq(tok);
        endmethod

        method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp() if (rewindReaderQ.first() == REGSTATE_REWR_COMMITRESULTS);
            rewindReaderQ.deq();
            let r <- rewindInfo.readRsp();
            return r;
        endmethod

    endinterface


    interface REGSTATE_REG_MAPPING_EXCEPTION exceptionQueue;

        method Action readRewindReq(TOKEN_INDEX tokIdx);
            rewExceptionReadInQ.enq(tokIdx);
        endmethod

        method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp() if (rewindReaderQ.first() == REGSTATE_REWR_EXCEPTION);
            rewindReaderQ.deq();
            let r <- rewindInfo.readRsp();
            return r;
        endmethod

        method Action updateMap(REGSTATE_NEW_MAPPINGS map_dsts);
            exceptInQ.enq(map_dsts);
            newMapTableReqQ_EXC.enq(map_dsts.context_id);
        endmethod

    endinterface

endmodule

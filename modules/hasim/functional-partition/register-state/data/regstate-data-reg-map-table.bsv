//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
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
`include "asim/provides/debug_scan_service.bsh"
 
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
// REGSTATE_REG_MAPPING_GETDEPENDENCIES -- Interface used by getDependencies
//   pipeline for reading current mappings for register sources and
//   updating mappings for destinations.
//
interface REGSTATE_REG_MAPPING_GETDEPENDENCIES;
    method Action decodeReq(TOKEN tok,
                            Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)) ar_srcs,
                            Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)) ar_dsts,
                            Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_dsts);

    method ActionValue#(Vector#(ISA_MAX_SRCS,
                                Maybe#(FUNCP_PHYSICAL_REG_INDEX))) decodeRsp();
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
    method Action updateMap(REGSTATE_NEW_MAPPINGS map_dsts);
    method Action readRewindReq(TOKEN tok);
    method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp();
endinterface

//
// REGSTATE_REG_MAPPING_REWIND --
//   Rewind pipeline interface.
//
interface REGSTATE_REG_MAPPING_REWIND;
    method Action readRewindReq(TOKEN_INDEX tokIdx);
    method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp();

    method Action updateMap(REGSTATE_NEW_MAPPINGS map_dsts);
endinterface


interface REGSTATE_REG_MAPPING;
    interface REGSTATE_REG_MAPPING_GETDEPENDENCIES getDependencies;
    interface REGSTATE_REG_MAPPING_GETRESULTS      getResults;
    interface REGSTATE_REG_MAPPING_COMMITRESULTS   commitResults;
    interface REGSTATE_REG_MAPPING_REWIND          rewindQueue;
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
    REGSTATE_MAPT_REWIND,
    REGSTATE_MAPT_READMAP
}
MAPTABLE_CONSUMER
    deriving (Eq, Bits);

// 
// REGSTATE_REWIND_READER manages responses for multiple readers of rewind
// BRAM.
//
`define REGSTATE_REWR_GETRESULTS 0 
`define REGSTATE_REWR_COMMITRESULTS 1
`define REGSTATE_REWR_REWIND 2


//
// REGSTATE_MAPTABLE_IDX is a combination of the context and a virtual
// register.
//
typedef struct
{
    CONTEXT_ID ctxId;
    ISA_REG_INDEX ar;
}
REGSTATE_MAPTABLE_IDX
    deriving (Eq, Bits);


// ========================================================================
//
//   The busy vector tracks active map table read requests.  Only one
//   access may be in flight for a given context, though multiple contexts
//   may be in flight.  Only one may be in flight for a context because
//   rewind requests must be given priority over other queued requests.
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
    (REGSTATE_REG_MAPPING)
    provisos (Add#(ISA_MAX_SRCS, ISA_MAX_DSTS, t_MAP_READ_PORTS));

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
    function FUNCP_PHYSICAL_REG_INDEX initMapTable(REGSTATE_MAPTABLE_IDX idx);
        return zeroExtend(idx.ctxId) * fromInteger(valueOf(ISA_NUM_REGS)) +
               zeroExtend(pack(idx.ar));
    endfunction

    // Uninitialized map table
    BRAM_MULTI_READ#(t_MAP_READ_PORTS, REGSTATE_MAPTABLE_IDX, FUNCP_PHYSICAL_REG_INDEX) uninitMapTable <- mkBRAMMultiRead();

    // Map table
    BRAM_MULTI_READ#(t_MAP_READ_PORTS, REGSTATE_MAPTABLE_IDX, FUNCP_PHYSICAL_REG_INDEX) mapTable <-
        mkMultiMemInitializedWith(uninitMapTable, initMapTable);
    
    // Queue to track proper consumer of read data coming from map table BRAM
    FIFO#(MAPTABLE_CONSUMER) mapTableConsumerQ <- mkFIFO();
    
    //
    // Incoming request queues for decode, read and rewind queues.
    //
    FIFO#(Tuple4#(TOKEN,
                  Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)),
                  Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)),
                  Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)))) newMapTableReqQ_DEC <- mkBypassFIFO();
    FIFO#(Tuple2#(CONTEXT_ID, ISA_REG_INDEX)) newMapTableReqQ_READ <- mkBypassFIFO();
    FIFO#(REGSTATE_NEW_MAPPINGS) newMapTableReqQ_EXC <- mkBypassFIFO();

    // mapTableBusy tracks which context are in flight.
    MAP_BUSY_VECTOR mapTableBusy <- mkMapBusyVector();

    //
    // Rewind information
    //
    BRAM_MULTI_READ#(3, TOKEN_INDEX, Maybe#(REGSTATE_REWIND_INFO)) rewindInfo <- mkBRAMBufferedPseudoMultiRead(False);

    // Internal decode queues
    FIFOF#(Tuple4#(TOKEN,
                   Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)),
                   Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)),
                   Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)))) decodeReadCurQ <- mkFIFOF();
    FIFOF#(Tuple3#(TOKEN,
                   Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)),
                   Bool)) decodeUpdateQ <- mkFIFOF();
    // Merge FIFO collapses multiple write port requests into a series of write
    // requests -- one per cycle.
    MERGE_FIFOF#(ISA_MAX_DSTS, Tuple2#(ISA_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX)) decodeUpdateDstQ <- mkMergeFIFOF();

    // Internal read mapping request queue.
    FIFOF#(Tuple2#(CONTEXT_ID, ISA_REG_INDEX)) getResultsReqInQ <- mkFIFOF();
    FIFOF#(FUNCP_PHYSICAL_REG_INDEX) readMapRspQ <- mkSizedBypassFIFOF(8);

    // Rewind register updates.  Map table has only one write port so multiple
    // register updates go into a merge FIFO and are handled one at a time.
    MERGE_FIFOF#(ISA_MAX_DSTS, Tuple2#(ISA_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX)) exceptUpdateQ <- mkMergeFIFOF();
    FIFO#(CONTEXT_ID) exceptInfoQ <- mkFIFO();

    // Incoming token rewind info requests
    FIFOF#(TOKEN_INDEX) rewGetResultsReadInQ <- mkFIFOF();
    FIFOF#(TOKEN) rewCommitReadInQ <- mkFIFOF();
    FIFOF#(TOKEN_INDEX) rewReadInQ <- mkFIFOF();

    // Outgoing queues
    FIFOF#(Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) mapDecodeOutQ <- mkSizedFIFOF(8);
    COUNTER#(4) mapDecodeOutNumInFlight <- mkLCounter(0);


    // ====================================================================
    //
    //   Debug scan
    //
    // ====================================================================

    DEBUG_SCAN_FIELD_LIST dbg_list = List::nil;
    dbg_list <- addDebugScanField(dbg_list, "decodeReadCurQ notEmpty", decodeReadCurQ.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "decodeUpdateQ notEmpty", decodeUpdateQ.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "getResultsReqInQ notEmpty", getResultsReqInQ.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "rewGetResultsReadInQ notEmpty", rewGetResultsReadInQ.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "rewCommitReadInQ notEmpty", rewCommitReadInQ.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "rewReadInQ notEmpty", rewReadInQ.notEmpty);

    let dbgNode <- mkDebugScanNode("FUNCP REGSTATE Reg Map Table", dbg_list);


    // ====================================================================
    //
    //   All incoming requests begin rule processing here...
    //
    // ====================================================================

    //
    // mapTableIdx --
    //     Index into the map table given a context ID and architectural register.
    //
    function mapTableIdx(CONTEXT_ID ctxId, ISA_REG_INDEX ar);
        return REGSTATE_MAPTABLE_IDX {ctxId: ctxId, ar: ar};
    endfunction


    //
    // newMapTableReq_DEC --
    //     Head of the decode pipeline.  Begin by requesting the physical register
    //     mappings of the instruction's source and destination registers.
    //
    (* conservative_implicit_conditions *)
    rule newMapTableReq_DEC (mapTableBusy.notBusy(tokContextId(tpl_1(newMapTableReqQ_DEC.first()))));
        match {.tok, .ar_srcs, .ar_dsts, .phy_dsts} = newMapTableReqQ_DEC.first();
        newMapTableReqQ_DEC.deq();

        // Lock map table for this context
        let ctx_id = tokContextId(tok);
        mapTableBusy.set(ctx_id);
        mapTableConsumerQ.enq(REGSTATE_MAPT_DECODE);

        //
        // Request current physical mappings for instruction source registers.
        //
        for (Integer x = 0; x < valueOf(ISA_MAX_SRCS); x = x + 1)
        begin
            // Make the request on all possible registers so we can use
            // conservative scheduling rules on the consumer.
            let ar = validValue(ar_srcs[x]);
            mapTable.readPorts[x].readReq(mapTableIdx(ctx_id, ar));
        end

        //
        // Request current (old) destination mappings for filling in rewind info.
        //
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            // Make the request on all possible registers so we can use
            // conservative scheduling rules on the consumer.
            let ar = validValue(ar_dsts[x]);
            mapTable.readPorts[valueOf(ISA_MAX_SRCS) + x].readReq(mapTableIdx(ctx_id, ar));
        end

        debugLog.record($format("MAP: Table read context %0d, consumer DECODE", ctx_id));

        // Forward request to next decode rule
        decodeReadCurQ.enq(tuple4(tok, ar_srcs, ar_dsts, phy_dsts));
    endrule


    //
    // newMapTableReq_READ --
    //     Simple read of a single register mapping.
    //
    rule newMapTableReq_READ (mapTableBusy.notBusy(tpl_1(newMapTableReqQ_READ.first())));
        match {.ctx_id, .ar} = newMapTableReqQ_READ.first();
        newMapTableReqQ_READ.deq();

        mapTableBusy.set(ctx_id);
        mapTableConsumerQ.enq(REGSTATE_MAPT_READMAP);

        // Read mapping
        mapTable.readPorts[0].readReq(mapTableIdx(ctx_id, ar));

        debugLog.record($format("MAP: Table read context %0d, consumer READ", ctx_id));
    endrule


    //
    // newMapTableReq_EXC --
    //     Rewind handling flow.  Update register mapping with
    //     the mapping that preceded the instruction.
    //
    (* descending_urgency = "newMapTableReq_EXC, newMapTableReq_READ, newMapTableReq_DEC" *)
    (* conservative_implicit_conditions *)
    rule newMapTableReq_EXC (mapTableBusy.notBusy(newMapTableReqQ_EXC.first().context_id));
        let new_maps = newMapTableReqQ_EXC.first();
        newMapTableReqQ_EXC.deq();

        // Request specific register updates.  Writing them to a MergeFIFO
        // serializes multiple write requests.
        Bool updated_map = False;
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            if (new_maps.mappings[x] matches tagged Valid { .ar, .pr })
            begin
                exceptUpdateQ.ports[x].enq(tuple2(ar, pr));
                updated_map = True;
            end
        end

        if (updated_map)
        begin
            // Lock the map table because updates may take multiple cycles if more
            // then one register mapping is being changed.
            mapTableBusy.set(new_maps.context_id);
            exceptInfoQ.enq(new_maps.context_id);
        end

        debugLog.record($format("MAP: Table read context %0d, consumer EXC", new_maps.context_id));
    endrule


    // ====================================================================
    //
    //   Mapping for decode stage pipeline
    //
    // ====================================================================

    //
    // doMapDecode1 --
    //     Receive source and current destination register mapping.  The
    //     current destination register mapping are written to the
    //     token's rewind info.
    //
    //     Also begins the process of updating the destination register mapping
    //     to the new physical registers provided on the newPhyDstsInQ.
    //
    (* conservative_implicit_conditions *)
    rule doMapDecodeReadCur (mapTableConsumerQ.first() == REGSTATE_MAPT_DECODE);
        mapTableConsumerQ.deq();

        match {.tok, .ar_srcs, .ar_dsts, .phy_dsts} = decodeReadCurQ.first();
        decodeReadCurQ.deq();

        //
        // Compute source mappings.
        //
        Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_srcs;
        for (Integer x = 0; x < valueOf(ISA_MAX_SRCS); x = x + 1)
        begin
            let pr <- mapTable.readPorts[x].readRsp();
            if (ar_srcs[x] matches tagged Valid .ar)
            begin
                phy_srcs[x] = tagged Valid pr;
                debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Slot #%0d source AR %0d is PR %0d", x, validValue(ar_srcs[x]), pr));
            end
            else
            begin
                phy_srcs[x] = tagged Invalid;
            end
        end

        //
        // Compute physical registers that are freed when this token commits.
        // These free registers are the physical registers previously mapped
        // to the token's architectural destinations.
        //
        Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) old_phy_dsts;
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            let pr <- mapTable.readPorts[valueOf(ISA_MAX_SRCS) + x].readRsp();
            if (ar_dsts[x] matches tagged Valid .ar)
            begin
                old_phy_dsts[x] = tagged Valid pr;
                debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Slot #%0d AR %0d will free PR %0d", x, validValue(ar_dsts[x]), pr));
            end
            else
            begin
                old_phy_dsts[x] = tagged Invalid;
            end
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
        // Now it is safe to update the mappings for new destinations.  The
        // updates are written to decodeUpdateDstQ and will be written to
        // the map table in the next stage.
        //
        Bool updated_map = False;
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            if (ar_dsts[x] matches tagged Valid .ar &&&
                phy_dsts[x] matches tagged Valid .pr)
            begin
                decodeUpdateDstQ.ports[x].enq(tuple2(ar, pr));
                updated_map = True;
            end
        end

        decodeUpdateQ.enq(tuple3(tok, phy_srcs, updated_map));
    endrule


    //
    // doMapDecodeNoUpdate --
    //     This path is taken when the instruction writes no registers.
    //
    rule doMapDecodeNoUpdate (! tpl_3(decodeUpdateQ.first()));
        match {.tok, .phy_srcs, .updated_map} = decodeUpdateQ.first();
        let ctx_id = tokContextId(tok);

        decodeUpdateQ.deq();
        mapTableBusy.clear(ctx_id);

        mapDecodeOutQ.enq(phy_srcs);
        debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Done"));
    endrule


    //
    // doMapDecodeWithUpdate --
    //     Stage three iterates over all the registers written by the instruction,
    //     updating the map table.
    //
    (* conservative_implicit_conditions *)
    rule doMapDecodeWithUpdate (tpl_3(decodeUpdateQ.first()));
        match {.tok, .phy_srcs, .updated_map} = decodeUpdateQ.first();
        let ctx_id = tokContextId(tok);

        // Update one register mapping
        match {.ar, .pr} = decodeUpdateDstQ.first();
        decodeUpdateDstQ.deq();

        mapTable.write(mapTableIdx(ctx_id, ar), pr);
        debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Slot #%0d dest AR %0d -> PR %0d", decodeUpdateDstQ.firstPortID(), ar, pr));

        // If this the end of the current instruction's update request then
        // unlock the context and return the physical source mapping to the
        // caller.
        if (decodeUpdateDstQ.lastInGroup())
        begin
            decodeUpdateQ.deq();
            mapTableBusy.clear(ctx_id);

            mapDecodeOutQ.enq(phy_srcs);
            debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Done"));
        end
    endrule


    // ====================================================================
    //
    //   Read map table processing
    //
    // ====================================================================

    //
    // doReadMap --
    //   Complete the read of a single entry in the map table.
    //
    rule doReadMap (mapTableConsumerQ.first() == REGSTATE_MAPT_READMAP);
        match {.ctx_id, .req_ar} = getResultsReqInQ.first();
        getResultsReqInQ.deq();

        // Physical register mapping
        let pr <- mapTable.readPorts[0].readRsp();
        debugLog.record($format("MAP: Read: context %0d, AR %0d is PR %0d", ctx_id, req_ar, pr));

        // Release mapTable and context ID locks
        mapTableConsumerQ.deq();
        mapTableBusy.clear(ctx_id);

        readMapRspQ.enq(pr);
    endrule

    // ====================================================================
    //
    //   Rewind processing
    //
    // ====================================================================

    //
    // doRewindUpdates --
    //   Update map table during rewind.
    //
    (* descending_urgency = "doRewindUpdates, doReadMap, doMapDecodeWithUpdate, doMapDecodeNoUpdate" *)
    (* conservative_implicit_conditions *)
    rule doRewindUpdates (True);
        let ctx_id = exceptInfoQ.first();

        // Update one register mapping
        match {.ar, .pr} = exceptUpdateQ.first();
        exceptUpdateQ.deq();

        mapTable.write(mapTableIdx(ctx_id, ar), pr);
        debugLog.record($format("MAP: EXC Update: context %0d, AR %0d -> PR %0d", ctx_id, ar, pr));

        // If this the end of the current instruction's rewind request then
        // unlock the context.
        if (exceptUpdateQ.lastInGroup())
        begin
            exceptInfoQ.deq();
            mapTableBusy.clear(ctx_id);
        end
    endrule


    // ====================================================================
    //
    //   Rewind info logic
    //
    // ====================================================================

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

            rewindInfo.readPorts[`REGSTATE_REWR_GETRESULTS].readReq(tok_idx);

            debugLog.record($format("MAP: ") + fshow(tok_idx) + $format(": Request REWIND info for GETRESULTS"));
        end
        else if (rewCommitReadInQ.notEmpty())
        begin
            //
            // Normal commit path.
            //
            let tok = rewCommitReadInQ.first();
            rewCommitReadInQ.deq();

            rewindInfo.readPorts[`REGSTATE_REWR_COMMITRESULTS].readReq(tok.index);

            debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": Request REWIND info for COMMIT"));
        end
        else if (rewReadInQ.notEmpty())
        begin
            let tok_idx = rewReadInQ.first();
            rewReadInQ.deq();

            rewindInfo.readPorts[`REGSTATE_REWR_REWIND].readReq(tok_idx);

            debugLog.record($format("MAP: ") + fshow(tok_idx) + $format(": Request REWIND info for EXCEPT"));
        end
    endrule


    // ====================================================================
    //
    //   Interfaces
    //
    // ====================================================================


    interface REGSTATE_REG_MAPPING_GETDEPENDENCIES getDependencies;

        method Action decodeReq(TOKEN tok,
                                Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)) ar_srcs,
                                Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)) ar_dsts,
                                Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_dsts)
                                    if (msb(mapDecodeOutNumInFlight.value) == 0);
            mapDecodeOutNumInFlight.up();
            newMapTableReqQ_DEC.enq(tuple4(tok, ar_srcs, ar_dsts, phy_dsts));
            debugLog.record($format("MAP: ") + fshow(tok.index) + $format(": decodeReq"));
        endmethod

        // Decode response describes source register mappings.
        method ActionValue#(Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) decodeRsp();
            let r = mapDecodeOutQ.first();
            mapDecodeOutQ.deq();
            mapDecodeOutNumInFlight.down();
            return r;
        endmethod

    endinterface


    interface REGSTATE_REG_MAPPING_GETRESULTS getResults;

        method Action readMapReq(CONTEXT_ID ctx_id, ISA_REG_INDEX ar);
            getResultsReqInQ.enq(tuple2(ctx_id, ar));
            newMapTableReqQ_READ.enq(tuple2(ctx_id, ar));

            debugLog.record($format("MAP: Request context %0d, AR %0d for RSLT", ctx_id, ar));
        endmethod

        method ActionValue#(FUNCP_PHYSICAL_REG_INDEX) readMapRsp();
            let pr = readMapRspQ.first();
            readMapRspQ.deq();

            return pr;
        endmethod

        method Action readRewindReq(TOKEN_INDEX tokIdx);
            rewGetResultsReadInQ.enq(tokIdx);
        endmethod

        method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp();
            let r <- rewindInfo.readPorts[`REGSTATE_REWR_GETRESULTS].readRsp();
            return r;
        endmethod

    endinterface


    interface REGSTATE_REG_MAPPING_COMMITRESULTS commitResults;

        method Action readRewindReq(TOKEN tok);
            rewCommitReadInQ.enq(tok);
        endmethod

        method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp();
            let r <- rewindInfo.readPorts[`REGSTATE_REWR_COMMITRESULTS].readRsp();
            return r;
        endmethod

        method Action updateMap(REGSTATE_NEW_MAPPINGS map_dsts);
            newMapTableReqQ_EXC.enq(map_dsts);
        endmethod

    endinterface


    interface REGSTATE_REG_MAPPING_REWIND rewindQueue;

        method Action readRewindReq(TOKEN_INDEX tokIdx);
            rewReadInQ.enq(tokIdx);
        endmethod

        method ActionValue#(Maybe#(REGSTATE_REWIND_INFO)) readRewindRsp();
            let r <- rewindInfo.readPorts[`REGSTATE_REWR_REWIND].readRsp();
            return r;
        endmethod

        method Action updateMap(REGSTATE_NEW_MAPPINGS map_dsts);
            newMapTableReqQ_EXC.enq(map_dsts);
        endmethod

    endinterface

endmodule

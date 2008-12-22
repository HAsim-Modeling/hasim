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
import Vector::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_modellib.bsh"
 
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

typedef Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING)) REGSTATE_NEW_MAPPINGS;

//
// Rewind info describes how to roll back side effects of a token.
//
typedef struct
{
    FUNCP_PHYSICAL_REG_INDEX freeListPos;
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
                               Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_dsts,
                               FUNCP_PHYSICAL_REG_INDEX freeListPos);

    method ActionValue#(Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) decodeRsp();
endinterface

//
// REGSTATE_REG_MAPPING_GETRESULTS --
//   Get results pipeline interface.  Methods only read state.
//
interface REGSTATE_REG_MAPPING_GETRESULTS;
    method Action readMapReq(ISA_REG_INDEX ar);
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

//
// Map table index
//
//typedef Bit#(TLog#(ISA_NUM_REGS)) REGSTATE_REG_MAP_IDX;

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
    // Architectural to physical register map
    //
    Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) initMap = newVector();
    for (Integer x  = 0; x < valueof(ISA_NUM_REGS); x = x + 1)
    begin
      initMap[x] = fromInteger(x);
    end

    Reg#(Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) mapTable <- mkReg(initMap);

    //
    // Rewind information
    //
    BRAM#(TOKEN_INDEX, Maybe#(REGSTATE_REWIND_INFO)) rewindInfo <- mkBRAM();
    FIFO#(REGSTATE_REWIND_READER) rewindReaderQ <- mkFIFO();

    // Incoming request queues
    FIFO#(Tuple2#(Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)),
                  Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)))) decodeStage1InQ <- mkFIFO();
    FIFO#(Tuple3#(TOKEN,
                  Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)),
                  FUNCP_PHYSICAL_REG_INDEX)) decodeStage2InQ <- mkFIFO();

    FIFO#(ISA_REG_INDEX) getResultsReqInQ <- mkFIFO();
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
    //   Initialization
    //
    // ====================================================================

//    Reg#(Bool) initialized <- mkReg(False);
//    Reg#(ISA_REG_INDEX) initRegIdx <- mkReg(0);
//
//    rule initializeMapTable (! initialized);
//        mapTable.write(initRegIdx, zeroExtend(pack(initRegIdx)));
//
//        // We're done if we've initialized the last register.
//        if (initRegIdx == maxBound)
//        begin
//            initialized <= True;
//        end
//
//        initRegIdx <= initRegIdx + 1;
//    endrule


    // ====================================================================
    //
    //   Map table logic
    //
    // ====================================================================


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
    rule doMapDecode (True);
        match {.ar_srcs, .ar_dsts} = decodeStage1InQ.first();
        decodeStage1InQ.deq();

        match {.tok, .phy_dsts, .freeListPos} = decodeStage2InQ.first();
        decodeStage2InQ.deq();

        //
        // Compute physical registers that are freed when this token commits.
        // These free registers are the physical registers previously mapped
        // to the token's architectural destinations.
        //
        Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) old_phy_dsts;
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            old_phy_dsts[x] = case (ar_dsts[x]) matches
                                  tagged Valid .ar: tagged Valid mapTable[pack(ar)];
                                  tagged Invalid: tagged Invalid;
                              endcase;

            if (old_phy_dsts[x] matches tagged Valid .pr)
                debugLog.record($format("MAP: Token %0d: Slot #%0d AR %0d will free PR %0d", tok.index, x, validValue(ar_dsts[x]), pr));
        end
        
        // For instruction with no true destination free the dummy physical reg.
        if (! isValid(ar_dsts[0]))
        begin
            old_phy_dsts[0] = phy_dsts[0];

            if (old_phy_dsts[0] matches tagged Valid .pr)
                debugLog.record($format("MAP: Token %0d: Slot #0 (hidden) will free PR %0d", tok.index, pr));
        end
            
        // Store rewind info
        rewindInfo.write(tok.index,
                         tagged Valid REGSTATE_REWIND_INFO { freeListPos: freeListPos,
                                                             regsToFree: old_phy_dsts });


        //
        // Update mappings for destinations.
        //
        Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) updated_map = mapTable;
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            if (ar_dsts[x] matches tagged Valid .ar &&&
                phy_dsts[x] matches tagged Valid .pr)
            begin
                updated_map[pack(ar)] = pr;
                debugLog.record($format("MAP: Token %0d: Slot #%0d dest AR %0d -> PR %0d", tok.index, x, ar, pr));
            end
        end


        //
        // Compute source mappings.
        //
        Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_srcs;
        for (Integer x = 0; x < valueOf(ISA_MAX_SRCS); x = x + 1)
        begin
            phy_srcs[x] = case (ar_srcs[x]) matches
                                  tagged Valid .ar: tagged Valid mapTable[pack(ar)];
                                  tagged Invalid: tagged Invalid;
                          endcase;

            if (phy_srcs[x] matches tagged Valid .pr)
                debugLog.record($format("MAP: Token %0d: Slot #%0d source AR %0d is PR %0d", tok.index, x, validValue(ar_srcs[x]), pr));
        end
        

        mapTable <= updated_map;
        mapDecodeOutQ.enq(phy_srcs);
    endrule


    //
    // doExceptionUpdates --
    //   Update map table during exception rewind.
    //
    (* descending_urgency = "doExceptionUpdates, doMapDecode" *)
    rule doExceptionUpdates (True);
        let new_maps = exceptInQ.first();
        exceptInQ.deq();

        Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) updated_map = mapTable;

        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            if (new_maps[x] matches tagged Valid { .ar, .pr })
            begin
                updated_map[pack(ar)] = pr;
                debugLog.record($format("MAP: Update: AR %0d -> PR %0d", ar, pr));
            end
        end
        
        mapTable <= updated_map;
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
        debugLog.record($format("MAP: Token %0d: Init REWIND", tok.index));
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

            debugLog.record($format("MAP: Token %0d: Request REWIND info for GETRESULTS", tok_idx));
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

            debugLog.record($format("MAP: Token %0d: Request REWIND info for COMMIT", tok.index));
        end
        else if (rewExceptionReadInQ.notEmpty())
        begin
            let tok_idx = rewExceptionReadInQ.first();
            rewExceptionReadInQ.deq();

            rewindReaderQ.enq(REGSTATE_REWR_EXCEPTION);
            rewindInfo.readReq(tok_idx);

            debugLog.record($format("MAP: Token %0d: Request REWIND info for EXCEPT", tok_idx));
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
            //
            // XXX When the map table is stored in BRAM this is where
            //     we would request the table associated with the token's context.
            // XXX
    
            decodeStage1InQ.enq(tuple2(ar_srcs, ar_dsts));
            debugLog.record($format("MAP: Token %0d: decodeStage1", tok.index));
        endmethod

        method Action decodeStage2(TOKEN tok,
                                   Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_dsts,
                                   FUNCP_PHYSICAL_REG_INDEX freeListPos);
            decodeStage2InQ.enq(tuple3(tok, phy_dsts, freeListPos));
            debugLog.record($format("MAP: Token %0d: decodeStage2", tok.index));
        endmethod

        method ActionValue#(Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) decodeRsp();
            let r = mapDecodeOutQ.first();
            mapDecodeOutQ.deq();
            return r;
        endmethod

    endinterface


    interface REGSTATE_REG_MAPPING_GETRESULTS getResults;

        method Action readMapReq(ISA_REG_INDEX ar);
            getResultsReqInQ.enq(ar);
            debugLog.record($format("MAP: Request AR %0d for RSLT", ar));
        endmethod

        method ActionValue#(FUNCP_PHYSICAL_REG_INDEX) readMapRsp();
            let req_ar = getResultsReqInQ.first();
            getResultsReqInQ.deq();

            let pr = mapTable[pack(req_ar)];
            debugLog.record($format("MAP: Resp: AR %0d is PR %0d", req_ar, pr));

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
        endmethod

    endinterface

endmodule

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
// Physical register file --
//
//   Manage both the physical register file and the valid bits on each register.
//   Separate interfaces are provided for individual pipeline stages.  Relative
//   priority of pipeline stages is managed inside the module.
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
 

//
// REGSTATE_PHYSICAL_REGS_INVAL_REGS --
//   Interface that allows invalidation of one register per cycle.
//
interface REGSTATE_PHYSICAL_REGS_INVAL_REGS;
    //
    // Invalidate uses a request/response interface so the caller can know when
    // the physical register has been marked invalid.
    //
    method Action invalReq(Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) regs);
    method Action invalRsp();
endinterface

//
// REGSTATE_PHYSICAL_REGS_WRITE_REG --
//   Interface that allows writing to one register per cycle.
//
interface REGSTATE_PHYSICAL_REGS_WRITE_REG;
    method Action write(FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
endinterface

//
// REGSTATE_PHYSICAL_REGS_RW_REGS --
//   Interfaces with multiple read ports and one write port.
//
interface REGSTATE_PHYSICAL_REGS_RW_REGS;
    // Vector read interface to read all inputs for an instruction.  There
    // is no corresponding response to this request because the register
    // values are forwarded directly to the ISA data path through the
    // "isa_datapath_srcvals" soft connection.
    method Action readRegVecReq(ISA_INST_SRCS rVec);

    // Single register read interface (used by emulation)
    method Action readReq(FUNCP_PHYSICAL_REG_INDEX r);
    method ActionValue#(ISA_VALUE) readRsp();

    // Write a single register
    method Action write(FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
endinterface

interface REGSTATE_PHYSICAL_REGS#(numeric type numPrfReadPorts);
    interface REGSTATE_PHYSICAL_REGS_INVAL_REGS getDependencies;
    interface REGSTATE_PHYSICAL_REGS_RW_REGS    getResults;
    interface REGSTATE_PHYSICAL_REGS_WRITE_REG  doLoads;
endinterface


// ========================================================================
//
//   Internal data structures
//
// ========================================================================

//
// Read state controls access to register file BRAM
//
typedef enum
{
    REGSTATE_PRF_READ_READY,
    REGSTATE_PRF_READ_GETRESULTS_SINGLE,
    REGSTATE_PRF_READ_GETRESULTS_VEC
}
REGSTATE_PRF_READ_STATE
    deriving (Eq, Bits);


//
// Physical register file container.  The number of read ports is configurable,
// trading area for simulator performance.
//
module [HASIM_MODULE] mkFUNCP_Regstate_Physical_Regs
    // interface:
    (REGSTATE_PHYSICAL_REGS#(numPrfReadPorts));

    let v_numPrfReadPorts = valueOf(numPrfReadPorts);

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_DATA_LOGFILE_PREFIX + "_prf.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Send#(FUNCP_ISA_DATAPATH_SRCVALS) linkToDatapathSrcVals <- mkConnection_Send("isa_datapath_srcvals");


    // ====================================================================
    //
    //   Local state
    //
    // ====================================================================

    // The physical register file
    BRAM_MULTI_READ#(numPrfReadPorts, FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE) prf <- mkBRAMMultiRead();
    
    // Valid bits for PRF
    LUTRAM#(FUNCP_PHYSICAL_REG_INDEX, Bool) prfValids <- mkLUTRAMU();

    // Control access to PRF storage
    Reg#(REGSTATE_PRF_READ_STATE) readState <- mkReg(REGSTATE_PRF_READ_READY);

    //
    // State for reading all inputs registers for an instruction
    //
    Reg#(ISA_INST_SRCS) vecRegReq <- mkRegU();
    // Counters for outstanding register read requests and responses
    Vector#(numPrfReadPorts, Reg#(ISA_SRC_COUNTER)) vecNumReqs  = newVector();
    Vector#(numPrfReadPorts, Reg#(ISA_SRC_COUNTER)) vecNumResps = newVector();
    // Counter used for triggering read requests.  vecReqIdx needs to be able to
    // count one beyond ISA_MAX_SRCS due to the loop code.
    Vector#(numPrfReadPorts, Reg#(ISA_SRC_COUNTER)) vecReqIdx = newVector();
    // Queue for associating register reads with argument positions in the instruction
    Vector#(numPrfReadPorts, FIFO#(ISA_SRC_COUNTER)) vecQ = newVector();
    
    for (Integer pipe = 0; pipe < v_numPrfReadPorts; pipe = pipe + 1)
    begin
        vecNumReqs[pipe]  <- mkRegU();
        vecNumResps[pipe] <- mkRegU();
        vecReqIdx[pipe]   <- mkRegU();
        vecQ[pipe] <- mkFIFO();
    end

    // Holder for vector read response
    Vector#(ISA_MAX_SRCS, Reg#(ISA_VALUE)) srcVals = newVector();
    for (Integer x = 0; x < valueOf(ISA_MAX_SRCS); x = x + 1)
    begin
        srcVals[x] <- mkRegU();
    end

    // Vector of work remaining from an invalidate vector
    Reg#(Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) invalVec <- mkReg(replicate(tagged Invalid));

    // A valid entry at the head of the queue indicates a quick PRF, pipelined,
    // read path with no buffering required.
    FIFO#(Maybe#(Vector#(numPrfReadPorts, Bool))) quickReadPrfQ <- mkFIFO();

    // Individual incoming queues for each pipeline
    FIFOF#(Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) rqGetDepInval <- mkFIFOF();
    FIFO#(Bool) rqGetDepInvalDone <- mkFIFO();

    FIFOF#(FUNCP_PHYSICAL_REG_INDEX)                     rqGetResRead    <- mkFIFOF();
    FIFOF#(ISA_INST_SRCS)                                rqGetResReadVec <- mkFIFOF();
    FIFOF#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE)) rqGetResWrite   <- mkFIFOF();

    FIFOF#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE)) rqDoLoadsWrite  <- mkFIFOF();


    // ====================================================================
    //
    //   Initialize the PRF
    //
    // ====================================================================

    Reg#(Bool) initialized <- mkReg(False);
    Reg#(FUNCP_PHYSICAL_REG_INDEX) initPrfIdx <- mkReg(0);

    // The maximum achitectural register.
    ISA_REG_INDEX maxAR = maxBound;

    // Each context gets maxAR physical registers.
    FUNCP_PHYSICAL_REG_INDEX maxInitPR = (1 + zeroExtend(pack(maxAR))) * fromInteger(valueof(NUM_CONTEXTS)) - 1;

    rule initializePrf (! initialized);
        prf.write(initPrfIdx, 0);

        // We start all mapped physical registers valid and at zero.
        if (initPrfIdx <= maxInitPR)
        begin
            prfValids.upd(initPrfIdx, True);
            debugLog.record($format("PRF: Initializing PR %0d VALID", initPrfIdx));
        end
        else
        begin
            prfValids.upd(initPrfIdx, False);
            debugLog.record($format("PRF: Initializing PR %0d INVALID", initPrfIdx));
        end
        
        // We're done if we've initialized the last register.
        if (initPrfIdx == maxBound)
        begin
            initialized <= True;
        end

        initPrfIdx <= initPrfIdx + 1;
    endrule


    // ====================================================================
    //
    //   PRF access logic
    //
    // ====================================================================

    //
    // sendInvalResponseWhenDone --
    //   Function to check pending vector of invalidations for a single request.
    //   Send a message when all invalidations are complete.
    //
    function Action sendInvalResponseWhenDone(Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) rVec);
    action
        if (! any(isValid, rVec))
        begin
            rqGetDepInvalDone.enq(?);
            debugLog.record($format("PRF: Update: Invalidate done"));
        end
    endaction
    endfunction


    //
    // updatePrf --
    //     Handle all possible writes to the register file.
    //
    rule updatePrf (initialized);
        //
        // Only one write allowed per cycle.  Give INVAL highest priority,
        // followed by priority in reverse pipeline order.
        //

        if (findIndex(isValid, invalVec) matches tagged Valid .idx)
        begin
            //
            // Still working on a previous invalidation vector...
            //
            let r = validValue(invalVec[idx]);
            prfValids.upd(r, False);
            debugLog.record($format("PRF: Update: Invalidate slot #%d, reg %0d", idx, r));

            //
            // Remove this entry from the work list.
            //
            let rVec = invalVec;
            rVec[idx] = tagged Invalid;
            invalVec <= rVec;
            
            // Done?
            sendInvalResponseWhenDone(rVec);
        end
        else if (rqGetDepInval.notEmpty())
        begin
            //
            // New invalidate request vector...
            //
            let rVec = rqGetDepInval.first();
            rqGetDepInval.deq();

            // Handle the first entry.
            if (rVec[0] matches tagged Valid .r)
            begin
                prfValids.upd(r, False);
                debugLog.record($format("PRF: Update: Invalidate slot #0, reg %0d", r));
            end

            // Put any remaining entries on the work list.
            rVec[0] = tagged Invalid;
            invalVec <= rVec;

            // Done?
            sendInvalResponseWhenDone(rVec);
        end
        else if (rqDoLoadsWrite.notEmpty() || rqGetResWrite.notEmpty())
        begin
            //
            // No invalidates pending.  Handle register writes...
            //
            Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE) rq;

            if (rqDoLoadsWrite.notEmpty())
            begin
                rq = rqDoLoadsWrite.first();
                rqDoLoadsWrite.deq();
            end
            else
            begin
                rq = rqGetResWrite.first();
                rqGetResWrite.deq();
            end

            match { .r, .v } = rq;

            prfValids.upd(r, True);
            prf.write(r, v);
            debugLog.record($format("PRF: Update: Write reg %0d <- 0x%x", r, v));
        end
    endrule


    //
    // startReadPprf --
    //     All register reads begin here.  There are two possible flavors:
    //     read a single register and read all registers for an instruction.
    //
    rule startReadPrf (initialized && readState == REGSTATE_PRF_READ_READY);
        if (rqGetResRead.notEmpty())
        begin
            //
            // Single register read.  When the register is valid do the read.
            // If not valid do nothing and the same test will be done next
            // FPGA cycle.
            //

            let r = rqGetResRead.first();
            
            if (prfValids.sub(r))
            begin
                //
                // Simplest implementation:  change the state and block until the
                // result is read.  This interface is used for forwarding the
                // register state over RRR for emulation, so speed is not important.
                //
                rqGetResRead.deq();
                readState <= REGSTATE_PRF_READ_GETRESULTS_SINGLE;
                prf.readPorts[0].readReq(r);
                quickReadPrfQ.enq(tagged Invalid);
                debugLog.record($format("PRF: Start Read: Single reg %0d ready", r));
            end
        end
        else if (rqGetResReadVec.notEmpty())
        begin
            //
            // Vector register read.  We use two read ports on the register file
            // BRAM to reduce the time to complete the request.
            //
            // Calculate the number of requests for each read port and prepare
            // to iterate over the requests.
            //

            let rVec = rqGetResReadVec.first();
            rqGetResReadVec.deq();
            
            // Track whether all requests for the vector have been satisfied
            // and build up quick read path in case no buffering is needed.
            Bool doneWithReqs = True;
            Vector#(numPrfReadPorts, Bool) didRead = Vector::replicate(False);

            for (Integer port = 0; port < v_numPrfReadPorts; port = port + 1)
            begin
                ISA_SRC_COUNTER nReqs = 0;
                let startIdx = port;

                // Count number of requests to be serviced by this port
                for (Integer x = startIdx; x < valueOf(ISA_MAX_SRCS); x = x + v_numPrfReadPorts)
                begin
                    if (rVec[x] matches tagged Valid .r)
                        nReqs = nReqs + 1;
                end

                debugLog.record($format("PRF: Start Vector Read Port %0d: %0d requests", port, nReqs));

                //
                // Try to get an early start on the first register
                //
                if (rVec[port] matches tagged Valid .r &&& prfValids.sub(r))
                begin
                    // First register in the list is ready.  Start it now.
                    debugLog.record($format("PRF: Vec Read Port %0d: reg %0d, slot %0d ready at start", port, r, port));

                    prf.readPorts[port].readReq(r);
                    vecQ[port].enq(fromInteger(port));

                    // Update start position and remaining number of requests
                    startIdx = startIdx + v_numPrfReadPorts;
                    vecNumReqs[port] <= nReqs - 1;

                    didRead[port] = True;
                    doneWithReqs = doneWithReqs && (nReqs == 1);
                end
                else
                begin
                    vecNumReqs[port] <= nReqs;
                    doneWithReqs = doneWithReqs && (nReqs == 0);
                end

                vecNumResps[port] <= nReqs;
                vecReqIdx[port] <= fromInteger(startIdx);
            end

            vecRegReq <= rVec;
            
            if (doneWithReqs)
            begin
                //
                // All register reads have been requested.  Use the quick path
                // and forward the data to the ISA data path as soon as the
                // BRAM reads return.
                //
                quickReadPrfQ.enq(tagged Valid didRead);
            end
            else
            begin
                //
                // Not all registers have been read.  Stall to generate all
                // requests.
                //
                quickReadPrfQ.enq(tagged Invalid);
                readState <= REGSTATE_PRF_READ_GETRESULTS_VEC;
            end
        end
    endrule


    //
    // Quick path, taken only when all register reads for an entire vector were
    // generated by startReadPrf.
    //
    rule quickReadReqVector (quickReadPrfQ.first() matches tagged Valid .didRead);
        quickReadPrfQ.deq();

        debugLog.record($format("PRF: Quick read path..."));

        ISA_SOURCE_VALUES regVals = ?;
        for (Integer port = 0; port < v_numPrfReadPorts; port = port + 1)
        begin
            if (didRead[port])
            begin
                let v <- prf.readPorts[port].readRsp();
                regVals[port] = v;
                debugLog.record($format("PRF:   Quick Read Port %0d: slot %0d <- 0x%x", port, port, v));

                vecQ[port].deq();
            end
        end

        linkToDatapathSrcVals.send(initISADatapathSrcVals(regVals));
    endrule


    //
    // Slow path:  generate separate rules for each register file read port.
    //
    for (Integer port = 0; port < v_numPrfReadPorts; port = port + 1)
    begin

        //
        // readReqVector --
        //     Wait for each input register to become ready and then read it.
        //
        rule readReqVector (readState == REGSTATE_PRF_READ_GETRESULTS_VEC &&&
                            vecNumReqs[port] != 0 &&&
                            quickReadPrfQ.first() matches tagged Invalid);
            let reqIdx = vecReqIdx[port];

            if (vecRegReq[reqIdx] matches tagged Valid .r)
            begin
                if (prfValids.sub(r))
                begin
                    debugLog.record($format("PRF: Vec Read Port %0d: reg %0d, slot %0d ready", port, r, reqIdx));

                    prf.readPorts[port].readReq(r);
                    vecQ[port].enq(truncate(reqIdx));

                    vecReqIdx[port] <= vecReqIdx[port] + fromInteger(v_numPrfReadPorts);
                    vecNumReqs[port] <= vecNumReqs[port] - 1;
                end
            end
            else
            begin
                debugLog.record($format("PRF: Vec Read Port %0d: slot %0d no request", port, reqIdx));
                vecReqIdx[port] <= vecReqIdx[port] + fromInteger(v_numPrfReadPorts);
            end
        endrule

        //
        // readRespVector --
        //     Receive responses to register file read requests.  Store the result
        //     in the srcVals vector.
        //
        rule readRespVector (readState == REGSTATE_PRF_READ_GETRESULTS_VEC &&&
                             vecNumResps[port] != 0 &&&
                             quickReadPrfQ.first() matches tagged Invalid);
            let rIdx = vecQ[port].first();
            vecQ[port].deq();

            let v <- prf.readPorts[port].readRsp();
            srcVals[rIdx] <= v;

            vecNumResps[port] <= vecNumResps[port] - 1;

            debugLog.record($format("PRF: Vec Read Port %0d: slot %0d <- 0x%x", port, rIdx, v));
        endrule

    end


    //
    // allValuesReady --
    //     Predicate for forwardSrcvals below.  True iff all registers have been
    //     read.
    //
    function Bool allValuesReady();
        Bool ready = True;
        for (Integer x = 0; x < valueOf(numPrfReadPorts); x = x + 1)
        begin
            ready = ready && (vecNumResps[x] == 0);
        end
        return ready;
    endfunction


    //
    // forwardSrcvals --
    //     End of the slow path.  Send values to ISA data path after all
    //     registers have been read.
    //
    rule forwardSrcvals ((readState == REGSTATE_PRF_READ_GETRESULTS_VEC) &&&
                         allValuesReady() &&&
                         quickReadPrfQ.first() matches tagged Invalid);
        debugLog.record($format("PRF: Forward register values to ISA datapath"));

        // Pop an entry (must be invalid)
        quickReadPrfQ.deq();

        ISA_SOURCE_VALUES regVals = ?;
        for (Integer x = 0; x < valueOf(ISA_MAX_SRCS); x = x + 1)
        begin
            regVals[x] = srcVals[x];
        end

        readState <= REGSTATE_PRF_READ_READY;

        linkToDatapathSrcVals.send(initISADatapathSrcVals(regVals));
    endrule


    // ====================================================================
    //
    //   Interfaces
    //
    // ====================================================================

    interface REGSTATE_PHYSICAL_REGS_INVAL_REGS getDependencies;

        method Action invalReq(Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) regs);
            rqGetDepInval.enq(regs);
        endmethod

        method Action invalRsp();
            rqGetDepInvalDone.deq();
        endmethod

    endinterface

    interface REGSTATE_PHYSICAL_REGS_RW_REGS getResults;

        //
        // Single register read interface
        //
        method Action readReq(FUNCP_PHYSICAL_REG_INDEX r);
            rqGetResRead.enq(r);
            debugLog.record($format("PRF: Single Read Req: %0d", r));
        endmethod

        method ActionValue#(ISA_VALUE) readRsp() if (readState == REGSTATE_PRF_READ_GETRESULTS_SINGLE &&&
                                                     quickReadPrfQ.first() matches tagged Invalid);
            quickReadPrfQ.deq();

            let v <- prf.readPorts[0].readRsp();
            readState <= REGSTATE_PRF_READ_READY;

            debugLog.record($format("PRF: Single Read: 0x%x", v));

            return v;
        endmethod


        //
        // Read all registers for an instruction
        //
        method Action readRegVecReq(ISA_INST_SRCS rVec);
            rqGetResReadVec.enq(rVec);
            debugLog.record($format("PRF: Vector Read Req"));
        endmethod


        //
        // Write to target register
        //
        method Action write(FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
            rqGetResWrite.enq(tuple2(r, v));
        endmethod

    endinterface

    interface REGSTATE_PHYSICAL_REGS_WRITE_REG doLoads;

        method Action write(FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
            rqDoLoadsWrite.enq(tuple2(r, v));
        endmethod

    endinterface

endmodule

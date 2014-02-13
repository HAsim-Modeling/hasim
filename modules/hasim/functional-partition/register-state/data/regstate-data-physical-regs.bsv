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
`include "asim/provides/debug_scan_service.bsh"

// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"

// ISA includes

`include "asim/provides/hasim_isa.bsh"
`include "asim/provides/hasim_isa_datapath.bsh"
 

//
// Tokens in the in interfaces below are passed in solely for debugging.
//


//
// REGSTATE_PHYSICAL_REGS_INVAL_REGS --
//   Interface that allows invalidation of one register per cycle.
//
interface REGSTATE_PHYSICAL_REGS_INVAL_REGS;
    //
    // Invalidate uses a request/response interface so the caller can know when
    // the physical register has been marked invalid.
    //
    method Action invalReq(TOKEN tok,
                           Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) regs);
    method Action invalRsp();
endinterface

//
// REGSTATE_PHYSICAL_REGS_WRITE_REG --
//   Interface that allows writing to one register per cycle.
//
interface REGSTATE_PHYSICAL_REGS_WRITE_REG;
    method Action write(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
endinterface

//
// REGSTATE_PHYSICAL_REGS_RW_REG --
//   Interface that allows reading and writing to one register per cycle.
//
interface REGSTATE_PHYSICAL_REGS_RW_REG;
    // Single register read interface
    method Action readReq(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r);
    method ActionValue#(ISA_VALUE) readRsp();

    // Write a single register
    method Action write(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
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
    method Action readRegVecReq(TOKEN tok, ISA_INST_SRCS rVec);

    // Single register read interface (used by emulation)
    method Action readReq(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r);
    method ActionValue#(ISA_VALUE) readRsp();

    // Write a single register
    method Action write(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
endinterface

interface REGSTATE_PHYSICAL_REGS;
    interface REGSTATE_PHYSICAL_REGS_INVAL_REGS getDependencies;
    interface REGSTATE_PHYSICAL_REGS_RW_REGS    getResults;
    interface REGSTATE_PHYSICAL_REGS_WRITE_REG  doLoads;
    interface REGSTATE_PHYSICAL_REGS_WRITE_REG  doStores;
    interface REGSTATE_PHYSICAL_REGS_RW_REG     commitResults;
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
    REGSTATE_PRF_READ_GETRESULTS_VEC,
    REGSTATE_PRF_READ_COMMITRESULTS
}
REGSTATE_PRF_READ_STATE
    deriving (Eq, Bits);


//
// Physical register file container.  The number of read ports is configurable,
// trading area for simulator performance.
//
module [HASIM_MODULE] mkFUNCP_Regstate_Physical_Regs
    // interface:
    (REGSTATE_PHYSICAL_REGS);

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

    // PRF read port used by the non-vector read cases
    let prf_side_port = valueOf(ISA_MAX_SRCS);

    // The physical register file
    NumTypeParam#(4) n_banks = ?;
    BRAM_MULTI_READ#(TAdd#(ISA_MAX_SRCS, 1), FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE)
        prf <- mkMultiReadBankedMemory(n_banks,
                                       MEM_BANK_SELECTOR_BITS_LOW,
                                       mkBRAMBufferedPseudoMultiRead(False));
    
    // Valid bits for PRF
    LUTRAM#(FUNCP_PHYSICAL_REG_INDEX, Bool) prfValids <- mkLUTRAMU();

    // Control access to PRF storage
    Reg#(REGSTATE_PRF_READ_STATE) readState <- mkReg(REGSTATE_PRF_READ_READY);

    // State for reading all inputs registers for an instruction
    FIFO#(ISA_INST_SRCS) vecRegReqQ <- mkFIFO();

    // Vector of work remaining from an invalidate vector
    Reg#(Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) invalVec <- mkReg(replicate(tagged Invalid));

    // Individual incoming queues for each pipeline
    FIFOF#(Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) rqGetDepInval <- mkFIFOF();
    FIFOF#(Bool) rqGetDepInvalDone <- mkSizedFIFOF(8);

    FIFOF#(FUNCP_PHYSICAL_REG_INDEX)                     rqGetResRead    <- mkFIFOF();
    FIFOF#(ISA_INST_SRCS)                                rqGetResReadVec <- mkFIFOF();
    FIFOF#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE)) rqGetResWrite   <- mkFIFOF();

    FIFOF#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE)) rqDoLoadsWrite  <- mkFIFOF();
    FIFOF#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE)) rqDoStoresWrite <- mkFIFOF();

    FIFOF#(FUNCP_PHYSICAL_REG_INDEX)                     rqCommitResRead  <- mkFIFOF();
    FIFOF#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE)) rqCommitResWrite <- mkFIFOF();


    // ====================================================================
    //
    //   Debug scan
    //
    // ====================================================================

    RWire#(Tuple2#(ISA_INST_SRCS,
                   Vector#(ISA_MAX_SRCS, Bool))) readReqVecDbgData <- mkRWire();

    Vector#(ISA_MAX_SRCS, Reg#(Bool)) rvecValidsDbg <- replicateM(mkRegU());
    Reg#(Bit#(TLog#(ISA_MAX_SRCS))) rvecValidsDbgIdx <- mkReg(0);

    rule readReqVecDbg (True);
        let req = rqGetResReadVec.first();

        // Not enough read ports to read valid state of all registers.
        // Cycle through them.  Since we care about deadlocks the state is
        // stable.
        let r = req[rvecValidsDbgIdx];
        rvecValidsDbg[rvecValidsDbgIdx] <= isValid(r) && prfValids.sub(validValue(r));

        if (rvecValidsDbgIdx == fromInteger(valueOf(TSub#(ISA_MAX_SRCS, 1))))
            rvecValidsDbgIdx <= 0;
        else
            rvecValidsDbgIdx <= rvecValidsDbgIdx + 1;

        Vector#(ISA_MAX_SRCS, Bool) tv = newVector();
        for (Integer i = 0; i <= valueOf(TSub#(ISA_MAX_SRCS, 1)); i = i + 1)
        begin
            tv[i] = rvecValidsDbg[i];
        end

        readReqVecDbgData.wset(tuple2(req, tv));
    endrule

    DEBUG_SCAN_FIELD_LIST dbg_list = List::nil;
    dbg_list <- addDebugScanField(dbg_list, "Read state", readState);
    dbg_list <- addDebugScanField(dbg_list, "rqGetResReadVecNotEmpty", rqGetResReadVec.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "rqGetResReadVecNotFull", rqGetResReadVec.notFull);
    dbg_list <- addDebugScanField(dbg_list, "rqGetResReadNotEmpty", rqGetResRead.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "rqGetResReadNotFull", rqGetResRead.notFull);
    dbg_list <- addDebugScanField(dbg_list, "rqCommitResReadNotEmpty", rqCommitResRead.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "rqCommitResReadNotFull", rqCommitResRead.notFull);

    function Bool dbgPrfIsReady(Integer idx);
        let dbg_valid = isValid(readReqVecDbgData.wget());
        match {.rvec, .rvec_valids} = validValue(readReqVecDbgData.wget());

        return rvec_valids[idx] && dbg_valid;
    endfunction

    function Maybe#(FUNCP_PHYSICAL_REG_INDEX) dbgPrf(Integer idx);
        let dbg_valid = isValid(readReqVecDbgData.wget());
        match {.rvec, .rvec_valids} = validValue(readReqVecDbgData.wget());

        return (dbg_valid ? rvec[idx] : tagged Invalid);
    endfunction

    dbg_list <- addDebugScanMaybeField(dbg_list, "rVecSrc0_PR", dbgPrf(0));
    dbg_list <- addDebugScanField(dbg_list, "rVecSrc0_Ready", dbgPrfIsReady(0));
    dbg_list <- addDebugScanMaybeField(dbg_list, "rVecSrc1_PR", dbgPrf(1));
    dbg_list <- addDebugScanField(dbg_list, "rVecSrc1_Ready", dbgPrfIsReady(1));
    if (valueOf(ISA_MAX_SRCS) > 2)
    begin
        dbg_list <- addDebugScanMaybeField(dbg_list, "rVecSrc2_PR", dbgPrf(2));
        dbg_list <- addDebugScanField(dbg_list, "rVecSrc2_Ready", dbgPrfIsReady(2));
    end

    dbg_list <- addDebugScanField(dbg_list, "linkToDatapathSrcValsNotFull", linkToDatapathSrcVals.notFull);

    let dbgNode <- mkDebugScanNode("FUNCP REGSTATE Physical Regs", dbg_list);


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
    // regReadyOrNoReq --
    //     Return true if a source register slot is either not a request or is a request for
    //     a data-ready register.
    //
    function Bool regReadyOrNoReq(Maybe#(FUNCP_PHYSICAL_REG_INDEX) src);        
        if (src matches tagged Valid .r)
            return prfValids.sub(r);
        else
            return True;
    endfunction


    //
    // sendInvalResponseWhenDone --
    //   Function to check pending vector of invalidations for a single request.
    //   Send a message when all invalidations are complete.
    //
    function Action sendInvalResponseWhenDone(Vector#(ISA_MAX_DSTS,
                                                      Maybe#(FUNCP_PHYSICAL_REG_INDEX)) rVec);
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
        // Request for updating mapping from getDeps.  Only start the multi-cycle
        // operation if there is room to hold the response.
        else if (rqGetDepInval.notEmpty && rqGetDepInvalDone.notFull)
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
        else if (rqCommitResWrite.notEmpty() ||
                 rqDoStoresWrite.notEmpty() ||
                 rqDoLoadsWrite.notEmpty() ||
                 rqGetResWrite.notEmpty())
        begin
            //
            // No invalidates pending.  Handle register writes...
            //
            Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE) rq;

            if (rqCommitResWrite.notEmpty())
            begin
                rq = rqCommitResWrite.first();
                rqCommitResWrite.deq();
            end
            else if (rqDoStoresWrite.notEmpty())
            begin
                rq = rqDoStoresWrite.first();
                rqDoStoresWrite.deq();
            end
            else if (rqDoLoadsWrite.notEmpty())
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
    // startReadPrf --
    //     Read a single register.
    //
    rule startReadPrf (initialized && readState == REGSTATE_PRF_READ_READY &&
                       (rqCommitResRead.notEmpty() || rqGetResRead.notEmpty()));
        //
        // Single register read.  When the register is valid do the read.
        // If not valid do nothing and the same test will be done next
        // FPGA cycle.
        //
        // We use the simplest implementation:  change the state and block
        // until the result is read.  This interface is used for forwarding the
        // register state over RRR for emulation and handling faulting
        // instructions.  Speed is not important.
        //

        if (rqCommitResRead.notEmpty())
        begin
            //
            // For the commit results path we assume that the register being
            // read is valid, since commits are in order.  This saves a read
            // port on prfValids (and we are already using the maximum for
            // a LUTRAM.)
            //
            let r = rqCommitResRead.first();
            rqCommitResRead.deq();

            readState <= REGSTATE_PRF_READ_COMMITRESULTS;
            prf.readPorts[prf_side_port].readReq(r);
            debugLog.record($format("PRF: Start Read (CR): Single reg %0d ready", r));
        end

        else if (rqGetResRead.notEmpty())
        begin
            // GetResults wants a read and the register is ready.
            let r = rqGetResRead.first();
            if (prfValids.sub(r))
            begin
                rqGetResRead.deq();

                readState <= REGSTATE_PRF_READ_GETRESULTS_SINGLE;
                prf.readPorts[prf_side_port].readReq(r);
                debugLog.record($format("PRF: Start Read (GR): Single reg %0d ready", r));
            end
        end
    endrule

    //
    // startReadPrfVec --
    //     Read a vector of registers.
    //
    (* descending_urgency = "startReadPrf, startReadPrfVec" *)
    rule startReadPrfVec (initialized &&
                          readState == REGSTATE_PRF_READ_READY &&
                          all(regReadyOrNoReq, rqGetResReadVec.first()));
        let rVec = rqGetResReadVec.first();
        rqGetResReadVec.deq();
        
        //
        // All register inputs are ready.  Request their values.
        //
        for (Integer s = 0; s < valueOf(ISA_MAX_SRCS); s = s + 1)
        begin
            if (rVec[s] matches tagged Valid .r)
            begin
                prf.readPorts[s].readReq(r);
                debugLog.record($format("PRF: Vec Read reg %0d, slot %0d", r, s));
            end
        end

        vecRegReqQ.enq(rVec);
    endrule


    //
    // forwardSrcvals --
    //     Collect all input register values and forward them to the ISA data path.
    //
    rule forwardSrcvals (True);
        let rVec = vecRegReqQ.first();
        vecRegReqQ.deq();
        
        //
        // All register inputs are ready.  Reuqest their values.
        //
        ISA_SOURCE_VALUES regVals = newVector();
        for (Integer s = 0; s < valueOf(ISA_MAX_SRCS); s = s + 1)
        begin
            if (rVec[s] matches tagged Valid .r)
            begin
                regVals[s] <- prf.readPorts[s].readRsp();
                debugLog.record($format("PRF: FWD src reg %0d, slot %0d, val 0x%h", r, s, regVals[s]));
            end
        end

        linkToDatapathSrcVals.send(initISADatapathSrcVals(regVals));
    endrule


    // ====================================================================
    //
    //   Interfaces
    //
    // ====================================================================

    interface REGSTATE_PHYSICAL_REGS_INVAL_REGS getDependencies;

        method Action invalReq(TOKEN tok,
                               Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) regs);
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
        method Action readReq(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r);
            rqGetResRead.enq(r);
            debugLog.record($format("PRF: Single Read Req: %0d", r));
        endmethod

        method ActionValue#(ISA_VALUE) readRsp() if (readState == REGSTATE_PRF_READ_GETRESULTS_SINGLE);
            let v <- prf.readPorts[prf_side_port].readRsp();
            readState <= REGSTATE_PRF_READ_READY;

            debugLog.record($format("PRF: Single Read: 0x%x", v));

            return v;
        endmethod


        //
        // Read all registers for an instruction
        //
        method Action readRegVecReq(TOKEN tok, ISA_INST_SRCS rVec);
            rqGetResReadVec.enq(rVec);
            debugLog.record($format("PRF: Vector Read Req"));
        endmethod


        //
        // Write to target register
        //
        method Action write(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
            rqGetResWrite.enq(tuple2(r, v));
        endmethod

    endinterface

    interface REGSTATE_PHYSICAL_REGS_WRITE_REG doLoads;

        method Action write(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
            rqDoLoadsWrite.enq(tuple2(r, v));
        endmethod

    endinterface

    interface REGSTATE_PHYSICAL_REGS_WRITE_REG doStores;

        method Action write(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
            rqDoStoresWrite.enq(tuple2(r, v));
        endmethod

    endinterface

    interface REGSTATE_PHYSICAL_REGS_RW_REG commitResults;

        //
        // Single register read interface
        //
        method Action readReq(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r);
            rqCommitResRead.enq(r);
            debugLog.record($format("PRF: Commit Results Read Req: %0d", r));
        endmethod

        method ActionValue#(ISA_VALUE) readRsp() if (readState == REGSTATE_PRF_READ_COMMITRESULTS);
            let v <- prf.readPorts[prf_side_port].readRsp();
            readState <= REGSTATE_PRF_READ_READY;

            debugLog.record($format("PRF: Commit Results Read: 0x%x", v));

            return v;
        endmethod


        method Action write(TOKEN tok, FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
            rqCommitResWrite.enq(tuple2(r, v));
        endmethod

    endinterface

endmodule

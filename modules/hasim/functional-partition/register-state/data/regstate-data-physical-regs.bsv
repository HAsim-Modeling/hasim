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
 
// ISA includes

`include "asim/provides/hasim_isa.bsh"
 

//
// REGSTATE_PHYSICAL_REGS_INVAL_REG --
//   Interface that allows invalidation of one register per cycle.
//
interface REGSTATE_PHYSICAL_REGS_INVAL_REG;
    method Action inval(FUNCP_PHYSICAL_REG_INDEX r);
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
    // Vector read interface to read all inputs for an instruction
    method Action readRegVecReq(ISA_INST_SRCS rVec);
    method ActionValue#(ISA_SOURCE_VALUES) readRegVecRsp();

    // Single register read interface (used by emulation)
    method Action readReq(FUNCP_PHYSICAL_REG_INDEX r);
    method ActionValue#(ISA_VALUE) readRsp();

    // Write a single register
    method Action write(FUNCP_PHYSICAL_REG_INDEX r, ISA_VALUE v);
endinterface

interface REGSTATE_PHYSICAL_REGS;
    interface REGSTATE_PHYSICAL_REGS_INVAL_REG getDependencies;
    interface REGSTATE_PHYSICAL_REGS_RW_REGS   getResults;
    interface REGSTATE_PHYSICAL_REGS_WRITE_REG doLoads;
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
    //   Local state
    //
    // ====================================================================

    // The physical register file
    BRAM_MULTI_READ#(2, FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE) prf <- mkBRAMMultiRead();
    
    // Valid bits for PRF
    LUTRAM#(FUNCP_PHYSICAL_REG_INDEX, Bool) prfValids <- mkLUTRAMU();

    // Control access to PRF storage
    Reg#(REGSTATE_PRF_READ_STATE) readState <- mkReg(REGSTATE_PRF_READ_READY);

    //
    // State for reading all inputs registers for an instruction
    //
    Reg#(ISA_INST_SRCS) vecRegReq <- mkRegU();
    // Counters for outstanding register read requests and responses
    Vector#(2, Reg#(ISA_SRC_INDEX)) vecNumReqs  = newVector();
    Vector#(2, Reg#(ISA_SRC_INDEX)) vecNumResps = newVector();
    // Index used for triggering read requests.  vecReqIdx needs to be able to
    // count one beyond ISA_MAX_SRCS due to the loop code.
    Vector#(2, Reg#(Bit#(TLog#(TAdd#(1, ISA_MAX_SRCS))))) vecReqIdx = newVector();
    Vector#(2, Reg#(ISA_SRC_INDEX)) vecRespIdx  = newVector();
    // Queue for associating register reads with argument positions in the instruction
    Vector#(2, FIFO#(ISA_SRC_INDEX)) vecQ = newVector();
    
    for (Integer pipe = 0; pipe < 2; pipe = pipe + 1)
    begin
        vecNumReqs[pipe]  <- mkRegU();
        vecNumResps[pipe] <- mkRegU();
        vecReqIdx[pipe]   <- mkRegU();
        vecRespIdx[pipe]  <- mkRegU();
        vecQ[pipe] <- mkFIFO();
    end

    // Holder for vector read response
    Vector#(ISA_MAX_SRCS, Reg#(ISA_VALUE)) srcVals = newVector();
    for (Integer x = 0; x < valueOf(ISA_MAX_SRCS); x = x + 1)
    begin
        srcVals[x] <- mkRegU();
    end

    // Individual incoming queues for each pipeline
    FIFOF#(FUNCP_PHYSICAL_REG_INDEX)                     rqGetDepInval   <- mkFIFOF();

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

    // The highest register in the ISA (the last one which is initially valid).
    ISA_REG_INDEX         highestReg = maxBound;
    FUNCP_PHYSICAL_REG_INDEX maxInit = zeroExtend(pack(highestReg));

    rule initialize_prf (! initialized);
        // For safety we start all physical registers valid and at zero.
        // In the future this might change.
        prfValids.upd(initPrfIdx, True);
        prf.write(initPrfIdx, 0);
        
        // We're done if we've initialized the last register.
        if (initPrfIdx >= maxInit)
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
    // update_prf --
    //     Handle all possible writes to the register file.
    //
    rule update_prf (initialized);
        //
        // Only one write allowed per cycle.  Give INVAL highest priority,
        // followed by priority in reverse pipeline order.
        //

        if (rqGetDepInval.notEmpty())
        begin
            let r = rqGetDepInval.first();
            rqGetDepInval.deq();

            prfValids.upd(r, False);
            debugLog.record($format("PRF: Update: Invalidate reg %0d", r));
        end
        else if (rqDoLoadsWrite.notEmpty() || rqGetResWrite.notEmpty())
        begin
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
    // start_read_prf --
    //     All register reads begin here.  There are two possible flavors:
    //     read a single register and read all registers for an instruction.
    //
    rule start_read_prf (initialized && readState == REGSTATE_PRF_READ_READY);
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
            
            for (Integer port = 0; port < 2; port = port + 1)
            begin
                ISA_SRC_INDEX nReqs = 0;
                let startIdx = port;

                // Count number of requests to be serviced by this port
                for (Integer x = startIdx; x < valueOf(ISA_MAX_SRCS); x = x + 2)
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
                    startIdx = startIdx + 2;
                    vecNumReqs[port] <= nReqs - 1;
                end
                else
                begin
                    vecNumReqs[port] <= nReqs;
                end

                vecNumResps[port] <= nReqs;
                vecReqIdx[port] <= fromInteger(startIdx);
            end

            vecRegReq <= rVec;
            readState <= REGSTATE_PRF_READ_GETRESULTS_VEC;
        end
    endrule

    //
    // Generate separate rules for each register file read port.
    //
    for (Integer port = 0; port < 2; port = port + 1)
    begin

        //
        // read_req_vector --
        //     Wait for each input register to become ready and then read it.
        //
        rule read_req_vector (readState == REGSTATE_PRF_READ_GETRESULTS_VEC && vecNumReqs[port] != 0);
            let reqIdx = vecReqIdx[port];

            if (vecRegReq[reqIdx] matches tagged Valid .r)
            begin
                if (prfValids.sub(r))
                begin
                    debugLog.record($format("PRF: Vec Read Port %0d: reg %0d, slot %0d ready", port, r, reqIdx));

                    prf.readPorts[port].readReq(r);
                    vecQ[port].enq(truncate(reqIdx));

                    vecReqIdx[port] <= vecReqIdx[port] + 2;
                    vecNumReqs[port] <= vecNumReqs[port] - 1;
                end
            end
            else
            begin
                debugLog.record($format("PRF: Vec Read Port %0d: slot %0d no request", port, reqIdx));
                vecReqIdx[port] <= vecReqIdx[port] + 2;
            end
        endrule

        //
        // read_resp_vector --
        //     Receive responses to register file read requests.  Store the result
        //     in the srcVals vector.
        //
        rule read_resp_vector (readState == REGSTATE_PRF_READ_GETRESULTS_VEC);
            let rIdx = vecQ[port].first();
            vecQ[port].deq();

            let v <- prf.readPorts[port].readRsp();
            srcVals[rIdx] <= v;

            vecNumResps[port] <= vecNumResps[port] - 1;

            debugLog.record($format("PRF: Vec Read Port %0d: slot %0d <- 0x%x", port, rIdx, v));
        endrule

    end

    // The expression to decide when all registers have been read:
    let vecReadReady = (readState == REGSTATE_PRF_READ_GETRESULTS_VEC) &&
                       (vecNumResps[0] == 0) &&
                       (vecNumResps[1] == 0);

    // ====================================================================
    //
    //   Interfaces
    //
    // ====================================================================

    interface REGSTATE_PHYSICAL_REGS_INVAL_REG getDependencies;

        method Action inval(FUNCP_PHYSICAL_REG_INDEX r);
            rqGetDepInval.enq(r);
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

        method ActionValue#(ISA_VALUE) readRsp() if (readState == REGSTATE_PRF_READ_GETRESULTS_SINGLE);
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

        method ActionValue#(ISA_SOURCE_VALUES) readRegVecRsp() if (vecReadReady);
            debugLog.record($format("PRF: Vector Read Resp"));

            ISA_SOURCE_VALUES regVals = ?;
            for (Integer x = 0; x < valueOf(ISA_MAX_SRCS); x = x + 1)
            begin
                regVals[x] = srcVals[x];
            end

            readState <= REGSTATE_PRF_READ_READY;

            return regVals;
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

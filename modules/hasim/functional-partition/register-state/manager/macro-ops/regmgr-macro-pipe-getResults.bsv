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

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_modellib.bsh"
 
// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"
  
// Dictionary includes
`include "asim/dict/STATS_REGMGR_GETRESULTS.bsh"

// RRR includes
`include "asim/provides/rrr.bsh"
`include "asim/rrr/service_ids.bsh"
`include "asim/provides/isa_emulator.bsh"
`include "asim/rrr/remote_client_stub_ISA_EMULATOR.bsh"
`include "asim/rrr/remote_server_stub_ISA_EMULATOR.bsh"


// ========================================================================
//
//   Internal data structures
//
// ========================================================================

// STATE_RES4
typedef union tagged
{
    void        RES4_NORMAL;
    struct
    {
        Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE))) remainingValues;
        FUNCP_ISA_EXECUTION_RESULT result;
        Bit#(4)    current; 
    }
    RES4_ADDITIONAL_WB;
}
STATE_RES4
    deriving (Eq, Bits);



module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_GetResults#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_PHYSICAL_REGS_RW_REGS prf,
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokAddr,
    FUNCP_SNAPSHOT snapshots,
    BRAM#(TOKEN_INDEX, ISA_INST_SRCS) tokWriters,
    BRAM_MULTI_READ#(3, TOKEN_INDEX, ISA_INST_DSTS) tokDsts,
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_INSTRUCTION) tokInst,
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokMemAddr,
    BRAM#(TOKEN_INDEX, ISA_VALUE) tokStoreValue)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_getResults.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_GET_RESULTS, 
                       FUNCP_RSP_GET_RESULTS) linkGetResults <- mkConnection_Server("funcp_getResults");

    Connection_Client#(FUNCP_ISA_DATAPATH_REQ,
                       FUNCP_ISA_DATAPATH_RSP) linkToDatapath <- mkConnection_Client("isa_datapath");

    // Emulation RRR Stubs
    ClientStub_ISA_EMULATOR client_stub <- mkClientStub_ISA_EMULATOR();
    ServerStub_ISA_EMULATOR server_stub <- mkServerStub_ISA_EMULATOR();


    // ====================================================================
    //
    //   Local names for global data 
    //
    // ====================================================================

    let state = glob.state;
    let assertion = glob.assertion;
    let tokScoreboard = glob.tokScoreboard;


    // ====================================================================
    //
    //   Local state
    //
    // ====================================================================

    // Intermediate state between pipeline stages
    FIFO#(TOKEN) res1Q <- mkFIFO();
    FIFO#(TOKEN) res2Q <- mkFIFO();
    FIFO#(Tuple2#(TOKEN, ISA_ADDRESS)) res3Q   <- mkFIFO();
    FIFO#(Tuple2#(ISA_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX)) syncQ <- mkFIFO();

    Reg#(STATE_RES4) stateRes4 <- mkReg(RES4_NORMAL);

    // Which snapshot should we refer to for emulation?
    Reg#(FUNCP_SNAPSHOT_INDEX) emulatingSnap <- mkRegU();
    // Which token's instruction are we emulating?
    Reg#(TOKEN) emulatingToken <- mkRegU();
    // PC of emulating token
    Reg#(ISA_ADDRESS) emulatingPC <- mkRegU();
    // Which register are we currently synchronizing?
    Reg#(ISA_REG_INDEX) synchronizingCurReg <- mkReg(minBound);


    Stat stat_isa_emul <- mkStatCounter(`STATS_REGMGR_GETRESULTS_EMULATED_INSTRS);

    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================

    // ******* getResults ******* //
    
    // 4-stage macro operation. Stages 2 and 4 can stall.

    // When:   When the timing model requests an execution.
    // Effect: Perform register reads, then send to datapath for execution.
    // Soft Inputs:  Token
    // Soft Returns: Token, Result
    
    // getResults1

    // When:   When the timing model starts a getResults().
    // Effect: Lookup the locations of this token's sources.

    rule getResults1 (state.readyToBegin());

        // Get parameter from the timing model. Begin macro-operation.
        let req = linkGetResults.getReq();
        linkGetResults.deq();
        let tok = req.token;
        debugLog.record($format("TOKEN %0d: GetResults: Begin.", tok.index));

        // Update the scoreboard.
        tokScoreboard.exeStart(tok.index);
        
        if (tokScoreboard.emulateInstruction(tok.index) && tokScoreboard.isAllocated(tok.index))
        begin

            // Record that we're emulating an instruction.
            state.setState(RSM_DrainingForEmulate);

            // Record which token is being emulated.
            emulatingToken <= tok;

            // Lookup the snapshot we should be working with.
            let msnap = snapshots.hasSnapshot(tok.index);

            // If there's no snapshot, something is really wrong.
            assertion.haveSnapshotOfEmulatedInstruction(isValid(msnap));

            // Record which snap we should use.
            emulatingSnap <= validValue(msnap);

            // Pre-request the first snapshot.
            snapshots.requestSnapshot(validValue(msnap));

             // Log it.
            debugLog.record($format("TOKEN %0d: GetResults1: Beginning Instruction Emulation.", tok.index));

        end
        else
        begin
        
            // Look up the writers.
            tokWriters.readReq(tok.index);

            // Pass it along to the next stage.
            res1Q.enq(tok);
        
        end

    endrule

    // getResults2
    // When:   After getResults1.
    // Effect: Use the writers to look up values from the PRF. 
    //         Also retreive the instruction itself and the PC.
    //         If the writers are not all ready then a stall can occur.

    rule getResults2 (state.readyToContinue());

        // Get input from getResults1.
        let tok = res1Q.first();
        res1Q.deq();

        // Response from previous stage.
        let ws <- tokWriters.readRsp();
        
        // We let junk proceed
        if (!tokScoreboard.isAllocated(tok.index))
        begin
            // No values are needed for junk
            debugLog.record($format("TOKEN %0d: GetResults2: Letting Junk Proceed!", tok.index));

            prf.readRegVecReq(Vector::replicate(tagged Invalid));
        end
        else
        begin
            debugLog.record($format("TOKEN %0d: GetResults2: Requesting srcs", tok.index));

            prf.readRegVecReq(ws);
        end

        tokAddr.readReq(tok.index);
        tokInst.readPorts[1].readReq(tok.index);
        res2Q.enq(tok);

    endrule

    
    // getResults3
    // When:    After getResults2 or alternatively getResults2StallEnd
    // Effect:  Send all the data to the datapath.

    rule getResults3 (state.readyToContinue());

        // Get input from the previous stage.
        let tok = res2Q.first();
        res2Q.deq();

        // Get all the data the previous stage kicked off.
        let addr <- tokAddr.readRsp();
        let inst <- tokInst.readPorts[1].readRsp();
        let values <- prf.readRegVecRsp();

        // Log it.
        debugLog.record($format("TOKEN %0d: GetResults3: Sending to Datapath.", tok.index));

        // Send it to the datapath.
        linkToDatapath.makeReq(initISADatapathReq(inst, addr, values));

        // Look up the destinations for the writeback.
        tokDsts.readPorts[0].readReq(tok.index);

        // Pass it to the next stage.
        res3Q.enq(tuple2(tok, addr));

    endrule
    
    // getResults4
    // When:   After getResults3 and the datapath returns the result.
    // Effect: If one or fewer destinations, write back the result and 
    //         return the result to the timing partition.
    //         If more results then the stall and continue to write them back.

    rule getResults4 (state.readyToContinue() &&& stateRes4 matches tagged RES4_NORMAL);

        // Get the token from the previous stage.
        match {.tok, .addr} = res3Q.first();

        // Get the response from the datapath.
        let rsp = linkToDatapath.getResp();
        let wbvals = rsp.writebacks;
        linkToDatapath.deq();

        // Tag illegal instruction.  An error will be raised on attempts to commit.
        if (rsp.except != FUNCP_ISA_EXCEPT_NONE)
        begin
            debugLog.record($format("TOKEN %0d: GetResults: Illegal instruction", tok.index));
            tokScoreboard.setFault(tok.index, FAULT_ILLEGAL_INSTR);
        end

        // Update the memaddress (only useful for loads/stores)
        tokMemAddr.write(tok.index, rsp.memAddress);

        // Get the destination response
        let dsts <- tokDsts.readPorts[0].readRsp();
        
        // The first dest should always be valid (it may not be architecturally visible)
        let dst = validValue(dsts[0]);

        // Perform the first writeback, if any.
        case (wbvals[0]) matches
            tagged Invalid:  noAction; // Not writing back, either a Load, or no dests.
            tagged Valid .v: 
            begin // Do the first writeback.
                
                if (rsp.isStore)
                begin
                
                    // Stores write dest0 insto the token table instead of the PRF.
                    tokStoreValue.write(tok.index, v);
                
                end
                else  // A normal PRF writeback
                begin
            
                    prf.write(dst, v);
                    debugLog.record($format("TOKEN %0d: GetResults4: Writing (PR%0d <= 0x%x)", tok.index, dst, v));
                
                end

            end
        endcase
        
        // Is there anything more to writeback?

        Bool writing_back_more = False;

        for (Integer x = 1; x < valueof(ISA_MAX_DSTS); x = x + 1)
        begin // There is more to do if both the dest and val are valid.
          writing_back_more = writing_back_more || (isValid(dsts[x]) && isValid(wbvals[x]));
        end

        if (!writing_back_more)
        begin
        
            // We're done, so don't stall.
            res3Q.deq();

            // Update scoreboard.
            tokScoreboard.exeFinish(tok.index);

            // Return timing model. End of macro-operation (path 1).
            linkGetResults.makeResp(initFuncpRspGetResults(tok, addr, rsp.timepResult));
            debugLog.record($format("TOKEN %0d: GetResults: End (path 1).", tok.index));

        end
        else // We've got to write back more.
        begin
            
            // Log it.
            debugLog.record($format("TOKEN %0d: GetResults4: Writing back additional values.", tok.index));

            // Marshall up the values for writeback.

            Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE))) remaining_values = newVector();
            for (Integer x = 1; x < valueof(ISA_MAX_DSTS) ; x = x + 1)
            begin
                remaining_values[x-1] = case (dsts[x]) matches
                                         tagged Invalid:  tagged Invalid;
                                         tagged Valid .d:
                                           case (wbvals[x]) matches 
                                              tagged Invalid:  tagged Invalid; // Not writing it now - presumably it's a load.
                                              tagged Valid .v: tagged Valid tuple2(d, v);
                                           endcase
                                     endcase;
            end

            // Stall the pipeline.
            stateRes4 <= tagged RES4_ADDITIONAL_WB
                                {
                                    remainingValues: remaining_values,
                                    result: rsp.timepResult,
                                    current: 0 
                                };
        end
      
    endrule

    // getResults4AdditionalWriteback
    
    // When:   After a result from getResults4 writes back additonal destinations.
    // Effect: Finish the writebacks of the physical register file.
    
    if(valueOf(ISA_MAX_DSTS) > 1)
    begin

        rule getResults4AdditionalWriteback (state.readyToContinue() &&& stateRes4 matches tagged RES4_ADDITIONAL_WB .wb_info);
        
            // Get the info from the previous stage.
            match {.tok, .addr} = res3Q.first();
            
            // Do the writeback.
            case (wb_info.remainingValues[wb_info.current]) matches
                tagged Invalid:
                begin
                    // Hopefully this doesn't happen too much.
                    debugLog.record($format("TOKEN %0d: GetResults4: Skipping Dest %0d", tok.index, wb_info.current + 1));

                end
                tagged Valid {.dst, .val}:
                begin

                    // An actual writeback.
                    prf.write(dst, val);
                    debugLog.record($format("TOKEN %0d: GetResults4: Writing Dest %0d (PR%0d <= 0x%x)", tok.index, wb_info.current + 1, dst, val));

                end
            endcase
      
            // We're done when we've checked every additional dest.
            if (wb_info.current == fromInteger(valueOf(ISA_MAX_DSTS) - 2))
            begin
      
                // We're done. Unstall the pipeline.
                res3Q.deq();
                stateRes4 <= tagged RES4_NORMAL;

                // Update scoreboard.
                tokScoreboard.exeFinish(tok.index);
          
                // Return to timing model. End of macro-operation (path 2).
                linkGetResults.makeResp(initFuncpRspGetResults(tok, addr, wb_info.result));
                debugLog.record($format("TOKEN %0d: GetResults: End (path 2).", tok.index));

            end
            else
            begin
            
                // We're not done. Update the state for next time.
                stateRes4 <= tagged RES4_ADDITIONAL_WB
                                    {
                                        remainingValues: wb_info.remainingValues,
                                        result: wb_info.result,
                                        current: wb_info.current + 1
                                    };
            
            end
    
        endrule
    end
    else
    begin
        //
        // Dummy rule to keep execution_order pragma below happy
        //
        rule getResults4AdditionalWriteback (True);
        endrule
    end

    
    // ******* emulateInstruction ******* //

    // 4-stage macro-operation that interacts with software via RRR.
    // This is completely unpipelined and always stalls the whole system.
    
    // When:   After the getResults operation detects an instruction which must be emulated.
    // Effect: First this sends every archtectural register value to software.
    //         Then it makes a call to emulate the instruction.
    //         Then it accepts any number of register updates from software.
    //         Finally it gets an ACK and returns the result of getResults to the timing model.

    // emulateInstruction1
    
    // When:   After getResults operation puts us in the emulation state.
    // Effect: Stall until all younger operations have completed. Then we can proceed.
    
    rule emulateInstruction1 (state.getState() == RSM_DrainingForEmulate && tokScoreboard.canEmulate());

        // Did the timing model do drain before correctly?
        assertion.expectedOldestTok(emulatingToken.index == tokScoreboard.oldest());
        if (emulatingToken.index != tokScoreboard.oldest())
            debugLog.record($format("TOKEN %0d: emulateInstruction1:  Token is not oldest! (Oldest: %0d)", emulatingToken.index, tokScoreboard.oldest()));

        // Reset the counter for syncing registers.
        synchronizingCurReg <= minBound;

        // Start syncing registers.
        state.setState(RSM_SyncingRegisters);
               
    endrule

    // emulateInstruction2_Req
    
    // When:   After the getResults operation puts us into the emulation state, this
    //         rule happens once for each architectural register.
    // Effect: Look up the current physical register in the maptable and request it from the regfile.
    
    
    rule emulateInstruction2_Req (state.getState() == RSM_SyncingRegisters);
    
        // Some ISA's have a sparse packing of register names.  They should define Arith so we 
        // don't transmit them spuriously.

        // Get the maptable at the time of the emulated instruction.
        let emulation_map <- snapshots.returnSnapshot();

        // Lookup which register to send next.
        FUNCP_PHYSICAL_REG_INDEX current_pr = emulation_map[pack(synchronizingCurReg)];

        // Make the request to the regfile.
        prf.readReq(current_pr);

        // Pre-load the snapshot for next time.
        snapshots.requestSnapshot(emulatingSnap);

        // Pass it on to the next stage.
        syncQ.enq(tuple2(synchronizingCurReg, current_pr));
        
        // Was this our last request?
        if (synchronizingCurReg == maxBound)
        begin
        
            // Request the inst and current PC
            tokInst.readPorts[0].readReq(emulatingToken.index);
            tokAddr.readReq(emulatingToken.index);

            // End the loop.
            state.setState(RSM_RequestingEmulation);
        
        end
        
        // Increment, and possibly repeat.
        synchronizingCurReg <= synchronizingCurReg + 1;
        
    
    endrule

    // emulateInstruction2_Rsp
    
    // When:   After each occurance of emulateInstruction1_Req
    // Effect: Get the register value response and send it on to software via RRR.

    rule emulateInstruction2_Rsp (True);
    
        // Get the register from the previous stage.
        match {.arch_reg, .phys_reg} = syncQ.first();
        syncQ.deq();
        
        // Get the register value from the regfile.
        let reg_val <- prf.readRsp();
        
        // Send the regsiter on to software via RRR
        client_stub.makeRequest_sync(tuple2(arch_reg, reg_val));

        //Log it.
        debugLog.record($format("TOKEN %0d: EmulateInstruction2: Transmitting Register R%0d (PR%0d) = 0x%h.", emulatingToken.index, arch_reg, phys_reg, reg_val));
    
    endrule
    
    // emulateInstruction3
    
    // When:   After emulateInstruction1 has transmitted every architectural register.
    // Effect: Send the instruction emulation request to software via RRR.

    rule emulateInstruction3 (state.getState() == RSM_RequestingEmulation);
        
        // Get the instruction and current pc
        ISA_INSTRUCTION inst <- tokInst.readPorts[0].readRsp();
        ISA_ADDRESS       pc <- tokAddr.readRsp();
        
        emulatingPC <= pc;

        // Send the request on to software via RRR
        client_stub.makeRequest_emulate(tuple2(inst, pc));
        
        //Log it.
        debugLog.record($format("TOKEN %0d: EmulateInstruction3: Requesting Emulation of inst 0x%h from address 0x%h", emulatingToken.index, inst, pc));
        stat_isa_emul.incr();

        //Go to receiving updates.
        state.setState(RSM_UpdatingRegisters);

    endrule

    // emulateInstruction3_UpdateReg
    
    // When:   Whenever the software decides that it should update a register in hardware.
    //         These updates should really only occur when we're emulating an instruction.
    //         If they come during any other time then this is a fatal error.
    // Effect: Update the register to the new value.


    rule emulateInstruction3_UpdateReg (True);
        
        // Get an update request from software.
        match {.r, .v} <- server_stub.acceptRequest_updateRegister();
        
        // Get the maptable at the time of the emulated instruction.
        let emulation_map <- snapshots.returnSnapshot();

        // Assert that we're in the state we expected to be in.
        assertion.regUpdateAtExpectedTime(state.getState() == RSM_UpdatingRegisters);
        
        // Lookup the current physical register in the snapshot maptable.
        FUNCP_PHYSICAL_REG_INDEX pr = emulation_map[pack(r)];
        
        // Update the regfile.
        prf.write(pr, v);

        // Get the snapshot for the next time.
        snapshots.requestSnapshot(emulatingSnap);
        
        // Log it.
        debugLog.record($format("TOKEN %0d: EmulateInstruction3: Writing (PR%0d <= 0x%h)", emulatingToken.index, pr, v));
    
    endrule

    // emulateInstruction4
    
    // When:   After the software has finished all of its register writes it will send an ACK.
    // Effect: This means the emulation is complete. Resume normal operations.
    //         Return a NOP to the timing model.

    rule emulateInstruction4 (True);
        
        // Get the ACK from software that they're complete.
        let newPc <- client_stub.getResponse_emulate();
        
        // Assert that we're in the state we expected to be in.
        assertion.emulationFinishedAtExpectedTime(state.getState() == RSM_UpdatingRegisters);
        
        // Dequeue the final snapshot response.
        let junk <- snapshots.returnSnapshot();

        // We are no longer emulating an instruction.
        // Resume normal operations.
        state.setState(RSM_Running);

        // Update scoreboard.
        tokScoreboard.exeFinish(emulatingToken.index);

        // Hack alert -- until RRR allows us to pass multiple objects cleanly
        // we pass a branch target and flags as a single 64 bit value.  We use
        // the low 2 bits as flags.  This works for Alpha and MIPS but won't
        // work for x86.
        let tgtFlags = newPc[1:0];
        newPc[1:0] = 0;                         // Clear the flags
        let resp = case(tgtFlags)
                       0: tagged RNop;
                       1: tagged RBranchTaken newPc;
                       2: tagged RNop;          // Unused
                       3: tagged RTerminate (newPc[2] == 1); // Bit 2 is 1 for pass
                   endcase;

        //Log it
        debugLog.record($format("TOKEN %0d: EmulateInstruction3: Emulation finished.", emulatingToken.index));
  
        // Send the response to the timing model.
        // End of macro-operation.
        linkGetResults.makeResp(initFuncpRspGetResults(emulatingToken, emulatingPC, resp));

        debugLog.record($format("TOKEN %0d: GetResults: End (path 3).", emulatingToken.index));

    endrule

endmodule

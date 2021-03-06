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

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_services_lib.bsh"
`include "asim/provides/soft_services_deps.bsh"
`include "asim/provides/fpga_components.bsh"
  
// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_GetDependencies#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_REG_MAPPING_GETDEPENDENCIES regMapping,
    REGSTATE_PHYSICAL_REGS_INVAL_REGS prf,
    FUNCP_FREELIST freelist,
    BRAM_MULTI_READ#(n_tokAddr, TOKEN_INDEX, ISA_ADDRESS) tokAddr,
    BRAM_MULTI_READ#(n_tokInst, TOKEN_INDEX, ISA_INSTRUCTION) tokInst,
    BRAM#(TOKEN_INDEX, ISA_INST_SRCS) tokWriters,
    BRAM_MULTI_READ#(n_tokDsts, TOKEN_INDEX, REGMGR_DST_REGS) tokDsts,
    BRAM#(TOKEN_INDEX, Bool) tokIsLoadLocked,
    BRAM#(TOKEN_INDEX, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) tokIsStoreCond,
    STDIO#(Bit#(32)) stdio)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_getDependencies.out");

    let msgMapInst <- getGlobalStringUID("FUNCP GETDEP: alloc TOKEN (%d, %d): VA 0x%08lx%08lx inst 0x%08lx\n");
    let msgRegSrcs <- getGlobalStringUID("FUNCP GETDEP: read PR (%d, %d, %d) TOKEN (%d, %d)\n");
    let msgRegDsts <- getGlobalStringUID("FUNCP GETDEP: write PR (%d, %d, %d) TOKEN (%d, %d)\n");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_GET_DEPENDENCIES, 
                       FUNCP_RSP_GET_DEPENDENCIES) linkGetDeps <-
        mkFUNCPInterfaceServer("funcp_getDependencies");

    Connection_Send#(TOKEN_INDEX) storeBufferAllocate <- mkConnection_Send("storeBufferAllocate");


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

    FIFO#(Tuple2#(TOKEN, ISA_INSTRUCTION)) deps1Q <- mkFIFO();

    FIFO#(Tuple4#(TOKEN,
                  ISA_INSTRUCTION,
                  Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)),
                  Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)))) deps2Q <- mkSizedFIFO(8);

    FIFO#(Tuple3#(TOKEN,
                  Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)),
                  ISA_DST_MAPPING)) deps3Q <- mkSizedFIFO(8);


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    // ******* getDependencies *******
    // 4-stage macro-operation.
    
    // When:   When the timing partiton request the dependencies of an operation.
    // Effect: Allocate a token, allocate all destination registers in maptable. 
    //         Lookup all source registers in maptable.
    // Soft Inputs:  CONTEXT_ID, INSTRUCTION, ADDRESS
    // Soft Returns: TOKEN, ISA_DEPENDENCY_INFO
 
    //
    // getDependencies1 --
    //   Get a token for this instruction. 
    //
    let stdio1 <- mkStdIO_CondPrintf(ioMask_FUNCP_REGMGR, stdio);

    (* conservative_implicit_conditions *)
    rule getDependencies1 (state.readyToBegin(linkGetDeps.getReq().contextId));

        // Read inputs. Begin macro-operation.
        let req = linkGetDeps.getReq();
        linkGetDeps.deq();
        let ctx_id = req.contextId;

        // Get the next token from the scoreboard.
        let idx <- tokScoreboard.allocate(ctx_id);
        debugLog.record($format("GetDeps: Begin. Allocating ") + fshow(idx));

        // The timing partition scratchpad must be filled in by us.
        let newtok = TOKEN { index: idx,
                             poison: False,
                             dummy: req.dummy,
                             timep_info: TOKEN_TIMEP_INFO { scratchpad: 0 } };
        
        
        // Record the address. (For relative branches, etc.)
        tokAddr.write(idx, req.virtualAddress);

        // Record the instruction.
        tokInst.write(idx, req.instruction);

        // Debug message
        Vector#(5, Bit#(32)) dbg_args = newVector();
        dbg_args[0] = resize(tokContextId(newtok));
        dbg_args[1] = resize(tokTokenId(newtok));
        Bit#(64) dbg_va = resize(req.virtualAddress);
        dbg_args[2] = dbg_va[63:32];
        dbg_args[3] = dbg_va[31:0];
        dbg_args[4] = resize(req.instruction);
        stdio1.printf(msgMapInst, toList(dbg_args));

        // Pass on to stage 2.
        deps1Q.enq(tuple2(newtok, req.instruction));

    endrule


    //
    // getDependencies2 --
    //   Compute vectors of source and destination architectural registers.
    //   Ask the physical register free list for the physical registers that
    //   will be needed.  Also, send the set of architectural registers accessed
    //   to the register mapper.
    //
    rule getDependencies2 (deps1Q.first() matches {.tok, .inst} &&& state.readyToContinue());

        // Get the info from the previous stage.
        deps1Q.deq();

        // Token active or is it a dummy?
        let tok_active = !tokIsDummy(tok);

        //
        // Use the scoreboard to record other instruction properties.
        //
        // The states for load type, store type and emulation MUST be set here
        // as they are uninitialized during token allocation in order to
        // avoid a hazard with this rule.
        //
        Maybe#(ISA_MEMOP_TYPE) ld_type = tagged Invalid;
        if (isaIsLoad(inst))
        begin
            ld_type = tagged Valid isaLoadType(inst);
            debugLog.record(fshow(tok.index) + $format(": GetDeps2: Load type %0d", isaLoadType(inst)));
        end
        tokScoreboard.setLoadType(tok.index, ld_type);

        Maybe#(ISA_MEMOP_TYPE) st_type = tagged Invalid;
        if (isaIsStore(inst) && tok_active)
        begin
            st_type = tagged Valid isaStoreType(inst);
            storeBufferAllocate.send(tok.index);
            debugLog.record(fshow(tok.index) + $format(": GetDeps2: Store type %0d", isaStoreType(inst)));
        end
        tokScoreboard.setStoreType(tok.index, st_type);

        let is_emulated = isaEmulateInstruction(inst);
        tokScoreboard.setEmulation(tok.index, is_emulated);
        if (is_emulated)
        begin
            debugLog.record(fshow(tok.index) + $format(": GetDeps2: Instruction is emulated"));
        end

        // Load locked?  This will be needed in doLoads.
        tokIsLoadLocked.write(tok.index, isaIsLoadLocked(inst));

        //
        // COMPUTE SOURCE MAPPINGS
        //
        Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)) ar_srcs = newVector();

        for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
        begin
            // Get the architectural src (if any);
            Maybe#(ISA_REG_INDEX) arc_src = isaGetSrc(inst, x);

            // If there is a src, fill it in from the maptable.
            ar_srcs[x] = case (arc_src) matches
                             tagged Invalid:  tagged Invalid;
                             tagged Valid .r: tagged Valid r;
                         endcase;

            if (ar_srcs[x] matches tagged Valid .ar)
                debugLog.record(fshow(tok.index) + $format(": GetDeps2: Slot #%0d reads AR %0d", x, ar));
        end


        //
        // COMPUTE DESTINATION MAPPINGS
        //
        Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)) ar_dsts = newVector();
        Integer true_n_dsts = 0;        

        // Build a vector requesting registers from the free list
        Vector#(ISA_MAX_DSTS, Bool) dst_reg_reqs = newVector();

        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            // Get the architectural dst from the ISA.
            if (isaGetDst(inst, x) matches tagged Valid .ar)
            begin
                ar_dsts[x] = tagged Valid ar;
                dst_reg_reqs[x] = tok_active;  // Only request reg if token is alive
                true_n_dsts = true_n_dsts + 1;
                debugLog.record(fshow(tok.index) + $format(": GetDeps2: Slot #%0d writes AR %0d", x, ar));
            end
            else
            begin
                ar_dsts[x] = tagged Invalid;
                dst_reg_reqs[x] = False;
            end
        end
        
        // One destination register is always allocated.  Stores use this to
        // hold the value being stored.  Only request a register if the token is
        // alive.
        dst_reg_reqs[0] = tok_active;

        let num_dsts = isaGetNumDsts(inst);
        assertion.invalidNumDsts(num_dsts >= true_n_dsts);
        assertion.emulatedInstrNoDsts((num_dsts == 0) || !is_emulated);


        //
        // Now we have three vectors:
        //   - Architectural registers read by the instruction.
        //   - Architectural registers written by the instruction.
        //   - Vector of bools requesting allocation of physical registers
        //     corresponding to the registers written by the instruction.
        //
        
        // Request registers from the free list
        freelist.allocateRegs(dst_reg_reqs);
        debugLog.record(fshow(tok.index) + $format(": GetDeps2: Freelist request mask is %0b", pack(dst_reg_reqs)));

        deps2Q.enq(tuple4(tok, inst, ar_srcs, ar_dsts));

    endrule


    //
    // getDependencies3 --
    //   Wait for the free physical register manager to return the registers
    //   requested in stage 2.  Forward these new registers to the register
    //   mapper, which will be waiting for them.  The new registers will
    //   rendezvous with the mapping request from stage 2.
    //
    let stdio3 <- mkStdIO_CondPrintf(ioMask_FUNCP_REGMGR, stdio);

    (* conservative_implicit_conditions *)
    rule getDependencies3 (state.readyToContinue());

        match {.tok, .inst, .ar_srcs, .ar_dsts} = deps2Q.first();
        deps2Q.deq();

        // Only update the maptable for live tokens.
        let tok_active = !tokIsDummy(tok);

        // Get the physical registers requested earlier
        let phy_dsts <- freelist.allocateRsp();

        if (tok_active)
        begin
            // Reset new physical registers
            prf.invalReq(tok, phy_dsts);

            // Log it
            for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
            begin
                if (phy_dsts[x] matches tagged Valid .pr)
                    debugLog.record(fshow(tok.index) + $format(": GetDeps3: Inval slot #%0d PR %0d", x, pr));
            end
        end
        else
        begin
            // Don't update the maptable if this token is getting killed
            debugLog.record(fshow(tok.index) + $format(": GetDeps4: JUNK TOKEN (NO UPDATE)"));
            // Free the registers since we didn't need them.
            freelist.freeRegs(phy_dsts);
        end

        // Update register mapping
        regMapping.decodeReq(tok, ar_srcs, ar_dsts, phy_dsts);

        //
        // Associate allocated physical registers with architectural regs
        // for the timing model.
        //
        ISA_DST_MAPPING map_dsts = newVector();
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            map_dsts[x] = case (ar_dsts[x]) matches
                              tagged Valid .ar: tagged Valid tuple2(ar, validValue(phy_dsts[x]));
                              tagged Invalid: tagged Invalid;
                          endcase;
        end

        // Save destination physical registers
        let dst_regs = REGMGR_DST_REGS { ar: ar_dsts, pr: phy_dsts };
        tokDsts.write(tok.index, dst_regs);

        // Store conditional (e.g. Alpha)?  Register will be written in doStores
        // once it is known whether the address lock survived.
        if (isaIsStoreCond(inst) matches tagged Valid .dst_idx)
        begin
            tokIsStoreCond.write(tok.index, phy_dsts[dst_idx]);
            if (phy_dsts[dst_idx] matches tagged Valid .pr)
                debugLog.record(fshow(tok.index) + $format(": GetDeps3: Note ST_C dst PR%0d", pr));
            else
                debugLog.record(fshow(tok.index) + $format(": GetDeps3: Note ST_C dst ERROR (no dst pr)"));
        end
        else
        begin
            tokIsStoreCond.write(tok.index, tagged Invalid);
        end

        //
        // Log mapping details
        //
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            if (phy_dsts[x] matches tagged Valid .pr)
            begin
                if (ar_dsts[x] matches tagged Valid .ar)
                    debugLog.record(fshow(tok.index) + $format(": GetDeps3: Destination #%0d Mapped (%0d/%0d)", x, ar, pr));
                else
                    debugLog.record(fshow(tok.index) + $format(": GetDeps3: Destination #%0d writes unmapped PR%0d", x, pr));
            end
        end

        // Generate a debug message with up to 2 destinations.
        Vector#(5, Bit#(32)) reg_msg = replicate('hffffffff);
        for (Integer x = 0; x < min(3, valueOf(ISA_MAX_DSTS)); x = x + 1)
        begin
            if (tok_active &&& map_dsts[x] matches tagged Valid {.ar, .pr})
                reg_msg[x] = resize(pr);
        end
        reg_msg[3] = resize(tokContextId(tok));
        reg_msg[4] = resize(tokTokenId(tok));
        stdio3.printf(msgRegDsts, toList(reg_msg));

        deps3Q.enq(tuple3(tok, ar_srcs, map_dsts));

    endrule


    //
    // getDependencies4 --
    //   Mapping is complete.  Receive the input physical registers from the
    //   mapper and construct a response to the timing model.
    //
    let stdio4 <- mkStdIO_CondPrintf(ioMask_FUNCP_REGMGR, stdio);

    rule getDependencies4 (state.readyToContinue());

        match {.tok, .ar_srcs, .map_dsts} = deps3Q.first();
        deps3Q.deq();
        
        let tok_active = !tokIsDummy(tok);

        let phy_srcs <- regMapping.decodeRsp();

        // Also source mappings for the timing model
        Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_MAPPING)) map_srcs = newVector();
        for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
        begin
            map_srcs[x] = case (ar_srcs[x]) matches
                              tagged Invalid:  tagged Invalid;
                              tagged Valid .r: tagged Valid tuple2(r, validValue(phy_srcs[x]));
                          endcase;
        end

        // Update the token tables
        tokWriters.write(tok.index, phy_srcs);

        // Log all source mappings.
        for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
        begin
            if (map_srcs[x] matches tagged Valid {.ar, .pr})
                debugLog.record(fshow(tok.index) + $format(": GetDeps4: Source #%0d Mapped (%0d/%0d)", x, ar, pr));
        end

        // Wait for confirmation that new registers have been marked invalid
        if (tok_active)
        begin
            prf.invalRsp();
            debugLog.record(fshow(tok.index) + $format(": New PRs confirmed invalid"));
        end

        // If it was killed then don't tell the timing partition about the allocated register.
        let final_map_dsts = tok_active ? map_dsts : Vector::replicate(tagged Invalid);
 
        // Update the scoreboard.
        tokScoreboard.decFinish(tok.index);

        // Return everything to the timing partition. End of macro-operation (path 1).
        linkGetDeps.makeResp(initFuncpRspGetDependencies(tok, map_srcs, final_map_dsts));
        debugLog.record(fshow(tok.index) + $format(": GetDeps: End"));

        // Generate a debug message with up to 3 sources.
        Vector#(5, Bit#(32)) reg_msg = replicate('hffffffff);
        for (Integer x = 0; x < min(3, valueOf(ISA_MAX_SRCS)); x = x + 1)
        begin
            if (map_srcs[x] matches tagged Valid {.ar, .pr})
                reg_msg[x] = resize(pr);
        end
        reg_msg[3] = resize(tokContextId(tok));
        reg_msg[4] = resize(tokTokenId(tok));

        stdio4.printf(msgRegSrcs, toList(reg_msg));

    endrule
    
endmodule

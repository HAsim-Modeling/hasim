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


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_GetDependencies#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_REG_MAPPING_GETDEPENDENCIES regMapping,
    REGSTATE_PHYSICAL_REGS_INVAL_REGS prf,
    FUNCP_FREELIST freelist,
    BRAM#(TOKEN_INDEX, ISA_INST_SRCS) tokWriters,
    BRAM_MULTI_READ#(3, TOKEN_INDEX, ISA_INST_DSTS) tokDsts,
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_INSTRUCTION) tokInst)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_getDependencies.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_GET_DEPENDENCIES, 
                       FUNCP_RSP_GET_DEPENDENCIES) linkGetDeps <- mkConnection_Server("funcp_getDependencies");

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

    FIFO#(TOKEN) deps1Q <- mkFIFO();

    FIFO#(Tuple4#(TOKEN, Bool,
                  Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)),
                  Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX)))) deps2Q <- mkFIFO();

    FIFO#(Tuple4#(TOKEN, Bool,
                  Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_INDEX)),
                  Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING)))) deps3Q <- mkFIFO();


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    // ******* getDependencies *******
    // 4-stage macro-operation.
    
    // When:   When the timing partiton request the dependencies of an operation.
    // Effect: Allocate all destination registers in maptable. 
    //         Lookup all source registers in maptable.
    // Soft Inputs:  TOKEN
    // Soft Returns: TOKEN, ISA_DEPENDENCY_INFO
 
    //
    // getDependencies1 --
    //   Read instruction opcode.
    //
    rule getDependencies1 (state.readyToBegin());

        // Read inputs. Begin macro-operation.
        let req = linkGetDeps.getReq();
        linkGetDeps.deq();
        let tok = req.token;
        debugLog.record(fshow(tok.index) + $format(": GetDeps: Begin"));
        
        // Update the status.
        tokScoreboard.decStart(tok.index);
        
        // Retrieve the instruction.
        tokInst.readPorts[0].readReq(tok.index);

        // Pass on to stage 2.
        deps1Q.enq(tok);

    endrule


    //
    // getDependencies2 --
    //   Compute vectors of source and destination architectural registers.
    //   Ask the physical register free list for the physical registers that
    //   will be needed.  Also, send the set of architectural registers accessed
    //   to the register mapper.
    //
    (* conservative_implicit_conditions *)
    rule getDependencies2 (state.readyToContinue());

        // Get the info from the previous stage.
        let tok = deps1Q.first();
        deps1Q.deq();

        // Get instruction requested by previous stage.
        let inst <- tokInst.readPorts[0].readRsp();

        // Token active or was it killed?
        let tok_active = tokScoreboard.isAllocated(tok.index);

        // Use the scoreboard to record other instruction properties
        if (isaIsLoad(inst))
        begin
            tokScoreboard.setLoadType(tok.index, isaLoadType(inst));
        end

        if (isaIsStore(inst))
        begin
            tokScoreboard.setStoreType(tok.index, isaStoreType(inst));
            storeBufferAllocate.send(tok.index);
        end

        let is_emulated = isaEmulateInstruction(inst);
        tokScoreboard.setEmulation(tok.index, is_emulated);
        if (is_emulated)
        begin
            debugLog.record(fshow(tok.index) + $format(": GetDeps2: Instruction is emulated"));
        end


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
        
        // Send architectural register details to the register mapper.
        regMapping.decodeStage1(tok, ar_srcs, ar_dsts);

        // Request registers from the free list
        freelist.allocateRegs(dst_reg_reqs);
        debugLog.record(fshow(tok.index) + $format(": GetDeps2: Freelist request mask is %0b", pack(dst_reg_reqs)));

        deps2Q.enq(tuple4(tok, tok_active, ar_srcs, ar_dsts));

    endrule


    //
    // getDependencies3 --
    //   Wait for the free physical register manager to return the registers
    //   requested in stage 2.  Forward these new registers to the register
    //   mapper, which will be waiting for them.  The new registers will
    //   rendezvous with the mapping request from stage 2.
    //
    (* conservative_implicit_conditions *)
    rule getDependencies3 (state.readyToContinue());

        match {.tok, .tok_active, .ar_srcs, .ar_dsts} = deps2Q.first();
        deps2Q.deq();

        // Get the physical registers requested earlier
        let phy_dsts <- freelist.allocateRsp();

        if (tok_active)
        begin
            // Reset new physical registers
            prf.invalReq(phy_dsts);

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

        // Save destination physical registers
        tokDsts.write(tok.index, phy_dsts);

        // Register mapper is waiting for new physical registers to go with
        // the request started in the previous stage.
        regMapping.decodeStage2(tok, phy_dsts);

        //
        // Associate allocated physical registers with architectural regs
        // for the timing model.
        //
        Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING)) map_dsts = newVector();
        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
            map_dsts[x] = case (ar_dsts[x]) matches
                              tagged Valid .ar: tagged Valid tuple2(ar, validValue(phy_dsts[x]));
                              tagged Invalid: tagged Invalid;
                          endcase;
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

        deps3Q.enq(tuple4(tok, tok_active, ar_srcs, map_dsts));

    endrule


    //
    // getDependencies4 --
    //   Mapping is complete.  Receive the input physical registers from the
    //   mapper and construct a response to the timing model.
    //
    rule getDependencies4 (state.readyToContinue());

        match {.tok, .tok_active, .ar_srcs, .map_dsts} = deps3Q.first();
        deps3Q.deq();

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
                debugLog.record(fshow(tok.index) + $format(": GetDeps3: Source #%0d Mapped (%0d/%0d)", x, ar, pr));
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
        
    endrule
    
endmodule

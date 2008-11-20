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
  

// ========================================================================
//
//   Internal data structures
//
// ========================================================================

// STATE_DEPS2
typedef union tagged
{
    void DEPS2_NORMAL;
    struct 
    { 
        Bit#(4)          numToAlloc;
        Bit#(4)          current;
        ISA_SRC_MAPPING  mapSrcs;
        ISA_DST_MAPPING  mapDstsSoFar;
        ISA_INST_DSTS    regsToFreeSoFar;
    }
    DEPS2_ALLOC_MORE;
}
STATE_DEPS2
    deriving (Eq, Bits);


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_GetDependencies#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_PHYSICAL_REGS_INVAL_REG prf,
    FUNCP_SNAPSHOT snapshots,
    FUNCP_FREELIST freelist,
    BRAM#(TOKEN_INDEX, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) tokFreeListPos,
    Reg#(Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) maptable,
    BRAM#(TOKEN_INDEX, ISA_INST_DSTS) tokRegsToFree,
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

    FIFO#(TOKEN) depsQ <- mkFIFO();
    Reg#(STATE_DEPS2) stateDeps2 <- mkReg(DEPS2_NORMAL);


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    // ******* getDependencies *******
    // 2-stage macro-operation. Stage 2 can stall.
    
    // The final stage continues to stall until all destinations have been allocated.
    
    // When:   When the timing partiton request the dependencies of an operation.
    // Effect: Allocate all destination registers in maptable. 
    //         Lookup all source registers in maptable.
    //         Make snapshot of branches or emulated instructions.
    // Soft Inputs:  TOKEN
    // Soft Returns: TOKEN, ISA_DEPENDENCY_INFO
 
    // getDependencies1
    
    // When:   When the timing partition starts a new getDeps operation.
    // Effect: Update the scoreboard, start retrieving the instruction, start allocating a dest.

    rule getDependencies1 (state.readyToBegin());

        // Read inputs. Begin macro-operation.
        let req = linkGetDeps.getReq();
        linkGetDeps.deq();
        let tok = req.token;
        debugLog.record($format("TOKEN %0d: GetDeps: Begin.", tok.index));
        
        // Update the status.
        tokScoreboard.decStart(tok.index);
        
        // Retrieve the instruction.
        tokInst.readPorts[0].readReq(tok.index);

        // Everyone gets a Physical Register, even if they don't have a destination.
        // Otherwise we would need another stage here.
        freelist.forwardReq();

        // Pass on to stage 2.
        depsQ.enq(tok);

    endrule

    // getDependencies2
    // When:   After getDependencies1 has occured. Note that we allow this to proceed with "junk" tokens.
    // Effect: Use the maptable to lookup sources, then update it to include one of our dests.
    //         If an instruction has more than one dest then the third stage will occur,
    //         otherwise this rule itself will return the result to the timing model.

    rule getDependencies2 (state.readyToContinue() &&& stateDeps2 matches tagged DEPS2_NORMAL);

        // Get the info from the previous stage.
        let tok = depsQ.first();

        //Get the info the previous stage requested.
        let inst     <- tokInst.readPorts[0].readRsp();
        let new_preg <- freelist.forwardResp();

        // Decode the instruction using ISA-provided functions.

        // Create vectors with info on the physical sources.
        Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_srcs = newVector();
        Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_MAPPING))          map_srcs = newVector();

        // Use a for-loop to fill in the vector from the instruction and maptable.
        for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
        begin

            // Get the architectural src (if any);
            Maybe#(ISA_REG_INDEX) arc_src = isaGetSrc(inst, x);

            // If there is a src, fill it in from the maptable.
            let phys_src = case (arc_src) matches
                               tagged Invalid:  tagged Invalid;
                               tagged Valid .r: tagged Valid select(maptable,pack(r));
                           endcase;

            phy_srcs[x] = phys_src;

            // Also record the info for the timing partition.
            map_srcs[x] = case (arc_src) matches
                              tagged Invalid:  tagged Invalid;
                              tagged Valid .r: tagged Valid tuple2(r, select(maptable, pack(r)));
                          endcase;

        end

        // Create vectors with info on the destinations.
        Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX))            arc_dsts = newVector();
        Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_dsts = replicate(Invalid);
        Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING))          map_dsts = newVector();
        Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phy_regs_to_free = replicate(Invalid);

        // Use a for-loop to fill in the architectural dests.

        Integer true_n_dsts = 0;        

        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
          // Get the architectural dst from the ISA.
          let arc_dst = isaGetDst(inst, x);
          // Update the vectors.
          arc_dsts[x] = arc_dst;
          if (arc_dst matches tagged Valid .r)
          begin
              map_dsts[x] = tagged Valid tuple2(r, new_preg); //This could be overwritten if we stall.
              true_n_dsts = true_n_dsts + 1;
          end
          else
          begin
              map_dsts[x] = tagged Invalid;
          end
        end

        // Unfortunately we can only record one physical dest here, since we only got one from
        // the freelist. If the instruction has more we will stall and allocate more.

        phy_dsts[0] = tagged Valid new_preg;

        // If we have a dest, update the maptable with the correct physical register.

        Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) new_map = case (arc_dsts[0]) matches
            tagged Invalid:  return maptable;
            tagged Valid .d: return update(maptable, pack(d), new_preg);
          endcase;

        let tok_killed = !tokScoreboard.isAllocated(tok.index);

        if (!tok_killed)
        begin
             maptable <= new_map;
            // Also we must reset the physical register dest to Invalid.
            prf.inval(new_preg);
        end
        else
        begin
            // Don't update the maptable if this token is getting killed

            // Unallocate the register we just got.
            freelist.back();

            //Log it.
            debugLog.record($format("TOKEN %0d: GetDeps2: JUNK TOKEN (NO UPDATE)", tok.index));

        end

        // The phyRegToFree is the physical register which gets freed when we are committed/killed.
        // If we have a dest, this register is the old writer of the register.
        // Otherwise the dest we requested in stage 1 is a dummy.

        phy_regs_to_free[0] = case (arc_dsts[0]) matches
                                 tagged Invalid:  tagged Valid new_preg; // Free the dummy when you free this token.
                                 tagged Valid .d: tagged Valid select(maptable, pack(d)); // Free the actual old writer.
                              endcase;

        debugLog.record($format("TOKEN %0d: GetDeps2: Free 0 on Commit (%0d)", tok.index, validValue(phy_regs_to_free[0])));

        // Update the token tables with all this information.
         tokRegsToFree.write(tok.index, phy_regs_to_free);
            tokWriters.write(tok.index, phy_srcs);
               tokDsts.write(tok.index, phy_dsts);
        tokFreeListPos.write(tok.index, tagged Valid freelist.current());

        // Use the scoreboard to record other relevant info.
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

        // Make a snapshot for branches or emulated instructions.
        // Note that there is an implicit assumption here that no branch or emulated instruction has more than one destination.
        if (isaIsBranch(inst))
        begin
            let sidx <- snapshots.makeSnapshot(tok.index, new_map);
            debugLog.record($format("TOKEN %0d: GetDeps2: Making Snapshot %d of Branch.", tok.index, sidx));
        end
        else if (is_emulated)
        begin
            let sidx <- snapshots.makeSnapshot(tok.index, new_map);
            debugLog.record($format("TOKEN %0d: GetDeps2: Making Snapshot %d of Emulated Instruction.", tok.index, sidx));
        end
             

        // If there was one dest or less, we are done.

        let num_dsts = isaGetNumDsts(inst);
        
        assertion.invalidNumDsts(num_dsts >= true_n_dsts);
        assertion.emulatedInstrNoDsts((num_dsts == 0) || !is_emulated);

        // Log all source mappings.
        for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
        begin
          case (map_srcs[x]) matches
              tagged Invalid: debugLog.record($format("TOKEN %0d: GetDeps2: No Source %0d.", tok.index, fromInteger(x)));
              tagged Valid {.ar, .pr}: debugLog.record($format("TOKEN %0d: GetDeps2: Source %0d Mapped (%0d/%0d).", tok.index, fromInteger(x), ar, pr));
          endcase
        end

        // Log the dest mapping
        case (map_dsts[0]) matches
            tagged Invalid: debugLog.record($format("TOKEN %0d: GetDeps2: No Destination.", tok.index));
            tagged Valid {.ar, .pr}: debugLog.record($format("TOKEN %0d: GetDeps2: Destination 0 Mapped (%0d/%0d).", tok.index, ar, pr));
        endcase
            
        if (num_dsts <= 1 || tok_killed)
        begin

            // 1 Dest or less, so don't stall.
            depsQ.deq();

            // If it was killed then don't tell the timing partition about the allocated register.
            let final_map_dsts = tok_killed ? Vector::replicate(tagged Invalid) : map_dsts;
 
            // Update the scoreboard.
            tokScoreboard.decFinish(tok.index);

            // Return everything to the timing partition. End of macro-operation (path 1).
            linkGetDeps.makeResp(initFuncpRspGetDependencies(tok, map_srcs, final_map_dsts));
            debugLog.record($format("TOKEN %0d: GetDeps: End (path 1).", tok.index));

        end
        else
        begin 

            // More dests to allocate. Log it.
            debugLog.record($format("TOKEN %0d: GetDeps2: Need to allocate %0d more destinations.", tok.index, num_dsts-1));

            // Request another phys reg
            freelist.forwardReq();

            // Stall this stage.
            stateDeps2 <= tagged DEPS2_ALLOC_MORE 
                                 {
                                     numToAlloc: fromInteger(num_dsts - 1),
                                     current: 1, 
                                     mapSrcs: map_srcs, 
                                     mapDstsSoFar: map_dsts, 
                                     regsToFreeSoFar: phy_regs_to_free
                                 };

        end

    endrule

    // getDependencies2AdditionalMappings
    // When:   When an instruction in getDeps2 had more than one destination.
    // Effect: Keep allocating destinations until you've got them all.
    
     rule getDependencies2AdditionalMappings (state.readyToContinue() &&& stateDeps2 matches tagged DEPS2_ALLOC_MORE .dep_info);

        // Get the data from the previous stage.
        let tok = depsQ.first();
        let cur = dep_info.current;
        let map_dsts = dep_info.mapDstsSoFar;
      
        // Get the new phys reg.
        let phy_dst <- freelist.forwardResp();

        // The new mapping.
        match {.arc_dst, .dummy} = validValue(map_dsts[cur]); // Perhaps we should assert that this is valid?
        let new_map_dsts = update(map_dsts, cur, tagged Valid tuple2(arc_dst, phy_dst));

        // The reg to free is the old writer of this destination.
        let actual_phy_reg_to_free = isValid(map_dsts[cur])? tagged Valid select(maptable, pack(arc_dst)): tagged Valid phy_dst;
        let new_phy_regs_to_free = update(dep_info.regsToFreeSoFar, cur, actual_phy_reg_to_free);

        debugLog.record($format("TOKEN %0d: GetDeps2: Free %0d on Commit (%0d)", tok.index, cur, validValue(actual_phy_reg_to_free)));

        if (isValid(map_dsts[cur]))
        begin

            // Update the maptable.
            maptable <= update(maptable, pack(arc_dst), phy_dst);

            // Reset the reg to unready.
            prf.inval(phy_dst);

            // Log it.
            debugLog.record($format("TOKEN %0d: GetDeps2: Destination %0d Mapped (%0d/%0d)", tok.index, cur, arc_dst, phy_dst));

        end

        if (cur < dep_info.numToAlloc) // We're not done yet;
        begin

            // Get a new physical reg for the next time around.
            freelist.forwardReq();

            // Update the state for the next time around.
            stateDeps2 <= tagged DEPS2_ALLOC_MORE 
                                 {
                                     numToAlloc: dep_info.numToAlloc,
                                     current: cur + 1,
                                     mapSrcs: dep_info.mapSrcs, 
                                     mapDstsSoFar: new_map_dsts, 
                                     regsToFreeSoFar: new_phy_regs_to_free
                                 };
        end
        else
        begin // We're done!

            // Update the token table with the destinations and regs to free.
            ISA_INST_DSTS final_phy_dsts = newVector();

            for (Integer x = 0; x < valueof(ISA_MAX_DSTS); x = x + 1)
            begin

                final_phy_dsts[x] = case (new_map_dsts[x]) matches
                                        tagged Invalid:  tagged Invalid;
                                        tagged Valid {.ad, .pd}: tagged Valid pd;
                                    endcase;

            end

                   tokDsts.write(tok.index, final_phy_dsts);
             tokRegsToFree.write(tok.index, new_phy_regs_to_free);
            tokFreeListPos.write(tok.index, tagged Valid freelist.current()); // XXX Is this right or should it be the first?

            // Unstall the pipeline.
            stateDeps2 <= tagged DEPS2_NORMAL;
            depsQ.deq();

            // Update the scoreboard.
            tokScoreboard.decFinish(tok.index);

            // Return everything to the timing partition. End of macro-operation (path 2).
            linkGetDeps.makeResp(initFuncpRspGetDependencies(tok, dep_info.mapSrcs, new_map_dsts));
            debugLog.record($format("TOKEN %0d: GetDeps: End (path 2).", tok.index));

        end
            
    endrule
    
endmodule

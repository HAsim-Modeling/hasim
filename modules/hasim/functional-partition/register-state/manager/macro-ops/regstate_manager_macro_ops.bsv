// regstate_manager_macro_ops

// The manager of the register state, using a macro-op organization.


// Library includes.

import FIFO::*;
import Vector::*;
import RegFile::*;

// Project foundation includes.

`include "hasim_common.bsh"
`include "soft_connections.bsh"
`include "fpga_components.bsh"

// Functional Partition includes.

`include "funcp_regstate_scoreboard.bsh"
`include "funcp_regstate_freelist.bsh"
`include "funcp_memstate_manager.bsh"

// ISA includes

`include "hasim_isa.bsh"
`include "hasim_isa_datapath.bsh"

// ***** Typedefs ***** //

// FUNCP_SNAPSHOT_INDEX

// The index into the snapshots, as defined by the parameter.

typedef Bit#(TLog#(`REGSTATE_NUM_SNAPSHOTS)) FUNCP_SNAPSHOT_INDEX;

// mkFUNCP_RegStateManager

// The manager of the register state, and the bulk of the work of the functional partition.

module [HASim_Module] mkFUNCP_RegStateManager
    //interface:
                ()
    provisos
            (Bits#(TOKEN_INDEX, idx_SZ),      // The number of tokens.
             Bits#(ISA_REG_INDEX, rname_SZ),  // The number of architectural registers.
             Bits#(FUNCP_SNAPSHOT_INDEX, snapshotptr_SZ)); // The number of snapshots.

    // ******* Debuging State *******

    // Fake register to hold our debugging file descriptor.
    let debug_log     <- mkReg(InvalidFile);

    // The current FPGA clock cycle
    Reg#(Bit#(32)) fpga_cc <- mkReg(0);

    // A convenience function for debugging.

    function Action funcpDebug(Action a);
    action

      $fwrite(debug_log, "[%d]: ", fpga_cc);
      a;
      $fwrite(debug_log, "\n");

    endaction
    endfunction

    // ******* Submodules *******

    // The Token State is a big scoreboard which tracks the status of inflight tokens.
    let tok_scoreboard <- mkFUNCP_Scoreboard();

    // The Freelist tracks which physical registers are available.
    let freelist <- mkFUNCP_Freelist(debug_log, fpga_cc);

    // ******* Local State *******

    // Tables to track info about in-flight instructions.

    // The address we got the instruction from (told to us by the timing model).
    BRAM#(TOKEN_INDEX, ISA_ADDRESS)     tok_addr <- mkBRAM_Full();

    // The instruction that was at that address (from mem_state).
    BRAM_2#(TOKEN_INDEX, ISA_INSTRUCTION) tok_inst <- mkBRAM_2_Full();

    // The destinations of the instruction (a convenience which saves us from reading the instruction/maptable). 
    BRAM_3#(TOKEN_INDEX, Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) tok_dsts <- mkBRAM_3_Full();

    // If an instruction has sources in other inflight instructions it will be noted here.
    BRAM#(TOKEN_INDEX, Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)))   tok_writers <- mkBRAM_Full();

    // The memaddress is used by Loads/Stores so we don't have to repeat the calculation.
    BRAM_2#(TOKEN_INDEX, ISA_ADDRESS) tok_memaddr <- mkBRAM_2_Full();

    // The physical registers to free when the token is committed/killed.
    BRAM#(TOKEN_INDEX, Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) tok_regs_to_free <- mkBRAM_Full();

    // The Physical Register File

    BRAM_3#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE) prf <- mkBRAM_3_Full();
    
    // Valid bits for PRF
    Vector#(FUNCP_PHYSICAL_REGS, Reg#(Bool)) prf_valids = newVector();
    
    for (Integer x = 0; x < valueOf(FUNCP_PHYSICAL_REGS); x = x + 1)
    begin
      prf_valids[x] <- mkReg(False);
    end

    // The Map Table

    // This gets pounded nearly every FPGA cycle, so it's NOT in RAM.
    // Also this lets us snapshot/reload the entire maptable in a single cyle.

    // The highest register in the ISA (the last one which is initially valid).
    ISA_REG_INDEX            highestReg = maxBound;
    FUNCP_PHYSICAL_REG_INDEX maxInit = zeroExtend(highestReg);

    // The initial map is that all architectural registers are mapped 1-to-1 to
    // physical registers and are all valid.

    Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX) initmap = newVector();
    
    // Note: this loop ends at _architectural_ register size.
    
    for (Integer x  = 0; x < valueof(TExp#(rname_SZ)); x = x + 1)
    begin
      initmap[x] = fromInteger(x);
    end

    Reg#(Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX)) maptable   <- mkReg(initmap);

    // Snapshots 
    // Allow for fast rewinds.

    // The valid bits tell us which location contains a valid snapshot.
    Reg#(Vector#(TExp#(idx_SZ), Bool))                snap_valids     <- mkReg(replicate(False));

    // The IDs tell us which snapshot is in a given location.
    Reg#(Vector#(TExp#(snapshotptr_SZ), TOKEN_INDEX)) snap_ids        <- mkRegU();
    
    // The next pointer points to the next location where we should write a snapshot.
    // (Possibly overwriting an old snapshot, which is okay.)
    Reg#(FUNCP_SNAPSHOT_INDEX)                          snap_next       <- mkReg(0);

    // The actual snapshots of the entire maptable.
    BRAM#(FUNCP_SNAPSHOT_INDEX, Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX)) snaps    <- mkBRAM_Full();

    // An additional snapshot of the location of the freelist.
    BRAM#(FUNCP_SNAPSHOT_INDEX, FUNCP_PHYSICAL_REG_INDEX)                           snaps_fl <- mkBRAM_Full();

    // ******* Miscellaneous *******

    // Are we currently doing a "rewind"?
    Reg#(Bool)     rewinding <- mkReg(False);

    // Is it a fast rewind or a slow one?
    Reg#(Bool)     fast_rewind <- mkReg(False);

    // Are we initializing after a reset?
    Reg#(Bool)     initializing <- mkReg(True);

    // This register stores the current Phys Reg we are initializing.
    Reg#(FUNCP_PHYSICAL_REG_INDEX)   init_cur <- mkReg(0);

    // We are only ready to go if we are neither rewinding nor initializing.
    let ready = !rewinding && !initializing;

    // These support "slow rewinds" which are currently not quite right.
    Reg#(TOKEN_INDEX) rewindTok <- mkRegU();
    Reg#(TOKEN_INDEX) rewindCur <- mkRegU();

    // The Epoch tells us when to discard junk tokens that were in flight when the timing partition killed them.
    Reg#(TOKEN_TIMEP_EPOCH) epoch <- mkReg(0);

    // Stall info for the getDeps operation.
    Reg#(Maybe#(Tuple5#(TOKEN, ISA_DST_INDEX, ISA_SRC_MAPPING, ISA_DST_MAPPING, ISA_INST_DSTS))) finish_deps <- mkReg(tagged Invalid);

    // Is the getResult stage stalling?
    Reg#(Bool)     exec_stalling <- mkReg(False);
    // The token we're stalling on.
    Reg#(TOKEN)    exec_stall_tok <- mkRegU();
    // Record the writers we've requested so far while stalling.
    Reg#(Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) exec_stall_writers <- mkReg(Vector::replicate(tagged Invalid));
    // Record the values that have come back while stalling.
    Reg#(Vector#(TSub#(ISA_MAX_SRCS, 2), Maybe#(ISA_VALUE))) exec_stall_values <- mkReg(Vector::replicate(tagged Invalid));
    
    // Is the getResult stage writing back more values?
    Reg#(Bool) exec_writeback_more <- mkReg(False);
    // The token we're stalling on.
    Reg#(TOKEN)    exec_writeback_tok <- mkRegU();
    // Record the values for writeback.
    Reg#(Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE)))) exec_writeback_vals <- mkReg(Vector::replicate(tagged Invalid));
    // Record the result for the timing model while we're writing back.
    Reg#(ISA_INSTRUCTION_RESULT) exec_writeback_result <- mkRegU();
    
    // Does the commit stage have to free more registers?
    Reg#(Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(FUNCP_PHYSICAL_REG_INDEX))) additional_regs_to_free <- mkReg(Vector::replicate(tagged Invalid));
    
    // These Queues are intermediate state between the pipeline stages.

    FIFO#(TOKEN) inst_q  <- mkFIFO();
    FIFO#(TOKEN) deps1_q <- mkFIFO();
    FIFO#(TOKEN) deps2_q <- mkFIFO();
    FIFO#(TOKEN) res1_q  <- mkFIFO();
    FIFO#(TOKEN) res2_q <- mkFIFO();
    FIFO#(TOKEN) res3_q   <- mkFIFO();
    FIFO#(TOKEN) load1_q  <- mkFIFO();
    FIFO#(TOKEN) load2_q  <- mkFIFO();
    FIFO#(TOKEN) store1_q <- mkFIFO();
    FIFO#(TOKEN) store2_q <- mkFIFO();
    FIFO#(TOKEN) comm_q   <- mkFIFO();
    FIFO#(TOKEN_INDEX) rewind_q <- mkFIFO();

    // ******* Soft Connections *******

    // Request type is top line.
    // Response type is bottom line.

    // Connections to the timing partition.

    Connection_Server#(void, 
                       TOKEN)                                  link_newInFlight <- mkConnection_Server("funcp_newInFlight");

    Connection_Server#(Tuple2#(TOKEN, ISA_ADDRESS),
                       Tuple2#(TOKEN, ISA_INSTRUCTION))        link_getInst   <- mkConnection_Server("funcp_getInstruction");

    Connection_Server#(TOKEN, 
                       Tuple2#(TOKEN, ISA_DEPENDENCY_INFO))    link_getDeps   <- mkConnection_Server("funcp_getDependencies");

    Connection_Server#(TOKEN, 
                       Tuple2#(TOKEN, ISA_INSTRUCTION_RESULT)) link_getResults <- mkConnection_Server("funcp_getResults");

    Connection_Server#(TOKEN, 
                       TOKEN)                                  link_doLoads   <- mkConnection_Server("funcp_doLoads");

    Connection_Server#(TOKEN, 
                       TOKEN)                                  link_doStores  <- mkConnection_Server("funcp_doSpeculativeStores");

    Connection_Server#(TOKEN,
                       TOKEN)                                  link_commitResults <- mkConnection_Server("funcp_commitResults");

    Connection_Server#(TOKEN,
                       TOKEN)                                  link_commitStores  <- mkConnection_Server("funcp_commitStores");  

    Connection_Receive#(TOKEN)                                 link_rewindToToken <- mkConnection_Receive("funcp_rewindToToken");


    // Connections to Mem State.

    Connection_Client#(MEMSTATE_REQ, 
                       MEMSTATE_RSP)                           link_to_dmem <- mkConnection_Client("funcp_mem_dmem");

    Connection_Client#(ISA_ADDRESS,
                       ISA_VALUE)                              link_to_imem <- mkConnection_Client("funcp_mem_imem");

    Connection_Send#(TOKEN)                                    link_mem_commit <- mkConnection_Send("funcp_mem_commit");

    Connection_Send#(Tuple2#(TOKEN_INDEX, 
                             TOKEN_INDEX))                     link_mem_rewind <- mkConnection_Send("funcp_mem_rewind");

    // Connection to Datapath.

    Connection_Client#(Tuple3#(ISA_INSTRUCTION, ISA_ADDRESS, ISA_SOURCE_VALUES), 
                       Tuple3#(ISA_INSTRUCTION_RESULT, ISA_ADDRESS, ISA_RESULT_VALUES)) link_datapath <- mkConnection_Client("isa_datapath");

    // ***** Assertion Checkers ***** //

    Assertion assert_instruction_is_actually_a_load   <- mkAssertionChecker("FUNCP: Regstate: Told to do loads on a non-load instruction!", ASSERT_WARNING);
    Assertion assert_load_dest_reg_is_ready           <- mkAssertionChecker("FUNCP: Regstate: Told to do a load when the destination register had already been written!", ASSERT_ERROR);
    Assertion assert_instruction_is_actually_a_store  <- mkAssertionChecker("FUNCP: Regstate: Told to do stores for an instruction that is not a store!", ASSERT_WARNING);
    Assertion assert_commited_store_is_actually_store <- mkAssertionChecker("FUNCP: Regstate: Told to commit a store which was not a store!", ASSERT_WARNING);

    // ******* Rules *******

    // initialize

    // When:    Only at the beginning of time (after a reset).
    // Effects: Makes sure all RAMS are in the right state before we begin computing.
    //          Additionally the first time it runs it will open the debug logfiles.

    rule initialize (initializing);

        //Open the debug logs. (First time only. Afterwards it is not InvalidFile.)

        if (debug_log == InvalidFile)
        begin
            let fd <- $fopen(`REGSTATE_LOGFILE_NAME, "w");

            if (fd == InvalidFile)
            begin
                $display(strConcat("Error opening FUNCP RegState logfile ", `REGSTATE_LOGFILE_NAME));
                $finish(1);
            end

            debug_log <= fd;
        end

        // For safety we start all physical registers at zero. In the future this might change.
        prf.write(init_cur, 0);
        prf_valids[init_cur] <= True;
        
        // We're done if we've initialized the last register.
        initializing <= (init_cur <= maxInit);
        init_cur <= init_cur + 1;

    endrule
  
    // currentCC
    // When:   Always
    // Effect: Just record the current FPGA cycle for debugging purposes.

    rule currentCC (True);

        fpga_cc <= fpga_cc + 1;

    endrule

    // ******* newInFlight ******* //

    // 1-stage macro-operation
    
    // When:         The timing model tells us to allocate a new in-flight instruction.
    // Effect:       Allocates a slot on the token state scoreboard.
    // Soft Inputs:  req from timing model
    // Soft Returns: a TOKEN which the timing model can use to refer to that slot.

    rule newInFlight (ready);

        // Get the input from the timing model. Begin macro operation.
        let x = link_newInFlight.getReq();
        link_newInFlight.deq();
        
        // Get the next token from the scoreboard.
        let idx <- tok_scoreboard.allocate();
        
        // Log it.
        
        funcpDebug($fwrite(debug_log, "NewInFlight: Allocating TOKEN %0d", idx));
        
        // Zero out our scratchpad.
        let inf = TOKEN_FUNCP_INFO {epoch: 0, scratchpad: 0};

        // The timing partition scratchpad must be filled in by up.
        let newtok = TOKEN {index: idx, timep_info: ?, funcp_info: inf};

        // Respond to the timing partition. End of macro operation.
        link_newInFlight.makeResp(newtok);

    endrule

    // ******* getInstruction ******* //

    // 2-stage macro-operation
    
    // When:         The timing model tells us to fetch the instruction at a given address.
    // Effect:       Reads the memstate, updates the scoreboard.
    // Soft Inputs:  TOKEN from timing model.
    // Soft Returns: TOKEN and ISA_INSTRUCTION.


    // getInstruction1
    
    // When:   The timing model makes a new FETCH req.
    // Effect: Record the address, kick over to Mem State. 

    rule getInstruction1 (ready);

        // Read input. Beginning of macro-operation.
        match {.tok, .addr} = link_getInst.getReq();
        link_getInst.deq();

        // Log it.
        funcpDebug($fwrite(debug_log, "TOKEN %0d: FETCH: Start (Address: 0x%h)", tok.index, addr));

        // Update scoreboard.
        tok_scoreboard.fetStart(tok.index);

        // Record the address. (For relative branches, etc.)
        tok_addr.write(tok.index, addr);

        // Kick to Mem State.
        link_to_imem.makeReq(addr);

        // Send on to getInstruction2.
        inst_q.enq(tok);

    endrule

    // getInstruction2

    // When:   Some time after fetch1.
    // Effect: Record the instruction, kick back to timing model.

    rule getInstruction2 (ready);

        // Input from previous stage.
        let tok = inst_q.first();
        inst_q.deq();

        // Get resp from Mem State.
        ISA_INSTRUCTION inst = link_to_imem.getResp();
        link_to_imem.deq();

        // Log it.
        funcpDebug($fwrite(debug_log, "TOKEN %0d: Fetch: End (INSTRUCTION: 0x%h)", tok.index, inst));

        // Record the instruction.
        tok_inst.write(tok.index, inst);

        // Update scoreboard.
        tok_scoreboard.fetFinish(tok.index);

        // Send response to timing partition. End of macro-operation.
        link_getInst.makeResp(tuple2(tok, inst));

    endrule

    // ******* getDependencies *******
    // Macro-operation implemented as a variable stage pipeline.
    
    // The operation is divided into 2 stages, plus one optional recurring stage.
    // The final stage continues to recur until all destinations have been allocated.
    
    // When:   When the timing partiton request the dependencies of an operation.
    // Effect: Allocate all destination registers in maptable. 
    //         Lookup all source registers in maptable.
    //         Make snapshot of branches.
    // Soft Inputs:  TOKEN
    // Soft Returns: TOKEN, ISA_DEPENDENCY_INFO
 
    // getDependencies1
    
    // When:   When the timing partition starts a new getDeps operation.
    // Effect: Update the scoreboard, start retrieving the instruction, start allocating a dest.

    rule getDependencies1 (ready);

        // Read inputs. Begin macro-operation.
        let tok = link_getDeps.getReq();
        link_getDeps.deq();
        
        // Log it.
        funcpDebug($fwrite(debug_log, "TOKEN %0d: getDeps: Start", tok.index));
        
        // Update the status.
        tok_scoreboard.decStart(tok.index);
        
        // Retrieve the instruction.
        tok_inst.read_req1(tok.index);

        // Everyone gets a Physical Register, even if they don't have a destination.
        // Otherwise we would need another stage here.
        freelist.forwardReq();

        // Pass on to stage 2.
        deps1_q.enq(tok);

    endrule

    // getDependencies2
    // When:   After getDependencies1 has occured. Note that we allow this to proceed with "junk" tokens.
    // Effect: Use the maptable to lookup sources, then update it to include one of our dests.
    //         If an instruction has more than one dest then the third stage will occur,
    //         otherwise this rule itself will return the result to the timing model.

    rule getDependencies2 (!initializing &&& finish_deps matches tagged Invalid);

        // Get the info from the previous stage.
        let tok = deps1_q.first();
        deps1_q.deq();

        //Get the info the previous stage requested.
        let inst    <- tok_inst.read_resp1();
        let newPReg <- freelist.forwardResp();

        // Decode the instruction using ISA-provided functions.

        // Create vectors with info on the physical sources.
        Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phySrcs = newVector();
        Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_MAPPING))          mapSrcs = newVector();

        // Use a for-loop to fill in the vector from the instruction and maptable.
        for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
        begin

            // Get the architectural src (if any);
            Maybe#(ISA_REG_INDEX) arcSrc = isaGetSrc(inst, x);

            // If there is a src, fill it in from the maptable.
            let physSrc = case (arcSrc) matches
                              tagged Invalid:  tagged Invalid;
                              tagged Valid .r: tagged Valid select(maptable, r);
                          endcase;

            phySrcs[x] = physSrc;

            // Also record the info for the timing partition.
            mapSrcs[x] = case (arcSrc) matches
                             tagged Invalid:  tagged Invalid;
                             tagged Valid .r: tagged Valid tuple2(r, select(maptable, r));
                         endcase;

        end

        // Create vectors with info on the destinations.
        Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_INDEX))            arcDsts = newVector();
        Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phyDsts = replicate(Invalid);
        Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING))          mapDsts = newVector();
        Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) phyRegsToFree = replicate(Invalid);

        // Use a for-loop to fill in the architectural dests.

        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
          // Get the architectural dst from the ISA.
          let arcDst = isaGetDst(inst, x);
          // Update the vectors.
          arcDsts[x] = arcDst;
          mapDsts[x] = case (arcDst) matches
                           tagged Invalid:  tagged Invalid;
                           tagged Valid .r: tagged Valid tuple2(r, newPReg); //This could be overwritten by the next stage.
                       endcase;
        end

        // Unfortunately we can only record one physical dest here, since we only got one from
        // the freelist. If the instruction has more we will invoke additional stages.

        phyDsts[0] = tagged Valid newPReg;

        // If we have a dest, update the maptable with the correct physical register.

        let newMap = case (arcDsts[0]) matches
            tagged Invalid:  return maptable;
            tagged Valid .d: return update(maptable, d, newPReg);
          endcase;

        if (tok.timep_info.epoch == epoch) //Don't update the maptable if this token is getting killed
        begin

            maptable <= newMap;
            // Also we must reset the physical register dest to Invalid.
            prf_valids[newPReg] <= False;
        end
        else
        begin

            // Unallocate the register we just got.
            freelist.back();
            //Log it.
            funcpDebug($fwrite(debug_log, "TOKEN %0d: JUNK TOKEN (NO UPDATE)", tok.index));

        end

        // The phyRegToFree is the physical register which gets freed when we are committed/killed.
        // If we have a dest, this register is the old writer of the register.
        // Otherwise the dest we requested in stage 1 is a dummy.

        phyRegsToFree[0] = case (arcDsts[0]) matches
                               tagged Invalid:  tagged Valid newPReg; // Free the dummy when you free this token.
                               tagged Valid .d: tagged Valid select(maptable, d); // Free the actual old writer.
                           endcase;

        // Update the token tables with all this information.
        tok_regs_to_free.write(tok.index, phyRegsToFree);
        tok_writers.write(tok.index, phySrcs);
        tok_dsts.write(tok.index, phyDsts);

        // Use the scoreboard to record other relevant info.
        if (isaIsLoad(inst))
            tok_scoreboard.setLoadType(tok.index, isaLoadType(inst));

        if (isaIsStore(inst))
            tok_scoreboard.setStoreType(tok.index, isaStoreType(inst));

        // Make a snapshot for branches.
        // Note that there is an implicit assumption here that no branch instruction has more than one destination.  

        if (isaIsBranch(inst))
        begin
            // Log it.
            funcpDebug($fwrite(debug_log, "TOKEN %0d: getDeps: Branch Detected. Making Snapshot (Number %0d).", tok.index, snap_next));

            // Mark the snap as valid.
            snap_valids[tok.index] <= True;

            // Record which token is at this snapshot.
            snap_ids[snap_next] <= tok.index;

            // Snapshot the maptable.
            snaps.write(snap_next, newMap);

            // Snapshot the freelist.
            snaps_fl.write(snap_next, freelist.current());

            // Increment the current snapshot pointer (with overflow, but we don't care if we erase an old snapshot.)
            snap_next <= snap_next + 1;
        end


        // If there was one dest or less, we are done.
        
        let numDsts = isaGetNumDsts(inst);
        
        if (numDsts <= 1)
        begin

            // Log all source mappings.
            for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
            begin
              case (mapSrcs[x]) matches
                  tagged Invalid: funcpDebug($fwrite(debug_log, "TOKEN %0d: getDeps: No Source %0d.", tok.index, fromInteger(x)));
                  tagged Valid {.ar, .pr}: funcpDebug($fwrite(debug_log, "TOKEN %0d: getDeps: Source %0d Mapped (%0d/%0d).", tok.index, fromInteger(x), ar, pr));
              endcase
            end

            // Log the dest mapping
              case (mapDsts[0]) matches
                  tagged Invalid: funcpDebug($fwrite(debug_log, "TOKEN %0d: getDeps: No Destination.", tok.index));
                  tagged Valid {.ar, .pr}: funcpDebug($fwrite(debug_log, "TOKEN %0d: getDeps: Destination 1 Mapped (%0d/%0d).", tok.index, ar, pr));
              endcase
            
            // Update the scoreboard.
            tok_scoreboard.decFinish(tok.index);

            // Return everything to the timing partition. End of macro-operation (path 1).
            link_getDeps.makeResp(tuple2(tok, tuple2(mapSrcs, mapDsts)));
        end
        else // Not done.
        begin 
            // Request another phys reg
            freelist.forwardReq();
            // Pass it along to the next stage.
            finish_deps <= tagged Valid tuple5(tok, fromInteger(numDsts - 1), mapSrcs, mapDsts, phyRegsToFree);
        end

    endrule

    // getDependencies2AdditionalMappings
    // When:   When an instruction in the previous stage had more than one destination.
    // Effect: Keep allocating destinations until you've got them all.
    
     rule getDependencies2AdditonalMappings (!initializing &&& finish_deps matches tagged Valid {.tok, .num, .mapSrcs, .mapDsts, .phyRegsToFree});
      
        // Get the new phys reg.
        let phyDst <- freelist.forwardResp();

        // The new mapping.
        match {.arcDst, .dummy} = validValue(mapDsts[num]); // Perhaps we should assert that this is valid?
        let newMapDsts = update(mapDsts, num, tagged Valid tuple2(arcDst, phyDst));

        // The reg to free is the old writer of this destination.
        let newPhyRegsToFree = update(phyRegsToFree, num, tagged Valid select(maptable, arcDst));

        if (tok.timep_info.epoch == epoch) //Don't update the maptable if this token is getting killed
        begin

            // Update the maptable.
            maptable <= update(maptable, arcDst, phyDst);
            // Reset the reg to unready.
            prf_valids[phyDst] <= False;
        end
        else
        begin
            //Log it.
            funcpDebug($fwrite(debug_log, "TOKEN %0d: JUNK TOKEN (NO ADDITIONAL UPDATE)", tok.index));
        end

        if (num > 0) // We're not done yet;
        begin

            // Get a new physical reg for the next time around.
            freelist.forwardReq();

            // Update the status register for the next time around.
            finish_deps <= tagged Valid tuple5(tok, num - 1, mapSrcs, newMapDsts, newPhyRegsToFree);
        end
        else
        begin // We're done!

          // Update the token table with the destinations and regs to free.
          ISA_INST_DSTS finalPhyDsts = newVector();
          
          for (Integer x = 0; x < valueof(ISA_MAX_DSTS); x = x + 1)
          begin

              finalPhyDsts[x] = case (newMapDsts[x]) matches
                                    tagged Invalid:  tagged Invalid;
                                    tagged Valid {.ad, .pd}: tagged Valid pd;
                                endcase;

          end

          tok_dsts.write(tok.index, finalPhyDsts);
          tok_regs_to_free.write(tok.index, newPhyRegsToFree);

          // Invalidate the reg. so we don't do this again.
          finish_deps <= tagged Invalid;

          // Update the scoreboard.
          tok_scoreboard.decFinish(tok.index);
          
          // Marshall up the dependencies for the timing model.
          let finalDeps = tuple2(mapSrcs, newMapDsts);

          // Return everything to the timing partition. End of macro-operation (path 2).
          link_getDeps.makeResp(tuple2(tok, finalDeps));

        end
            
    endrule

    // ******* getResults ******* //
    
    // 4-stage macro operation which can stall at stage 2, and can repeat the last stage.

    // When:   When the timing model requests an execution.
    // Effect: Perform register reads, then send to datapath for execution.
    // Soft Inputs:  Token
    // Soft Returns: Token, Result
    
    // getResults1

    // When:   When the timing model starts a getResults().
    // Effect: Lookup the locations of this token's sources.

    rule getResults1 (ready);

      // Get parameter from the timing model. Begin macro-operation.
      let tok = link_getResults.getReq();
      link_getResults.deq();

      // Log it.
      funcpDebug($fwrite(debug_log, "TOKEN %0d: Execute: Start", tok.index));

      // Update the scoreboard.
      tok_scoreboard.exeStart(tok.index);

      // Look up the writers.
      tok_writers.read_req(tok.index);

      // Pass it along to the next stage.
      res1_q.enq(tok);

    endrule

    // getResults2
    // When:   After getResults1.
    // Effect: Use the writers to look up values from the PRF. 
    //         Also retreive the instruction itself and the PC.
    //         If the writers are not all ready then a stall can occur.

    rule getResults2 (ready && !exec_stalling);

      // Get input from getResults1.
      let tok = res1_q.first();
      res1_q.deq();
      
      // Log it.
      funcpDebug($fwrite(debug_log, "TOKEN %0d: Execute: Reg Read", tok.index));

      // Response from previous stage.
      let ws <- tok_writers.read_resp();
      
      // We are ready when all the source PRs are valid.
      
      Bool ready = True;
      for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
      begin
          let srcIsReady = case (ws[x]) matches
                              tagged Invalid:  True; // No writer, so it is ready.
                              tagged Valid .v: prf_valids[v]; // Ready if phys reg is valid.
                           endcase;
          ready = ready && srcIsReady; // We are ready when ALL are ready.
      end

      // Additionally let junk proceed
      let isJunk = !tok_scoreboard.isAllocated(tok.index);
      
      // Log it.
      if (isJunk)
          funcpDebug($fwrite(debug_log, "TOKEN %0d: Execute: Letting Junk Proceed!", tok.index));
        

      if (ready || isJunk) // Go ahead and pass it to the next stage.
      begin

          // Request the first 2 sources (harmless if they don't exist).
          prf.read_req1(validValue(ws[0]));
          prf.read_req2(validValue(ws[1]));

          // Also look up the PC of the instruction and the instruction itself.
          tok_addr.read_req(tok.index);
          tok_inst.read_req2(tok.index);

          // Log it.
          funcpDebug($fwrite(debug_log, "TOKEN %0d: Execute: Reg Read Complete", tok.index));

          // Pass on to the next stage.
          res2_q.enq(tok);
      end
      else // We're stalling.
      begin
          // Log it.
          funcpDebug($fwrite(debug_log, "TOKEN %0d: Execute: Reg Read Stalling!", tok.index));

          exec_stalling <= True;
          exec_stall_tok <= tok;
          exec_stall_writers <= ws;
          exec_stall_values <= Vector::replicate(tagged Invalid);
      end


    endrule
    
    // getResults2Stall
    
    // When:   Occurs when a getResults2 stalls because of one of two reasons.
    //         A) An operation has more than 2 sources.
    //         B) An operation's sources were not all ready.
    // Effect: Once all the sources are ready, retrieve them and send them on to the next stage.
    
    // Elaborated Rule: N copies, where N is the maximum number of dests minus 2.
    //                  These two are handled at the end when we pass on to the next stage.

    for (Integer x = 2; x < valueof(ISA_MAX_SRCS); x = x + 1)
    begin
    
      // This rule ensures that if there is no writer, it is marked ready to go.
      rule getResults2StallPass (exec_stalling && !isValid(exec_stall_values[x]) &&& exec_stall_writers[x] matches tagged Invalid);
          exec_stall_values[x] <= tagged Valid(?);
      endrule
      
      // If there was a writer, when the RF is ready we request it and send it onwards.
      rule getResults2StallReq (exec_stalling && !isValid(exec_stall_values[x]) &&& 
                               exec_stall_writers[x] matches tagged Valid .r &&& 
                               prf_valids[r]);
          prf.read_req1(r);
      endrule
      
      // Get the response and record it.
      rule getResults2StallRsp (exec_stalling);
      
        let v <- prf.read_resp1();
        exec_stall_values[x] <= tagged Valid v;
      
      endrule
      
      // Note: in the future these rules could be expanded to also use PRF port 2.
    
    end
    
    // Some helper functions to determine when we're done stalling.
    
    let noMoreVectorStalls = Vector::all(isValid, exec_stall_values);
    let src1IsRdy = case (exec_stall_writers[0]) matches
                        tagged Invalid: True;
                        tagged Valid .r: prf_valids[r];
                    endcase;
    let src2IsRdy = case (exec_stall_writers[1]) matches
                        tagged Invalid: True;
                        tagged Valid .r: prf_valids[r];
                    endcase;
    let noMoreStalls = noMoreVectorStalls && src1IsRdy && src2IsRdy;
    
    rule getResults2StallEnd (exec_stalling && noMoreStalls);
        
        exec_stalling <= False;
    
        // Finish up the work of getResult2.
    
        // Request the first 2 sources (harmless if no sources).
        prf.read_req1(validValue(exec_stall_writers[0]));
        prf.read_req2(validValue(exec_stall_writers[1]));

        // Also look up the PC of the instruction and the instruction itself.
        tok_addr.read_req(exec_stall_tok.index);
        tok_inst.read_req2(exec_stall_tok.index);

        // Pass on to the next stage.
        res2_q.enq(exec_stall_tok);

    endrule
    
    // getResults3
    // When:    After getResults2 or alternatively getResults2StallEnd
    // Effect:  Send all the data to the datapath.

    rule getResults3 (ready);

        // Get input from the previous stage.
        let tok = res2_q.first();
        res2_q.deq();

        // Get all the data the previous stage kicked off.
        let v1 <- prf.read_resp1();
        let v2 <- prf.read_resp2();
        let addr <- tok_addr.read_resp();
        let inst <- tok_inst.read_resp2();

        // Combine the data we just go with any possible data from stalling.
        Vector#(ISA_MAX_SRCS, ISA_VALUE) values = newVector();

        values[0] = v1;
        values[1] = v2;

        for (Integer x = 2; x < valueof(ISA_MAX_SRCS); x = x + 1)
        begin

           values[x] = validValue(exec_stall_values[x]);

        end

        // Log it.
        funcpDebug($fwrite(debug_log, "TOKEN %0d: Execute: Sending to Datapath.", tok.index));

        // Send it to the datapath.
        link_datapath.makeReq(tuple3(inst, addr, values));

        // Look up the destinations for the writeback.
        tok_dsts.read_req1(tok.index);

        // Pass it to the next stage.
        res3_q.enq(tok);

    endrule
    
    // getResults4
    // When:   After getResults3 and the datapath returns the result.
    // Effect: If one or fewer destinations, write back the result and 
    //         return the result to the timing partition.
    //         If more results then the getResults4AdditionalWriteback rule will take care of it.

    rule getResults4 (ready && !exec_writeback_more);

        // Get the token from the previous stage.
        let tok = res3_q.first();
        res3_q.deq();

        // Get the response from the datapath.
        match {.res, .eaddr, .wbvals} = link_datapath.getResp();
        link_datapath.deq();

        // Update the memaddress (only useful for loads/stores)
        tok_memaddr.write(tok.index, eaddr);

        // Get the destination response
        let dsts <- tok_dsts.read_resp1();
        
        // The first dest should always be valid (it may not be architecturally visible)
        let dst = validValue(dsts[0]);

        // Perform the first writeback, if any.
        case (wbvals[0]) matches
            tagged Invalid:  noAction; // Not writing back, either a Load, or no dests.
            tagged Valid .v: 
            begin // Do the first writeback.
                prf.write(dst, v);
                prf_valids[dst] <= True;
            end
        endcase
        
        // Is there anything more to writeback?

        Bool writingBackMore = False;

        for (Integer x = 1; x < valueof(ISA_MAX_DSTS); x = x + 1)
        begin // There is more to do if both the dest and val are valid.
          writingBackMore = writingBackMore || (isValid(dsts[x]) && isValid(wbvals[x]));
        end

        if (!writingBackMore) // We're done
        begin

            // Log it.
            funcpDebug($fwrite(debug_log, "TOKEN %0d: Execute: Writeback complete.", tok.index));

            // Update scoreboard.
            tok_scoreboard.exeFinish(tok.index);

            // Return timing model. End of macro-operation (path 1).
            link_getResults.makeResp(tuple2(tok, res));

        end
        else // We've got to write back more.
        begin
            exec_writeback_more   <= True;
            
            // Log it.
            funcpDebug($fwrite(debug_log, "TOKEN %0d: Execute: Writing back additional values.", tok.index));

            // Marshall up the values for writeback.

            Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE))) remainingValues = newVector();
            for (Integer x = 0; x < valueof(ISA_MAX_DSTS) - 1; x = x + 1)
            begin
                remainingValues[x] = case (dsts[x]) matches
                                         tagged Invalid:  tagged Invalid;
                                         tagged Valid .d:
                                           case (wbvals[x]) matches 
                                              tagged Invalid:  tagged Invalid; // Not writing it now - presumably it's a load.
                                              tagged Valid .v: tagged Valid tuple2(d, v);
                                           endcase
                                     endcase;
            end

            // Record intermediate values for the next rule.
            exec_writeback_vals   <= remainingValues;
            exec_writeback_result <= res;
            exec_writeback_tok <= tok;
        end
      
    endrule

    // getResults4AdditionalWriteback
    
    // When:   After a result from getResults4 writes back additonal destinations.
    // Effect: Finish the writeback of the physical registerfile.
    
    // Elaborated Rule: N copies, where N is the maximum number of destinations 
    //                  an instruction can have, minus one. (The one we wrote back
    //                  in getResult4.)
    
    for (Integer x = 0; x < (valueOf(ISA_MAX_DSTS) - 1); x = x + 1)
    begin
    
      rule getResult4AdditionalWriteback (exec_writeback_more &&& exec_writeback_vals[x] matches tagged Valid {.dst, .val});
      
        // Do the writeback.
        prf.write(dst, val);
        prf_valids[dst] <= True;
        exec_writeback_vals[x] <= tagged Invalid;
        
        // When the last rule fires it also finishes up the macro-op.
        
        if (x == 0)
        begin
            // We're done.
            exec_writeback_more <= False;
        
            // Log it.
            funcpDebug($fwrite(debug_log, "TOKEN %0d: Execute: Additonal writebacks complete.", exec_writeback_tok.index));

            // Update scoreboard.
            tok_scoreboard.exeFinish(exec_writeback_tok.index);
            
            // Return to timing model. End of macro-operation (path 2).
            link_getResults.makeResp(tuple2(exec_writeback_tok, exec_writeback_result));
        end
      
      endrule
    
    end

    // ******* doLoads ******* //

    // 3-stage macro operation which makes Loads read memory.

    // When:   When the timing model requests it.
    // Effect: Read the effective address, do a load from the memory state, and write it back.
    // Soft Inputs:  Token
    // Soft Returns: Token
    
    // doLoads1

    // When:   When the timing model starts a doLoads().
    // Effect: Lookup the effective address of this token.

    rule doLoads1 (ready);

        // Get the input from the timing model. Begin macro-operation.
        let tok = link_doLoads.getReq();
        link_doLoads.deq();

        // If it's not actually a load, it's an exception.
        let isLoad = tok_scoreboard.isLoad(tok.index);
        assert_instruction_is_actually_a_load(isLoad);

        if (!isLoad)
        begin

            // Log it.
            funcpDebug($fwrite(debug_log, "TOKEN %0d: doLoads1: I WAS TOLD TO LOAD THIS BUT IT'S NOT A LOAD!", tok.index));

        end
        else // Everything's okay.
        begin

            // Log it.
            funcpDebug($fwrite(debug_log, "TOKEN %0d: doLoads1: Start", tok.index)); 

            // Update the scoreboard.
            tok_scoreboard.loadStart(tok.index);

            // Read the effective address.
            tok_memaddr.read_req1(tok.index);

            // Pass to the next stage.
            load1_q.enq(tok);

        end

    endrule

    // doLoads2

    // When:   After doLoads1 occurs
    // Effect: Make the request to the memory state.

    rule doLoads2 (ready);

        // Read the parameters from the previous stage.
        let tok = load1_q.first();
        load1_q.deq();

        // Get the address.
        let addr <- tok_memaddr.read_resp1();

        // Log it.
        funcpDebug($fwrite(debug_log, "TOKEN %0d: doLoads2: Requesting Load (Addr: 0x%h)", tok.index, addr));
        
        // Get the load type.
        ISA_STORE_TYPE lType = tok_scoreboard.getLoadType(tok.index);
        
        // Make the request to the DMem.
        link_to_dmem.makeReq(MEMSTATE_REQ_LOAD {token: tok, addr: addr, load_type: lType});

        // Read the destination so we can writeback the correct register.
        tok_dsts.read_req2(tok.index);
            
        // Pass it on to the final stage.
        load2_q.enq(tok);

    endrule

    // doLoads3

    // When:   After doLoads2 occurs and we get a response from the memory state.
    // Effect: Record the result and pass it back to the timing model.

    rule doLoads3 (ready &&& link_to_dmem.getResp() matches tagged MEMSTATE_RSP_LOAD .val);

      // Get the data from the previous stage.
      let tok = load2_q.first();
      load2_q.deq();

      // Pop the response from the memory state.
      link_to_dmem.deq();

      // Get the destination for the purposes of writeback.
      let dsts <- tok_dsts.read_resp2();
      
      // We assume that the destination for the load is destination 1.
      let dst = validValue(dsts[0]);
      
      // Log it.
      funcpDebug($fwrite(debug_log, "TOKEN %0d: doLoads3: Load Response (PR%0d <= 0x%h)", tok.index, dst, val));
      
      // Update the physical register file.
      prf.write(dst, val);
      
      // Assert that the register was ready (not valid).
      assert_load_dest_reg_is_ready(!prf_valids[dst]);
      
      // The register is now valid.
      prf_valids[dst] <= True;

      // Update the scoreboard.
      tok_scoreboard.loadFinish(tok.index);

      // Respond to the timing model. End of macro-operation.
      link_doLoads.makeResp(tok);

    endrule

    // ******* doStores ******* //

    // 3-stage macro operation which makes Stores update memory.
    // One extra stage discards store responses.

    // When:   When the timing model requests it.
    // Effect: Read the effective address and result, do a store to the memory state.
    // Soft Inputs:  Token
    // Soft Returns: Token


    // doStores1

    // When:   When the timing model starts a doStores().
    // Effect: Lookup the destination of this token.

    rule doStores1 (ready);

        // Get the input from the timing model. Begin macro-operation.
        let tok = link_doStores.getReq();
        link_doStores.deq();

        // If it's not actually a store, it's an exception.
        let isStore = tok_scoreboard.isStore(tok.index);
        assert_instruction_is_actually_a_store(isStore);

        if (!isStore)
        begin

            // Log it.
            funcpDebug($fwrite(debug_log, "TOKEN %0d: I WAS TOLD TO STORE THIS BUT IT'S NOT A STORE!", tok.index));

        end
        else // Everything's fine.
        begin

            // Log it.
            funcpDebug($fwrite(debug_log, "TOKEN %0d: doStores: Start", tok.index)); 

            // Update the scoreboard.
            tok_scoreboard.storeStart(tok.index);

            // Read the destination.
            tok_dsts.read_req3(tok.index);

            // Pass to the next stage.
            store1_q.enq(tok);

        end

    endrule
    
    // doStores2

    // When:   After doStores1 occurs
    // Effect: Read the physical register file and the effective address. 

    rule doStores2 (ready);

        // Read the parameters from the previous stage.
        let tok = store1_q.first();
        store1_q.deq();

        // Get the destination.
        let dsts <- tok_dsts.read_resp3();
        
        // We use destination zero of a store for the value in order to avoid
        // figuring out where the source comes from.
        // This is safe because no one can see the dummy physical register.

        let dst = validValue(dsts[0]);

        // Look up the register value.
        prf.read_req3(dst);

        // Read the effective address.
        tok_memaddr.read_req2(tok.index);

        // Log it.
        funcpDebug($fwrite(debug_log, "TOKEN %0d: doStores2: Retrieving Store Value (PR%0d)", tok.index, dst)); 

        // Pass it on to the final stage.
        store2_q.enq(tok);

    endrule

    // doStores3

    // When:   After doStores2 occurs
    // Effect: Send the store request to the memory state. Also reply to the timing model.
    //         Note that this represents a "fast forwarding" of the response to the timing model
    //         before the memory state actually completes the store. The semantics of the
    //         memory state must be such that this is safe.
     
    rule doStores3 (ready);

      // Get the result from the previous stage.
      let tok = store2_q.first();
      store2_q.deq();

      // Get the address.
      let addr <- tok_memaddr.read_resp2();
      
      // Get the value.
      let val  <- prf.read_resp3();

      // Log it.
      funcpDebug($fwrite(debug_log, "TOKEN %0d: DMem: Requesting Store (Addr: 0x%h <= 0x%h)", tok.index, addr, val)); 
      
      // Get the store type.
      ISA_STORE_TYPE sType = tok_scoreboard.getStoreType(tok.index);
      
      // Make the request to the memory state.
      link_to_dmem.makeReq(MEMSTATE_REQ_STORE {token: tok, addr: addr, val: val, store_type: sType});

      // Update the scoreboard.
      tok_scoreboard.storeFinish(tok.index);

      // Make the response to the timing model. End of macro-operation.
      link_doStores.makeResp(tok);

    endrule

    // doStores4
    
    // When:   Some time after doStores3 occurs
    // Effect: Get the store response from the memory state and discard it.
    //         Note that we have already replied to the timing model in doStores3.

    rule doStores4 (ready &&& link_to_dmem.getResp() matches tagged MEMSTATE_RSP_STORE);

      // Simply discard the response.
      link_to_dmem.deq();

    endrule

    // ******* commitResults ******* //

    // 2 stage macro operation which commits all local results.
    // If a token has more than 1 destination, an additional stage commits them.

    // When:   When the timing model requests it.
    // Effect: For each allocated physical register destination, we free the "old writer"
    //         of that destination. Or, if there was no destination, free the "dummy" register.
    // Soft Inputs:  Token
    // Soft Returns: Token
    
    // commitResults1

    // When:   When the timing model starts a commitResults().
    // Effect: Lookup the destinations of this token, and the registers to free.

    rule commitResults1 (ready);

        // Get the input from the timing model. Begin macro-operation.
        let tok = link_commitResults.getReq();
        link_commitResults.deq();

        // Log it.
        funcpDebug($fwrite(debug_log, "TOKEN %0d: commitResults: Starting.", tok.index)); 
        
        // Update the scoreboard.
        tok_scoreboard.commitStart(tok.index);
        
        // Request the registers to be freed.
        tok_regs_to_free.read_req(tok.index);

        // Pass to the next stage.
        comm_q.enq(tok);

    endrule

    // commitResults2
    
    // When:   After a commitResults1 AND commitResultsAdditional is not occuring.
    // Effect: Free the appropriate physical register and respond to the timing model.
    //         If there is more work to do, the next rule will handle it.
    //         Note that it is safe to "short path" the response because the committing of more
    //         results has higher priority than starting the commit of a new token.

    // There are more registers to free if any member of the the vector is valid.

    Bool moreRegsToFree = Vector::any(isValid, additional_regs_to_free);

    rule commitResults2 (ready && !moreRegsToFree);

        // Get the input from the previous stage.
        let tok = comm_q.first();
        comm_q.deq();

        // Retrieve the registers to be freed.
        let regsToFree  <- tok_regs_to_free.read_resp();

        // Go ahead and free the first register, if present.
        case (regsToFree[0]) matches
            tagged Invalid:  noAction;
            tagged Valid .r: freelist.free(r);
        endcase

        // Store all the remaining register names for a later stage to handle.
        additional_regs_to_free <= tail(regsToFree);

        // Update the scoreboard so the token can be reused.
        tok_scoreboard.deallocate(tok.index);

        // Respond to the timing model. End of macro-operation (except any more registers below).
        link_commitResults.makeResp(tok);

    endrule

    // commitResultsAdditional
    
    // When:   After a commitResults2 AND there are more physical registers to free.
    // Effect: Free the appropriate physical register.
 
    // Elaborated Rule: N copies, where N is the maximum number of dests minus 1 (which was handled already).
    
    for (Integer x = 0; x < valueof(ISA_MAX_DSTS) - 1; x = x + 1)
    begin
        rule commitResultsAdditional (additional_regs_to_free[x] matches tagged Valid .r);

            freelist.free(r);
            additional_regs_to_free[x] <= tagged Invalid;

        endrule
    end

    // ******* commitStores ******* //

    // 1-stage macro operation which commits global stores.

    // When:   When the timing model requests it.
    // Effect: Tell the memory state to make a store globally visible.
    // Soft Inputs:  Token
    // Soft Returns: Token
    
    rule commitStores (ready);

        // Get the input from the timing model. Begin macro-operation.
        let tok = link_commitStores.getReq();
        link_commitStores.deq();

        // If the token was not actually a store, it's an exception.
        let isStore = tok_scoreboard.isStore(tok.index);
        assert_commited_store_is_actually_store(isStore);

        // Log it.
        funcpDebug($fwrite(debug_log, "TOKEN %0d: commitStores: Committing.", tok.index)); 

        link_mem_commit.send(tok);

        // Respond to timing model. End of macro-operation.
        link_commitStores.makeResp(tok);

    endrule

    // ******* rewindToToken ******* //

    // 2-stage macro operation which undoes the effects of tokens by backing up the maptable.

    // When:   When the timing model requests it.
    // Effect: If we have a snapshot we can quickly back up to that snapshot. Otherwise we XXX
    // Soft Inputs:  Token
    // Soft Returns: None

    // rewindToToken1

    // When:   When the timing model starts a rewindToToken()
    // Effect: Lookup the destinations of this token, and the registers to free.

    rule rewindToToken1 (ready);
      
        // Get the input from the timing model.
        let tok = link_rewindToToken.receive();
        link_rewindToToken.deq();

        // Log it.
        funcpDebug($fwrite(debug_log, "Rewind: Starting Rewind to TOKEN %0d (Youngest: %0d)", tok.index, tok_scoreboard.youngest())); 

        // Tell the memory to drop non-committed stores.
        link_mem_rewind.send(tuple2(tok.index, tok_scoreboard.youngest()));

        // Rewind the scoreboard.
        tok_scoreboard.rewindTo(tok.index);

        // Update the epoch so we can discard appropriate updates.
        epoch <= epoch + 1;

        // Check to see if we have a snapshot.

        Bool found = False;
        if (snap_valids[tok.index]) // There's a chance we have a snapshot
        begin

          // Log it.
          funcpDebug($fwrite(debug_log, "Potential Fast Rewind"));

          // Find the most recent snapshot of this token

          // Start at oldest entry in the list.        
          FUNCP_SNAPSHOT_INDEX idx = snap_next;

          for (Integer x = 0; x < valueof(TExp#(snapshotptr_SZ)); x = x + 1)
          begin
              // We look the list at an offset from the oldest entry.
              let cur = snap_next + fromInteger(x);

              // If the entry we examine is of the appropriate token, we've found a candidate!
              match {.new_idx, .new_found} = (snap_ids[cur] == tok.index) ? tuple2(cur, True) : tuple2(idx, found);
              found = new_found;
              idx = new_idx;

          end

          // Alright did we find anything?

          if (found)
          begin 
              // Log our success!
              funcpDebug($fwrite(debug_log, "Fast Rewind confirmed with Snapshot %0d", idx));

              // Retrieve the snapshots.
              snaps.read_req(idx);
              snaps_fl.read_req(idx);
          end

        end

        if (!found)
        begin

            // Log our failure.
            funcpDebug($fwrite(debug_log, "Initiating slow Rewind (Oldest: %0d)", tok_scoreboard.oldest()));  

        end

        // Temporarily disable all the other operations.
        rewinding <= True;

        // Are we going fast or slow?
        fast_rewind <= found;

        // ??? XXX
        rewindTok <= tok_scoreboard.youngest();

        // Start at the oldest and go forward.
        rewindCur <= tok_scoreboard.oldest();

    endrule

    // rewindToToken2

    // When:   After rewindToToken1 AND we have a snapshot.
    // Effect: Use the snapshot to overwrite existing values. Reply to the timing partition.

    rule rewindToToken2 (rewinding && fast_rewind);

        // Get the snapshots.
        let snp_map <- snaps.read_resp();
        let snp_fl  <- snaps_fl.read_resp();

        // Update the maptable.
        maptable <= snp_map;
        
        // Update the freelist.
        freelist.backTo(snp_fl);

        // Log it.
        funcpDebug($fwrite(debug_log, "Fast Rewind finished."));  

        // We're done. End of macro-operation (path 1).
        rewinding <= False;

    endrule

    //Slow rewind. Walk the tokens in age order
    //and reconstruct the maptable

    rule rewindToTokenSlow1 (rewinding && !fast_rewind);
    
      // Don't remap killed tokens
      if (tok_scoreboard.isAllocated(rewindCur))
      begin
          // Log it.
          funcpDebug($fwrite(debug_log, "Slow Rewind: Lookup TOKEN %0d", rewindCur));  
          // Look up the destinations
          tok_dsts.read_req2(rewindCur);
          // Pass it to the next stage who will free it.
          rewind_q.enq(rewindCur);
      end

      rewindCur <= rewindCur + 1;

      if (rewindCur == rewindTok) //Must take into account the last instruction
      begin
        rewinding <= False;
        funcpDebug($fwrite(debug_log, "Slow Rewind: No more tokens to lookup."));  
      end

    endrule

    // Urgency
    
    // A total ordering of all non-trivial rules in the system specifying who should get to
    // proceed in the case of a conflict. The logic here is straightforward. In terms of
    // macro-operations, the "later" operations are favored:
    
    // newInFlight < getInst < getDeps < getResult < doLoads < doStores < commitResults < commitStores
    
    // Thus getResults() should be favored over getDeps().
    
    // Within a single macro-operation a similar philosophy holds: favor the later stages 
    // of the pipeline. Thus:
    
    // doLoads1 < doLoads2 < doLoads3
    
    // This is _particularly_ important for the getDeps stages, which modify the maptable.
    
    // We specify all of this as a TOTAL ORDER, which is tedious, but guaranteed to be complete.
    
    // Do not change the following lines unless you understand all this and have a good reason.

    (* descending_urgency= "rewindToTokenSlow2, rewindToTokenSlow1, rewindToToken2, rewindToToken1, commitStores, commitResults2, commitResults1, doStores4, doStores3, doStores2, doStores1, doLoads3, doLoads2, doLoads1, getResults4, getResults3, getResults2StallEnd, getResults2, getResults1, getDependencies2AdditonalMappings, getDependencies2, getDependencies1, getInstruction2, getInstruction1, newInFlight" *)

    rule rewindToTokenSlow2 (True);

      let t = rewind_q.first();
      rewind_q.deq();


      freelist.back();
      let dst  <- tok_dsts.read_resp2();
      let inst <- tok_inst.read_resp2();
      // Commented out for now:
      
      /*
      case (getDest(inst)) matches
        tagged Invalid: funcpDebug($fwrite(debug_log, "Slow Rewind: TOKEN %0d had no dest", t));
        tagged Valid .d:
        begin
            funcpDebug($fwrite(debug_log, "Slow Rewind: TOKEN %0d: Remapping (R%0d/PR%0d)", t, d, dst));
            maptable[d] <= dst;
        end
      endcase
      */
    endrule
 
endmodule

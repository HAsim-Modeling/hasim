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
`include "funcp_memory.bsh"

// Functional Partition includes.

`include "funcp_regstate_scoreboard.bsh"
`include "funcp_regstate_freelist.bsh"
`include "funcp_memstate_manager.bsh"

// ISA includes

`include "hasim_isa.bsh"
`include "hasim_isa_datapath.bsh"

// Dictionary includes
`include "asim/dict/ASSERTIONS_REGMANAGER.bsh"

// RRR includes
`include "asim/provides/rrr.bsh"
`include "asim/rrr/service_ids.bsh"

`include "asim/provides/isa_emulator.bsh"

// ***** Typedefs ***** //

// FUNCP_SNAPSHOT_INDEX

// The index into the snapshots, as defined by the parameter.

typedef Bit#(TLog#(`REGSTATE_NUM_SNAPSHOTS)) FUNCP_SNAPSHOT_INDEX;

// MEM_PATH

// A type to distinguish where load responses should be sent.

typedef enum
{
  PATH_INST,
  PATH_LOAD,
  PATH_STORE
}
  MEM_PATH
      deriving (Eq, Bits);

// TEMPORARY: Define our RRR Method IDs by hand for now.
`define UPDATE_REGISTER_METHOD_ID 0
`define EMULATE_INSTRUCTION_METHOD_ID 1



// mkFUNCP_RegStateManager

// The manager of the register state, and the bulk of the work of the functional partition.

module [HASim_Module] mkFUNCP_RegStateManager
    //interface:
                ()
    provisos
            (Bits#(TOKEN_INDEX, idx_SZ),      // The number of tokens.
             Bits#(ISA_REG_INDEX, rname_SZ),  // The number of architectural registers.
             Bits#(ISA_VALUE, isa_val_SZ), // The width of arch regs.
             Bits#(FUNCP_SNAPSHOT_INDEX, snapshotptr_SZ)); // The number of snapshots.

    // ******* Debuging State *******

    // Fake register to hold our debugging file descriptor.
    let debugLog     <- mkReg(InvalidFile);

    // The current FPGA clock cycle
    Reg#(Bit#(32)) fpgaCC <- mkReg(0);

    // A convenience function for debugging.

    function Action funcpDebug(Action a);
    action

      $fwrite(debugLog, "[%d]: ", fpgaCC);
      a;
      $fwrite(debugLog, "\n");

    endaction
    endfunction

    // ******* Submodules *******

    // The Token State is a big scoreboard which tracks the status of inflight tokens.
    let tokScoreboard <- mkFUNCP_Scoreboard();

    // The Freelist tracks which physical registers are available.
    let freelist <- mkFUNCP_Freelist(debugLog, fpgaCC);

    // ******* Local State *******

    // Tables to track info about in-flight instructions.

    // The address we got the instruction from (told to us by the timing model).
    BRAM#(TOKEN_INDEX, ISA_ADDRESS)     tokAddr <- mkBRAM_Full();

    // The instruction that was at that address (from mem_state).
    BRAM_2#(TOKEN_INDEX, ISA_INSTRUCTION) tokInst <- mkBRAM_2_Full();

    // The destinations of the instruction (a convenience which saves us from reading the instruction/maptable). 
    BRAM_3#(TOKEN_INDEX, Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) tokDsts <- mkBRAM_3_Full();

    // If an instruction has sources in other inflight instructions it will be noted here.
    BRAM#(TOKEN_INDEX, Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)))   tokWriters <- mkBRAM_Full();

    // The memaddress is used by Loads/Stores so we don't have to repeat the calculation.
    BRAM_2#(TOKEN_INDEX, ISA_ADDRESS) tokMemAddr <- mkBRAM_2_Full();

    // The physical registers to free when the token is committed/killed.
    BRAM#(TOKEN_INDEX, Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) tokRegsToFree <- mkBRAM_Full();

    // The Physical Register File

    BRAM_3#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE) prf <- mkBRAM_3_Full();
    
    // Valid bits for PRF
    Vector#(FUNCP_PHYSICAL_REGS, Reg#(Bool)) prfValids = newVector();
    
    for (Integer x = 0; x < valueOf(FUNCP_PHYSICAL_REGS); x = x + 1)
    begin
      prfValids[x] <- mkReg(False);
    end

    // The Map Table

    // This gets pounded nearly every FPGA cycle, so it's NOT in RAM.
    // Also this lets us snapshot/reload the entire maptable in a single cyle.

    // The highest register in the ISA (the last one which is initially valid).
    Bit#(rname_SZ)            highestReg = maxBound;
    FUNCP_PHYSICAL_REG_INDEX maxInit = zeroExtend(pack(highestReg));

    // The initial map is that all architectural registers are mapped 1-to-1 to
    // physical registers and are all valid.

    Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX) initMap = newVector();
    
    // Note: this loop ends at _architectural_ register size.
    
    for (Integer x  = 0; x < valueof(TExp#(rname_SZ)); x = x + 1)
    begin
      initMap[x] = fromInteger(x);
    end

    Reg#(Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX)) maptable   <- mkReg(initMap);

    // Snapshots 
    // Allow for fast rewinds.

    // The valid bits tell us which location contains a valid snapshot.
    Reg#(Vector#(TExp#(idx_SZ), Bool))                snapValids     <- mkReg(replicate(False));

    // The IDs tell us which snapshot is in a given location.
    Reg#(Vector#(TExp#(snapshotptr_SZ), TOKEN_INDEX)) snapIDs        <- mkRegU();
    
    // The next pointer points to the next location where we should write a snapshot.
    // (Possibly overwriting an old snapshot, which is okay.)
    Reg#(FUNCP_SNAPSHOT_INDEX)                          snapNext       <- mkReg(0);

    // The actual snapshots of the entire maptable.
    BRAM#(FUNCP_SNAPSHOT_INDEX, Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX)) snaps    <- mkBRAM_Full();

    // An additional snapshot of the location of the freelist.
    BRAM#(FUNCP_SNAPSHOT_INDEX, FUNCP_PHYSICAL_REG_INDEX)                           snapsFL <- mkBRAM_Full();

    // ******* Miscellaneous *******

    // Are we currently doing a "rewind"?
    Reg#(Bool)     rewinding <- mkReg(False);

    // Is it a fast rewind or a slow one?
    Reg#(Bool)     fastRewind <- mkReg(False);

    // Are we initializing after a reset?
    Reg#(Bool)     initializing <- mkReg(True);

    // This register stores the current Phys Reg we are initializing.
    Reg#(FUNCP_PHYSICAL_REG_INDEX)   initCur <- mkReg(0);

    // These support "slow rewinds" which are currently not quite right.
    Reg#(TOKEN_INDEX) rewindTok <- mkRegU();
    Reg#(TOKEN_INDEX) rewindCur <- mkRegU();

    // The Epoch tells us when to discard junk tokens that were in flight when the timing partition killed them.
    Reg#(TOKEN_TIMEP_EPOCH) epoch <- mkReg(0);

    // Stall info for the getDeps operation.
    Reg#(Maybe#(Tuple5#(TOKEN, ISA_DST_INDEX, ISA_SRC_MAPPING, ISA_DST_MAPPING, ISA_INST_DSTS))) finishDeps <- mkReg(tagged Invalid);

    // Is the getResult stage stalling?
    Reg#(Bool)     execStalling <- mkReg(False);
    // The token we're stalling on.
    Reg#(TOKEN)    execStallTok <- mkRegU();
    // Record the writers we've requested so far while stalling.
    Reg#(Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) execStallWriters <- mkReg(Vector::replicate(tagged Invalid));
    // Record the values that have come back while stalling.
    Reg#(Vector#(TSub#(ISA_MAX_SRCS, 2), Maybe#(ISA_VALUE))) execStallValues <- mkReg(Vector::replicate(tagged Invalid));
 
    // Is the getResult stage writing back more values?
    Reg#(Bool) execWritebackMore <- mkReg(False);
    // The token we're stalling on.
    Reg#(TOKEN)    execWritebackTok <- mkRegU();
    // Record the values for writeback.
    Reg#(Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE)))) execWritebackValues <- mkReg(Vector::replicate(tagged Invalid));
    // Record the result for the timing model while we're writing back.
    Reg#(ISA_EXECUTION_RESULT) execWritebackResult <- mkRegU();
    
    // Are we pausing to emulate an instruction in software?
    Reg#(Bool) emulatingInstruction <- mkReg(False);
    // Which token's instruction are we emulating?
    Reg#(TOKEN) emulatingToken <- mkRegU();
    // Are we synchronizing are registers with software?
    Reg#(Bool) synchronizingRegs <- mkReg(False);
    // Which register are we currently synchronizing?
    Reg#(Bit#(rname_SZ)) synchronizingCurReg <- mkReg(minBound);
            
    // We are only ready to go if we are neither rewinding, initializing, nor emulating
    let ready = !rewinding && !initializing && !emulatingInstruction;

    // Does the commit stage have to free more registers?
    Reg#(Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(FUNCP_PHYSICAL_REG_INDEX))) additionalRegsToFree <- mkReg(Vector::replicate(tagged Invalid));
    
    // This queue records where load responses should be sent.
    FIFO#(MEM_PATH) memPathQ <- mkSizedFIFO(16);
    
    // These Queues are intermediate state between the pipeline stages.

    FIFO#(TOKEN) instQ  <- mkFIFO();
    FIFO#(TOKEN) deps1Q <- mkFIFO();
    FIFO#(TOKEN) deps2Q <- mkFIFO();
    FIFO#(TOKEN) res1Q  <- mkFIFO();
    FIFO#(TOKEN) res2Q <- mkFIFO();
    FIFO#(TOKEN) res3Q   <- mkFIFO();
    FIFO#(Bit#(rname_SZ)) syncQ <- mkFIFO();
    FIFO#(TOKEN) load1Q  <- mkFIFO();
    FIFO#(Tuple2#(TOKEN, MEM_ADDRESS)) load2Q  <- mkFIFO();
    FIFO#(TOKEN) store1Q <- mkFIFO();
    FIFO#(TOKEN) store2Q <- mkFIFO();
    FIFO#(Tuple5#(TOKEN, MEM_ADDRESS, ISA_ADDRESS, ISA_VALUE, ISA_MEMOP_TYPE)) store3Q <- mkFIFO();
    FIFO#(TOKEN) commQ   <- mkFIFO();
    FIFO#(TOKEN_INDEX) rewindQ <- mkFIFO();

    // ******* Soft Connections *******

    // Request type is top line.
    // Response type is bottom line.

    // Connections to the timing partition.

    Connection_Server#(void, 
                       TOKEN)                                  linkNewInFlight <- mkConnection_Server("funcp_newInFlight");

    Connection_Server#(Tuple2#(TOKEN, ISA_ADDRESS),
                       Tuple2#(TOKEN, ISA_INSTRUCTION))        linkGetInst   <- mkConnection_Server("funcp_getInstruction");

    Connection_Server#(TOKEN, 
                       Tuple2#(TOKEN, ISA_DEPENDENCY_INFO))    linkGetDeps   <- mkConnection_Server("funcp_getDependencies");

    Connection_Server#(TOKEN, 
                       Tuple2#(TOKEN, ISA_EXECUTION_RESULT))   linkGetResults <- mkConnection_Server("funcp_getResults");

    Connection_Server#(TOKEN, 
                       TOKEN)                                  linkDoLoads   <- mkConnection_Server("funcp_doLoads");

    Connection_Server#(TOKEN, 
                       TOKEN)                                  linkDoStores  <- mkConnection_Server("funcp_doSpeculativeStores");

    Connection_Server#(TOKEN,
                       TOKEN)                                  linkCommitResults <- mkConnection_Server("funcp_commitResults");

    Connection_Server#(TOKEN,
                       TOKEN)                                  linkCommitStores  <- mkConnection_Server("funcp_commitStores");  

    Connection_Receive#(TOKEN)                                 linkRewindToToken <- mkConnection_Receive("funcp_rewindToToken");


    // Connections to Mem State.

    Connection_Client#(MEMSTATE_REQ, 
                       MEM_VALUE)                              linkToMem <- mkConnection_Client("funcp_memstate");

    Connection_Send#(TOKEN)                                    linkMemCommit <- mkConnection_Send("funcp_mem_commit");

    Connection_Send#(Tuple2#(TOKEN_INDEX, 
                             TOKEN_INDEX))                     linkMemRewind <- mkConnection_Send("funcp_mem_rewind");

    // Connection to Datapath.

    Connection_Client#(Tuple3#(ISA_INSTRUCTION, ISA_ADDRESS, ISA_SOURCE_VALUES), 
                       Tuple3#(ISA_EXECUTION_RESULT, ISA_ADDRESS, ISA_RESULT_VALUES)) linkToDatapath <- mkConnection_Client("isa_datapath");
    
    // Connections to RRR
    
    Connection_Send#(RNAME_RVAL_TUPLE) linkRRRSync <- mkConnection_Send("rrr_client_ISA_EMULATOR_sync");
    Connection_Client#(INST_ISA_ADDR_TUPLE,
                       ISA_ADDRESS) linkRRREmulate <- mkConnection_Client("rrr_client_ISA_EMULATOR_emulate");
    Connection_Receive#(RNAME_RVAL_TUPLE) linkRRRUpdate <- mkConnection_Receive("rrr_server_ISA_EMULATOR_updateRegister");
    

    // ***** Assertion Checkers ***** //

    Assertion assertInstructionIsActuallyALoad    <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_LOAD_ON_NONLOAD, ASSERT_WARNING);
    Assertion assertLoadDestRegIsReady            <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_MALFORMED_LOAD_WRITEBACK, ASSERT_ERROR);
    Assertion assertInstructionIsActuallyAStore   <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_STORE_ON_NONSTORE, ASSERT_WARNING);
    Assertion assertCommitedStoreIsActuallyAStore <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_COMMIT_STORE_ON_NONSTORE, ASSERT_WARNING);
    Assertion assertRegUpdateAtExpectedTime       <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_UNEXPECTED_REG_UPDATE, ASSERT_WARNING);
    Assertion assertEmulationFinishedAtExpectedTime <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_UNEXPECTED_EMULATION_FINISHED, ASSERT_WARNING);

    // ******* Rules *******

    // initialize

    // When:    Only at the beginning of time (after a reset).
    // Effects: Makes sure all RAMS are in the right state before we begin computing.
    //          Additionally the first time it runs it will open the debug logfiles.

    rule initialize (initializing);

        //Open the debug logs. (First time only. Afterwards it is not InvalidFile.)

        if (debugLog == InvalidFile)
        begin
            let fd <- $fopen(`REGSTATE_LOGFILE_NAME, "w");

            if (fd == InvalidFile)
            begin
                $display(strConcat("Error opening FUNCP RegState logfile ", `REGSTATE_LOGFILE_NAME));
                $finish(1);
            end

            debugLog <= fd;
        end

        // For safety we start all physical registers at zero. In the future this might change.
        prf.write(initCur, 0);
        prfValids[initCur] <= True;
        
        // We're done if we've initialized the last register.
        initializing <= (initCur < maxInit);
        initCur <= initCur + 1;

    endrule
  
    // currentCC
    // When:   Always
    // Effect: Just record the current FPGA cycle for debugging purposes.

    rule currentCC (True);

        fpgaCC <= fpgaCC + 1;

    endrule

    // ******* newInFlight ******* //

    // 1-stage macro-operation
    
    // When:         The timing model tells us to allocate a new in-flight instruction.
    // Effect:       Allocates a slot on the token state scoreboard.
    // Soft Inputs:  req from timing model
    // Soft Returns: a TOKEN which the timing model can use to refer to that slot.

    rule newInFlight (ready);

        // Get the input from the timing model. Begin macro operation.
        let x = linkNewInFlight.getReq();
        linkNewInFlight.deq();
        
        // Get the next token from the scoreboard.
        let idx <- tokScoreboard.allocate();
        
        // Log it.
        
        funcpDebug($fwrite(debugLog, "NewInFlight: Allocating TOKEN %0d", idx));
        
        // Zero out our scratchpad.
        let inf = TOKEN_FUNCP_INFO {epoch: 0, scratchpad: 0};

        // The timing partition scratchpad must be filled in by up.
        let newtok = TOKEN {index: idx, timep_info: ?, funcp_info: inf};

        // Respond to the timing partition. End of macro operation.
        linkNewInFlight.makeResp(newtok);

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
        match {.tok, .addr} = linkGetInst.getReq();
        linkGetInst.deq();

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: FETCH: Start (Address: 0x%h)", tok.index, addr));

        // Update scoreboard.
        tokScoreboard.fetStart(tok.index);

        // Record the address. (For relative branches, etc.)
        tokAddr.write(tok.index, addr);

        // Transform the Address from an ISA address to an actual address 
        // using the ISA-provided conversion function.
        MEM_ADDRESS mem_addr = isaAddressToMemAddress(addr);

        // Kick to Mem State.
        linkToMem.makeReq(MEMSTATE_REQ_LOAD {token: tok, addr: mem_addr});
        
        // Record that the result should come to us.
        memPathQ.enq(PATH_INST);

        // Send on to getInstruction2.
        instQ.enq(tok);

    endrule

    // getInstruction2

    // When:   Some time after fetch1.
    // Effect: Record the instruction, kick back to timing model.

    rule getInstruction2 (ready && memPathQ.first() == PATH_INST);

        // Input from previous stage.
        let tok = instQ.first();
        instQ.deq();
        memPathQ.deq();

        // Get resp from the Mem State.
        MEM_VALUE v = linkToMem.getResp();
        linkToMem.deq();

        // Convert the value to an instruction using the ISA-provided conversion
        // function.
        ISA_INSTRUCTION inst = isaInstructionFromMemValue(v);

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: Fetch: End (INSTRUCTION: 0x%h)", tok.index, inst));

        // Record the instruction.
        tokInst.write(tok.index, inst);

        // Update scoreboard.
        tokScoreboard.fetFinish(tok.index);

        // Send response to timing partition. End of macro-operation.
        linkGetInst.makeResp(tuple2(tok, inst));

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
        let tok = linkGetDeps.getReq();
        linkGetDeps.deq();
        
        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: getDeps: Start", tok.index));
        
        // Update the status.
        tokScoreboard.decStart(tok.index);
        
        // Retrieve the instruction.
        tokInst.read_req1(tok.index);

        // Everyone gets a Physical Register, even if they don't have a destination.
        // Otherwise we would need another stage here.
        freelist.forwardReq();

        // Pass on to stage 2.
        deps1Q.enq(tok);

    endrule

    // getDependencies2
    // When:   After getDependencies1 has occured. Note that we allow this to proceed with "junk" tokens.
    // Effect: Use the maptable to lookup sources, then update it to include one of our dests.
    //         If an instruction has more than one dest then the third stage will occur,
    //         otherwise this rule itself will return the result to the timing model.

    rule getDependencies2 (!initializing &&& finishDeps matches tagged Invalid);

        // Get the info from the previous stage.
        let tok = deps1Q.first();
        deps1Q.deq();

        //Get the info the previous stage requested.
        let inst     <- tokInst.read_resp1();
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

        for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
        begin
          // Get the architectural dst from the ISA.
          let arc_dst = isaGetDst(inst, x);
          // Update the vectors.
          arc_dsts[x] = arc_dst;
          map_dsts[x] = case (arc_dst) matches
                           tagged Invalid:  tagged Invalid;
                           tagged Valid .r: tagged Valid tuple2(r, new_preg); //This could be overwritten by the next stage.
                       endcase;
        end

        // Unfortunately we can only record one physical dest here, since we only got one from
        // the freelist. If the instruction has more we will invoke additional stages.

        phy_dsts[0] = tagged Valid new_preg;

        // If we have a dest, update the maptable with the correct physical register.

        let new_map = case (arc_dsts[0]) matches
            tagged Invalid:  return maptable;
            tagged Valid .d: return update(maptable, pack(d), new_preg);
          endcase;

        if (tok.timep_info.epoch == epoch) //Don't update the maptable if this token is getting killed
        begin

            maptable <= new_map;
            // Also we must reset the physical register dest to Invalid.
            prfValids[new_preg] <= False;
        end
        else
        begin

            // Unallocate the register we just got.
            freelist.back();
            //Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: JUNK TOKEN (NO UPDATE)", tok.index));

        end

        // The phyRegToFree is the physical register which gets freed when we are committed/killed.
        // If we have a dest, this register is the old writer of the register.
        // Otherwise the dest we requested in stage 1 is a dummy.

        phy_regs_to_free[0] = case (arc_dsts[0]) matches
                                 tagged Invalid:  tagged Valid new_preg; // Free the dummy when you free this token.
                                 tagged Valid .d: tagged Valid select(maptable, pack(d)); // Free the actual old writer.
                              endcase;

        // Update the token tables with all this information.
        tokRegsToFree.write(tok.index, phy_regs_to_free);
           tokWriters.write(tok.index, phy_srcs);
              tokDsts.write(tok.index, phy_dsts);

        // Use the scoreboard to record other relevant info.
        if (isaIsLoad(inst))
            tokScoreboard.setLoadType(tok.index, isaLoadType(inst));

        if (isaIsStore(inst))
            tokScoreboard.setStoreType(tok.index, isaStoreType(inst));
            
        tokScoreboard.setEmulation(tok.index, isaEmulateInstruction(inst));

        // Make a snapshot for branches.
        // Note that there is an implicit assumption here that no branch instruction has more than one destination.  

        if (isaIsBranch(inst))
        begin

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: getDeps: Branch Detected. Making Snapshot (Number %0d).", tok.index, snapNext));

            // Mark the snap as valid.
            snapValids[tok.index] <= True;

            // Record which token is at this snapshot.
            snapIDs[snapNext] <= tok.index;

            // Snapshot the maptable.
            snaps.write(snapNext, new_map);

            // Snapshot the freelist.
            snapsFL.write(snapNext, freelist.current());

            // Increment the current snapshot pointer (with overflow, but we don't care if we erase an old snapshot.)
            snapNext <= snapNext + 1;
        end


        // If there was one dest or less, we are done.
        
        let num_dsts = isaGetNumDsts(inst);
        
        if (num_dsts <= 1)
        begin

            // Log all source mappings.
            for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
            begin
              case (map_srcs[x]) matches
                  tagged Invalid: funcpDebug($fwrite(debugLog, "TOKEN %0d: getDeps: No Source %0d.", tok.index, fromInteger(x)));
                  tagged Valid {.ar, .pr}: funcpDebug($fwrite(debugLog, "TOKEN %0d: getDeps: Source %0d Mapped (%0d/%0d).", tok.index, fromInteger(x), ar, pr));
              endcase
            end

            // Log the dest mapping
            case (map_dsts[0]) matches
                tagged Invalid: funcpDebug($fwrite(debugLog, "TOKEN %0d: getDeps: No Destination.", tok.index));
                tagged Valid {.ar, .pr}: funcpDebug($fwrite(debugLog, "TOKEN %0d: getDeps: Destination 1 Mapped (%0d/%0d).", tok.index, ar, pr));
            endcase
            
            // Update the scoreboard.
            tokScoreboard.decFinish(tok.index);

            // Return everything to the timing partition. End of macro-operation (path 1).
            linkGetDeps.makeResp(tuple2(tok, tuple2(map_srcs, map_dsts)));
        end
        else // Not done.
        begin 
            // Request another phys reg
            freelist.forwardReq();
            // Pass it along to the next stage.
            finishDeps <= tagged Valid tuple5(tok, fromInteger(num_dsts - 1), map_srcs, map_dsts, phy_regs_to_free);
        end

    endrule

    // getDependencies2AdditionalMappings
    // When:   When an instruction in the previous stage had more than one destination.
    // Effect: Keep allocating destinations until you've got them all.
    
     rule getDependencies2AdditonalMappings (!initializing &&& finishDeps matches tagged Valid {.tok, .num, .map_srcs, .map_dsts, .phy_regs_to_free});
      
        // Get the new phys reg.
        let phy_dst <- freelist.forwardResp();

        // The new mapping.
        match {.arc_dst, .dummy} = validValue(map_dsts[num]); // Perhaps we should assert that this is valid?
        let new_map_dsts = update(map_dsts, num, tagged Valid tuple2(arc_dst, phy_dst));

        // The reg to free is the old writer of this destination.
        let new_phy_regs_to_free = update(phy_regs_to_free, num, tagged Valid select(maptable, pack(arc_dst)));

        if (tok.timep_info.epoch == epoch) //Don't update the maptable if this token is getting killed
        begin

            // Update the maptable.
            maptable <= update(maptable, pack(arc_dst), phy_dst);
            // Reset the reg to unready.
            prfValids[phy_dst] <= False;
        end
        else
        begin
            //Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: JUNK TOKEN (NO ADDITIONAL UPDATE)", tok.index));
        end

        if (num > 0) // We're not done yet;
        begin

            // Get a new physical reg for the next time around.
            freelist.forwardReq();

            // Update the status register for the next time around.
            finishDeps <= tagged Valid tuple5(tok, num - 1, map_srcs, new_map_dsts, new_phy_regs_to_free);
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

            // Invalidate the reg. so we don't do this again.
            finishDeps <= tagged Invalid;

            // Update the scoreboard.
            tokScoreboard.decFinish(tok.index);

            // Marshall up the dependencies for the timing model.
            let final_deps = tuple2(map_srcs, new_map_dsts);

            // Return everything to the timing partition. End of macro-operation (path 2).
            linkGetDeps.makeResp(tuple2(tok, final_deps));

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

    rule getResults1 (ready && !emulatingInstruction);

        // Get parameter from the timing model. Begin macro-operation.
        let tok = linkGetResults.getReq();
        linkGetResults.deq();

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Start", tok.index));

        // Update the scoreboard.
        tokScoreboard.exeStart(tok.index);
        
        if (tokScoreboard.emulateInstruction(tok.index))
        begin
            
            emulatingInstruction <= True;
            emulatingToken <= tok;
            synchronizingRegs <= True;
            synchronizingCurReg <= minBound;
            
        end
        else
        begin
        
            // Look up the writers.
            tokWriters.read_req(tok.index);

            // Pass it along to the next stage.
            res1Q.enq(tok);
        
        end

    endrule

    // getResults2
    // When:   After getResults1.
    // Effect: Use the writers to look up values from the PRF. 
    //         Also retreive the instruction itself and the PC.
    //         If the writers are not all ready then a stall can occur.

    rule getResults2 (ready && !execStalling);

        // Get input from getResults1.
        let tok = res1Q.first();
        res1Q.deq();

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Reg Read", tok.index));

        // Response from previous stage.
        let ws <- tokWriters.read_resp();

        // We are ready when all the source PRs are valid.

        Bool ready = True;
        for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
        begin
            let src_is_ready = case (ws[x]) matches
                                  tagged Invalid:  True; // No writer, so it is ready.
                                  tagged Valid .v: prfValids[v]; // Ready if phys reg is valid.
                               endcase;
            ready = ready && src_is_ready; // We are ready when ALL are ready.
        end

        // Additionally let junk proceed
        let is_junk = !tokScoreboard.isAllocated(tok.index);

        // Log it.
        if (is_junk)
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Letting Junk Proceed!", tok.index));


        if (ready || is_junk) // Go ahead and pass it to the next stage.
        begin

            // Request the first 2 sources (harmless if they don't exist).
            prf.read_req1(validValue(ws[0]));
            prf.read_req2(validValue(ws[1]));

            // Also look up the PC of the instruction and the instruction itself.
            tokAddr.read_req(tok.index);
            tokInst.read_req2(tok.index);

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Reg Read Complete", tok.index));

            // Pass on to the next stage.
            res2Q.enq(tok);
        end
        else // We're stalling.
        begin
            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Reg Read Stalling!", tok.index));

            execStalling <= True;
            execStallTok <= tok;
            execStallWriters <= ws;
            execStallValues <= Vector::replicate(tagged Invalid);
        end

    endrule
    
    // getResults2Stall
    
    // When:   Occurs when a getResults2 stalls because of one of two reasons.
    //         A) An operation has more than 2 sources.
    //         B) An operation's sources were not all ready.
    // Effect: Once all the sources are ready, retrieve them and send them on to the next stage.
    
    // Elaborated Rule: N copies, where N is the maximum number of dests minus 2.
    //                  These two are handled at the end when we pass on to the next stage.

    for (Integer x = 0; x < (valueof(ISA_MAX_SRCS) - 2); x = x + 1)
    begin
    
      // This rule ensures that if there is no writer, it is marked ready to go.
      rule getResults2StallPass (execStalling && !isValid(execStallValues[x]) &&& execStallWriters[x] matches tagged Invalid);
          execStallValues[x] <= tagged Valid(?);
      endrule
      
      // If there was a writer, when the RF is ready we request it and send it onwards.
      rule getResults2StallReq (execStalling && !isValid(execStallValues[x]) &&& 
                                execStallWriters[x] matches tagged Valid .r &&& 
                                prfValids[r]);
          prf.read_req1(r);
      endrule
      
      // Get the response and record it.
      rule getResults2StallRsp (execStalling);
      
        let v <- prf.read_resp1();
        execStallValues[x] <= tagged Valid v;
      
      endrule
      
      // Note: in the future these rules could be expanded to also use PRF port 2.
    
    end
    
    // Some helper functions to determine when we're done stalling.
    
    let noMoreVectorStalls = Vector::all(isValid, execStallValues);
    let src1IsRdy = case (execStallWriters[0]) matches
                        tagged Invalid: True;
                        tagged Valid .r: prfValids[r];
                    endcase;
    let src2IsRdy = case (execStallWriters[1]) matches
                        tagged Invalid: True;
                        tagged Valid .r: prfValids[r];
                    endcase;
    let noMoreStalls = noMoreVectorStalls && src1IsRdy && src2IsRdy;
    
    rule getResults2StallEnd (execStalling && noMoreStalls);
        
        execStalling <= False;
    
        // Finish up the work of getResult2.
    
        // Request the first 2 sources (harmless if no sources).
        prf.read_req1(validValue(execStallWriters[0]));
        prf.read_req2(validValue(execStallWriters[1]));

        // Also look up the PC of the instruction and the instruction itself.
        tokAddr.read_req(execStallTok.index);
        tokInst.read_req2(execStallTok.index);

        // Pass on to the next stage.
        res2Q.enq(execStallTok);

    endrule
    
    // getResults3
    // When:    After getResults2 or alternatively getResults2StallEnd
    // Effect:  Send all the data to the datapath.

    rule getResults3 (ready);

        // Get input from the previous stage.
        let tok = res2Q.first();
        res2Q.deq();

        // Get all the data the previous stage kicked off.
        let v1 <- prf.read_resp1();
        let v2 <- prf.read_resp2();
        let addr <- tokAddr.read_resp();
        let inst <- tokInst.read_resp2();

        // Combine the data we just go with any possible data from stalling.
        Vector#(ISA_MAX_SRCS, ISA_VALUE) values = newVector();

        values[0] = v1;
        values[1] = v2;

        for (Integer x = 2; x < valueof(ISA_MAX_SRCS); x = x + 1)
        begin

           values[x] = validValue(execStallValues[x-2]);

        end

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Sending to Datapath.", tok.index));

        // Send it to the datapath.
        linkToDatapath.makeReq(tuple3(inst, addr, values));

        // Look up the destinations for the writeback.
        tokDsts.read_req1(tok.index);

        // Pass it to the next stage.
        res3Q.enq(tok);

    endrule
    
    // getResults4
    // When:   After getResults3 and the datapath returns the result.
    // Effect: If one or fewer destinations, write back the result and 
    //         return the result to the timing partition.
    //         If more results then the getResults4AdditionalWriteback rule will take care of it.

    rule getResults4 (ready && !execWritebackMore);

        // Get the token from the previous stage.
        let tok = res3Q.first();
        res3Q.deq();

        // Get the response from the datapath.
        match {.res, .eaddr, .wbvals} = linkToDatapath.getResp();
        linkToDatapath.deq();

        // Update the memaddress (only useful for loads/stores)
        tokMemAddr.write(tok.index, eaddr);

        // Get the destination response
        let dsts <- tokDsts.read_resp1();
        
        // The first dest should always be valid (it may not be architecturally visible)
        let dst = validValue(dsts[0]);

        // Perform the first writeback, if any.
        case (wbvals[0]) matches
            tagged Invalid:  noAction; // Not writing back, either a Load, or no dests.
            tagged Valid .v: 
            begin // Do the first writeback.
                prf.write(dst, v);
                prfValids[dst] <= True;
            end
        endcase
        
        // Is there anything more to writeback?

        Bool writing_back_more = False;

        for (Integer x = 1; x < valueof(ISA_MAX_DSTS); x = x + 1)
        begin // There is more to do if both the dest and val are valid.
          writing_back_more = writing_back_more || (isValid(dsts[x]) && isValid(wbvals[x]));
        end

        if (!writing_back_more) // We're done
        begin

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Writeback complete.", tok.index));

            // Update scoreboard.
            tokScoreboard.exeFinish(tok.index);

            // Return timing model. End of macro-operation (path 1).
            linkGetResults.makeResp(tuple2(tok, res));

        end
        else // We've got to write back more.
        begin
            execWritebackMore   <= True;
            
            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Writing back additional values.", tok.index));

            // Marshall up the values for writeback.

            Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE))) remaining_values = newVector();
            for (Integer x = 0; x < valueof(ISA_MAX_DSTS) - 1; x = x + 1)
            begin
                remaining_values[x] = case (dsts[x]) matches
                                         tagged Invalid:  tagged Invalid;
                                         tagged Valid .d:
                                           case (wbvals[x]) matches 
                                              tagged Invalid:  tagged Invalid; // Not writing it now - presumably it's a load.
                                              tagged Valid .v: tagged Valid tuple2(d, v);
                                           endcase
                                     endcase;
            end

            // Record intermediate values for the next rule.
            execWritebackValues   <= remaining_values;
            execWritebackResult <= res;
            execWritebackTok <= tok;
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
    
      rule getResult4AdditionalWriteback (execWritebackMore &&& execWritebackValues[x] matches tagged Valid {.dst, .val});
      
        // Do the writeback.
        prf.write(dst, val);
        prfValids[dst] <= True;
        execWritebackValues[x] <= tagged Invalid;
        
        // When the last rule fires it also finishes up the macro-op.
        
        if (x == 0)
        begin
            // We're done.
            execWritebackMore <= False;
        
            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Additonal writebacks complete.", execWritebackTok.index));

            // Update scoreboard.
            tokScoreboard.exeFinish(execWritebackTok.index);
            
            // Return to timing model. End of macro-operation (path 2).
            linkGetResults.makeResp(tuple2(execWritebackTok, execWritebackResult));
        end
      
      endrule
    
    end

    
    // ******* emulateInstruction ******* //
    // 3-stage macro-operation that interacts with software via RRR.
    
    // When:   After the getResults operation detects an instruction which must be emulated.
    // Effect: First this sends every archtectural register value to software.
    //         Then it makes a call to emulate the instruction.
    //         Then it accepts any number of register updates from software.
    //         Finally it gets an ACK and returns the result of getResults to the timing model.
    
    // emulateInstruction1_Req
    
    // When:   After the getResults operation puts us into the emulation state, this
    //         rule happens once for each architectural register.
    // Effect: Look up the current physical register in the maptable and request it from the regfile.
    
    
    rule emulateInstruction1_Req (emulatingInstruction && synchronizingRegs);
    
        // Lookup which register to send next.
        FUNCP_PHYSICAL_REG_INDEX current_pr = maptable[synchronizingCurReg];
        
        // Make the request to the regfile.
        prf.read_req1(current_pr);
        
        // Pass it on to the next stage.
        syncQ.enq(synchronizingCurReg);
        
        // Move on to the next register.
        Bit#(rname_SZ) next_r = synchronizingCurReg + 1;
        
        // Was this our last request?
        if (next_r == 0)
        begin
        
            // End the loop.
            synchronizingRegs <= False;
            // Request the inst and current PC
            tokInst.read_req1(emulatingToken.index);
            tokAddr.read_req(emulatingToken.index);
        
        end
        
        // Increment, and possibly repeat.
        synchronizingCurReg <= next_r;
        
    
    endrule

    // emulateInstruction1_Rsp
    
    // When:   After each occurance of emulateInstruction1_Req
    // Effect: Get the register value response and send it on to software via RRR.

    rule emulateInstruction1_Rsp (True);
    
        // Get the register from the previous stage.
        Bit#(rname_SZ) arch_reg = syncQ.first();
        syncQ.deq();
        
        // Get the register value from the regfile.
        ISA_VALUE reg_val <- prf.read_resp1();
        
        // Send the regsiter on to software via RRR
        linkRRRSync.send(tuple2(unpack(arch_reg), reg_val));
    
    endrule
    
    // emulateInstruction2
    
    // When:   After emulateInstruction1 has transmitted every architectural register.
    // Effect: Send the instruction emulation request to software via RRR.

    rule emulateInstruction2 (emulatingInstruction && !synchronizingRegs);
        
        // Get the instruction and current pc
        ISA_INSTRUCTION inst <- tokInst.read_resp1();
        ISA_ADDRESS       pc <- tokAddr.read_resp();
        
        // Send the request on to software via RRR
        linkRRREmulate.makeReq(tuple2(inst, pc));

    endrule

    // emulateInstruction2_UpdateReg
    
    // When:   Whenever the software decides that it should update a register in hardware.
    //         These updates should really only occur when we're emulating an instruction.
    //         If they come during any other time then this is a fatal error.
    // Effect: Update the register to the new value.


    rule emulateInstruction2_UpdateReg (True);
        
        // Get an update request from software.
        match {.r, .v} = linkRRRUpdate.receive();
        linkRRRUpdate.deq();
        
        // Assert that we're in the state we expected to be in.
        assertRegUpdateAtExpectedTime(emulatingInstruction && !synchronizingRegs);
        
        // Lookup the current physical register in the maptable.
        FUNCP_PHYSICAL_REG_INDEX pr = maptable[pack(r)];
        
        
        // Update the regfile.
        // Note: It's possible that we should be allocating new physical registers instead of
        //       updating the current ones.
        prf.write(pr, v);
        prfValids[pr] <= True;
    
    endrule

    // emulateInstruction3
    
    // When:   After the software has finished all of its register writes it will send an ACK.
    // Effect: This means the emulation is complete. Resume normal operations.
    //         Return a NOP to the timing model.

    rule emulateInstruction3 (True);
        
        // Get the ACK from software that they're complete.
        let newPc = linkRRREmulate.getResp();
        linkRRREmulate.deq();
        
        // Assert that we're in the state we expected to be in.
        assertEmulationFinishedAtExpectedTime(emulatingInstruction && !synchronizingRegs);
        
        // We are no longer emulating an instruction.
        // Resume normal operations.
        emulatingInstruction <= False;

        // Update scoreboard.
        tokScoreboard.exeFinish(emulatingToken.index);

        // FIXME: Hardcoded to 64 bits
        let resp = (newPc[63] == 0)? tagged RBranchTaken newPc: tagged RNop;

        // Send the response to the timing model.
        // End of macro-operation.
        linkGetResults.makeResp(tuple2(emulatingToken, resp));

    endrule

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
        let tok = linkDoLoads.getReq();
        linkDoLoads.deq();

        // If it's not actually a load, it's an exception.
        let isLoad = tokScoreboard.isLoad(tok.index);
        assertInstructionIsActuallyALoad(isLoad);

        if (!isLoad)
        begin

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: doLoads1: I WAS TOLD TO LOAD THIS BUT IT'S NOT A LOAD!", tok.index));

        end
        else // Everything's okay.
        begin

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: doLoads1: Start", tok.index)); 

            // Update the scoreboard.
            tokScoreboard.loadStart(tok.index);

            // Read the effective address.
            tokMemAddr.read_req1(tok.index);

            // Pass to the next stage.
            load1Q.enq(tok);

        end

    endrule

    // doLoads2

    // When:   After doLoads1 occurs
    // Effect: Make the request to the memory state.

    rule doLoads2 (ready);

        // Read the parameters from the previous stage.
        let tok = load1Q.first();
        load1Q.deq();

        // Get the address.
        let addr <- tokMemAddr.read_resp1();

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: doLoads2: Requesting Load (Addr: 0x%h)", tok.index, addr));
        
        // Convert the address using the ISA-provided conversion function.
        MEM_ADDRESS mem_addr = isaAddressToMemAddress(addr);

        // Make the request to the DMem.
        linkToMem.makeReq(MEMSTATE_REQ_LOAD {token: tok, addr: mem_addr});

        // Read the destination so we can writeback the correct register.
        tokDsts.read_req2(tok.index);
        
        // Record that the load response should go to us.
        memPathQ.enq(PATH_LOAD);
        
        // Pass it on to the final stage.
        load2Q.enq(tuple2(tok, addr));

    endrule

    // doLoads3

    // When:   After doLoads2 occurs and we get a response from the memory state.
    // Effect: Record the result and pass it back to the timing model.

    rule doLoads3 (ready && memPathQ.first() == PATH_LOAD);

        // Get the data from the previous stage.
        match {.tok, .addr} = load2Q.first();
        load2Q.deq();
        memPathQ.deq();

        // Get the load type.
        ISA_MEMOP_TYPE lType = tokScoreboard.getLoadType(tok.index);
        
        // Pop the response from the memory state.
        let val = linkToMem.getResp();
        linkToMem.deq();

        // Convert the response into value using the ISA-provided function.
        let final_val = isaValueFromMemValue(val, lType, addr);

        // Get the destination for the purposes of writeback.
        let dsts <- tokDsts.read_resp2();

        // We assume that the destination for the load is destination 1.
        let dst = validValue(dsts[0]);

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: doLoads3: Load Response (PR%0d <= 0x%h)", tok.index, dst, val));

        // Update the physical register file.
        prf.write(dst, val);

        // Assert that the register was ready (not valid).
        assertLoadDestRegIsReady(!prfValids[dst]);

        // The register is now valid.
        prfValids[dst] <= True;

        // Update the scoreboard.
        tokScoreboard.loadFinish(tok.index);

        // Respond to the timing model. End of macro-operation.
        linkDoLoads.makeResp(tok);

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
        let tok = linkDoStores.getReq();
        linkDoStores.deq();

        // If it's not actually a store, it's an exception.
        let isStore = tokScoreboard.isStore(tok.index);
        assertInstructionIsActuallyAStore(isStore);

        if (!isStore)
        begin

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: I WAS TOLD TO STORE THIS BUT IT'S NOT A STORE!", tok.index));

        end
        else // Everything's fine.
        begin

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: doStores: Start", tok.index)); 

            // Update the scoreboard.
            tokScoreboard.storeStart(tok.index);

            // Read the destination.
            tokDsts.read_req3(tok.index);

            // Pass to the next stage.
            store1Q.enq(tok);

        end

    endrule
    
    // doStores2

    // When:   After doStores1 occurs
    // Effect: Read the physical register file and the effective address. 

    rule doStores2 (ready);

        // Read the parameters from the previous stage.
        let tok = store1Q.first();
        store1Q.deq();

        // Get the destination.
        let dsts <- tokDsts.read_resp3();
        
        // We use destination zero of a store for the value in order to avoid
        // figuring out where the source comes from.
        // This is safe because no one can see the dummy physical register.
        let dst = validValue(dsts[0]);

        // Look up the register value.
        prf.read_req3(dst);

        // Read the effective address.
        tokMemAddr.read_req2(tok.index);

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: doStores2: Retrieving Store Value (PR%0d)", tok.index, dst)); 

        // Pass it on to the final stage.
        store2Q.enq(tok);

    endrule

    // doStores3

    // When:   After doStores2 occurs
    // Effect: Send the store request to the memory state. Also reply to the timing model.
    //         Note that this represents a "fast forwarding" of the response to the timing model
    //         before the memory state actually completes the store. The semantics of the
    //         memory state must be such that this is safe.
     
    rule doStores3 (ready);

      // Get the result from the previous stage.
      let tok = store2Q.first();
      store2Q.deq();

      // Get the address.
      let addr <- tokMemAddr.read_resp2();
      
      // Get the value.
      let val  <- prf.read_resp3();

      // Log it.
      funcpDebug($fwrite(debugLog, "TOKEN %0d: DMem: Requesting Store (Addr: 0x%h <= 0x%h)", tok.index, addr, val)); 
      
      // Get the store type.
      ISA_MEMOP_TYPE st_type = tokScoreboard.getStoreType(tok.index);
      
      // Convert the address using the ISA-provided conversion function.
      MEM_ADDRESS mem_addr = isaAddressToMemAddress(addr);

      // Use the ISA-provided function to see if we're doing read-modify-write.
      if (isaMemOpRequiresReadModifyWrite(st_type))
      begin

          // We're doing read-modify-write. Request a load.
          linkToMem.makeReq(MEMSTATE_REQ_LOAD {token: tok, addr: mem_addr});
        
          // Record that the load response should go to us.
          memPathQ.enq(PATH_STORE);
          
          // Pass the values on to the next stage.
          store3Q.enq(tuple5(tok, mem_addr, addr, val, st_type));
      
      end
      else  // It's a straightforward store.
      begin
      
          // Get the final value using the ISA-provided conversion function.
          let mem_val = isaValueToMemValue(val, st_type, addr);
      
          // Make the request to the memory state.
          linkToMem.makeReq(MEMSTATE_REQ_STORE {token: tok, addr: mem_addr, val: mem_val});

          // Update the scoreboard.
          tokScoreboard.storeFinish(tok.index);

          // Make the response to the timing model. End of macro-operation. (path 1)
          linkDoStores.makeResp(tok);

      end
    endrule

    // doStores3ReadModifyWrite
    
    // When:   Some time after doStores3 occurs and a read-modify-write is necessary.
    // Effect: Get the store response from the memory state and discard it.
    //         Note that we have already replied to the timing model in doStores3.

    rule doStores3ReadModifyWrite (ready && memPathQ.first() == PATH_STORE);

        // Get the data from the previous stage.
        match {.tok, .mem_addr, .addr, .val, .st_type} = store3Q.first();
        store3Q.deq();
        memPathQ.deq();

        // Pop the response from the memory state.
        let existing_val = linkToMem.getResp();
        linkToMem.deq();

        // Get the final value using the ISA-provided conversion function.
        let mem_val = isaValueToMemValueRMW(val, st_type, addr, existing_val);
      
        // Make the request to the memory state.
        linkToMem.makeReq(MEMSTATE_REQ_STORE {token: tok, addr: mem_addr, val: mem_val});

        // Update the scoreboard.
        tokScoreboard.storeFinish(tok.index);

        // Respond to the timing model. End of macro-operation. (path 2)
        linkDoStores.makeResp(tok);

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
        let tok = linkCommitResults.getReq();
        linkCommitResults.deq();

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: commitResults: Starting.", tok.index)); 
        
        // Update the scoreboard.
        tokScoreboard.commitStart(tok.index);
        
        // Request the registers to be freed.
        tokRegsToFree.read_req(tok.index);

        // Pass to the next stage.
        commQ.enq(tok);

    endrule

    // commitResults2
    
    // When:   After a commitResults1 AND commitResultsAdditional is not occuring.
    // Effect: Free the appropriate physical register and respond to the timing model.
    //         If there is more work to do, the next rule will handle it.
    //         Note that it is safe to "short path" the response because the committing of more
    //         results has higher priority than starting the commit of a new token.

    // There are more registers to free if any member of the the vector is valid.

    Bool moreRegsToFree = Vector::any(isValid, additionalRegsToFree);

    rule commitResults2 (ready && !moreRegsToFree);

        // Get the input from the previous stage.
        let tok = commQ.first();
        commQ.deq();

        // Retrieve the registers to be freed.
        let regsToFree  <- tokRegsToFree.read_resp();

        // Go ahead and free the first register, if present.
        case (regsToFree[0]) matches
            tagged Invalid:  noAction;
            tagged Valid .r: freelist.free(r);
        endcase

        // Store all the remaining register names for a later stage to handle.
        additionalRegsToFree <= tail(regsToFree);

        // Update the scoreboard so the token can be reused.
        tokScoreboard.deallocate(tok.index);

        // Respond to the timing model. End of macro-operation (except any more registers below).
        linkCommitResults.makeResp(tok);

    endrule

    // commitResultsAdditional
    
    // When:   After a commitResults2 AND there are more physical registers to free.
    // Effect: Free the appropriate physical register.
 
    // Elaborated Rule: N copies, where N is the maximum number of dests minus 1 (which was handled already).
    
    for (Integer x = 0; x < valueof(ISA_MAX_DSTS) - 1; x = x + 1)
    begin
        rule commitResultsAdditional (additionalRegsToFree[x] matches tagged Valid .r);

            freelist.free(r);
            additionalRegsToFree[x] <= tagged Invalid;

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
        let tok = linkCommitStores.getReq();
        linkCommitStores.deq();

        // If the token was not actually a store, it's an exception.
        let isStore = tokScoreboard.isStore(tok.index);
        assertCommitedStoreIsActuallyAStore(isStore);

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: commitStores: Committing.", tok.index)); 

        linkMemCommit.send(tok);

        // Respond to timing model. End of macro-operation.
        linkCommitStores.makeResp(tok);

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
        let tok = linkRewindToToken.receive();
        linkRewindToToken.deq();

        // Log it.
        funcpDebug($fwrite(debugLog, "Rewind: Starting Rewind to TOKEN %0d (Youngest: %0d)", tok.index, tokScoreboard.youngest())); 

        // Tell the memory to drop non-committed stores.
        linkMemRewind.send(tuple2(tok.index, tokScoreboard.youngest()));

        // Rewind the scoreboard.
        tokScoreboard.rewindTo(tok.index);

        // Update the epoch so we can discard appropriate updates.
        epoch <= epoch + 1;

        // Check to see if we have a snapshot.

        Bool found = False;
        if (snapValids[tok.index]) // There's a chance we have a snapshot
        begin

          // Log it.
          funcpDebug($fwrite(debugLog, "Potential Fast Rewind"));

          // Find the most recent snapshot of this token

          // Start at oldest entry in the list.        
          FUNCP_SNAPSHOT_INDEX idx = snapNext;

          for (Integer x = 0; x < valueof(TExp#(snapshotptr_SZ)); x = x + 1)
          begin
              // We look the list at an offset from the oldest entry.
              let cur = snapNext + fromInteger(x);

              // If the entry we examine is of the appropriate token, we've found a candidate!
              match {.new_idx, .new_found} = (snapIDs[cur] == tok.index) ? tuple2(cur, True) : tuple2(idx, found);
              found = new_found;
              idx = new_idx;

          end

          // Alright did we find anything?

          if (found)
          begin 
              // Log our success!
              funcpDebug($fwrite(debugLog, "Fast Rewind confirmed with Snapshot %0d", idx));

              // Retrieve the snapshots.
              snaps.read_req(idx);
              snapsFL.read_req(idx);
          end

        end

        if (!found)
        begin

            // Log our failure.
            funcpDebug($fwrite(debugLog, "Initiating slow Rewind (Oldest: %0d)", tokScoreboard.oldest()));  

        end

        // Temporarily disable all the other operations.
        rewinding <= True;

        // Are we going fast or slow?
        fastRewind <= found;

        // ??? XXX
        rewindTok <= tokScoreboard.youngest();

        // Start at the oldest and go forward.
        rewindCur <= tokScoreboard.oldest();

    endrule

    // rewindToToken2

    // When:   After rewindToToken1 AND we have a snapshot.
    // Effect: Use the snapshot to overwrite existing values. Reply to the timing partition.

    rule rewindToToken2 (rewinding && fastRewind);

        // Get the snapshots.
        let snp_map <- snaps.read_resp();
        let snp_fl  <- snapsFL.read_resp();

        // Update the maptable.
        maptable <= snp_map;
        
        // Update the freelist.
        freelist.backTo(snp_fl);

        // Log it.
        funcpDebug($fwrite(debugLog, "Fast Rewind finished."));  

        // We're done. End of macro-operation (path 1).
        rewinding <= False;

    endrule

    //Slow rewind. Walk the tokens in age order
    //and reconstruct the maptable

    rule rewindToTokenSlow1 (rewinding && !fastRewind);
    
      // Don't remap killed tokens
      if (tokScoreboard.isAllocated(rewindCur))
      begin
          // Log it.
          funcpDebug($fwrite(debugLog, "Slow Rewind: Lookup TOKEN %0d", rewindCur));  
          // Look up the destinations
          tokDsts.read_req2(rewindCur);
          // Pass it to the next stage who will free it.
          rewindQ.enq(rewindCur);
      end

      rewindCur <= rewindCur + 1;

      if (rewindCur == rewindTok) //Must take into account the last instruction
      begin
        rewinding <= False;
        funcpDebug($fwrite(debugLog, "Slow Rewind: No more tokens to lookup."));  
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

    (* descending_urgency= "rewindToTokenSlow2, rewindToTokenSlow1, rewindToToken2, rewindToToken1, commitStores, commitResults2, commitResults1, doStores3ReadModifyWrite, doStores3, doStores2, doStores1, doLoads3, doLoads2, doLoads1, getResults4, getResults3, getResults2StallEnd, getResults2, getResults1, getDependencies2AdditonalMappings, getDependencies2, getDependencies1, getInstruction2, getInstruction1, newInFlight" *)

    rule rewindToTokenSlow2 (True);

      let t = rewindQ.first();
      rewindQ.deq();


      freelist.back();
      let dst  <- tokDsts.read_resp2();
      let inst <- tokInst.read_resp2();
      // Commented out for now:
      
      /*
      case (getDest(inst)) matches
        tagged Invalid: funcpDebug($fwrite(debugLog, "Slow Rewind: TOKEN %0d had no dest", t));
        tagged Valid .d:
        begin
            funcpDebug($fwrite(debugLog, "Slow Rewind: TOKEN %0d: Remapping (R%0d/PR%0d)", t, d, dst));
            maptable[d] <= dst;
        end
      endcase
      */
    endrule
 
endmodule

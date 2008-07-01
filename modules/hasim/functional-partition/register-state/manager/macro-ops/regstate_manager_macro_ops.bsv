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
`include "hasim_modellib.bsh"

// Functional Partition includes.

`include "funcp_regstate_scoreboard.bsh"
`include "funcp_regstate_freelist.bsh"
`include "funcp_regstate_snapshot.bsh"
`include "funcp_memstate_manager.bsh"

// ISA includes

`include "hasim_isa.bsh"
`include "hasim_isa_datapath.bsh"

// Dictionary includes
`include "asim/dict/ASSERTIONS_REGMANAGER.bsh"
`include "asim/dict/STATS_REGMANAGER.bsh"

// RRR includes
`include "asim/provides/rrr.bsh"
`include "asim/rrr/service_ids.bsh"
`include "asim/provides/isa_emulator.bsh"
`include "asim/rrr/remote_client_stub_ISA_EMULATOR.bsh"
`include "asim/rrr/remote_server_stub_ISA_EMULATOR.bsh"

// ***** Typedefs ***** //

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

// REGMANAGER_STATE

// A type to indicating what we're doing on a high level.

typedef enum
{
  RSM_Initializing,
  RSM_Running,
  RSM_DrainingForRewind,
  RSM_Rewinding,
  RSM_DrainingForEmulate,
  RSM_SyncingRegisters,
  RSM_RequestingEmulation,
  RSM_UpdatingRegisters
}
  REGMANAGER_STATE
      deriving (Eq, Bits);

// mkFUNCP_RegStateManager

// The manager of the register state, and the bulk of the work of the functional partition.

module [HASim_Module] mkFUNCP_RegStateManager
    //interface:
                ()
    provisos
            (Bits#(TOKEN_INDEX, idx_SZ),      // The number of tokens.
             Bits#(ISA_REG_INDEX, rname_SZ),  // The number of architectural registers.
             Bits#(FUNCP_PHYSICAL_REG_INDEX, pname_SZ),
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
    BRAM#(idx_SZ, ISA_ADDRESS) tokAddr <- mkBramInitialized(0);

    // The instruction that was at that address (from mem_state).
    BRAM_MULTI_READ#(2, idx_SZ, ISA_INSTRUCTION) tokInst <- mkMultiReadBramInitialized(0);

    // The destinations of the instruction (a convenience which saves us from reading the instruction/maptable).
    BRAM_MULTI_READ#(3, idx_SZ, Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) tokDsts <- mkMultiReadBramInitialized(?);

    // If an instruction has sources in other inflight instructions it will be noted here.
    BRAM#(idx_SZ, Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)))   tokWriters <- mkBramInitialized(?);

    // The memaddress is used by Loads/Stores so we don't have to repeat the calculation.
    BRAM_MULTI_READ#(2, idx_SZ, ISA_ADDRESS) tokMemAddr <- mkMultiReadBramInitialized(0);

    // The physical registers to free when the token is committed/killed.
    BRAM#(idx_SZ, Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) tokRegsToFree <- mkBramInitialized(?);

    // The Physical Register File

    BRAM_MULTI_READ#(3, pname_SZ, ISA_VALUE) prf <- mkMultiReadBram();
    
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

    Snapshot#(rname_SZ) snapshots <- mkSnapshot();

    // ******* Miscellaneous *******

    // Is it a fast rewind or a slow one?
    Reg#(Bool)     fastRewind <- mkReg(False);

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
    // Record the writers for the token.
    Reg#(Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) execStallWriters <- mkReg(Vector::replicate(tagged Invalid));
    // Record the writers we need to request.
    Vector#(ISA_MAX_SRCS, Reg#(Bool)) execStallValuesNeeded = newVector();    
    // Record if the request is in flight.
    Vector#(ISA_MAX_SRCS, Reg#(Bool)) execStallReqsMade = newVector();
    // Record the values that have come back while stalling.
    Vector#(ISA_MAX_SRCS, Reg#(ISA_VALUE)) execStallValues = newVector();
    // Initialize the registers.
    for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
    begin
        
        execStallReqsMade[x] <- mkRegU();
        execStallValuesNeeded[x] <- mkRegU();
        execStallValues[x] <- mkRegU();
    
    end
    // Queues to guide the stall responses.
    FIFO#(Bit#(4)) resEvenQ <- mkFIFO();
    FIFO#(Bit#(4)) resOddQ  <- mkFIFO();
   
 
    // Is the getResult stage writing back more values?
    Reg#(Bool) execWritebackMore <- mkReg(False);
    // The token we're stalling on.
    Reg#(TOKEN)    execWritebackTok <- mkRegU();
    // Record the values for writeback.
    Reg#(Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE)))) execWritebackValues <- mkReg(Vector::replicate(tagged Invalid));
    // Record the result for the timing model while we're writing back.
    Reg#(ISA_EXECUTION_RESULT) execWritebackResult <- mkRegU();
    
    // Which snapshot should we refer to for emulation?
    Reg#(FUNCP_SNAPSHOT_INDEX) emulatingSnap <- mkRegU();
    // Which token's instruction are we emulating?
    Reg#(TOKEN) emulatingToken <- mkRegU();
    // Which register are we currently synchronizing?
    Reg#(Bit#(rname_SZ)) synchronizingCurReg <- mkReg(minBound);

    Reg#(Bit#(TLog#(ISA_MAX_DSTS))) execWritebackNum <- mkRegU();
    
    // A state variable to indicate what we're doing on a high-level.
    Reg#(REGMANAGER_STATE) state <- mkReg(RSM_Initializing);
    
    // We are only ready to put a new operation into a pipeline if we are neither rewinding, initializing, nor emulating
    let readyToBegin = state == RSM_Running;
    // We allow operations in pipelines to proceed under a looser set of circumstances.
    let readyToContinue = state == RSM_Running || state == RSM_DrainingForRewind || state == RSM_DrainingForEmulate;
    
    // Does the commit stage have to free more registers?
    Reg#(Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(FUNCP_PHYSICAL_REG_INDEX))) additionalRegsToFree <- mkReg(Vector::replicate(tagged Invalid));
    
    // This queue records where load responses should be sent.
    FIFO#(MEM_PATH) memPathQ <- mkSizedFIFO(16);
    
    // These Queues are intermediate state between the pipeline stages.

    FIFO#(Tuple2#(TOKEN, ISA_ADDRESS)) instQ  <- mkFIFO();
    FIFO#(TOKEN) deps1Q <- mkFIFO();
    FIFO#(TOKEN) deps2Q <- mkFIFO();
    FIFO#(TOKEN) res1Q  <- mkFIFO();
    FIFO#(TOKEN) res2Q <- mkFIFO();
    FIFO#(TOKEN) res3Q   <- mkFIFO();
    FIFO#(Tuple2#(Bit#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX)) syncQ <- mkFIFO();
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

    Connection_Server#(Bit#(1), 
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

    Connection_Server#(TOKEN,
                       Bit#(1))                                linkRewindToToken <- mkConnection_Server("funcp_rewindToToken");


    // Connections to Mem State.

    Connection_Client#(MEMSTATE_REQ, 
                       MEM_VALUE)                              linkToMem <- mkConnection_Client("funcp_memstate");

    Connection_Send#(TOKEN)                                    linkMemCommit <- mkConnection_Send("funcp_mem_commit");

    Connection_Send#(Tuple2#(TOKEN_INDEX, 
                             TOKEN_INDEX))                     linkMemRewind <- mkConnection_Send("funcp_mem_rewind");

    Connection_Send#(Bool)                                     link_funcp_memory_prep_for_emul <- mkConnection_Send("funcp_memory_prepare_to_emulate");
    Connection_Receive#(Bool)                                  link_funcp_memory_prep_for_emul_done <- mkConnection_Receive("funcp_memory_prepare_to_emulate_done");

    // Connection to Datapath.

    Connection_Client#(Tuple3#(ISA_INSTRUCTION, ISA_ADDRESS, ISA_SOURCE_VALUES), 
                       Tuple3#(ISA_EXECUTION_RESULT, ISA_ADDRESS, ISA_RESULT_VALUES)) linkToDatapath <- mkConnection_Client("isa_datapath");
    
    // RRR Stubs.
    ClientStub_ISA_EMULATOR client_stub <- mkClientStub_ISA_EMULATOR();
    ServerStub_ISA_EMULATOR server_stub <- mkServerStub_ISA_EMULATOR();
    
    // ***** Assertion Checkers ***** //

    Assertion assertInstructionIsActuallyALoad    <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_LOAD_ON_NONLOAD, ASSERT_WARNING);
    Assertion assertLoadDestRegIsReady            <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_MALFORMED_LOAD_WRITEBACK, ASSERT_ERROR);
    Assertion assertInstructionIsActuallyAStore   <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_STORE_ON_NONSTORE, ASSERT_WARNING);
    Assertion assertCommitedStoreIsActuallyAStore <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_COMMIT_STORE_ON_NONSTORE, ASSERT_WARNING);
    Assertion assertRegUpdateAtExpectedTime       <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_UNEXPECTED_REG_UPDATE, ASSERT_WARNING);
    Assertion assertEmulationFinishedAtExpectedTime <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_UNEXPECTED_EMULATION_FINISHED, ASSERT_WARNING);
    Assertion assertEmulatedInstrNoDsts           <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_EMULATED_INSTR_HAS_DST, ASSERT_ERROR);
    Assertion assertInvalidNumDsts                <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_INVALID_NUM_DSTS, ASSERT_ERROR);
    Assertion assertSlowRewindsUnimplemented      <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_SLOW_REWIND_UNIMPLEMENTED, ASSERT_ERROR);
    Assertion assertHaveSnapshotOfEmulatedInstruction <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_NO_EMULATION_SNAPSHOT, ASSERT_ERROR);

    // ***** Statistics ***** //

    Stat stat_isa_emul <- mkStatCounter(`STATS_REGMANAGER_EMULATED_INSTRS);

    // ******* Rules *******

    // initialize

    // When:    Only at the beginning of time (after a reset).
    // Effects: Makes sure all RAMS are in the right state before we begin computing.
    //          Additionally the first time it runs it will open the debug logfiles.

    rule initialize (state == RSM_Initializing);

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
        if (initCur >= maxInit)
        begin
            state <= RSM_Running;
        end
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

    rule newInFlight (readyToBegin);

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

    rule getInstruction1 (readyToBegin);

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
        //linkToMem.makeReq(MEMSTATE_REQ_LOAD {token: tok, addr: mem_addr});
        linkToMem.makeReq(MEMSTATE_REQ_LOAD {token: tok, addr: mem_addr});
        
        // Record that the result should come to us.
        memPathQ.enq(PATH_INST);

        // Send on to getInstruction2.
        instQ.enq(tuple2(tok, addr));

    endrule

    // getInstruction2

    // When:   Some time after fetch1.
    // Effect: Record the instruction, kick back to timing model.

    rule getInstruction2 (readyToContinue && memPathQ.first() == PATH_INST);

        // Input from previous stage.
        match{.tok, .addr} = instQ.first();
        instQ.deq();
        memPathQ.deq();

        // Get resp from the Mem State.
        MEM_VALUE v = linkToMem.getResp();
        linkToMem.deq();

        // Convert the value to an instruction using the ISA-provided conversion
        // function.
        ISA_INSTRUCTION inst = isaInstructionFromMemValue(v, addr);

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

    rule getDependencies1 (readyToBegin);

        // Read inputs. Begin macro-operation.
        let tok = linkGetDeps.getReq();
        linkGetDeps.deq();
        
        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: getDeps: Start", tok.index));
        
        // Update the status.
        tokScoreboard.decStart(tok.index);
        
        // Retrieve the instruction.
        tokInst.req[0].read(tok.index);

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

    rule getDependencies2 (readyToContinue &&& finishDeps matches tagged Invalid);

        // Get the info from the previous stage.
        let tok = deps1Q.first();
        deps1Q.deq();

        //Get the info the previous stage requested.
        let inst     <- tokInst.resp[0].read();
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
              map_dsts[x] = tagged Valid tuple2(r, new_preg); //This could be overwritten by the next stage.
              true_n_dsts = true_n_dsts + 1;
          end
          else
          begin
              map_dsts[x] = tagged Invalid;
          end
        end

        // Unfortunately we can only record one physical dest here, since we only got one from
        // the freelist. If the instruction has more we will invoke additional stages.

        phy_dsts[0] = tagged Valid new_preg;

        // If we have a dest, update the maptable with the correct physical register.

        Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX) new_map = case (arc_dsts[0]) matches
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
        begin
            tokScoreboard.setLoadType(tok.index, isaLoadType(inst));
        end

        if (isaIsStore(inst))
            tokScoreboard.setStoreType(tok.index, isaStoreType(inst));
            
        let is_emulated = isaEmulateInstruction(inst);
        tokScoreboard.setEmulation(tok.index, is_emulated);

        // Make a snapshot for branches or emulated instructions.
        // Note that there is an implicit assumption here that no branch or emulated instruction has more than one destination.
        if (isaIsBranch(inst))
        begin
             funcpDebug($fwrite(debugLog, "TOKEN %0d: getDeps: Making Snapshot of Branch.", tok.index));
             snapshots.makeSnapshot(tok.index, new_map, freelist.current());
        end
        else if (is_emulated)
        begin
             funcpDebug($fwrite(debugLog, "TOKEN %0d: getDeps: Making Snapshot of Emulated Instruction.", tok.index));
             snapshots.makeSnapshot(tok.index, new_map, freelist.current());
        end
             

        // If there was one dest or less, we are done.
        
        let num_dsts = isaGetNumDsts(inst);
        
        assertInvalidNumDsts(num_dsts >= true_n_dsts);
        assertEmulatedInstrNoDsts((num_dsts == 0) || ! is_emulated);

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
    
     rule getDependencies2AdditionalMappings (readyToContinue &&& finishDeps matches tagged Valid {.tok, .num, .map_srcs, .map_dsts, .phy_regs_to_free});
      
        // Get the new phys reg.
        let phy_dst <- freelist.forwardResp();

        // The new mapping.
        match {.arc_dst, .dummy} = validValue(map_dsts[num]); // Perhaps we should assert that this is valid?
        let new_map_dsts = update(map_dsts, num, tagged Valid tuple2(arc_dst, phy_dst));

        // The reg to free is the old writer of this destination.
        let actual_phy_reg_to_free = isValid(map_dsts[num])? tagged Valid select(maptable, pack(arc_dst)): tagged Valid phy_dst;
        let new_phy_regs_to_free = update(phy_regs_to_free, num, actual_phy_reg_to_free);

        if (isValid(map_dsts[num]) && tok.timep_info.epoch == epoch) //Don't update the maptable if this token is getting killed
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

    rule getResults1 (readyToBegin);

        // Get parameter from the timing model. Begin macro-operation.
        let tok = linkGetResults.getReq();
        linkGetResults.deq();

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Start", tok.index));

        // Update the scoreboard.
        tokScoreboard.exeStart(tok.index);
        
        if (tokScoreboard.emulateInstruction(tok.index))
        begin

            // Record that we're emulating an instruction.
            state <= RSM_DrainingForEmulate;
            // Record which token is being emulated.
            emulatingToken <= tok;
            // Invalidate the cache.
            link_funcp_memory_prep_for_emul.send(?);
            // Lookup the snapshot we should be working with.
            let msnap = snapshots.hasSnapshot(tok.index);
            // If there's no snapshot, something is really wrong.
            assertHaveSnapshotOfEmulatedInstruction(isValid(msnap));
            // Record which snap we should use.
            emulatingSnap <= validValue(msnap);
            // Pre-request the first snapshot.
            snapshots.requestSnapshot(validValue(msnap));
             // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Beginning Instruction Emulation.", tok.index));

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

    rule getResults2 (readyToContinue && !execStalling);

        // Get input from getResults1.
        let tok = res1Q.first();
        res1Q.deq();

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Reg Read", tok.index));

        // Response from previous stage.
        let ws <- tokWriters.readResp();
        
        // We let junk proceed
        if (!tokScoreboard.isAllocated(tok.index))
        begin
            // No values are needed for junk
            execStalling          <= True;
            execStallTok          <= tok;
            execStallWriters      <= ws;
            for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
            begin
                execStallValuesNeeded[x] <= False;
                execStallReqsMade[x] <= True;
            end
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Letting Junk Proceed!", tok.index));
        end
        else
        begin

            // We use a mask to determine which values are needed.
            Vector#(ISA_MAX_SRCS, Bool) values_needed = newVector();
            
            // We only need a value if there's a writer for it.
            for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
            begin
                values_needed[x] = isValid(ws[x]);
            end
            
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Need to request the srcs in this mask: %b", tok.index, pack(values_needed)));
            
            // Now we use a separate mask to record which requests have been made.
            // To speed things up we try to make the first 2 requests now.
            Vector#(ISA_MAX_SRCS, Bool) reqs_made = Vector::replicate(False);

            // Request src 0 if it is ready.
            case (ws[0]) matches
                tagged Invalid: noAction; // No src0 to request.
                tagged Valid .r: 
                begin
                    if (prfValids[r]) // The source is ready so we can make the request.
                    begin
                        prf.req[0].read(r);
                        reqs_made[0] = True;
                        resEvenQ.enq(0);
                        funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Requesting src 0 early!", tok.index));
                    end
                end
            endcase

            // If there's a source 1, try to request that too.
            if (valueof(ISA_MAX_SRCS) > 1)
                case (ws[1]) matches
                    tagged Invalid: noAction; // No src1 to request.
                    tagged Valid .r: 
                    begin
                        if (prfValids[r]) // The source is ready so we can make the request.
                        begin
                            prf.req[1].read(r);
                            reqs_made[1] = True;
                            resOddQ.enq(1);
                            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Requesting src 1 early!", tok.index));
                        end
                    end
                endcase
            
            // Scoreboard stuff for the next rules.
            execStalling          <= True;
            execStallTok          <= tok;
            execStallWriters      <= ws;
            for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
            begin
                execStallValuesNeeded[x] <= values_needed[x];
                execStallReqsMade[x] <= reqs_made[x];
            end
        end

    endrule

    // getResults2Stall (Req/Rsp) and (Odd/Even)
    
    // Elaborated Rule: N copies, one for each possible instruction source.
    
    // When:    After getResults2 and not all sources were available.
    // Effect:  Once a source becomes available the Req rules read the PRF.
    //          The Rsp rules get the response and record it.
    //          There are odd/even copies soley for performance: to use 2 ports of the PRF.

    for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 2)
    begin
    
        rule getResults2StallReqEven (readyToContinue && execStalling &&&
                                      execStallWriters[x] matches tagged Valid .r &&& // We need this register...
                                      prfValids[r] &&& // The register is ready...
                                      !execStallReqsMade[x]); //We haven't made the request yet.
            
            prf.req[0].read(r);
            execStallReqsMade[x] <= True;
            resEvenQ.enq(fromInteger(x));
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Requesting src %0d.", execStallTok.index, fromInteger(x)));

        endrule
    
        rule getResults2StallRspEven (readyToContinue && execStalling &&&
                                      execStallValuesNeeded[x] &&& // We need this register.
                                      execStallReqsMade[x] &&&  //We made the request already.
                                      resEvenQ.first() == fromInteger(x)); // Our response is next in line.
            
            let v <- prf.resp[0].read();
            execStallValues[x] <= v;
            execStallValuesNeeded[x] <= False;
            resEvenQ.deq();
            funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Receiving src %0d.", execStallTok.index, fromInteger(x)));

        endrule
        
        // Repeat the above two rules using the second PRF port. (Should be exact copies except for index.)
        if (x+1 < valueof(ISA_MAX_SRCS))
        begin
            rule getResults2StallReqOdd (readyToContinue && execStalling &&&
                                          execStallWriters[x+1] matches tagged Valid .r &&& // We need this register...
                                          prfValids[r] &&& // The register is ready...
                                          !execStallReqsMade[x+1]); //We haven't made the request yet.

                prf.req[1].read(r);
                execStallReqsMade[x+1] <= True;
                resOddQ.enq(fromInteger(x+1));
                funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Requesting src %0d.", execStallTok.index, fromInteger(x+1)));

            endrule

            rule getResults2StallRspOdd (readyToContinue && execStalling &&&
                                         execStallValuesNeeded[x+1] &&& // We need this register
                                         execStallReqsMade[x+1] &&&  //We made the request already.
                                         resOddQ.first() == fromInteger(x+1)); // Our response is next in line.

                let v <- prf.resp[1].read();
                execStallValues[x+1] <= v;
                execStallValuesNeeded[x+1] <= False;
                resOddQ.deq();
                funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Receiving src %0d.", execStallTok.index, fromInteger(x+1)));

            endrule
        end
        
    end

    // getResults2StallEnd
    // When:    After getResults2 or the getResults2Stall rules have succesfully retrieved their data.
    // Effect:  Unstall and lookup the address and inst.

    Bool noMoreStalls = True;
    
    for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
    begin
        noMoreStalls = noMoreStalls && !execStallValuesNeeded[x];
    end
    
    rule getResults2StallEnd (readyToContinue && execStalling && noMoreStalls);

        execStalling <= False;
        res2Q.enq(execStallTok);
        tokAddr.readReq(execStallTok.index);
        tokInst.req[1].read(execStallTok.index);
        funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: All sources ready. Proceeding...", execStallTok.index));

    endrule

    // getResults3
    // When:    After getResults2 or alternatively getResults2StallEnd
    // Effect:  Send all the data to the datapath.

    rule getResults3 (readyToContinue);

        // Get input from the previous stage.
        let tok = res2Q.first();
        res2Q.deq();

        // Get all the data the previous stage kicked off.
        let addr <- tokAddr.readResp();
        let inst <- tokInst.resp[1].read();

        // Combine the data we just go with any possible data from stalling.
        Vector#(ISA_MAX_SRCS, ISA_VALUE) values = newVector();

        for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
           values[x] = execStallValues[x];

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: Execute: Sending to Datapath.", tok.index));

        // Send it to the datapath.
        linkToDatapath.makeReq(tuple3(inst, addr, values));

        // Look up the destinations for the writeback.
        tokDsts.req[0].read(tok.index);

        // Pass it to the next stage.
        res3Q.enq(tok);

    endrule
    
    // getResults4
    // When:   After getResults3 and the datapath returns the result.
    // Effect: If one or fewer destinations, write back the result and 
    //         return the result to the timing partition.
    //         If more results then the getResults4AdditionalWriteback rule will take care of it.

    rule getResults4 (readyToContinue && !execWritebackMore);

        // Get the token from the previous stage.
        let tok = res3Q.first();
        res3Q.deq();

        // Get the response from the datapath.
        match {.res, .eaddr, .wbvals} = linkToDatapath.getResp();
        linkToDatapath.deq();

        // Update the memaddress (only useful for loads/stores)
        tokMemAddr.write(tok.index, eaddr);

        // Get the destination response
        let dsts <- tokDsts.resp[0].read();
        
        // The first dest should always be valid (it may not be architecturally visible)
        let dst = validValue(dsts[0]);

        // Perform the first writeback, if any.
        case (wbvals[0]) matches
            tagged Invalid:  noAction; // Not writing back, either a Load, or no dests.
            tagged Valid .v: 
            begin // Do the first writeback.
                prf.write(dst, v);
                prfValids[dst] <= True;
                funcpDebug($fwrite(debugLog, "TOKEN %0d: getResults4: Writing (PR%0d <= 0x%x)", tok.index, dst, v));
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

            // Record intermediate values for the next rule.
            execWritebackValues   <= remaining_values;
            execWritebackResult <= res;
            execWritebackTok <= tok;
            execWritebackNum <= 0;
        end
      
    endrule

    // getResults4AdditionalWriteback
    
    // When:   After a result from getResults4 writes back additonal destinations.
    // Effect: Finish the writeback of the physical registerfile.
    
    // Elaborated Rule: N copies, where N is the maximum number of destinations 
    //                  an instruction can have, minus one. (The one we wrote back
    //                  in getResult4.)
    
    if(valueOf(ISA_MAX_DSTS) > 1)
    begin
        rule getResult4AdditionalWriteback (execWritebackMore &&& execWritebackValues[execWritebackNum] matches tagged Valid {.dst, .val});
    
            // Do the writeback.
            prf.write(dst, val);
            prfValids[dst] <= True;
            execWritebackValues[execWritebackNum] <= tagged Invalid;

            funcpDebug($fwrite(debugLog, "getResults4AdditionalWriteback: Writing (PR%0d <= 0x%x)", dst, val));

            execWritebackNum <= execWritebackNum + 1;
      
            // When the last rule fires it also finishes up the macro-op.
            if(execWritebackNum == fromInteger(valueOf(ISA_MAX_DSTS) - 2))
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
    else
    begin
        //
        // Dummy rule to keep execution_order pragma below happy
        //
        rule getResult4AdditionalWriteback (True);
        endrule
    end

    
    // ******* emulateInstruction ******* //
    // 4-stage macro-operation that interacts with software via RRR.
    
    // When:   After the getResults operation detects an instruction which must be emulated.
    // Effect: First this sends every archtectural register value to software.
    //         Then it makes a call to emulate the instruction.
    //         Then it accepts any number of register updates from software.
    //         Finally it gets an ACK and returns the result of getResults to the timing model.

    // finishDrainForEmulate
    
    // When:   After getResults operation puts us in the emulation state.
    // Effect: Stall until all younger operations have completed, and the cache is emptied. Then we can proceed.
    
    rule finishDrainForEmulate (state == RSM_DrainingForEmulate && tokScoreboard.canEmulate());

       // Get the response from the cache.
        link_funcp_memory_prep_for_emul_done.deq();
        funcpDebug($fwrite(debugLog, "TOKEN %0d: finishDrainForEmulate: Memory sync done", emulatingToken.index));

       // Reset the counter for syncing registers.
       synchronizingCurReg <= minBound;

       // Start syncing registers.
       state <= RSM_SyncingRegisters;
               
    endrule

    // emulateInstruction1_Req
    
    // When:   After the getResults operation puts us into the emulation state, this
    //         rule happens once for each architectural register.
    // Effect: Look up the current physical register in the maptable and request it from the regfile.
    
    
    rule emulateInstruction1_Req (state == RSM_SyncingRegisters);
    
        // Some ISA's have a sparse packing of register names.  Don't sync
        // a register if the current index doesn't map to a real register index.
        ISA_REG_INDEX isa_reg = unpack(synchronizingCurReg);
        if (pack(isa_reg) == synchronizingCurReg)
        begin

            // Get the maptable at the time of the emulated instruction.
            match {.emulation_map, .emu_fl} <- snapshots.returnSnapshot();

            // Lookup which register to send next.
            FUNCP_PHYSICAL_REG_INDEX current_pr = emulation_map[synchronizingCurReg];
        
            // Make the request to the regfile.
            prf.req[0].read(current_pr);
        
            // Pre-load the snapshot for next time.
            snapshots.requestSnapshot(emulatingSnap);
        
            // Pass it on to the next stage.
            syncQ.enq(tuple2(synchronizingCurReg, current_pr));
        end
        
        // Move on to the next register.
        Bit#(rname_SZ) next_r = synchronizingCurReg + 1;
        
        // Was this our last request?
        if (next_r == 0)
        begin
        
            // Request the inst and current PC
            tokInst.req[0].read(emulatingToken.index);
            tokAddr.readReq(emulatingToken.index);
            // End the loop.
            state <= RSM_RequestingEmulation;
        
        end
        
        // Increment, and possibly repeat.
        synchronizingCurReg <= next_r;
        
    
    endrule

    // emulateInstruction1_Rsp
    
    // When:   After each occurance of emulateInstruction1_Req
    // Effect: Get the register value response and send it on to software via RRR.

    rule emulateInstruction1_Rsp (True);
    
        // Get the register from the previous stage.
        match {.arch_reg, .phys_reg} = syncQ.first();
        syncQ.deq();
        
        // Get the register value from the regfile.
        ISA_VALUE reg_val <- prf.resp[0].read();
        
        // Send the regsiter on to software via RRR
        client_stub.makeRequest_sync(tuple2(unpack(arch_reg), reg_val));

        //Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: emulateInstruction: Transmitting Register R%0d (PR%0d) = 0x%h.", emulatingToken.index, unpack(arch_reg), phys_reg, reg_val));
    
    endrule
    
    // emulateInstruction2
    
    // When:   After emulateInstruction1 has transmitted every architectural register.
    // Effect: Send the instruction emulation request to software via RRR.

    rule emulateInstruction2 (state == RSM_RequestingEmulation);
        
        // Get the instruction and current pc
        ISA_INSTRUCTION inst <- tokInst.resp[0].read();
        ISA_ADDRESS       pc <- tokAddr.readResp();
        
        // Send the request on to software via RRR
        client_stub.makeRequest_emulate(tuple2(inst, pc));
        
        //Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: emulateInstruction: Requesting Emulation of inst 0x%h from address 0x%h", emulatingToken.index, inst, pc));
        stat_isa_emul.incr();

        //Go to receiving updates.
        state <= RSM_UpdatingRegisters;

    endrule

    // emulateInstruction2_UpdateReg
    
    // When:   Whenever the software decides that it should update a register in hardware.
    //         These updates should really only occur when we're emulating an instruction.
    //         If they come during any other time then this is a fatal error.
    // Effect: Update the register to the new value.


    rule emulateInstruction2_UpdateReg (True);
        
        // Get an update request from software.
        match {.r, .v} <- server_stub.acceptRequest_updateRegister();
        
        // Get the maptable at the time of the emulated instruction.
        match {.emulation_map, .emu_fl} <- snapshots.returnSnapshot();

        // Assert that we're in the state we expected to be in.
        assertRegUpdateAtExpectedTime(state == RSM_UpdatingRegisters);
        
        // Lookup the current physical register in the snapshot maptable.
        FUNCP_PHYSICAL_REG_INDEX pr = emulation_map[pack(r)];
        
        // Update the regfile.
        prf.write(pr, v);
        prfValids[pr] <= True;
        funcpDebug($fwrite(debugLog, "emulateInstruction: Writing (PR%0d <= 0x%x)", pr, v));
    
        // Get the snapshot for the next time.
        snapshots.requestSnapshot(emulatingSnap);
        
        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: emulateInstruction: Writing (PR%0d <= 0x%x)", emulatingToken.index, pr, v));
    
    endrule

    // emulateInstruction3
    
    // When:   After the software has finished all of its register writes it will send an ACK.
    // Effect: This means the emulation is complete. Resume normal operations.
    //         Return a NOP to the timing model.

    rule emulateInstruction3 (True);
        
        // Get the ACK from software that they're complete.
        let newPc <- client_stub.getResponse_emulate();
        
        // Assert that we're in the state we expected to be in.
        assertEmulationFinishedAtExpectedTime(state == RSM_UpdatingRegisters);
        
        // Dequeue the final snapshot response.
        let junk <- snapshots.returnSnapshot();

        // We are no longer emulating an instruction.
        // Resume normal operations.
        state <= RSM_Running;

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
        funcpDebug($fwrite(debugLog, "TOKEN %0d: emulateInstruction: Emulation finished.", emulatingToken.index));
  
        // Send the response to the timing model.
        // End of macro-operation.
        linkGetResults.makeResp(tuple2(emulatingToken, resp));

        funcpDebug($fwrite(debugLog, "emulateInstruction: Done"));

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

    rule doLoads1 (readyToBegin);

        // Get the input from the timing model. Begin macro-operation.
        let tok = linkDoLoads.getReq();
        linkDoLoads.deq();

        // If it's not actually a load, it's an exception.
        let isLoad = tokScoreboard.isLoad(tok.index);
        assertInstructionIsActuallyALoad(isLoad);

        if (tokScoreboard.emulateInstruction(tok.index)) // Emulated loads were taken care of previously.
        begin

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: doLoads1: Ignoring emulated instruction.", tok.index));

            // Respond to the timing model. End of macro-operation.
            linkDoLoads.makeResp(tok);

        end
        else // Everything's okay.
        begin

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: doLoads1: Start", tok.index)); 

            // Update the scoreboard.
            tokScoreboard.loadStart(tok.index);

            // Read the effective address.
            tokMemAddr.req[0].read(tok.index);

            // Pass to the next stage.
            load1Q.enq(tok);

        end

    endrule

    // doLoads2

    // When:   After doLoads1 occurs
    // Effect: Make the request to the memory state.

    rule doLoads2 (readyToContinue);

        // Read the parameters from the previous stage.
        let tok = load1Q.first();
        load1Q.deq();

        // Get the address.
        let addr <- tokMemAddr.resp[0].read();

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: doLoads2: Requesting Load (Addr: 0x%h)", tok.index, addr));
        
        // Convert the address using the ISA-provided conversion function.
        MEM_ADDRESS mem_addr = isaAddressToMemAddress(addr);

        // Make the request to the DMem.
        linkToMem.makeReq(MEMSTATE_REQ_LOAD {token: tok, addr: mem_addr});

        // Read the destination so we can writeback the correct register.
        tokDsts.req[1].read(tok.index);
        
        // Record that the load response should go to us.
        memPathQ.enq(PATH_LOAD);
        
        // Pass it on to the final stage.
        load2Q.enq(tuple2(tok, addr));

    endrule

    // doLoads3

    // When:   After doLoads2 occurs and we get a response from the memory state.
    // Effect: Record the result and pass it back to the timing model.

    rule doLoads3 (readyToContinue && memPathQ.first() == PATH_LOAD);

        // Get the data from the previous stage.
        match {.tok, .addr} = load2Q.first();
        load2Q.deq();
        memPathQ.deq();

        // Get the load type.
        ISA_MEMOP_TYPE lType = tokScoreboard.getLoadType(tok.index);
        
        // Pop the response from the memory state.
        let mem_val = linkToMem.getResp();
        linkToMem.deq();

        // Convert the response into value using the ISA-provided function.
        let val = isaValueFromMemValue(mem_val, lType, addr);

        // Get the destination for the purposes of writeback.
        let dsts <- tokDsts.resp[1].read();

        // We assume that the destination for the load is destination 1.
        let dst = validValue(dsts[0]);

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: doLoads3: Load Response writing (PR%0d <= 0x%h)", tok.index, dst, val));

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

    rule doStores1 (readyToBegin);

        // Get the input from the timing model. Begin macro-operation.
        let tok = linkDoStores.getReq();
        linkDoStores.deq();

        // If it's not actually a store, it's an exception.
        let isStore = tokScoreboard.isStore(tok.index);
        assertInstructionIsActuallyAStore(isStore);

        if (tokScoreboard.emulateInstruction(tok.index)) // Emulated stores were taken care of previously.
        begin

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: doStores1: Ignoring emulated instruction.", tok.index));

            // Respond to the timing model. End of macro-operation.
            linkDoLoads.makeResp(tok);

        end
        else // Everything's fine.
        begin

            // Log it.
            funcpDebug($fwrite(debugLog, "TOKEN %0d: doStores: Start", tok.index)); 

            // Update the scoreboard.
            tokScoreboard.storeStart(tok.index);

            // Read the destination.
            tokDsts.req[2].read(tok.index);

            // Pass to the next stage.
            store1Q.enq(tok);

        end

    endrule
    
    // doStores2

    // When:   After doStores1 occurs
    // Effect: Read the physical register file and the effective address. 

    rule doStores2 (readyToContinue);

        // Read the parameters from the previous stage.
        let tok = store1Q.first();
        store1Q.deq();

        // Get the destination.
        let dsts <- tokDsts.resp[2].read();
        
        // We use destination zero of a store for the value in order to avoid
        // figuring out where the source comes from.
        // This is safe because no one can see the dummy physical register.
        let dst = validValue(dsts[0]);

        // Look up the register value.
        prf.req[2].read(dst);

        // Read the effective address.
        tokMemAddr.req[1].read(tok.index);

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
     
    rule doStores3 (readyToContinue);

      // Get the result from the previous stage.
      let tok = store2Q.first();
      store2Q.deq();

      // Get the address.
      let addr <- tokMemAddr.resp[1].read();
      
      // Get the value.
      let val  <- prf.resp[2].read();

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
          linkToMem.makeReq(MEMSTATE_REQ_STORE {token: tok, addr: addr, val: mem_val});

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

    rule doStores3ReadModifyWrite (readyToContinue && memPathQ.first() == PATH_STORE);

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

    rule commitResults1 (readyToBegin);

        // Get the input from the timing model. Begin macro-operation.
        let tok = linkCommitResults.getReq();
        linkCommitResults.deq();

        // Log it.
        funcpDebug($fwrite(debugLog, "TOKEN %0d: commitResults: Starting.", tok.index)); 
        
        // Update the scoreboard.
        tokScoreboard.commitStart(tok.index);
        
        // Request the registers to be freed.
        tokRegsToFree.readReq(tok.index);

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

    rule commitResults2 (readyToContinue && !moreRegsToFree);

        // Get the input from the previous stage.
        let tok = commQ.first();
        commQ.deq();

        // Retrieve the registers to be freed.
        let regsToFree  <- tokRegsToFree.readResp();

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
    
    rule commitStores (readyToBegin);

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

    rule rewindToToken1 (readyToBegin);
      
        // Get the input from the timing model.
        let tok = linkRewindToToken.getReq();
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
        Maybe#(FUNCP_SNAPSHOT_INDEX) midx = snapshots.hasSnapshot(tok.index);

        // Alright did we find anything?
        case (midx) matches
            tagged Valid .idx:
            begin 

                // Log our success!
                funcpDebug($fwrite(debugLog, "Rewind: Fast Rewind confirmed with Snapshot %0d", idx));

                // Retrieve the snapshots.
                snapshots.requestSnapshot(idx);
                
                fastRewind <= True;

            end
            tagged Invalid:
            begin

                // Log our failure.
                funcpDebug($fwrite(debugLog, "Rewind: Initiating slow Rewind (Oldest: %0d)", tokScoreboard.oldest()));
                
                // When slow rewinds are implemented this assertion will go away...
                assertSlowRewindsUnimplemented(isValid(midx));

                // Retrieve the last good snapshot.
                // snapshots.requestLastGoodSnapshot();
                
                fastRewind <= False;

            end
        endcase
        
        // Disable everything else.
        state <= RSM_DrainingForRewind;

        // Stop when we get to the token.
        rewindTok <= tok.index;

        // Start at the oldest and go forward.
        rewindCur <= tokScoreboard.oldest();

    endrule

    rule finishDraining (state == RSM_DrainingForRewind && tokScoreboard.canRewind());
    
        // Log it.
        funcpDebug($fwrite(debugLog, "Rewind: Draining finished.", tokScoreboard.oldest()));
    
        // Proceed with rewind.
        state <= RSM_Rewinding;
    
    endrule

    // rewindToToken2

    // When:   After rewindToToken1 AND we have a snapshot.
    // Effect: Use the snapshot to overwrite existing values. Reply to the timing partition.

    rule rewindToToken2 (state == RSM_Rewinding && fastRewind);

        // Get the snapshots.
        match {.snp_map, .snp_fl} <- snapshots.returnSnapshot();

        // Update the maptable.
        maptable <= snp_map;
        
        // Update the freelist.
        freelist.backTo(snp_fl);

        // Log it.
        funcpDebug($fwrite(debugLog, "Fast Rewind finished."));  

        // We're done. End of macro-operation (path 1).
        state <= RSM_Running;
        linkRewindToToken.makeResp(?);
    endrule

    //Slow rewind. Walk the tokens in age order
    //and reconstruct the maptable

    rule rewindToTokenSlow1 (state == RSM_Rewinding && !fastRewind);
    
      // Don't remap killed tokens
      if (tokScoreboard.isAllocated(rewindCur))
      begin
          // Log it.
          funcpDebug($fwrite(debugLog, "Slow Rewind: Lookup TOKEN %0d", rewindCur));  
          // Look up the destinations
          tokDsts.req[1].read(rewindCur);
          // Pass it to the next stage who will free it.
          rewindQ.enq(rewindCur);
      end

      rewindCur <= rewindCur + 1;

      if (rewindCur == rewindTok) //Must take into account the last instruction
      begin
        linkRewindToToken.makeResp(?);
        state <= RSM_Running;
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

    (* descending_urgency= "rewindToTokenSlow2, rewindToTokenSlow1, rewindToToken2, rewindToToken1, commitStores, commitResults2, commitResults1, doStores3ReadModifyWrite, doStores3, doStores2, doStores1, doLoads3, doLoads2, doLoads1, getResults4, getResult4AdditionalWriteback, getResults3, getResults2StallEnd, getResults2, getResults1, getDependencies2AdditionalMappings, getDependencies2, getDependencies1, getInstruction2, getInstruction1, newInFlight, emulateInstruction2_UpdateReg" *)

    // The execution_order pragma doesn't affect the schedule but does get rid of
    // compiler warnings caused by the appearance of multiple writers to the
    // prfvalids vector.  According to Bluespec the order here affects the
    // priority encoder within a cycle but not the scheduling rules.

    (* execution_order = "getResults4, getResult4AdditionalWriteback, emulateInstruction2_UpdateReg, getDependencies2AdditionalMappings, getDependencies2" *)

    rule rewindToTokenSlow2 (state == RSM_Rewinding && !fastRewind);

      let t = rewindQ.first();
      rewindQ.deq();


      freelist.back();
      let dst  <- tokDsts.resp[1].read();
      let inst <- tokInst.resp[1].read();
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

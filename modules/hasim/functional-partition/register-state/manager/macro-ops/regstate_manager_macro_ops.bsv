// regstate_manager_macro_ops

// The manager of the register state, using a macro-op organization.


// Library includes.

import FIFO::*;
import Vector::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/funcp_memory.bsh"
`include "asim/provides/funcp_memory_tlb.bsh"
`include "asim/provides/hasim_modellib.bsh"

// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"
`include "asim/provides/funcp_regstate_scoreboard.bsh"
`include "asim/provides/funcp_regstate_freelist.bsh"
`include "asim/provides/funcp_regstate_snapshot.bsh"
`include "asim/provides/funcp_memstate_manager.bsh"

// ISA includes

`include "asim/provides/hasim_isa.bsh"
`include "asim/provides/hasim_isa_datapath.bsh"

// Dictionary includes
`include "asim/dict/ASSERTIONS_REGMANAGER.bsh"
`include "asim/dict/STATS_REGMANAGER.bsh"

// RRR includes
`include "asim/provides/rrr.bsh"
`include "asim/rrr/service_ids.bsh"
`include "asim/provides/isa_emulator.bsh"
`include "asim/rrr/remote_client_stub_ISA_EMULATOR.bsh"
`include "asim/rrr/remote_server_stub_ISA_EMULATOR.bsh"



// mkFUNCP_RegStateManager

// The manager of the register state, and the bulk of the work of the functional partition.

module [HASIM_MODULE] mkFUNCP_RegStateManager
    //interface:
                ();

    // ******* Debuging State *******

    // Our debug file for messages during simulation.
    DEBUG_FILE  debugLog  <- mkDebugFile(`REGSTATE_LOGFILE_NAME);

    // ******* Submodules *******

    // The Token State is a big scoreboard which tracks the status of inflight tokens.
    let tokScoreboard <- mkFUNCP_Scoreboard();

    // The Freelist tracks which physical registers are available.
    let freelist <- mkFUNCP_Freelist(debugLog);

    // The Snapshots allow for fast rewinds.
    FUNCP_SNAPSHOT snapshots <- mkFUNCP_Snapshot();

    // ******* Local State *******

    // Tables to track info about in-flight instructions.

    // The address we got the instruction from (told to us by the timing model).
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokAddr <- mkLiveTokenBRAMInitialized(0);

    // The physical address(es) for the instruction.
    BRAM#(TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalAddrs <- mkLiveTokenBRAMInitialized(tagged ONE 0);

    // The instruction that was at that address (from mem_state).
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_INSTRUCTION) tokInst <- mkLiveTokenBRAMMultiReadInitialized(0);

    // The destinations of the instruction (a convenience which saves us from reading the instruction/maptable).
    BRAM_MULTI_READ#(3, TOKEN_INDEX, ISA_INST_DSTS) tokDsts <- mkLiveTokenBRAMMultiReadInitialized(Vector::replicate(tagged Invalid));

    // If an instruction has sources in other inflight instructions it will be noted here.
    BRAM#(TOKEN_INDEX, ISA_INST_SRCS)   tokWriters <- mkLiveTokenBRAMInitialized(Vector::replicate(tagged Invalid));

    // The memaddress is used by Loads/Stores so we don't have to repeat the calculation.
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokMemAddr <- mkLiveTokenBRAMInitialized(0);

    // The value a store will write to memory
    BRAM#(TOKEN_INDEX, ISA_VALUE) tokStoreValue <- mkLiveTokenBRAMInitialized(0);

    // The physical memaddress(es) for the instruction.
    BRAM_MULTI_READ#(2, TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalMemAddrs <- mkLiveTokenBRAMMultiReadInitialized(tagged ONE 0);

    // Position of freelist for token's physical regs.  Used by rewind.
    BRAM#(TOKEN_INDEX, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) tokFreeListPos <- mkLiveTokenBRAMInitialized(tagged Invalid);

    // The physical registers to free when the token is committed/killed.
    BRAM#(TOKEN_INDEX, ISA_INST_DSTS) tokRegsToFree <- mkLiveTokenBRAMInitialized(Vector::replicate(tagged Invalid));

    // The Physical Register File

    BRAM_MULTI_READ#(3, FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE) prf <- mkBRAMMultiReadInitialized(0);
    
    // Valid bits for PRF
    Vector#(FUNCP_NUM_PHYSICAL_REGS, Reg#(Bool)) prfValids = newVector();
    
    for (Integer x = 0; x < valueOf(FUNCP_NUM_PHYSICAL_REGS); x = x + 1)
    begin
      prfValids[x] <- mkReg(True);
    end

    // The Map Table

    // This gets pounded nearly every FPGA cycle, so it's NOT in RAM.
    // Also this lets us snapshot/reload the entire maptable in a single cyle.

    // The highest register in the ISA (the last one which is initially valid).
    ISA_REG_INDEX         highestReg = maxBound;
    FUNCP_PHYSICAL_REG_INDEX maxInit = zeroExtend(pack(highestReg));

    // The initial map is that all architectural registers are mapped 1-to-1 to
    // physical registers and are all valid.

    Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) initMap = newVector();
    
    // Note: this loop ends at _architectural_ register size.
    
    for (Integer x  = 0; x < valueof(ISA_NUM_REGS); x = x + 1)
    begin
      initMap[x] = fromInteger(x);
    end

    Reg#(Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) maptable   <- mkReg(initMap);

    // ******* High-Level FSM State *******

    // The epoch tells us when to discard junk tokens that were in flight when
    // the timing partition killed them.
    Reg#(TOKEN_BRANCH_EPOCH) branchEpoch <- mkReg(0);

    // The fault epoch tells us when to discard junk tokens that were in flight
    // killed by the timing partition's fault handler.
    Reg#(TOKEN_FAULT_EPOCH) faultEpoch <- mkReg(0);
     
    // A state variable to indicate what we're doing on a high-level.
    Reg#(REGMANAGER_STATE) state <- mkReg(RSM_Initializing);
    
    // We are only ready to put a new operation into a pipeline if we are neither rewinding, initializing, nor emulating
    let readyToBegin = state == RSM_Running;
    // We allow operations in pipelines to proceed under a looser set of circumstances.
    let readyToContinue = state == RSM_Running || state == RSM_DrainingForRewind || state == RSM_DrainingForEmulate;
    
    // ******* Pipeline Stage State *******
    
    // These are the local state of stages which can stall.
    
    Reg#(STATE_ITRANS1) stateITrans1 <- mkReg(ITRANS1_NORMAL);
    Reg#(STATE_ITRANS2) stateITrans2 <- mkReg(ITRANS2_NORMAL);
    Reg#(STATE_INST2)   stateInst2   <- mkReg(INST2_NORMAL);
    Reg#(STATE_INST3)   stateInst3   <- mkReg(INST3_NORMAL);
    Reg#(STATE_DEPS2)   stateDeps2   <- mkReg(DEPS2_NORMAL);
    Reg#(STATE_RES4)    stateRes4    <- mkReg(RES4_NORMAL);
    Reg#(STATE_DTRANS2) stateDTrans2 <- mkReg(DTRANS2_NORMAL);
    Reg#(STATE_DTRANS3) stateDTrans3 <- mkReg(DTRANS3_NORMAL);
    Reg#(STATE_LOADS2)  stateLoads2  <- mkReg(LOADS2_NORMAL);
    Reg#(STATE_LOADS3)  stateLoads3  <- mkReg(LOADS3_NORMAL);
    Reg#(STATE_STORES2) stateStores2 <- mkReg(STORES2_NORMAL);

    // GetResults register stall state
    
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
 

    // Emulation state

    // Which snapshot should we refer to for emulation?
    Reg#(FUNCP_SNAPSHOT_INDEX) emulatingSnap <- mkRegU();
    // Which token's instruction are we emulating?
    Reg#(TOKEN) emulatingToken <- mkRegU();
    // PC of emulating token
    Reg#(ISA_ADDRESS) emulatingPC <- mkRegU();
    // Which register are we currently synchronizing?
    Reg#(ISA_REG_INDEX) synchronizingCurReg <- mkReg(minBound);

    // CommitResults state

    // Does the commit stage have to free more registers?
    Reg#(Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(FUNCP_PHYSICAL_REG_INDEX))) additionalRegsToFree <- mkReg(Vector::replicate(tagged Invalid));
    
    // Rewind state

    // Is it a fast rewind or a slow one?
    Reg#(Bool)     fastRewind <- mkReg(False);

    // This register stores the current Phys Reg we are initializing.
    Reg#(FUNCP_PHYSICAL_REG_INDEX)   initCur <- mkReg(0);

    // These support "slow rewinds"
    Reg#(TOKEN) rewindTok <- mkRegU();
    Reg#(TOKEN_INDEX) rewindCur <- mkRegU();
    Reg#(Bool) rewindForFault <- mkRegU();

    
    // ******* Pipeline Queues *******
    
    // These Queues are intermediate state between the pipeline stages.

    FIFO#(ITRANS_INFO) iTransQ <- mkFIFO();
    FIFO#(TOKEN) inst1Q  <- mkFIFO();
    FIFO#(INST_INFO) inst2Q  <- mkFIFO();
    FIFO#(TOKEN) depsQ <- mkFIFO();
    FIFO#(TOKEN) res1Q  <- mkFIFO();
    FIFO#(TOKEN) res2Q <- mkFIFO();
    FIFO#(Tuple2#(TOKEN, ISA_ADDRESS)) res3Q   <- mkFIFO();
    FIFO#(Tuple2#(ISA_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX)) syncQ <- mkFIFO();
    FIFO#(Tuple2#(TOKEN, ISA_MEMOP_TYPE)) dTrans1Q <- mkFIFO();
    FIFO#(DTRANS_INFO) dTrans2Q <- mkFIFO();
    FIFO#(TOKEN) loads1Q  <- mkFIFO();
    FIFO#(LOADS_INFO) loads2Q  <- mkFIFO();
    FIFO#(Tuple2#(TOKEN, ISA_MEMOP_TYPE)) stores1Q <- mkFIFO();
    FIFO#(STORES_INFO) stores2Q <- mkFIFO();
    FIFO#(TOKEN) commQ   <- mkFIFO();
    FIFO#(TOKEN) faultQ <- mkFIFO();
    FIFO#(Tuple2#(TOKEN, ISA_ADDRESS)) faultResumeQ <- mkFIFO();
    FIFO#(Tuple2#(TOKEN_INDEX, Bool)) rewindQ <- mkFIFO();
    FIFO#(TOKEN) nextSeqInstAddrQ <- mkFIFO();

    // This queue records where load responses should be sent.
    FIFO#(MEM_PATH) memPathQ <- mkSizedFIFO(16);
    
    // ******* Soft Connections *******

    // Request type is top line.
    // Response type is bottom line.

    // Connections to the timing partition.

    Connection_Server#(FUNCP_REQ_NEW_IN_FLIGHT, 
                       FUNCP_RSP_NEW_IN_FLIGHT)        linkNewInFlight <- mkConnection_Server("funcp_newInFlight");

    Connection_Server#(FUNCP_REQ_DO_ITRANSLATE,
                       FUNCP_RSP_DO_ITRANSLATE)       linkDoITranslate <- mkConnection_Server("funcp_doITranslate");

    Connection_Server#(FUNCP_REQ_GET_INSTRUCTION,
                       FUNCP_RSP_GET_INSTRUCTION)        linkGetInst   <- mkConnection_Server("funcp_getInstruction");

    Connection_Server#(FUNCP_REQ_GET_DEPENDENCIES, 
                       FUNCP_RSP_GET_DEPENDENCIES)       linkGetDeps   <- mkConnection_Server("funcp_getDependencies");

    Connection_Server#(FUNCP_REQ_GET_RESULTS, 
                       FUNCP_RSP_GET_RESULTS)           linkGetResults <- mkConnection_Server("funcp_getResults");

    Connection_Server#(FUNCP_REQ_DO_DTRANSLATE,
                       FUNCP_RSP_DO_DTRANSLATE)       linkDoDTranslate <- mkConnection_Server("funcp_doDTranslate");

    Connection_Server#(FUNCP_REQ_DO_LOADS, 
                       FUNCP_RSP_DO_LOADS)               linkDoLoads   <- mkConnection_Server("funcp_doLoads");

    Connection_Server#(FUNCP_REQ_DO_STORES, 
                       FUNCP_RSP_DO_STORES)              linkDoStores  <- mkConnection_Server("funcp_doSpeculativeStores");

    Connection_Server#(FUNCP_REQ_COMMIT_RESULTS,
                       FUNCP_RSP_COMMIT_RESULTS)     linkCommitResults <- mkConnection_Server("funcp_commitResults");

    Connection_Server#(FUNCP_REQ_COMMIT_STORES,
                       FUNCP_RSP_COMMIT_STORES)      linkCommitStores  <- mkConnection_Server("funcp_commitStores");  

    Connection_Server#(FUNCP_REQ_HANDLE_FAULT,
                       FUNCP_RSP_HANDLE_FAULT)       linkHandleFault   <- mkConnection_Server("funcp_handleFault");

    Connection_Server#(FUNCP_REQ_REWIND_TO_TOKEN,
                       FUNCP_RSP_REWIND_TO_TOKEN)    linkRewindToToken <- mkConnection_Server("funcp_rewindToToken");


    // Connections to Mem State.

    Connection_Send#(TOKEN_INDEX)                  storeBufferAllocate <- mkConnection_Send("storeBufferAllocate");

    Connection_Client#(MEMSTATE_REQ, 
                       MEM_VALUE)                            linkToMem <- mkConnection_Client("funcp_memstate");

    // Connection to TLB

    Connection_Client#(FUNCP_TLB_QUERY, FUNCP_TLB_RESP)      link_itlb <- mkConnection_Client("funcp_itlb");
    Connection_Client#(FUNCP_TLB_QUERY, FUNCP_TLB_RESP)      link_dtlb <- mkConnection_Client("funcp_dtlb");

    // Connection to Datapath.

    Connection_Client#(FUNCP_ISA_DATAPATH_REQ, FUNCP_ISA_DATAPATH_RSP) linkToDatapath <- mkConnection_Client("isa_datapath");
    
    // RRR Stubs.
    ClientStub_ISA_EMULATOR client_stub <- mkClientStub_ISA_EMULATOR();
    ServerStub_ISA_EMULATOR server_stub <- mkServerStub_ISA_EMULATOR();
    
    // ***** Assertion Checkers ***** //

    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_REGMANAGER__BASE);
    ASSERTION assertInstructionIsActuallyALoad    <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_LOAD_ON_NONLOAD, ASSERT_WARNING, assertNode);
    ASSERTION assertLoadDestRegIsReady            <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_MALFORMED_LOAD_WRITEBACK, ASSERT_ERROR, assertNode);
    ASSERTION assertInstructionIsActuallyAStore   <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_STORE_ON_NONSTORE, ASSERT_WARNING, assertNode);
    ASSERTION assertExpectedOldestTok             <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_EXPECTED_OLDEST_TOK, ASSERT_ERROR, assertNode);
    ASSERTION assertCommitedStoreIsActuallyAStore <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_COMMIT_STORE_ON_NONSTORE, ASSERT_WARNING, assertNode);
    ASSERTION assertRegUpdateAtExpectedTime       <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_UNEXPECTED_REG_UPDATE, ASSERT_WARNING, assertNode);
    ASSERTION assertEmulationFinishedAtExpectedTime <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_UNEXPECTED_EMULATION_FINISHED, ASSERT_WARNING, assertNode);
    ASSERTION assertEmulatedInstrNoDsts           <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_EMULATED_INSTR_HAS_DST, ASSERT_ERROR, assertNode);
    ASSERTION assertInvalidNumDsts                <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_INVALID_NUM_DSTS, ASSERT_ERROR, assertNode);
    ASSERTION assertHaveSnapshotOfEmulatedInstruction <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_NO_EMULATION_SNAPSHOT, ASSERT_ERROR, assertNode);
    ASSERTION assertNoEpochChange                 <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_UNEXPECTED_EPOCH_CHANGE, ASSERT_ERROR, assertNode);
    ASSERTION assertDTranslateOnMemOp             <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_DTRANSLATE_ON_MEMOP, ASSERT_ERROR, assertNode);
    ASSERTION assertIllegalInstruction            <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_ILLEGAL_INSTRUCTION, ASSERT_ERROR, assertNode);
    ASSERTION assertCommitFaultingInstr           <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_COMMIT_FAULTING_INSTR, ASSERT_ERROR, assertNode);
    ASSERTION assertPoisonBit                     <- mkAssertionChecker(`ASSERTIONS_REGMANAGER_BAD_POISON_BIT, ASSERT_ERROR, assertNode);


    // ***** Statistics ***** //

    Stat stat_isa_emul <- mkStatCounter(`STATS_REGMANAGER_EMULATED_INSTRS);


    // ******* Rules *******

    // initialize

    // When:    Only at the beginning of time (after a reset).
    // Effects: Makes sure all RAMS are in the right state before we begin computing.
    //          Additionally the first time it runs it will open the debug logfiles.

    rule initialize (state == RSM_Initializing);

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
  

    // ******* newInFlight ******* //

    // 1-stage macro-operation
    
    // When:         The timing model tells us to allocate a new in-flight instruction.
    // Effect:       Allocates a slot on the token state scoreboard.
    // Soft Inputs:  req from timing model
    // Soft Returns: a TOKEN which the timing model can use to refer to that slot.

    rule newInFlight (readyToBegin);

        // Get the input from the timing model. Begin macro operation.
        let req = linkNewInFlight.getReq();
        linkNewInFlight.deq();
        
        // Get the next token from the scoreboard.
        let idx <- tokScoreboard.allocate();
        
        // Log it.
        
        debugLog.record($format("NewInFlight: Allocating TOKEN %0d", idx));
        
        // Reset the free list pointer so rewind knows whether this token has
        // registers allocated.
        tokFreeListPos.write(idx, tagged Invalid);

        // Invalidate old snapshots
        snapshots.invalSnapshot(idx);

        // The timing partition scratchpad must be filled in by up.
        let newtok = TOKEN { index: idx,
                             poison: False,
                             epoch: TOKEN_EPOCH { branch: branchEpoch, fault: faultEpoch },
                             timep_info: TOKEN_TIMEP_INFO { scratchpad: 0 } };

        // Respond to the timing partition. End of macro operation.
        linkNewInFlight.makeResp(initFuncpRspNewInFlight(newtok));

    endrule

    // ******* DoITranslate ******* //

    // 2-stage macro-operation. Stages 1 and 2 may stall.
    
    // When:         The timing model tells us to translate a fetch address.
    // Effect:       Record the virtual address, access the TLB, return and cache the result.
    // Soft Inputs:  TOKEN, ISA_ADDRESS.
    // Soft Returns: One or Two MEM_ADDRESS, depending on the alignment.


    // doITranslate1
    
    // When:   The timing model makes a new ITranslate req.
    // Effect: Record the virtual address, make the req to the TLB.

    rule doITranslate1 (readyToBegin);

        // Get the input from the timing model. Begin macro operation.
        let req = linkDoITranslate.getReq();
        let tok = req.token;
        let vaddr = req.address;
        debugLog.record($format("TOKEN %0d: DoITranslate: Begin.", tok.index));
        
        // Update scoreboard.
        tokScoreboard.iTransStart(tok.index);

        // Record the address. (For relative branches, etc.)
        tokAddr.write(tok.index, vaddr);
        
        // Align the address.
        match {.aligned_addr, .offset_addr} = isaAlignAddress(vaddr);
        
        // Record the offset for the fetch stage.
        tokScoreboard.setFetchOffset(tok.index, offset_addr);

        if (!isaFetchSpansTwoMemValues(vaddr))
        begin
        
            // A normal ITranslate. No stall.
            linkDoITranslate.deq();

            // Get the translation from the TLB.
            link_itlb.makeReq(normalTLBQuery(tok, aligned_addr));
            
            // Log it.
            debugLog.record($format("TOKEN %0d: DoITranslate1: ITLB Req (VA: 0x%h, AA: 0x%h)", tok.index, vaddr, aligned_addr));
  
            // Pass to the next stage.
            iTransQ.enq(tagged ITRANS_NORMAL tok);

        end
        else     // A spanning fetch.
        begin

            // Log it.
            debugLog.record($format("TOKEN %0d: DoITranslate1: Spanning ITLB Req 1 (VA: 0x%h, AA1: 0x%h)", tok.index, vaddr, aligned_addr));
  
            // A spanning ITranslate. Make the first request to the TLB.
            link_itlb.makeReq(normalTLBQuery(tok, aligned_addr));
  
            // Stall to make the second request.
            stateITrans1 <= tagged ITRANS1_SPAN_REQ aligned_addr;

        end

    endrule

    // doITranslate1Span
    
    // When:   After doITranslate1 stalls because of an unaligned access.
    // Effect: Make the second request to the TLB and unstall.

    rule doITranslate1Span (readyToContinue &&& stateITrans1 matches tagged ITRANS1_SPAN_REQ .aligned_addr1);
         
        // Get the data from the previous stage.
        let req = linkDoITranslate.getReq();
        let tok = req.token;
    
        // Calculate the second virtual address.
        let aligned_addr2 = aligned_addr1 + fromInteger(valueOf(SizeOf#(MEM_VALUE)) / 8);
    
        // Make the second request to the tlb.
        link_itlb.makeReq(normalTLBQuery(tok, aligned_addr2));

        // Log it.
        debugLog.record($format("TOKEN %0d: DoITranslate1: Second ITLB Req 2 (AA2: 0x%h)", tok.index, aligned_addr2));
  
        // Unstall this stage.
        linkDoITranslate.deq();  
        stateITrans1 <= tagged ITRANS1_NORMAL;  

        // Pass to the next stage.
        iTransQ.enq(tagged ITRANS_SPAN tok);
    
    endrule

    // doITranslate2
    
    // When:   Some time after doITranslate1.
    // Effect: Get the response from the TLB, record it and return it.

    rule doITranslate2 (readyToContinue &&& stateITrans2 matches tagged ITRANS2_NORMAL);
    
        // Get the response from the TLB.
        let translated_addr = link_itlb.getResp();
        link_itlb.deq();

        // If the TLB couldn't translate it we're in big trouble.
        MEM_ADDRESS mem_addr = translated_addr.pa;
        Bool page_fault = translated_addr.page_fault;

        // Get the data from the previous stage.
        case (iTransQ.first()) matches
            tagged ITRANS_NORMAL .tok:
            begin

                // A single access. We do not stall.
                iTransQ.deq();

                // Log it.
                debugLog.record($format("TOKEN %0d: DoITranslate2: ITLB Rsp (PA: 0x%h)", tok.index, mem_addr));

                if (page_fault)
                begin
                    debugLog.record($format("TOKEN %0d: DoITranslate2: ITLB PAGE FAULT", tok.index));
                    tokScoreboard.setFault(tok.index, FAULT_ITRANS);
                end

                // Record the physical addr.
                tokPhysicalAddrs.write(tok.index, tagged ONE mem_addr);

                // Update the scoreboard.
                tokScoreboard.iTransFinish(tok.index);

                // Return it to the timing partition. End of macro-operation (path 1)
                linkDoITranslate.makeResp(initFuncpRspDoITranslate(tok, mem_addr, page_fault));
                debugLog.record($format("TOKEN %0d: DoITranslate: End (path 1).", tok.index));

            end
            tagged ITRANS_SPAN .tok:
            begin

                // A spanning access.

                // Log it.
                debugLog.record($format("TOKEN %0d: DoITranslate2: ITLB Spanning Rsp 1 (PA1: 0x%h)", tok.index, mem_addr));

                if (page_fault)
                begin
                    debugLog.record($format("TOKEN %0d: DoITranslate2: ITLB PAGE FAULT", tok.index));
                    tokScoreboard.setFault(tok.index, FAULT_ITRANS);
                end

                // Return the first part to the timing partition.
                linkDoITranslate.makeResp(initFuncpRspDoITranslate_part1(tok, mem_addr, page_fault));

                // Stall this stage to get the second response.
                stateITrans2 <= tagged ITRANS2_SPAN_RSP
                                       {
                                           firstPA: mem_addr,
                                           firstRefFaulted: page_fault
                                       };

            end
        endcase
    
    endrule
    
    // doITranslate2Span
    
    // When:   After doITranslate1 stalls for a lookup which spans two locations.
    // Effect: Get the second response, record it, and return it.

    rule doITranslate2Span (readyToContinue &&& stateITrans2 matches tagged ITRANS2_SPAN_RSP .trans1);
    
        // Get the data from the previous stage.
        let tok = getITransToken(iTransQ.first());
    
        // Propagate poison bit from first translation
        tok.poison = tok.poison || trans1.firstRefFaulted;

        // Get the response from the TLB.
        let translated_addr = link_itlb.getResp();
        link_itlb.deq();

        // If the TLB couldn't translate it we're in big trouble.
        MEM_ADDRESS mem_addr2 = translated_addr.pa;
        Bool page_fault = translated_addr.page_fault;

        if (page_fault)
        begin
            debugLog.record($format("TOKEN %0d: DoITranslate2: ITLB Spanning 2 PAGE FAULT", tok.index));
            tokScoreboard.setFault(tok.index, FAULT_ITRANS2);
        end

        // Log it.
        debugLog.record($format("TOKEN %0d: DoITranslate2: ITLB Spanning Rsp 2 (PA2: 0x%h)", tok.index, mem_addr2));

        // Record the physical addr.
        tokPhysicalAddrs.write(tok.index, tagged TWO tuple2(trans1.firstPA, mem_addr2));

        // Unstall the pipeline.
        stateITrans2 <= ITRANS2_NORMAL;
        iTransQ.deq();

        // Update the scoreboard.
        tokScoreboard.iTransFinish(tok.index);

        // Return the rest to the timing partition. End of macro-operation (path 2).
        linkDoITranslate.makeResp(initFuncpRspDoITranslate_part2(tok, mem_addr2, page_fault));
        debugLog.record($format("TOKEN %0d: DoITranslate: End (path 2).", tok.index));
    
    endrule

    // ******* getInstruction ******* //

    // 3-stage macro-operation. Stages 2 and 3 may stall.
    
    // When:         The timing model tells us to fetch the instruction at a given address.
    // Effect:       Reads the DT (twice if unaligned), records the instruction.
    // Soft Inputs:  TOKEN from timing model.
    // Soft Returns: TOKEN and ISA_INSTRUCTION.


    // getInstruction1
    
    // When:   The timing model makes a new FETCH req.
    // Effect: Retrieve the phsyical address(es).

    rule getInstruction1 (readyToBegin);

        // Read input. Beginning of macro-operation.
        let req = linkGetInst.getReq();
        let tok = req.token;
        linkGetInst.deq();
        debugLog.record($format("TOKEN %0d: GetInstruction: Begin.", tok.index));

        // Update scoreboard.
        tokScoreboard.fetStart(tok.index);

        // Retrieve the physical address.
        tokPhysicalAddrs.readReq(tok.index);
        
        // Send to the next stage.
        inst1Q.enq(tok);
        
    endrule
    
    // getInstruction2 
    
    // When:   Some time after getInstruction1 and we are not stalled.
    // Effect: Pass the physical address on to memory. If the address spans
    //         memory locations stall for a second request.

    rule getInstruction2 (readyToContinue &&& stateInst2 matches tagged INST2_NORMAL);
    
        // Get the info from the previous stage.
        let tok = inst1Q.first();
        
        // Get the physical address(es).
        let p_addrs <- tokPhysicalAddrs.readRsp();
        
        case (p_addrs) matches
            tagged ONE .p_addr:
            begin

                // We are not stalled.
                inst1Q.deq();
                
                // Log it.
                debugLog.record($format("TOKEN %0d: GetInstruction2: Load Req (PA: 0x%h)", tok.index, p_addr));

                // Kick to Mem State.
                let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr};
                linkToMem.makeReq(tagged REQ_LOAD m_req);

                // Record that the result should come to us.
                memPathQ.enq(PATH_INST);
                
                // Pass it to the next stage.
                inst2Q.enq(INST_INFO {token: tok, memAddrs: p_addrs});

            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Log it.
                debugLog.record($format("TOKEN %0d: GetInstruction2: Spanning Load Req 1 (PA1: 0x%h, PA2: 0x%h)", tok.index, p_addr1, p_addr2));

                // Kick the first request to the MemState.
                let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr1};
                linkToMem.makeReq(tagged REQ_LOAD m_req);

                // Record that the result should come to us.
                memPathQ.enq(PATH_INST);
                
                // Stall to make the second request.
                stateInst2 <= tagged INST2_SPAN_REQ (INST_INFO {token: tok, memAddrs: p_addrs});

            end

        endcase
        
    
    endrule
    
    // getInstruction2Span
    
    // When:   After getInstruction2 stalls because of a spanning instruction.
    // Effect: Make the second request to memory. Unstall the stage.

    rule getInstruction2Span (readyToContinue &&& stateInst2 matches tagged INST2_SPAN_REQ .fetch_info);
            
        // Get the data from the previous stage.
        let tok = inst1Q.first();
        
        // Kick the second request to MemState.
        let p_addr2 = getSecondOfTwo(fetch_info.memAddrs);
        let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr2};
        linkToMem.makeReq(tagged REQ_LOAD m_req);

        // Log it.
        debugLog.record($format("TOKEN %0d: GetInstruction2: Spanning Load Req 2 (PA2: 0x%h)", tok.index, p_addr2));

        // Record that the result should come to us.
        memPathQ.enq(PATH_INST);

        // Unstall this stage.
        inst1Q.deq();
        stateInst2 <= tagged INST2_NORMAL;
        
        // Pass it to the next stage.
        inst2Q.enq(fetch_info);
        
    endrule

    // getInstruction3

    // When:   Physical address and instruction are available after getInstruction2.
    // Effect: If there was just one request, record the instruction, kick back to timing model.
    //         Otherwise stall to get the second response.

    rule getInstruction3 (readyToContinue &&& stateInst3 matches tagged INST3_NORMAL
                          &&& memPathQ.first() matches tagged PATH_INST);

        // Get the data from the previous stage.
        let fetch_info = inst2Q.first();

        // Get resp from the Mem State.
        MEM_VALUE v = linkToMem.getResp();
        linkToMem.deq();
        memPathQ.deq();
     
        case (fetch_info.memAddrs) matches
            tagged ONE .p_addr:
            begin

                // Normal fetch. We are not stalled.
                inst2Q.deq();
                
                // Get the offset from ITranslate.
                let offset = tokScoreboard.getFetchOffset(fetch_info.token.index);

                ISA_INSTRUCTION inst = isaInstructionFromMemValue(v, offset);

                // Log it.
                debugLog.record($format("TOKEN %0d: GetInstruction3: Load Rsp (V: 0x%h, I: 0x%h)", fetch_info.token.index, v, inst));

                // Record the instruction.
                tokInst.write(fetch_info.token.index, inst);

                // Update scoreboard.
                tokScoreboard.fetFinish(fetch_info.token.index);

                // Send response to timing partition. End of macro-operation (path 1).
                linkGetInst.makeResp(initFuncpRspGetInstruction(fetch_info.token, inst));
                debugLog.record($format("TOKEN %0d: GetInstruction: End (path 1).", fetch_info.token.index));


            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Log it.
                debugLog.record($format("TOKEN %0d: getInstruction3: Spanning Load Rsp 1 (V1: 0x%h)", fetch_info.token.index, v));

                // We need two fetches for this guy. Stall for the second response.
                stateInst3 <= tagged INST3_SPAN_RSP v;

            end
        endcase

    endrule

    // getInstruction3Span

    // When:   After getInstruction3 has stalled waiting for a second response.
    // Effect: Use both responses to create the instruction. Record it and return it to the timing model.

    rule getInstruction3Span (readyToContinue &&& stateInst3 matches tagged INST3_SPAN_RSP .v1
                              &&& memPathQ.first() matches tagged PATH_INST);
    
        // Get the data from the previous stage.
        INST_INFO fetch_info = inst2Q.first();
        
        // Get resp from the Mem State.
        MEM_VALUE v2 = linkToMem.getResp();
        linkToMem.deq();
        memPathQ.deq();
        
        // Get the offset from ITranslate.
        let offset = tokScoreboard.getFetchOffset(fetch_info.token.index);

        // Convert the raw bits to an instruction.
        ISA_INSTRUCTION inst = isaInstructionFromSpanningMemValues(v1, v2, offset);

        // Log it.
        debugLog.record($format("TOKEN %0d: GetInstruction3: Spanning Load Rsp 2 (V2: 0x%h, I: 0x%h)", fetch_info.token.index, v2, inst));

        // Record the instruction.
        tokInst.write(fetch_info.token.index, inst);

        // Update scoreboard.
        tokScoreboard.fetFinish(fetch_info.token.index);

        // Unstall this stage.
        inst2Q.deq();
        stateInst3 <= tagged INST3_NORMAL;
        
        // Send response to timing partition. End of macro-operation (path 2).
        linkGetInst.makeResp(initFuncpRspGetInstruction(fetch_info.token, inst));
        debugLog.record($format("TOKEN %0d: GetInstruction: End (path 2).", fetch_info.token.index));
        
    endrule


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

    rule getDependencies1 (readyToBegin);

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

    rule getDependencies2 (readyToContinue &&& stateDeps2 matches tagged DEPS2_NORMAL);

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
            prfValids[new_preg] <= False;
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
        
        assertInvalidNumDsts(num_dsts >= true_n_dsts);
        assertEmulatedInstrNoDsts((num_dsts == 0) || !is_emulated);

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
    
     rule getDependencies2AdditionalMappings (readyToContinue &&& stateDeps2 matches tagged DEPS2_ALLOC_MORE .dep_info);

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

        // Check that epoch didn't advance in the middle of getDependences.
        // Epoch is changed by rewind and rewind can only begin when the no
        // instruction is in the middle of physical register mappings.
        // getDependencies2 is supposed to ensure we never get this far with
        // junk tokens.
        assertNoEpochChange(tokBranchEpoch(tok) == branchEpoch);

        if (isValid(map_dsts[cur]))
        begin

            // Update the maptable.
            maptable <= update(maptable, pack(arc_dst), phy_dst);

            // Reset the reg to unready.
            prfValids[phy_dst] <= False;

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

    // ******* getResults ******* //
    
    // 4-stage macro operation. Stages 2 and 4 can stall.

    // When:   When the timing model requests an execution.
    // Effect: Perform register reads, then send to datapath for execution.
    // Soft Inputs:  Token
    // Soft Returns: Token, Result
    
    // getResults1

    // When:   When the timing model starts a getResults().
    // Effect: Lookup the locations of this token's sources.

    rule getResults1 (readyToBegin);

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
            state <= RSM_DrainingForEmulate;

            // Record which token is being emulated.
            emulatingToken <= tok;

            // Lookup the snapshot we should be working with.
            let msnap = snapshots.hasSnapshot(tok.index);

            // If there's no snapshot, something is really wrong.
            assertHaveSnapshotOfEmulatedInstruction(isValid(msnap));

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

    rule getResults2 (readyToContinue && !execStalling);

        // Get input from getResults1.
        let tok = res1Q.first();
        res1Q.deq();

        // Response from previous stage.
        let ws <- tokWriters.readRsp();
        
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
            debugLog.record($format("TOKEN %0d: GetResults2: Letting Junk Proceed!", tok.index));
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
            
            // Log it.
            debugLog.record($format("TOKEN %0d: GetResults2: Need to request the srcs in this mask: %b", tok.index, pack(values_needed)));
            
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
                        prf.readPorts[0].readReq(r);
                        reqs_made[0] = True;
                        resEvenQ.enq(0);
                        debugLog.record($format("TOKEN %0d: GetResults2: Requesting src 0.", tok.index));
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
                            prf.readPorts[1].readReq(r);
                            reqs_made[1] = True;
                            resOddQ.enq(1);
                            debugLog.record($format("TOKEN %0d: GetResults2: Requesting src 1.", tok.index));
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
            
            prf.readPorts[0].readReq(r);
            execStallReqsMade[x] <= True;
            resEvenQ.enq(fromInteger(x));
            debugLog.record($format("TOKEN %0d: GetResults2S: Requesting src %0d.", execStallTok.index, fromInteger(x)));

        endrule
    
        rule getResults2StallRspEven (readyToContinue && execStalling &&&
                                      execStallValuesNeeded[x] &&& // We need this register.
                                      execStallReqsMade[x] &&&  //We made the request already.
                                      resEvenQ.first() == fromInteger(x)); // Our response is next in line.
            
            let v <- prf.readPorts[0].readRsp();
            execStallValues[x] <= v;
            execStallValuesNeeded[x] <= False;
            resEvenQ.deq();
            debugLog.record($format("TOKEN %0d: GetResults2S: Receiving src %0d.", execStallTok.index, fromInteger(x)));

        endrule
        
        // Repeat the above two rules using the second PRF port. (Should be exact copies except for index.)
        if (x+1 < valueof(ISA_MAX_SRCS))
        begin
            rule getResults2StallReqOdd (readyToContinue && execStalling &&&
                                          execStallWriters[x+1] matches tagged Valid .r &&& // We need this register...
                                          prfValids[r] &&& // The register is ready...
                                          !execStallReqsMade[x+1]); //We haven't made the request yet.

                prf.readPorts[1].readReq(r);
                execStallReqsMade[x+1] <= True;
                resOddQ.enq(fromInteger(x+1));
                debugLog.record($format("TOKEN %0d: GetResults2S: Requesting src %0d.", execStallTok.index, fromInteger(x+1)));

            endrule

            rule getResults2StallRspOdd (readyToContinue && execStalling &&&
                                         execStallValuesNeeded[x+1] &&& // We need this register
                                         execStallReqsMade[x+1] &&&  //We made the request already.
                                         resOddQ.first() == fromInteger(x+1)); // Our response is next in line.

                let v <- prf.readPorts[1].readRsp();
                execStallValues[x+1] <= v;
                execStallValuesNeeded[x+1] <= False;
                resOddQ.deq();
                debugLog.record($format("TOKEN %0d: GetResults2S: Receiving src %0d.", execStallTok.index, fromInteger(x+1)));

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
        tokInst.readPorts[1].readReq(execStallTok.index);
        debugLog.record($format("TOKEN %0d: GetResults2: All sources ready. Proceeding...", execStallTok.index));

    endrule

    // getResults3
    // When:    After getResults2 or alternatively getResults2StallEnd
    // Effect:  Send all the data to the datapath.

    rule getResults3 (readyToContinue);

        // Get input from the previous stage.
        let tok = res2Q.first();
        res2Q.deq();

        // Get all the data the previous stage kicked off.
        let addr <- tokAddr.readRsp();
        let inst <- tokInst.readPorts[1].readRsp();

        // Combine the data we just go with any possible data from stalling.
        Vector#(ISA_MAX_SRCS, ISA_VALUE) values = newVector();

        for (Integer x = 0; x < valueof(ISA_MAX_SRCS); x = x + 1)
           values[x] = execStallValues[x];

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

    rule getResults4 (readyToContinue &&& stateRes4 matches tagged RES4_NORMAL);

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
                    prfValids[dst] <= True;
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

        rule getResults4AdditionalWriteback (readyToContinue &&& stateRes4 matches tagged RES4_ADDITIONAL_WB .wb_info);
        
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
                    prfValids[dst] <= True;
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
    
    rule emulateInstruction1 (state == RSM_DrainingForEmulate && tokScoreboard.canEmulate());

        // Did the timing model do drain before correctly?
        assertExpectedOldestTok(emulatingToken.index == tokScoreboard.oldest());
        if (emulatingToken.index != tokScoreboard.oldest())
            debugLog.record($format("TOKEN %0d: emulateInstruction1:  Token is not oldest! (Oldest: %0d)", emulatingToken.index, tokScoreboard.oldest()));

        // Reset the counter for syncing registers.
        synchronizingCurReg <= minBound;

        // Start syncing registers.
        state <= RSM_SyncingRegisters;
               
    endrule

    // emulateInstruction2_Req
    
    // When:   After the getResults operation puts us into the emulation state, this
    //         rule happens once for each architectural register.
    // Effect: Look up the current physical register in the maptable and request it from the regfile.
    
    
    rule emulateInstruction2_Req (state == RSM_SyncingRegisters);
    
        // Some ISA's have a sparse packing of register names.  They should define Arith so we 
        // don't transmit them spuriously.

        // Get the maptable at the time of the emulated instruction.
        let emulation_map <- snapshots.returnSnapshot();

        // Lookup which register to send next.
        FUNCP_PHYSICAL_REG_INDEX current_pr = emulation_map[pack(synchronizingCurReg)];

        // Make the request to the regfile.
        prf.readPorts[0].readReq(current_pr);

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
            state <= RSM_RequestingEmulation;
        
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
        ISA_VALUE reg_val <- prf.readPorts[0].readRsp();
        
        // Send the regsiter on to software via RRR
        client_stub.makeRequest_sync(tuple2(arch_reg, reg_val));

        //Log it.
        debugLog.record($format("TOKEN %0d: EmulateInstruction2: Transmitting Register R%0d (PR%0d) = 0x%h.", emulatingToken.index, arch_reg, phys_reg, reg_val));
    
    endrule
    
    // emulateInstruction3
    
    // When:   After emulateInstruction1 has transmitted every architectural register.
    // Effect: Send the instruction emulation request to software via RRR.

    rule emulateInstruction3 (state == RSM_RequestingEmulation);
        
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
        state <= RSM_UpdatingRegisters;

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
        assertRegUpdateAtExpectedTime(state == RSM_UpdatingRegisters);
        
        // Lookup the current physical register in the snapshot maptable.
        FUNCP_PHYSICAL_REG_INDEX pr = emulation_map[pack(r)];
        
        // Update the regfile.
        prf.write(pr, v);
        prfValids[pr] <= True;
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
        debugLog.record($format("TOKEN %0d: EmulateInstruction3: Emulation finished.", emulatingToken.index));
  
        // Send the response to the timing model.
        // End of macro-operation.
        linkGetResults.makeResp(initFuncpRspGetResults(emulatingToken, emulatingPC, resp));

        debugLog.record($format("TOKEN %0d: GetResults: End (path 3).", emulatingToken.index));

    endrule

    // ******* DoDTranslate ******* //

    // 3-stage macro-operation
    
    // When:         The timing model tells us to translate an effective address.
    // Effect:       Access the TLB, return and cache the result.
    // Soft Inputs:  TOKEN
    // Soft Returns: One or Two MEM_ADDRESS, depending on the alignment.


    // doDTranslate1
    
    // When:   The timing model makes a new DTranslate req.
    // Effect: Retrieve the effective address.

    rule doDTranslate1 (readyToBegin);

        // Get the input from the timing model. Begin macro operation.
        let req = linkDoDTranslate.getReq();
        linkDoDTranslate.deq();
        let tok = req.token;
        debugLog.record($format("TOKEN %0d: DoDTranslate: Start.", tok.index));

        // Update scoreboard.
        tokScoreboard.dTransStart(tok.index);

        // This operation on non-Load/Stores is a problem.
        let is_load = tokScoreboard.isLoad(tok.index);
        let is_store = tokScoreboard.isStore(tok.index);
        assertDTranslateOnMemOp(is_load || is_store);

        // Get the optype since we're using the port now.
        let op_type = is_load ? tokScoreboard.getLoadType(tok.index) : tokScoreboard.getStoreType(tok.index);

        // Retrieve the mem address.
        tokMemAddr.readReq(tok.index);

        // Pass to the next stage.
        dTrans1Q.enq(tuple2(tok, op_type));

    endrule

    // doDTranslate2
    
    // When:   After the response come back from the RAM.
    // Effect: Make the request to the DTLB.
    
    rule doDTranslate2 (readyToContinue &&& stateDTrans2 matches tagged DTRANS2_NORMAL);
    
        // Get the value from the previous stage.
        match {.tok, .op_type} = dTrans1Q.first();
        
        // Get the response from memory.        
        let vaddr <- tokMemAddr.readRsp();

        // Align the address.
        match {.aligned_addr, .offset_addr} = isaAlignAddress(vaddr);
        
        // Record the offset for the load/store stage.
        tokScoreboard.setMemOpOffset(tok.index, offset_addr);

        if (!isaMemOpSpansTwoMemValues(vaddr, op_type))
        begin
        
            // A normal DTranslate. No stall.
            dTrans1Q.deq();

            // Get the translation from the TLB.
            link_dtlb.makeReq(normalTLBQuery(tok, aligned_addr));

            // Log it.
            debugLog.record($format("TOKEN %0d: DoDTranslate2: DTLB Req (VA: 0x%h AA: 0x%h)", tok.index, vaddr, aligned_addr));
  
            // Pass to the next stage.
            dTrans2Q.enq(tagged DTRANS_NORMAL tok);

        end
        else // A spanning load/store.
        begin

            // Log it.
            debugLog.record($format("TOKEN %0d: DoDTranslate2: DTLB Req Spanning 1 (VA: 0x%h, AA1: 0x%h)", tok.index, vaddr, aligned_addr));
  
            // A spanning DTranslate. Make the first request to the TLB.
            link_dtlb.makeReq(normalTLBQuery(tok, aligned_addr));
            
            // Stall to make the second request.
            stateDTrans2 <= tagged DTRANS2_SPAN_REQ aligned_addr;

        end

    endrule

    // doDTranslate2Span
    
    // When:   After doDTranslate2 stalls because of a spanning access.
    // Effect: Make the second request to the TLB and unstall.

    rule doDTranslate2Span (readyToContinue &&& stateDTrans2 matches tagged DTRANS2_SPAN_REQ .aligned_addr1);
    
        // Get the value from the previous stage.
        match {.tok, .op_type} = dTrans1Q.first();
        
        // Calculate the second virtual address.
        let aligned_addr2 = aligned_addr1 + fromInteger(valueOf(SizeOf#(MEM_VALUE)) / 8);
        
        // Make the second request to the tlb.
        link_dtlb.makeReq(normalTLBQuery(tok, aligned_addr2));

        // Log it.
        debugLog.record($format("TOKEN %0d: DoDTranslate2: DTLB Req Spanning 2 (AA2: 0x%h)", tok.index, aligned_addr2));
  
        // Unstall this stage.
        dTrans1Q.deq();  
        stateDTrans2 <= tagged DTRANS2_NORMAL;  

        // Pass to the next stage.
        dTrans2Q.enq(tagged DTRANS_SPAN tok);
    
    endrule

    // doDTranslate3
    
    // When:   Some time after doDTranslate3.
    // Effect: Get the response from the TLB, record it and return it.

    rule doDTranslate3 (readyToContinue &&& stateDTrans3 matches tagged DTRANS3_NORMAL);
        
        // Get the response from the TLB.
        let translated_addr = link_dtlb.getResp();
        link_dtlb.deq();

        // If the TLB couldn't translate it we're in big trouble.
        MEM_ADDRESS mem_addr = translated_addr.pa;
        Bool page_fault = translated_addr.page_fault;

        case (dTrans2Q.first()) matches
            tagged DTRANS_NORMAL .tok:
            begin

                // A single access. We do not stall.
                dTrans2Q.deq();

                // Log it.
                debugLog.record($format("TOKEN %0d: DoDTranslate3: DTLB Rsp (PA: 0x%h)", tok.index, mem_addr));

                if (page_fault)
                begin
                    debugLog.record($format("TOKEN %0d: DoDTranslate3: DTLB PAGE FAULT", tok.index));
                    tokScoreboard.setFault(tok.index, FAULT_DTRANS);
                end

                // Record the physical addr.
                tokPhysicalMemAddrs.write(tok.index, tagged ONE mem_addr);

                // Update the scoreboard.
                tokScoreboard.dTransFinish(tok.index);

                // Return it to the timing partition. End of macro-operation (path 1)
                linkDoDTranslate.makeResp(initFuncpRspDoDTranslate(tok, mem_addr, page_fault));
                debugLog.record($format("TOKEN %0d: doDTranslate: End (path 1).", tok.index));
                

            end
            tagged DTRANS_SPAN .tok:
            begin

                // A spanning access.

                // Log it.
                debugLog.record($format("TOKEN %0d: DoDTranslate3: DTLB Span Rsp 1 (PA1: 0x%h)", tok.index, mem_addr));

                if (page_fault)
                begin
                    debugLog.record($format("TOKEN %0d: DoDTranslate3: DTLB PAGE FAULT", tok.index));
                    tokScoreboard.setFault(tok.index, FAULT_DTRANS);
                end

                // Return the first part to the timing partition.
                linkDoDTranslate.makeResp(initFuncpRspDoDTranslate_part1(tok, mem_addr, page_fault));

                // Stall this stage to get the second response.
                stateDTrans3 <= tagged DTRANS3_SPAN_RSP
                                       {
                                           firstPA: mem_addr,
                                           firstRefFaulted: page_fault
                                       };

            end
        endcase
    
    endrule
    
    rule doDTranslate3Span (readyToContinue &&& stateDTrans3 matches tagged DTRANS3_SPAN_RSP .trans1);
    
        // Get the value from the previous stage.
        let tok = getDTransToken(dTrans2Q.first());

        // Propagate poison bit from first translation
        tok.poison = tok.poison || trans1.firstRefFaulted;

        // Get the response from the TLB.
        let translated_addr = link_dtlb.getResp();
        link_dtlb.deq();

        // If the TLB couldn't translate it we're in big trouble.
        MEM_ADDRESS mem_addr2 = translated_addr.pa;
        Bool page_fault = translated_addr.page_fault;

        if (page_fault)
        begin
            debugLog.record($format("TOKEN %0d: DoDTranslate3: DTLB Spanning 2 PAGE FAULT", tok.index));
            tokScoreboard.setFault(tok.index, FAULT_DTRANS2);
        end

        // Log it.
        debugLog.record($format("TOKEN %0d: DoDTranslate3: DTLB Span Rsp 2 (PA2: 0x%h)", tok.index, mem_addr2));

        // Record the physical addresses.
        tokPhysicalMemAddrs.write(tok.index, tagged TWO tuple2(trans1.firstPA, mem_addr2));

        // Unstall the pipeline.
        dTrans2Q.deq();
        stateDTrans3 <= DTRANS3_NORMAL;

        // Update the scoreboard.
        tokScoreboard.dTransFinish(tok.index);

        // Return the rest to the timing partition. End of macro-operation (path 2).
        linkDoDTranslate.makeResp(initFuncpRspDoDTranslate_part2(tok, mem_addr2, page_fault));
        debugLog.record($format("TOKEN %0d: DoDTranslate: End (path 2).", tok.index));
    
    endrule


    // ******* doLoads ******* //

    // 3 or 4-stage macro operation which makes Loads read memory.

    // When:   When the timing model requests it.
    // Effect: Read the effective address, do a load from the memory state, and write it back.
    // Soft Inputs:  Token
    // Soft Returns: Token
    
    // doLoads1

    // When:   When the timing model starts a doLoads().
    // Effect: Lookup the effective address(es) of this token.

    rule doLoads1 (readyToBegin);

        // Get the input from the timing model. Begin macro-operation.
        let req = linkDoLoads.getReq();
        linkDoLoads.deq();
        let tok = req.token;

        // Confirm timing model propagated poison bit correctly
        assertPoisonBit(tokIsPoisoned(tok) == isValid(tokScoreboard.getFault(tok.index)));

        // If it's not actually a load, it's an exception.
        let isLoad = tokScoreboard.isLoad(tok.index);
        assertInstructionIsActuallyALoad(isLoad);

        // Emulated loads were taken care of previously.  Also ignore killed tokens.
        if (tokScoreboard.emulateInstruction(tok.index) ||
            ! tokScoreboard.isAllocated(tok.index))
        begin
            // Log it.
            debugLog.record($format("TOKEN %0d: DoLoads1: Ignoring junk token or emulated instruction.", tok.index));

            // Respond to the timing model. End of macro-operation.
            linkDoLoads.makeResp(initFuncpRspDoLoads(tok));
        end
        else // Everything's okay.
        begin
            // Log it.
            debugLog.record($format("TOKEN %0d: DoLoads: Begin.", tok.index)); 

            // Update the scoreboard.
            tokScoreboard.loadStart(tok.index);

            // Read the effective address.
            tokPhysicalMemAddrs.readPorts[0].readReq(tok.index);

            // Pass to the next stage.
            loads1Q.enq(tok);
        end

    endrule

    // doLoads2

    // When:   After doLoads1 occurs
    // Effect: Make the request to the memory state.

    rule doLoads2 (readyToContinue &&& stateLoads2 matches tagged LOADS2_NORMAL);

        // Read the parameters from the previous stage.
        let tok = loads1Q.first();

        // Get the address(es).
        let p_addrs <- tokPhysicalMemAddrs.readPorts[0].readRsp();
        
        // Get the offset.
        let offset = tokScoreboard.getMemOpOffset(tok.index);
        
        // Get the optype.
        let l_type = tokScoreboard.getLoadType(tok.index);
        
        case (p_addrs) matches
            tagged ONE .p_addr:
            begin

                // Normal Load. We're not stalled.
                loads1Q.deq();

                // Log it.
                debugLog.record($format("TOKEN %0d: DoLoads2: Requesting Load (PA: 0x%h)", tok.index, p_addr));

                // Make the request to the DMem.
                let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr};
                linkToMem.makeReq(tagged REQ_LOAD m_req);

                // Record that the load response should go to us.
                memPathQ.enq(PATH_LOAD);

                // Read the destination so we can writeback the correct register.
                tokDsts.readPorts[1].readReq(tok.index);

                // Pass it on to the final stage.
                let load_info = LOADS_INFO {token: tok, memAddrs: p_addrs, offset: offset, opType: l_type};
                loads2Q.enq(load_info);

            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Log it.
                debugLog.record($format("TOKEN %0d: DoLoads2: Starting Spanning Load (PA1: 0x%h)", tok.index, p_addr1));

                // Make the request to the DMem.
                let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr1};
                linkToMem.makeReq(tagged REQ_LOAD m_req);

                // Record that the load response should go to us.
                memPathQ.enq(PATH_LOAD);

                // Stall this stage for the second req.
                let load_info = LOADS_INFO {token: tok, memAddrs: p_addrs, offset: offset, opType: l_type};
                stateLoads2 <= tagged LOADS2_SPAN_REQ load_info;

            end
        endcase

    endrule

    rule doLoads2Span (readyToContinue &&& stateLoads2 matches tagged LOADS2_SPAN_REQ .load_info);

        // Kick the second request to MemState.
        let p_addr2 = getSecondOfTwo(load_info.memAddrs);
        let m_req = MEMSTATE_REQ_LOAD {tok: load_info.token, addr: p_addr2};
        linkToMem.makeReq(tagged REQ_LOAD m_req);

        // Record that the load response should go to us.
        memPathQ.enq(PATH_LOAD);
        
        // Log it.
        debugLog.record($format("TOKEN %0d: DoLoads2: Finishing Spanning Load (PA2: 0x%h)", load_info.token.index, p_addr2));

        // Read the destination so we can writeback the correct register.
        tokDsts.readPorts[1].readReq(load_info.token.index);

        // Unstall this stage.
        loads1Q.deq();
        stateLoads2 <= tagged LOADS2_NORMAL;

        // Pass it on to the final stage.
        loads2Q.enq(load_info);

    endrule


    // doLoads3

    // When:   Load response is available after doLoads2.
    // Effect: If there was just one request, record the resut, kick back to timing model.
    //         Otherwise stall to get the second response.

    rule doLoads3 (readyToContinue &&& stateLoads3 matches tagged LOADS3_NORMAL
                                   &&& memPathQ.first() matches tagged PATH_LOAD);

        // Get the data from the previous stage.
        let load_info = loads2Q.first();
        let tok = load_info.token;

        // Get resp from the Mem State.
        MEM_VALUE v = linkToMem.getResp();
        linkToMem.deq();
        memPathQ.deq();
     
        case (load_info.memAddrs) matches
            tagged ONE .p_addr:
            begin

                // Normal load. We are not stalled.
                loads2Q.deq();

                // Convert the value using the ISA-provided conversion function.
                ISA_VALUE val = isaLoadValueFromMemValue(v, load_info.offset, load_info.opType);
                debugLog.record($format("TOKEN %0d: DoLoads3: ISA Load (V: 0x%h, T: %0d, O: %b) = 0x%h", tok.index, v, pack(load_info.opType), load_info.offset, val)); 

                // Get the destination for the purposes of writeback.
                let dsts <- tokDsts.readPorts[1].readRsp();

                // We assume that the destination for the load is destination 1.
                let dst = validValue(dsts[0]);

                // Log it.
                debugLog.record($format("TOKEN %0d: DoLoads3: Load Response Writing (PR%0d <= 0x%h)", tok.index, dst, val));

                // Update the physical register file.
                prf.write(dst, val);

                // Assert that the register was ready (not valid).
                assertLoadDestRegIsReady(!prfValids[dst]);

                // The register is now valid.
                prfValids[dst] <= True;

                // Update the scoreboard.
                tokScoreboard.loadFinish(tok.index);

                // Respond to the timing model. End of macro-operation (path 1).
                linkDoLoads.makeResp(initFuncpRspDoLoads(tok));

            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Log it.
                debugLog.record($format("TOKEN %0d: DoLoads3: First Span Response (V1: 0x%h)", load_info.token.index, v));

                // We needed two loads for this guy. Stall for the second response.
                stateLoads3 <= tagged LOADS3_SPAN_RSP v;

            end

        endcase
    

    endrule

    // doLoads3Span

    // When:   After doLoads3 has stalled waiting for a second response.
    // Effect: Use both responses to create the value. Write it back and return to the timing model.

    rule doLoads3Span (readyToContinue &&& stateLoads3 matches tagged LOADS3_SPAN_RSP .v1
                              &&& memPathQ.first() matches tagged PATH_LOAD);
    
        // Get the data from the previous stage.
        LOADS_INFO load_info = loads2Q.first();
        let tok = load_info.token;
        
        // Get resp from the Mem State.
        MEM_VALUE v2 = linkToMem.getResp();
        linkToMem.deq();
        memPathQ.deq();
        
        // Log it.
        debugLog.record($format("TOKEN %0d: DoLoads3: Second Span Response (V2: 0x%h)", tok.index, v2));

        // Convert the value using the ISA-provided conversion function.
        ISA_VALUE val = isaLoadValueFromSpanningMemValues(v1, v2, load_info.offset, load_info.opType);
        debugLog.record($format("TOKEN %0d: DoLoads3: ISA SpanLoad (V1: 0x%h, V2: 0x%hm, T: %0d, O: %b) = 0x%h", tok.index, v1, v2, pack(load_info.opType), load_info.offset, val)); 

        // Get the destination for the purposes of writeback.
        let dsts <- tokDsts.readPorts[1].readRsp();

        // We assume that the destination for the load is destination 1.
        let dst = validValue(dsts[0]);

        // Log it.
        debugLog.record($format("TOKEN %0d: DoLoads3: Load Response Writing (PR%0d <= 0x%h)", tok.index, dst, val));

        // Update the physical register file.
        prf.write(dst, val);

        // Assert that the register was ready (not valid).
        assertLoadDestRegIsReady(!prfValids[dst]);

        // The register is now valid.
        prfValids[dst] <= True;

        // Unstall this stage.
        loads2Q.deq();
        stateLoads3 <= tagged LOADS3_NORMAL;

        // Update the scoreboard.
        tokScoreboard.loadFinish(tok.index);

        // Respond to the timing model. End of macro-operation (path 2).
        linkDoLoads.makeResp(initFuncpRspDoLoads(tok));
        
    endrule


    // ******* doStores ******* //

    // 2-stage macro operation. Stage 2 can stall in two different ways.

    // When:   When the timing model requests it.
    // Effect: Read the effective address and result, do a store to the memory state.
    // Soft Inputs:  Token
    // Soft Returns: Token


    // doStores1

    // When:   When the timing model starts a doStores().
    // Effect: Lookup the destination of this token.

    rule doStores1 (readyToBegin);

        // Get the input from the timing model. Begin macro-operation.
        let req = linkDoStores.getReq();
        linkDoStores.deq();
        let tok = req.token;

        // Confirm timing model propagated poison bit correctly
        assertPoisonBit(tokIsPoisoned(tok) == isValid(tokScoreboard.getFault(tok.index)));

        // If it's not actually a store, it's an exception.
        let isStore = tokScoreboard.isStore(tok.index);
        assertInstructionIsActuallyAStore(isStore);

        if (tokScoreboard.emulateInstruction(tok.index) ||
            ! tokScoreboard.isAllocated(tok.index))
        begin
            // Log it.
            debugLog.record($format("TOKEN %0d: DoStores: Ignoring junk token or emulated instruction.", tok.index));

            // Respond to the timing model. End of macro-operation.
            linkDoStores.makeResp(initFuncpRspDoStores(tok));
        end
        else // Everything's fine.
        begin
            // Log it.
            debugLog.record($format("TOKEN %0d: DoStores: Begin.", tok.index)); 

            // Update the scoreboard.
            tokScoreboard.storeStart(tok.index);

            // Read the store value.
            tokStoreValue.readReq(tok.index);

            // Read the effective address(es).
            tokPhysicalMemAddrs.readPorts[1].readReq(tok.index);

            // Get the store type.
            let st_type = tokScoreboard.getStoreType(tok.index);

            // Pass to the next stage.
            stores1Q.enq(tuple2(tok, st_type));
        end

    endrule
    
    // doStores2

    // When:   After we get a response from the address RAM.
    // Effect: Read the physical register file and the effective address. 

    rule doStores2 (readyToContinue &&& stateStores2 matches tagged STORES2_NORMAL);

        // Read the parameters from the previous stage.
        match {.tok, .st_type} = stores1Q.first();

        // Get the offset.
        let offset = tokScoreboard.getMemOpOffset(tok.index);

        // Get the store value.
        let store_val <- tokStoreValue.readRsp();

        // Get the physical address(es).
        let p_addrs <- tokPhysicalMemAddrs.readPorts[1].readRsp();
        
        case (p_addrs) matches
            tagged ONE .p_addr:
            begin

                // There's only one address, but we may still need to do a read-modify-write.

                if (isaStoreRequiresReadModifyWrite(st_type))
                begin

                    // We're doing read-modify-write. Request a load.
                    let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr};
                    linkToMem.makeReq(tagged REQ_LOAD m_req);

                    // Record that the load response should go to us.
                    memPathQ.enq(PATH_STORE);

                    // Log it.
                    debugLog.record($format("TOKEN %0d: DoStores2: Load Req for Read-Modify-Write. (PA: 0x%h).", tok.index, p_addr)); 

                    // Stall this stage.
                    let store_info = STORES_INFO
                                     {
                                        token: tok,
                                        memAddrs: p_addrs,
                                        opType: st_type,
                                        offset: offset, 
                                        storeValue: store_val
                                     };
                    stateStores2 <= tagged STORES2_RMW_RSP store_info;

                end
                else
                begin

                    // It's a normal store. We're not stalled.
                    stores1Q.deq(); 

                    // Convert the store.
                    let mem_store_value = isaStoreValueToMemValue(store_val, st_type);
                    debugLog.record($format("TOKEN %0d: DoStores2: ISA Store (V: 0x%h, T: %0d, O: %b) = 0x%h", tok.index, store_val,  pack(st_type), offset, mem_store_value)); 

                    // Make the request to the memory state.
                    let m_req = MEMSTATE_REQ_STORE {tok: tok, addr: p_addr, value: mem_store_value};
                    linkToMem.makeReq(tagged REQ_STORE m_req);

                    // Log it.
                    debugLog.record($format("TOKEN %0d: DoStores2: Sending Store to Memory (PA: 0x%h, V: 0x%h).", tok.index, p_addr, mem_store_value)); 

                    // Update the scoreboard.
                    tokScoreboard.storeFinish(tok.index);

                    // Return to the timing partition. End of macro-operation (path 1).
                    linkDoStores.makeResp(initFuncpRspDoStores(tok));
                    debugLog.record($format("TOKEN %0d: DoStores: End (path 1).", tok.index));


                end

            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Two addresses means load two values, then modify them, then write them back.
                // Make the first load now.
                let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr1};
                linkToMem.makeReq(tagged REQ_LOAD m_req);

                // Record that the load response should go to us.
                memPathQ.enq(PATH_STORE);

                // Log it.
                debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Load Req 1 (PA1: 0x%h, PA2: 0x%h).", tok.index, p_addr1, p_addr2)); 

                // Stall this stage.
                let store_info = STORES_INFO
                                 {
                                    token: tok,
                                    memAddrs: p_addrs,
                                    opType: st_type,
                                    offset: offset,
                                    storeValue: store_val
                                 };
                stateStores2 <= tagged STORES2_SPAN_REQ store_info;


            end
        endcase
        
        
    endrule

    // doStores2RMW
    
    // When:   After a store has stalled to do a read-modify-write and the load has come back.
    // Effect: Do the "modify-write" portion. Unstall the pipeline.
    
    rule doStores2RMW (readyToContinue &&& stateStores2 matches tagged STORES2_RMW_RSP .store_info
                                       &&& memPathQ.first() == PATH_STORE);
    
        // Get the info from the previous stage.
        match {.tok, .st_type} = stores1Q.first();
        
        // Get the load from memory.
        MEM_VALUE existing_val = linkToMem.getResp();
        linkToMem.deq();
        memPathQ.deq();

        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Got RMW Load Rsp (V: 0x%h).", tok.index, existing_val)); 

        // Merge the values.
        let new_mem_val = isaStoreValueToMemValueRMW(existing_val, store_info.storeValue, store_info.offset, store_info.opType);
        debugLog.record($format("TOKEN %0d: doStores2: ISA StoreRMW (EV: 0x%h, V: 0x%h, T: %0d, O: %b) = 0x%h", tok.index, existing_val, store_info.storeValue,  pack(store_info.opType), store_info.offset, new_mem_val)); 

        // Write the store to memory.
        let mem_addr = getFirst(store_info.memAddrs);
        let m_req = MEMSTATE_REQ_STORE {tok: tok, addr: mem_addr, value: new_mem_val};
        linkToMem.makeReq(tagged REQ_STORE m_req);

        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Sending RMW Store to Memory (PA: 0x%h, V: 0x%h).", tok.index, mem_addr, new_mem_val)); 

        // Unstall this stage.
        stateStores2 <= tagged STORES2_NORMAL;
        stores1Q.deq();

        // Update the scoreboard.
        tokScoreboard.storeFinish(tok.index);
        
        // Return to the timing partition. End of macro-operation (path 2).
        linkDoStores.makeResp(initFuncpRspDoStores(tok));
        debugLog.record($format("TOKEN %0d: DoStores: End (path 2).", tok.index));
        
    
    endrule
    
    // doStores2SpanReq
    
    // When:   After a store has stalled to do a spanning load.
    // Effect: Make the second request, then start to wait for responses.
    
    rule doStores2SpanReq (readyToContinue &&& stateStores2 matches tagged STORES2_SPAN_REQ .store_info);
    
        // Get the data from the previous stage.
        match {.tok, .st_type} = stores1Q.first();
    
        // Make the second load request.
        let p_addr2 = getSecondOfTwo(store_info.memAddrs);
        let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr2};
        linkToMem.makeReq(tagged REQ_LOAD m_req);
        
        // Record that the load response should go to us.
        memPathQ.enq(PATH_STORE);
        
        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Load Req 2 (PA2: 0x%h).", tok.index, p_addr2));

        // Wait for the first response.
        stateStores2 <= tagged STORES2_SPAN_RSP1 store_info;
    
    endrule

    // doStores2SpanRsp1
    
    // When:   After the first load comes back from a spanning store.
    // Effect: Record the intermediate value, wait for the final response.
    
    rule doStores2SpanRsp1 (readyToContinue &&& stateStores2 matches tagged STORES2_SPAN_RSP1 .store_info
                                            &&& memPathQ.first() == PATH_STORE);
    
        // Get the value from the previous stage.
        match {.tok, .st_type} = stores1Q.first();
    
        // Get the first value from memory.
        MEM_VALUE existing_val1 = linkToMem.getResp();
        linkToMem.deq();
        memPathQ.deq();
        
        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Load Rsp 1 (V: 0x%h).", tok.index, existing_val1));
        
        // Wait for the second response.
        stateStores2 <= tagged STORES2_SPAN_RSP2 tuple2(store_info, existing_val1);
    
    endrule

    // doStores2SpanRsp2
    
    // When:   After the second load comes back from a spanning store.
    // Effect: Figure out the values and make the first store request.
    
    rule doStores2SpanRsp2 (readyToContinue &&& stateStores2 matches tagged STORES2_SPAN_RSP2 {.store_info, .existing_val1}
                                            &&& memPathQ.first() == PATH_STORE);
    
        // Get the value from the previous stage.
        match {.tok, .st_type} = stores1Q.first();

        // Get the second value from memory.
        MEM_VALUE existing_val2 = linkToMem.getResp();
        linkToMem.deq();
        memPathQ.deq();
        
        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Load Rsp 2 (V: 0x%h).", tok.index, existing_val2));
        
        // Use the ISA-provided conversion function.
        match {.new_val1, .new_val2} = isaStoreValueToSpanningMemValues(existing_val1, existing_val2, store_info.storeValue, store_info.offset, store_info.opType);
        debugLog.record($format("TOKEN %0d: DoStores2: ISA StoreSpan (EV1: 0x%h, EV2, 0x%h, V: 0x%h, T: %0d, O: %b) = 0x%h, 0x%h", tok.index, existing_val1, existing_val2, store_info.storeValue,  pack(store_info.opType), store_info.offset, new_val1, new_val2));

        // Make the first store request.
        MEM_ADDRESS p_addr1 = getFirst(store_info.memAddrs);
        let m_req = MEMSTATE_REQ_STORE {tok: tok, addr: p_addr1, value: new_val1};
        linkToMem.makeReq(tagged REQ_STORE m_req);

        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Req 1 (PA1: V1: 0x%h).", tok.index, p_addr1, new_val1));

        // Stall to make the second request.
        stateStores2 <= tagged STORES2_SPAN_END tuple2(store_info, new_val2);
    
    endrule

    // doStores2SpanEnd
    
    // When:   After making the first store request for a spanning store.
    // Effect: Make the second store request. Unstall the pipeline.

    rule doStores2SpanEnd (readyToContinue &&& stateStores2 matches tagged STORES2_SPAN_END {.store_info, .new_val2});
    
        let tok = store_info.token;
    
        // Make the second store.
        let p_addr2 = getSecondOfTwo(store_info.memAddrs);
        let m_req = MEMSTATE_REQ_STORE {tok: tok, addr: p_addr2, value: new_val2};
        linkToMem.makeReq(tagged REQ_STORE m_req);
        
        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Req 2 (PA2: V2: 0x%h).", tok.index, p_addr2, new_val2));
        
        // Unstall this stage.
        stores1Q.deq();
        stateStores2 <= tagged STORES2_NORMAL;
        
        // Update the scoreboard.
        tokScoreboard.storeFinish(tok.index);
        
        // Return to the timing partition. End of macro-operation (path 3).
        linkDoStores.makeResp(initFuncpRspDoStores(tok));
        debugLog.record($format("TOKEN %0d: DoStores: End (path 3).", tok.index));

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
        let req = linkCommitResults.getReq();
        linkCommitResults.deq();
        let tok = req.token;

        // Log it.
        debugLog.record($format("TOKEN %0d: CommitResults: Begin.", tok.index)); 

        // Confirm timing model propagated poison bit correctly
        assertPoisonBit(tokIsPoisoned(tok) == isValid(tokScoreboard.getFault(tok.index)));

        if (tokScoreboard.getFault(tok.index) matches tagged Valid .fault)
        begin
            // Timing model tried to commit an instruction with an exception.
            assertCommitFaultingInstr(False);
            debugLog.record($format("TOKEN %0d: CommitResults: FAULTING instruction!  Aborting.", tok.index)); 
        end

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

        assertExpectedOldestTok(tok.index == tokScoreboard.oldest());
        if (tok.index != tokScoreboard.oldest())
            debugLog.record($format("TOKEN %0d: commitResults1:  Token is not oldest! (Oldest: %0d)", tok.index, tokScoreboard.oldest()));

        // Retrieve the registers to be freed.
        let regsToFree  <- tokRegsToFree.readRsp();

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
        linkCommitResults.makeResp(initFuncpRspCommitResults(tok));
        debugLog.record($format("TOKEN %0d: CommitResults: End.", tok.index)); 

    endrule

    // commitResultsAdditional
    
    // When:   After a commitResults2 AND there are more physical registers to free.
    // Effect: Free the appropriate physical register.
 
    // Elaborated Rule: N copies, where N is the maximum number of dests minus 1 (which was handled already).
    
    for (Integer x = 0; x < valueof(ISA_MAX_DSTS) - 1; x = x + 1)
    begin
        rule commitResultsAdditional (additionalRegsToFree[x] matches tagged Valid .r);

            debugLog.record($format("Committing Additional PR: %0d", r)); 
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
        let req = linkCommitStores.getReq();
        linkCommitStores.deq();
        let tok = req.token;

        // If the token was not actually a store, it's an exception.
        let isStore = tokScoreboard.isStore(tok.index);
        let fault = isValid(tokScoreboard.getFault(tok.index));
        assertCommitedStoreIsActuallyAStore(isStore && !fault);

        // Log it.
        debugLog.record($format("TOKEN %0d: CommitStores: Committing.", tok.index)); 

        linkToMem.makeReq(tagged REQ_COMMIT MEMSTATE_REQ_COMMIT {tok: tok});

        // Respond to timing model. End of macro-operation.
        linkCommitStores.makeResp(initFuncpRspCommitStores(tok));

    endrule

    // ******* handleFault ******* //

    // Handle a fault raised in an earlier stage.  Returns the address from
    // which fetch should resume.

    // When:   When the timing model requests it.
    // Effect: Do fault action & rewind
    // Soft Inputs:  Token
    // Soft Returns: Token & next fetch address
    
    rule handleFault1 (readyToBegin && tokScoreboard.canRewind());

        // Get the input from the timing model.
        let req = linkHandleFault.getReq();
        linkHandleFault.deq();
        let tok = req.token;
        
        // Log it.
        debugLog.record($format("TOKEN %0d: handleFault", tok.index)); 

        // Read all possibly interesting addresses
        tokAddr.readReq(tok.index);           // PC
        tokMemAddr.readReq(tok.index);
        
        state <= RSM_HandleFault;
        faultQ.enq(tok);

    endrule


    rule handleFault2 (state == RSM_HandleFault);

        // Instruction & data addresses
        let iAddr <- tokAddr.readRsp();
        let dAddr <- tokMemAddr.readRsp();

        match { .iAddr_aligned, .iAddr_offset } = isaAlignAddress(iAddr);
        match { .dAddr_aligned, .dAddr_offset } = isaAlignAddress(dAddr);

        let tok = faultQ.first();
        faultQ.deq();

        if (tokScoreboard.getFault(tok.index) matches tagged Valid .fault)
        begin
            let mem_ref_bytes = fromInteger(valueOf(SizeOf#(MEM_VALUE)) / 8);

            //
            // handleTLBPageFault mode for TLBs returns no response.  Just triggering
            // the request is enough to allocate the page.
            //

            case (fault)
            FAULT_ITRANS:
            begin
                let addr = iAddr_aligned;
                debugLog.record($format("TOKEN %0d: handleFault2: ITRANS (VA: 0x%h)", tok.index, addr)); 
                link_itlb.makeReq(handleTLBPageFault(tok, addr));
            end

            FAULT_ITRANS2:
            begin
                let addr = iAddr_aligned + mem_ref_bytes;
                debugLog.record($format("TOKEN %0d: handleFault2: ITRANS2 (VA: 0x%h)", tok.index, addr)); 
                link_itlb.makeReq(handleTLBPageFault(tok, addr));
            end

            FAULT_DTRANS:
            begin
                let addr = dAddr_aligned;
                debugLog.record($format("TOKEN %0d: handleFault2: DTRANS (VA: 0x%h)", tok.index, addr)); 
                link_dtlb.makeReq(handleTLBPageFault(tok, addr));
            end

            FAULT_DTRANS2:
            begin
                let addr = dAddr_aligned + mem_ref_bytes;
                debugLog.record($format("TOKEN %0d: handleFault2: DTRANS2 (VA: 0x%h)", tok.index, addr)); 
                link_dtlb.makeReq(handleTLBPageFault(tok, addr));
            end

            default:
            begin
                assertIllegalInstruction(False);
                debugLog.record($format("TOKEN %0d: handleFault2: No handler for fault %d", tok.index, fault)); 
            end
            endcase
        end
        else
        begin
            assertIllegalInstruction(False);
            debugLog.record($format("TOKEN %0d: handleFault2: Instruction did not fault", tok.index));
        end

        //
        // Start a slow rewind, killing all younger than the faulting token and
        // the faulting token.  Fast rewind won't work because the faulting token
        // is being killed.
        //
        // Make a dummy token since rewind really needs tok.index - 1
        //
        let rewind_to = TOKEN {index: (tok.index - 1),
                               poison: False,
                               epoch: ?,
                               timep_info: ?};

        rewindTok <= rewind_to;
        fastRewind <= False;
        rewindForFault <= True;
        
        // Tell the memory to drop non-committed stores.
        let m_req = MEMSTATE_REQ_REWIND {rewind_to: rewind_to.index, rewind_from: tokScoreboard.youngest()};
        linkToMem.makeReq(tagged REQ_REWIND m_req);

        debugLog.record($format("Rewind: Initiating slow rewind to token %0d", rewind_to.index));
                
        // After rewind, fetch should resume at this token's address
        faultResumeQ.enq(tuple2(tok, iAddr));

        // Disable everything else.
        state <= RSM_DrainingForRewind;

    endrule

    //
    // handleFault3 -- Control returns here following rewind.
    //
    rule handleFault3 (state == RSM_HandleFaultRewindDone);

        match { .tok, .resumeInstrAddr } = faultResumeQ.first();
        faultResumeQ.deq();

        // Log it.
        debugLog.record($format("TOKEN %0d: handleFault3: Restart at 0x%h", tok.index, resumeInstrAddr)); 

        // Update the fault epoch so we can discard appropriate updates.
        let new_fault_epoch = faultEpoch + 1;
        faultEpoch <= new_fault_epoch;

        // Send response to timing model
        linkHandleFault.makeResp(initFuncpRspHandleFault(tok, resumeInstrAddr, initEpoch(branchEpoch, new_fault_epoch)));

        state <= RSM_Running;

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

    rule rewindToToken1 (readyToBegin && tokScoreboard.canRewind());
      
        // Get the input from the timing model.
        let req = linkRewindToToken.getReq();
        linkRewindToToken.deq();
        let tok = req.token;

        // Log it.
        debugLog.record($format("Rewind: Starting Rewind to TOKEN %0d (Youngest: %0d)", tok.index, tokScoreboard.youngest())); 

        // Tell the memory to drop non-committed stores.
        let m_req = MEMSTATE_REQ_REWIND {rewind_to: tok.index, rewind_from: tokScoreboard.youngest()};
        linkToMem.makeReq(tagged REQ_REWIND m_req);

        // Update the epoch so we can discard appropriate updates.
        branchEpoch <= branchEpoch + 1;

        // Check to see if we have a snapshot.
        Maybe#(FUNCP_SNAPSHOT_INDEX) midx = snapshots.hasSnapshot(tok.index);

        // Alright did we find anything?
        case (midx) matches
            tagged Valid .idx:
            begin 

                // Log our success!
                debugLog.record($format("Rewind: Fast Rewind confirmed with Snapshot %0d", idx));

                // Rewind the scoreboard.
                tokScoreboard.rewindTo(tok.index);

                // Retrieve the snapshots.
                snapshots.requestSnapshot(idx);
                tokFreeListPos.readReq(tok.index);
                
                fastRewind <= True;

            end
            tagged Invalid:
            begin

                // Log our failure.
                debugLog.record($format("Rewind: Initiating slow rewind (Oldest: %0d)", tokScoreboard.oldest()));
                
                fastRewind <= False;

            end
        endcase
        
        // Disable everything else.
        state <= RSM_DrainingForRewind;

        // Stop when we get to the token.
        rewindTok <= tok;

        // Normal rewind
        rewindForFault <= False;

    endrule

    rule rewindToToken2 (state == RSM_DrainingForRewind && tokScoreboard.canRewind());
    
        // Log it.
        debugLog.record($format("Rewind: Draining finished (Oldest: %0d)", tokScoreboard.oldest()));

        // Start at the youngest and go backward.
        rewindCur <= tokScoreboard.youngest();

        // Proceed with rewind.
        state <= RSM_Rewinding;
    
    endrule

    // rewindToToken2

    // When:   After rewindToToken1 AND we have a snapshot.
    // Effect: Use the snapshot to overwrite existing values. Reply to the timing partition.

    rule rewindToToken3Fast (state == RSM_Rewinding && fastRewind);

        // Get the snapshots.
        let snp_map <- snapshots.returnSnapshot();
        let snp_fl <- tokFreeListPos.readRsp();

        // Update the maptable.
        maptable <= snp_map;
        
        // Update the freelist.  Must be valid since snapshot and freelist position
        // are both set in same phase of getDependencies.
        freelist.backTo(validValue(snp_fl));

        // Log it.
        debugLog.record($format("Fast Rewind finished."));  

        // We're done. End of macro-operation (path 1).
        state <= RSM_Running;
        linkRewindToToken.makeResp(initFuncpRspRewindToToken(rewindTok, initEpoch(branchEpoch, faultEpoch )));

    endrule

    //Slow rewind. Walk the tokens in age order
    //and reconstruct the maptable

    rule rewindToToken3Slow (state == RSM_Rewinding && !fastRewind);
    
        // Look up the token properties
        tokRegsToFree.readReq(rewindCur);
        tokFreeListPos.readReq(rewindCur);
        tokInst.readPorts[1].readReq(rewindCur);
        tokDsts.readPorts[1].readReq(rewindCur);

        // Pass it to the next stage who will free it.
        let done = (rewindCur == rewindTok.index);
        rewindQ.enq(tuple2(rewindCur, done));

        rewindCur <= rewindCur - 1;

        if (done)
        begin
            // No more tokens.  Wait for remapping to finish.
            state <= RSM_RewindingWaitForSlowRemap;
        end
    endrule


    // Urgency
    
    // A total ordering of all non-trivial rules in the system specifying who should get to
    // proceed in the case of a conflict. The logic here is straightforward. In terms of
    // macro-operations, the "later" operations are favored:
    
    // newInFlight < doITrans < getInst < getDeps < getResult < doDTrans < doLoads < doStores < commitResults < commitStores
    
    // Thus getResults() should be favored over getDeps().
    
    // Within a single macro-operation a similar philosophy holds: favor the later stages 
    // of the pipeline. Thus:
    
    // doLoads1 < doLoads2 < doLoads3
    
    // This is _particularly_ important for the getDeps stages, which modify the maptable.
    
    // We specify all of this as a TOTAL ORDER, which is tedious, but guaranteed to be complete.
    
    // Do not change the following lines unless you understand all this and have a good reason.

    (* descending_urgency= "initialize, rewindToToken4, rewindToToken3Slow, rewindToToken3Fast, rewindToToken2, rewindToToken1, emulateInstruction4, emulateInstruction3_UpdateReg, emulateInstruction3, emulateInstruction2_Rsp, emulateInstruction2_Req, emulateInstruction1, commitStores, commitResults2, commitResults1, doStores2SpanEnd, doStores2SpanRsp2, doStores2SpanRsp1, doStores2SpanReq, doStores2RMW, doStores2, doStores1, doLoads3Span, doLoads3, doLoads2Span, doLoads2, doLoads1, doDTranslate3Span, doDTranslate3, doDTranslate2Span, doDTranslate2, doDTranslate1, getResults4AdditionalWriteback, getResults4, getResults3, getResults2StallEnd, getResults2, getResults1, getDependencies2AdditionalMappings, getDependencies2, getDependencies1, getInstruction3Span, getInstruction3, getInstruction2Span, getInstruction2, getInstruction1, doITranslate2Span, doITranslate2, doITranslate1Span, doITranslate1, newInFlight" *)

    // The execution_order pragma doesn't affect the schedule but does get rid of
    // compiler warnings caused by the appearance of multiple writers to the
    // prfvalids vector.  According to Bluespec the order here affects the
    // priority encoder within a cycle but not the scheduling rules.

    (* execution_order = "getResults4, getResults4AdditionalWriteback, emulateInstruction3_UpdateReg, getDependencies2AdditionalMappings, getDependencies2" *)

    //
    // Free registers for tokens coming from rewindToTokenSlow1
    //
    rule rewindToToken4 (True);

        match { .tok_idx, .done } = rewindQ.first();
        rewindQ.deq();

        let regs_to_remap <- tokRegsToFree.readRsp();
        let freelist_pos <- tokFreeListPos.readRsp();
        let inst <- tokInst.readPorts[1].readRsp();
        let dsts <- tokDsts.readPorts[1].readRsp();

        //
        // Unwind register mappings if token has been through getDeps and
        // thus has physical registers allocated.
        //
        if (freelist_pos matches tagged Valid .fr_pos)
        begin
            //
            // Rewind register mappings if not at the target state
            //
            if (!done && tokScoreboard.isAllocated(tok_idx))
            begin
                debugLog.record($format("Slow Rewind: Lookup TOKEN %0d", tok_idx));  

                Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) new_maptable = maptable;

                for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
                begin
                    if (isaGetDst(inst, x) matches tagged Valid .arc_r &&&
                        regs_to_remap[x] matches tagged Valid .r)
                    begin
                        // Set the mapping back
                        debugLog.record($format("Slow Rewind: TOKEN %0d: Remapping (%0d/%0d)", tok_idx, arc_r, r));
                        new_maptable = update(new_maptable, pack(arc_r), r);
                    end
                end

                maptable <= new_maptable;
            end
            
            if (done)
                debugLog.record($format("Slow Rewind: Lookup last TOKEN %0d", tok_idx));  

            freelist.backTo(fr_pos);
        end

        // Done with slow rewind?
        if (done)
        begin
            debugLog.record($format("Slow Rewind: Done."));  
            tokScoreboard.rewindTo(rewindTok.index);
            if (! rewindForFault)
            begin
                // Normal rewind -- return response
                linkRewindToToken.makeResp(initFuncpRspRewindToToken(rewindTok, initEpoch(branchEpoch, faultEpoch )));
                state <= RSM_Running;
            end
            else
            begin
                // Rewind for fault handler -- resume fault handler path
                state <= RSM_HandleFaultRewindDone;
            end
        end

    endrule
 
endmodule

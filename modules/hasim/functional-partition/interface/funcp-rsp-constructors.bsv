
// initFuncpRspNewInFlight

function FUNCP_RSP_NEW_IN_FLIGHT initFuncpRspNewInFlight(TOKEN tok);

    return FUNCP_RSP_NEW_IN_FLIGHT 
            {
                newToken: tok
            };

endfunction

// initFuncpRspDoITranslate

function FUNCP_RSP_DO_ITRANSLATE initFuncpRspDoITranslate(CONTEXT_ID ctx_id, MEM_ADDRESS addr, MEM_OFFSET offs, Bool fault);

    return FUNCP_RSP_DO_ITRANSLATE 
            {
                contextId: ctx_id,
                physicalAddress: addr, 
                offset: offs,
                fault: fault,
                hasMore: False
            };

endfunction

// initFuncpRspDoITranslate_part1

function FUNCP_RSP_DO_ITRANSLATE initFuncpRspDoITranslate_part1(CONTEXT_ID ctx_id, MEM_ADDRESS addr, MEM_OFFSET offs, Bool fault);

    return FUNCP_RSP_DO_ITRANSLATE 
            {
                contextId: ctx_id,
                physicalAddress: addr,
                offset: offs, 
                fault: fault,
                hasMore: True
            };

endfunction

// initFuncpRspDoITranslate_part2

function FUNCP_RSP_DO_ITRANSLATE initFuncpRspDoITranslate_part2(CONTEXT_ID ctx_id, MEM_ADDRESS addr, MEM_OFFSET offs, Bool fault);

    return FUNCP_RSP_DO_ITRANSLATE 
            {
                contextId: ctx_id,
                physicalAddress: addr, 
                offset: offs,
                fault: fault,
                hasMore: False
            };

endfunction


// initFuncpRspGetInstruction

function FUNCP_RSP_GET_INSTRUCTION initFuncpRspGetInstruction(CONTEXT_ID ctx_id, ISA_INSTRUCTION inst);

    return FUNCP_RSP_GET_INSTRUCTION
            {
                contextId: ctx_id,
                instruction: inst
            };

endfunction

// initFuncpRspGetDependencies

function FUNCP_RSP_GET_DEPENDENCIES initFuncpRspGetDependencies(TOKEN tok, ISA_SRC_MAPPING srcs, ISA_DST_MAPPING dsts);
    
    return FUNCP_RSP_GET_DEPENDENCIES
            {
                token: tok,
                srcMap: srcs,
                dstMap: dsts
            };

endfunction

// initFuncpRspGetResults

function FUNCP_RSP_GET_RESULTS initFuncpRspGetResults(TOKEN tok, ISA_ADDRESS pc, FUNCP_ISA_EXECUTION_RESULT res);

    return FUNCP_RSP_GET_RESULTS
            {
                token: tok,
                instructionAddress: pc,
                instructionSize: 4,
                result: res
            };

endfunction

// initFuncpRspDoDTranslate

function FUNCP_RSP_DO_DTRANSLATE initFuncpRspDoDTranslate(TOKEN tok, MEM_ADDRESS addr, Bool fault);

    // Update poison bit
    tok.poison = tok.poison || fault;

    return FUNCP_RSP_DO_DTRANSLATE 
            {
                token: tok, 
                physicalAddress: addr, 
                fault: fault,
                hasMore: False
            };

endfunction

// initFuncpRspDoDTranslate_part1

function FUNCP_RSP_DO_DTRANSLATE initFuncpRspDoDTranslate_part1(TOKEN tok, MEM_ADDRESS addr, Bool fault);

    // Update poison bit
    tok.poison = tok.poison || fault;

   return FUNCP_RSP_DO_DTRANSLATE 
            {
                token: tok,
                physicalAddress: addr, 
                fault: fault,
                hasMore: True
            };

endfunction

// initFuncpRspDoDTranslate_part2

function FUNCP_RSP_DO_DTRANSLATE initFuncpRspDoDTranslate_part2(TOKEN tok, MEM_ADDRESS addr, Bool fault);

    // Update poison bit
    tok.poison = tok.poison || fault;

    return FUNCP_RSP_DO_DTRANSLATE 
            {
                token: tok, 
                physicalAddress: addr, 
                fault: fault,
                hasMore: False
            };

endfunction


// initFuncpRspDoLoads

function FUNCP_RSP_DO_LOADS initFuncpRspDoLoads(TOKEN tok);

    return FUNCP_RSP_DO_LOADS
            {
                token: tok
            };

endfunction

// initFuncpRspDoStores

function FUNCP_RSP_DO_STORES initFuncpRspDoStores(TOKEN tok);

    return FUNCP_RSP_DO_STORES
            {
                token: tok
            };

endfunction

// initFuncpRspCommitResults

function FUNCP_RSP_COMMIT_RESULTS initFuncpRspCommitResults(TOKEN tok);

    return FUNCP_RSP_COMMIT_RESULTS
            {
                token: tok
            };

endfunction

// initFuncpRspCommitStores

function FUNCP_RSP_COMMIT_STORES initFuncpRspCommitStores(TOKEN tok);

    return FUNCP_RSP_COMMIT_STORES
            {
                token: tok
            };

endfunction

// initFuncpRspHandleFault

function FUNCP_RSP_HANDLE_FAULT initFuncpRspHandleFault(TOKEN tok, ISA_ADDRESS nextInstrAddr);

    return FUNCP_RSP_HANDLE_FAULT
            {
                token: tok,
                nextInstructionAddress: nextInstrAddr
            };

endfunction

// initFuncpRspRewindToToken

function FUNCP_RSP_REWIND_TO_TOKEN initFuncpRspRewindToToken(TOKEN tok);

    return FUNCP_RSP_REWIND_TO_TOKEN
            {
                token: tok
            };

endfunction

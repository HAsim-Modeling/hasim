
// initFuncpReqNewInFlight

function FUNCP_REQ_NEW_IN_FLIGHT initFuncpReqNewInFlight(CONTEXT_ID ctx_id);

    return FUNCP_REQ_NEW_IN_FLIGHT 
            {
                context_id: ctx_id
            };

endfunction


// initFuncpReqDoITranslate

function FUNCP_REQ_DO_ITRANSLATE initFuncpReqDoITranslate(CONTEXT_ID ctx_id, ISA_ADDRESS addr);

    return FUNCP_REQ_DO_ITRANSLATE 
            {
                contextId: ctx_id,
                virtualAddress: addr
            };

endfunction


// initFuncpReqGetInstruction

function FUNCP_REQ_GET_INSTRUCTION initFuncpReqGetInstruction(CONTEXT_ID ctx_id, MEM_ADDRESS addr, MEM_OFFSET offs);

    return FUNCP_REQ_GET_INSTRUCTION
            {
                contextId: ctx_id,
                physicalAddress: addr,
                offset: offs,
                hasMore: False
            };

endfunction


// initFuncpReqGetInstruction_part1

function FUNCP_REQ_GET_INSTRUCTION initFuncpReqGetInstruction_part1(CONTEXT_ID ctx_id, MEM_ADDRESS addr, MEM_OFFSET offs);

    return FUNCP_REQ_GET_INSTRUCTION
            {
                contextId: ctx_id,
                physicalAddress: addr,
                offset: offs,
                hasMore: True
            };

endfunction


// initFuncpReqGetInstruction_part2

function FUNCP_REQ_GET_INSTRUCTION initFuncpReqGetInstruction_part2(CONTEXT_ID ctx_id, MEM_ADDRESS addr, MEM_OFFSET offs);

    return FUNCP_REQ_GET_INSTRUCTION
            {
                contextId: ctx_id,
                physicalAddress: addr,
                offset: offs,
                hasMore: False
            };

endfunction


// initFuncpReqGetDependencies

function FUNCP_REQ_GET_DEPENDENCIES initFuncpReqGetDependencies(CONTEXT_ID ctx_id, ISA_INSTRUCTION inst, ISA_ADDRESS virt_addr);
    
    return FUNCP_REQ_GET_DEPENDENCIES
            {
                contextId: ctx_id,
                dummy: False,
                instruction: inst,
                virtualAddress: virt_addr
            };

endfunction

// initFuncpReqGetDummyToken

function FUNCP_REQ_GET_DEPENDENCIES initFuncpReqGetDummyToken(CONTEXT_ID ctx_id, ISA_INSTRUCTION inst, ISA_ADDRESS virt_addr);
    
    return FUNCP_REQ_GET_DEPENDENCIES
            {
                contextId: ctx_id,
                dummy: True,
                instruction: inst,
                virtualAddress: virt_addr
            };

endfunction


// initFuncpReqGetResults

function FUNCP_REQ_GET_RESULTS initFuncpReqGetResults(TOKEN tok);

    return FUNCP_REQ_GET_RESULTS
            {
                token: tok
            };

endfunction


// initFuncpReqDoDTranslate

function FUNCP_REQ_DO_DTRANSLATE initFuncpReqDoDTranslate(TOKEN tok);

    return FUNCP_REQ_DO_DTRANSLATE 
            {
                token: tok
            };

endfunction


// initFuncpReqDoLoads

function FUNCP_REQ_DO_LOADS initFuncpReqDoLoads(TOKEN tok);

    return FUNCP_REQ_DO_LOADS
            {
                token: tok
            };

endfunction


// initFuncpReqDoStores

function FUNCP_REQ_DO_STORES initFuncpReqDoStores(TOKEN tok);

    return FUNCP_REQ_DO_STORES
            {
                token: tok
            };

endfunction


// initFuncpReqCommitResults

function FUNCP_REQ_COMMIT_RESULTS initFuncpReqCommitResults(TOKEN tok);

    return FUNCP_REQ_COMMIT_RESULTS
            {
                token: tok,
                abort: False
            };

endfunction


// initFuncpReqCommitStores

function FUNCP_REQ_COMMIT_STORES initFuncpReqCommitStores(TOKEN tok);

    return FUNCP_REQ_COMMIT_STORES
            {
                token: tok
            };

endfunction


// initFuncpReqHandleFault

function FUNCP_REQ_HANDLE_FAULT initFuncpReqHandleFault(TOKEN tok);

    return FUNCP_REQ_HANDLE_FAULT
            {
                token: tok
            };

endfunction

// initFuncpReqRewindToToken

function FUNCP_REQ_REWIND_TO_TOKEN initFuncpReqRewindToToken(TOKEN tok);

    return FUNCP_REQ_REWIND_TO_TOKEN
            {
                token: tok
            };

endfunction

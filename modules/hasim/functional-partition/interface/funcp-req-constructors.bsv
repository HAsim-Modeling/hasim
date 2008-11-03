
// initFuncpReqNewInFlight

function FUNCP_REQ_NEW_IN_FLIGHT initFuncpReqNewInFlight();

    return FUNCP_REQ_NEW_IN_FLIGHT 
            {
                dummy: ?
            };

endfunction


// initFuncpReqDoITranslate

function FUNCP_REQ_DO_ITRANSLATE initFuncpReqDoITranslate(TOKEN tok, ISA_ADDRESS addr);

    return FUNCP_REQ_DO_ITRANSLATE 
            {
                token: tok, 
                address: addr
            };

endfunction


// initFuncpReqGetInstruction

function FUNCP_REQ_GET_INSTRUCTION initFuncpReqGetInstruction(TOKEN tok);

    return FUNCP_REQ_GET_INSTRUCTION
            {
                token: tok
            };

endfunction


// initFuncpReqGetDependencies

function FUNCP_REQ_GET_DEPENDENCIES initFuncpReqGetDependencies(TOKEN tok);
    
    return FUNCP_REQ_GET_DEPENDENCIES
            {
                token: tok
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
                token: tok
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

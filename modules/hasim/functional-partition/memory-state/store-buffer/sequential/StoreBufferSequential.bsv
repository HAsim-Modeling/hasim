import Vector::*;
import FIFOF::*;
import RegFile::*;

`include "fpga_components.bsh"
`include "hasim_common.bsh"
`include "soft_connections.bsh"
`include "funcp_memory.bsh"

typedef union tagged
{
    struct {Bit#(1) num; TOKEN tok; MEM_VALUE value; MEM_ADDRESS addr;} SBUFFER_REQ_INSERT;
    struct {TOKEN tok; MEM_ADDRESS addr;}                               SBUFFER_REQ_LOOKUP;
    TOKEN                                                               SBUFFER_REQ_COMMIT;
    struct {TOKEN_INDEX rewind; TOKEN_INDEX youngest;}                  SBUFFER_REQ_REWIND;
} MEMSTATE_SBUFFER_REQ deriving (Eq, Bits);

typedef union tagged
{
    void                                                                 SBUFFER_RSP_INSERT;
    struct {TOKEN tok; MEM_ADDRESS addr; Maybe#(MEM_VALUE) mresult;}     SBUFFER_RSP_LOOKUP;
    struct {TOKEN tok; MEM_ADDRESS addr; Bool hasMore; MEM_VALUE value;} SBUFFER_RSP_COMMIT;
    void                                                                 SBUFFER_RSP_REWIND;
} MEMSTATE_SBUFFER_RSP deriving (Eq, Bits);

typedef struct {
    TOKEN_INDEX tokenIndex;
    MEM_ADDRESS addr;
    MEM_VALUE value;
} SBUFFER_ENTRY deriving (Bits, Eq);

function Vector#(TExp#(width), dataT) updateRange(dataT data, Bit#(width) lo, Bit#(width) hi, Vector#(TExp#(width), dataT) oldVec);
    Vector#(TExp#(width), dataT) newVec = newVector();
    for(Integer i = 0; i < valueOf(TExp#(width)); i = i + 1)
    begin
        if(lo < hi)
            newVec[i] = (fromInteger(i) > lo && fromInteger(i) < hi)? data: oldVec[i];
        else
            newVec[i] = (fromInteger(i) > lo || fromInteger(i) < hi)? data: oldVec[i];
    end
    return newVec;
endfunction

typedef TExp#(`SBUFFER_IDX_SIZE) SBUFFER_SIZE;
typedef Bit#(`SBUFFER_IDX_SIZE) SBUFFER_INDEX;
typedef Bit#(TAdd#(`SBUFFER_IDX_SIZE, 1)) SBUFFER_PTR;

typedef SizeOf#(TOKEN_INDEX) INDEX_SIZE;
typedef TExp#(INDEX_SIZE) INDEX_NUM;

typedef enum {SBUFFER_STATE_READY, SBUFFER_STATE_INSERT1, SBUFFER_STATE_LOOKUP_REQ, SBUFFER_STATE_LOOKUP_RESP, SBUFFER_STATE_COMMIT1, SBUFFER_STATE_COMMIT2, SBUFFER_STATE_COMMIT3} SBUFFER_STATE deriving (Bits, Eq);

module [HASIM_MODULE] mkFUNCP_StoreBuffer();
    FpgaDebugFile                                                         debug <- mkFpgaDebugFile("StoreBuffer.out");

    Connection_Server#(TOKEN, UNIT)                                    allocate <- mkConnection_Server("storeBufferAllocate");
    Connection_Server#(MEMSTATE_SBUFFER_REQ, MEMSTATE_SBUFFER_RSP) linkMemState <- mkConnection_Server("mem_storebuf");

    Vector#(2, BRAM#(SBUFFER_INDEX, SBUFFER_ENTRY))                 sbuffer <- replicateM(mkBRAM());
    Vector#(2, BRAM#(SBUFFER_INDEX, Bool))                       entryValid <- replicateM(mkBRAM());
    Reg#(SBUFFER_PTR)                                                    enqNum <- mkReg(0);
    Reg#(SBUFFER_PTR)                                                    deqNum <- mkReg(0);

    RegFile#(INDEX_SIZE, SBUFFER_PTR)                                tokenEntryMap <- mkRegFileFullInitialized(0);
    Reg#(Vector#(INDEX_NUM, Bool))                                   tokenValid <- mkReg(replicate(False));
    Reg#(TOKEN_INDEX)                                                lastCommit <- mkReg(0);

    Reg#(SBUFFER_STATE)                                                   state <- mkReg(SBUFFER_STATE_READY);

    Reg#(SBUFFER_PTR)                                                 lookupPtr <- mkRegU();

    SBUFFER_INDEX enqIndex = truncate(enqNum);
    SBUFFER_INDEX deqIndex = truncate(deqNum);

    SBUFFER_INDEX lookupIndex = truncate(lookupPtr);

    SBUFFER_PTR credits = fromInteger(valueOf(SBUFFER_SIZE))  - (enqNum - deqNum);

    rule alloc(state == SBUFFER_STATE_READY);
        let token = allocate.getReq();
        if(credits == 0)
        begin
            debug <= $format("ERROR store buffer full, hence recompile with bigger size enqNum: %d deqNum: %d token: %d", enqNum, deqNum, token.index);
            $display("ERROR store buffer full, hence recompile with bigger size enqNum: %d deqNum: %d token: %d", enqNum, deqNum, token.index);
            $finish(1);
        end
        allocate.deq();

        entryValid[0].write(enqIndex, False);
        entryValid[1].write(enqIndex, False);
        enqNum <= enqNum + 1;

        tokenEntryMap.upd(token.index, enqNum);
        tokenValid[token.index] <= True;

        debug <= $format("allocate: token: %d enqNum: %d", token.index, enqNum);

        //allocate.makeResp(?);
    endrule

    rule insert0(linkMemState.getReq() matches tagged SBUFFER_REQ_INSERT .req &&& state == SBUFFER_STATE_READY);
        debug <= $format("insert: token: %d addr: 0x%x value: 0x%x", req.tok.index, req.addr, req.value);
        debug <= $format("insert: tokenValid: %b enqNum: %d deqNum: %d", tokenValid, enqNum, deqNum);
        let entryPtr = tokenEntryMap.sub(req.tok.index);
        debug <= $format("insert: entryPtr: %d", entryPtr);
        sbuffer[req.num].write(truncate(entryPtr), tagged SBUFFER_ENTRY{tokenIndex: req.tok.index, addr: req.addr, value: req.value});
        entryValid[req.num].write(truncate(entryPtr), True);
        linkMemState.deq();
        //linkMemState.makeResp(tagged SBUFFER_RSP_INSERT);
        state <= SBUFFER_STATE_READY;
    endrule

    rule lookup0(linkMemState.getReq() matches tagged SBUFFER_REQ_LOOKUP .req &&& state == SBUFFER_STATE_READY);
        if(credits == fromInteger(valueOf(SBUFFER_SIZE)))
        begin
            debug <= $format("lookup: StoreBuffer empty");
            linkMemState.deq();
            linkMemState.makeResp(tagged SBUFFER_RSP_LOOKUP{tok: req.tok, addr: req.addr, mresult: tagged Invalid});
        end
        else
        begin
            let newLookupPtr = enqNum - 1;
            debug <= $format("lookup: enqNum: %d token: %d newLookupPtr", enqNum, req.tok.index, newLookupPtr);
            lookupPtr <= newLookupPtr;
            for(Integer i = 0; i < 2; i = i + 1)
            begin
                sbuffer[i].readReq(truncate(newLookupPtr));
                entryValid[i].readReq(truncate(newLookupPtr));
            end
            state <= SBUFFER_STATE_LOOKUP_RESP;
        end
    endrule

    rule lookupReq(linkMemState.getReq() matches tagged SBUFFER_REQ_LOOKUP .req &&& state == SBUFFER_STATE_LOOKUP_REQ);
        debug <= $format("lookupPtr: %d", lookupPtr);
        for(Integer i = 0; i < 2; i = i + 1)
        begin
            sbuffer[i].readReq(lookupIndex);
            entryValid[i].readReq(lookupIndex);
        end
        state <= SBUFFER_STATE_LOOKUP_RESP;
    endrule

    rule lookupResp(linkMemState.getReq() matches tagged SBUFFER_REQ_LOOKUP .req &&& state == SBUFFER_STATE_LOOKUP_RESP);
        Vector#(2, Bool) entryValids = newVector();
        Vector#(2, SBUFFER_ENTRY) entrys = newVector();
        Maybe#(MEM_VALUE) mresult = tagged Invalid;
        for(Integer i = 0; i < 2; i = i + 1)
        begin
            entrys[i] <- sbuffer[i].readRsp();
            entryValids[i] <- entryValid[i].readRsp();
            debug <= $format("lookup: i: %1d entryValids[i]: %b, token: %d, addr: 0x%x", i, entryValids[i], entrys[i].tokenIndex, entrys[i].addr);
            if(tokenValid[entrys[i].tokenIndex] && entrys[i].addr == req.addr && isOlder(entrys[i].tokenIndex, req.tok.index))
            begin
                if(!entryValids[i])
                begin
                    debug <= $format("lookup: Memory hazard detected, timing partition is probably wrong");
                    $display("StoreBuffer: lookup: Memory hazard detected, timing partition is probably wrong");
                end
                debug <= $format("lookup: match");
                mresult = tagged Valid entrys[i].value;
            end
        end
        if(isValid(mresult) || lookupPtr == deqNum)
        begin
            linkMemState.makeResp(tagged SBUFFER_RSP_LOOKUP{tok: req.tok, addr: req.addr, mresult: mresult});
            linkMemState.deq();
            debug <= $format("lookup: token: %d addr: 0x%x found: %b", req.tok.index, req.addr, isValid(mresult));
            state <= SBUFFER_STATE_READY;
        end
        else
        begin
            state <= SBUFFER_STATE_LOOKUP_REQ;
            lookupPtr <= lookupPtr - 1;
        end
    endrule

    rule commit1 (linkMemState.getReq() matches tagged SBUFFER_REQ_COMMIT .req &&& state == SBUFFER_STATE_READY);
        lastCommit <= req.index;
        let newTokenValid = updateRange(False, lastCommit, req.index, tokenValid);
        newTokenValid[req.index] = False;
        tokenValid <= newTokenValid;
        debug <= $format("commit1: enqNum: %d deqNum: %d token: %d lastCommit: %d oldTokenValid: %b newTokenValid: %b", enqNum, deqNum, req.index, lastCommit, tokenValid, newTokenValid);
        let entryPtr = tokenEntryMap.sub(req.index);
        deqNum <= entryPtr + 1;
        debug <= $format("commit1: entryPtr: %d newDeqNum: %d", entryPtr, entryPtr + 1);
        for(Integer i = 0; i < 2; i = i + 1)
        begin
            entryValid[i].readReq(truncate(entryPtr));
            sbuffer[i].readReq(truncate(entryPtr));
        end
        state <= SBUFFER_STATE_COMMIT2;
    endrule

    rule commit2(linkMemState.getReq() matches tagged SBUFFER_REQ_COMMIT .req &&& state == SBUFFER_STATE_COMMIT2);
        let entryValid0 <- entryValid[0].readRsp();
        let entryValid1 <- entryValid[1].readRsp();

        let entry <- sbuffer[0].readRsp();
        debug <= $format("commit2: entryValid0: %b token: %d addr: 0x%x value: 0x%x", entryValid0, entry.tokenIndex, entry.addr, entry.value);
        linkMemState.makeResp(tagged SBUFFER_RSP_COMMIT{tok: req, hasMore: entryValid1, addr: entry.addr, value: entry.value});

        if(entryValid1)
            state <= SBUFFER_STATE_COMMIT3;
        else
        begin
            let dummy <- sbuffer[1].readRsp();
            linkMemState.deq();
            state <= SBUFFER_STATE_READY;
        end
    endrule

    rule commit3(linkMemState.getReq() matches tagged SBUFFER_REQ_COMMIT .req &&& state == SBUFFER_STATE_COMMIT3);
        let entry <- sbuffer[1].readRsp();
        debug <= $format("commit3: token: %d addr: 0x%x value: 0x%x", entry.tokenIndex, entry.addr, entry.value);
        linkMemState.makeResp(tagged SBUFFER_RSP_COMMIT{tok: req, hasMore: False, addr: entry.addr, value: entry.value});
        linkMemState.deq();
        state <= SBUFFER_STATE_READY;
    endrule

    rule rewind(linkMemState.getReq matches tagged SBUFFER_REQ_REWIND .req &&& state == SBUFFER_STATE_READY);
        let newTokenValid = updateRange(False, req.rewind, lastCommit, tokenValid);
        tokenValid <= newTokenValid;
        debug <= $format("rewind: token: %d lastCommit: %d oldTokenValid: %b newTokenValid: %b", req.rewind, lastCommit, tokenValid, newTokenValid);
        //linkMemState.makeResp(tagged SBUFFER_RSP_REWIND);
        linkMemState.deq();
    endrule
endmodule

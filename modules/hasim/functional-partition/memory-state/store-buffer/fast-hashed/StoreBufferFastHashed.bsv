`include "fpga_components.bsh"
`include "hasim_common.bsh"
`include "funcp_memory.bsh"
`include "soft_connections.bsh"

import HashedStoreBuffer::*;

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

typedef enum {SBUFFER_STATE_READY, SBUFFER_STATE_LOOKUP, SBUFFER_STATE_COMMIT} SBUFFER_STATE deriving (Bits, Eq);

module [HASIM_MODULE] mkFUNCP_StoreBuffer();
    Connection_Server#(TOKEN, VOID)                                                                                     allocate <- mkConnection_Server("storeBufferAllocate");
    Connection_Server#(MEMSTATE_SBUFFER_REQ, MEMSTATE_SBUFFER_RSP)                                                  linkMemState <- mkConnection_Server("mem_storebuf");

    HashedStoreBuffer#(TSub#(SizeOf#(TOKEN_INDEX), 1), SizeOf#(MEM_ADDRESS), SizeOf#(MEM_VALUE), `SBUFFER_HASH_WIDTH, 2) sbuffer <- mkHashedStoreBuffer();

    Reg#(SBUFFER_STATE)                                                                                                    state <- mkReg(SBUFFER_STATE_READY);
    Reg#(TOKEN)                                                                                                            token <- mkRegU();

    rule alloc0(state == SBUFFER_STATE_READY);
        allocate.deq();
        sbuffer.allocate(allocate.getReq.index);
        //allocate.makeResp(?);
    endrule

    rule insert0(linkMemState.getReq matches tagged SBUFFER_REQ_INSERT .req &&& state == SBUFFER_STATE_READY);
        linkMemState.deq();
        sbuffer.insert(req.tok.index, req.addr, req.value, req.num);
        //linkMemState.makeResp(tagged SBUFFER_RSP_INSERT);
    endrule

    rule lookup0(linkMemState.getReq matches tagged SBUFFER_REQ_LOOKUP .req &&& state == SBUFFER_STATE_READY);
        linkMemState.deq;
        sbuffer.lookupReq(req.tok.index, req.addr);
        state <= SBUFFER_STATE_LOOKUP;
        token <= req.tok;
    endrule

    rule lookup1(state == SBUFFER_STATE_LOOKUP);
        state <= SBUFFER_STATE_READY;
        let resp <- sbuffer.lookupResp();
        linkMemState.makeResp(tagged SBUFFER_RSP_LOOKUP {tok: token, addr: resp.addr, mresult: resp.value });
    endrule

    rule commit0(linkMemState.getReq matches tagged SBUFFER_REQ_COMMIT .req &&& state == SBUFFER_STATE_READY);
        linkMemState.deq;
        state <= SBUFFER_STATE_COMMIT;
        sbuffer.commitReq(req.index);
        token <= req;
    endrule

    rule commit1(state == SBUFFER_STATE_COMMIT);
        let resp <- sbuffer.commitResp();
        linkMemState.makeResp(tagged SBUFFER_RSP_COMMIT {tok: token, addr: resp.addr, value: resp.value, hasMore: resp.hasMore});
        if(!resp.hasMore)
            state <= SBUFFER_STATE_READY;
    endrule

    rule rewind0(linkMemState.getReq matches tagged SBUFFER_REQ_REWIND .req &&& state == SBUFFER_STATE_READY);
        linkMemState.deq;
        sbuffer.rewind(req.rewind, req.youngest + 1);
        //linkMemState.makeResp(tagged SBUFFER_RSP_REWIND);
    endrule
endmodule

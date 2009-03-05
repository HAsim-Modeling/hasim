//
// Copyright (C) 2008 Massachusetts Institute of Technology
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
// Tokens are the main way for HAsim to track data across simulator      
// partitions. The token type includes an index for token tables, epochs,
// and scratchpads which partitions can use as they see fit.             

import FShow::*;

`include "asim/provides/fpga_components.bsh"

//
// TOKEN_INDEX is a combination of two ID's:  the context ID and the token ID
// within a context.
//

typedef `CONTEXT_ID_BITS        CONTEXT_ID_SIZE;
typedef TExp#(CONTEXT_ID_SIZE)  NUM_CONTEXTS;
typedef Bit#(CONTEXT_ID_SIZE)   CONTEXT_ID;

// Hack for RRR because it needs natural sized data
typedef `CONTEXT_ID_BITS_RRR      CONTEXT_ID_SIZE_RRR;
typedef Bit#(CONTEXT_ID_SIZE_RRR) CONTEXT_ID_RRR;
function CONTEXT_ID_RRR contextIdToRRR(CONTEXT_ID ctxId) = zeroExtend(ctxId);
function CONTEXT_ID contextIdFromRRR(CONTEXT_ID_RRR ctxId) = truncate(ctxId);

typedef `TOKEN_ID_BITS          TOKEN_ID_SIZE;
typedef TExp#(TOKEN_ID_SIZE)    NUM_TOKENS_PER_CONTEXT;
typedef Bit#(TOKEN_ID_SIZE)     TOKEN_ID;

typedef struct
{
    //
    // The order of fields is important here, mainly so that the implementation
    // of Literal#(TOKEN_INDEX) makes sense.  The token ID must be in the low
    // bits so that expressions such as TOKEN_INDEX + 1 affect the token ID
    // and not the context ID.
    //
    CONTEXT_ID context_id;
    TOKEN_ID   token_id;
}
TOKEN_INDEX
    deriving (Eq, Bits);

typedef SizeOf#(TOKEN_INDEX)    TOKEN_INDEX_SIZE;
typedef TExp#(TOKEN_INDEX_SIZE) NUM_TOKENS;


//
// Standard constructor
//
function TOKEN_INDEX tokenIndexFromIds(CONTEXT_ID c_id, TOKEN_ID t_id);
    return TOKEN_INDEX { context_id: c_id, token_id: t_id };
endfunction


//
// Literal for TOKEN_INDEX converts an integer to an entire index, mapping the
// integer across both the token_id (low bits) and the context_id (high bits).
// This may be useful during an initialization loop.
//
instance Literal#(TOKEN_INDEX);

    function TOKEN_INDEX fromInteger(Integer x);
        Bit#(TOKEN_INDEX_SIZE) t = fromInteger(x);
        return unpack(t);
    endfunction
    
    function Bool inLiteralRange(TOKEN_INDEX x, Integer y);
        return (y < valueOf(NUM_TOKENS));
    endfunction

endinstance


//
// Arithmetic operations on TOKEN_INDEX assume the context IDs of the two
// arguments are identical.  The operations apply only to the token ID.
//
instance Arith#(TOKEN_INDEX);

    function TOKEN_INDEX \+ (TOKEN_INDEX a, TOKEN_INDEX b);
        return TOKEN_INDEX { context_id: a.context_id, token_id: a.token_id + b.token_id };
    endfunction

    function TOKEN_INDEX \- (TOKEN_INDEX a, TOKEN_INDEX b);
        return TOKEN_INDEX { context_id: a.context_id, token_id: a.token_id - b.token_id };
    endfunction

    //
    // Arithmetic operators below this point don't make much sense, but the
    // compiler wants them defined...
    //

    function TOKEN_INDEX \* (TOKEN_INDEX a, TOKEN_INDEX b);
        return TOKEN_INDEX { context_id: a.context_id, token_id: a.token_id * b.token_id };
    endfunction

    function TOKEN_INDEX \/ (TOKEN_INDEX a, TOKEN_INDEX b);
        return TOKEN_INDEX { context_id: a.context_id, token_id: a.token_id / b.token_id };
    endfunction

    function TOKEN_INDEX \% (TOKEN_INDEX a, TOKEN_INDEX b);
        return TOKEN_INDEX { context_id: a.context_id, token_id: a.token_id % b.token_id };
    endfunction

    function TOKEN_INDEX negate (TOKEN_INDEX a);
        return TOKEN_INDEX { context_id: a.context_id, token_id: -a.token_id };
    endfunction

endinstance




//
// In spite of being finite, HAsim requires that tokens be ordered in time.
// There must be a way to figure out which of a pair of tokens is older.
// We accomplish this by making the token index space twice the size of the
// number of tokens that may be in flight.  Since in-flight (live) tokens
// can differ by no more than the maximum token index / 2 it is possible to
// compare the relative ages of live tokens.
//
// We call the full index space TOKEN_INDEX.  We call the live space with
// the high TOKEN_INDEX bit dropped LIVE_TOKEN_INDEX.
//

typedef TSub#(TOKEN_INDEX_SIZE, 1)   LIVE_TOKEN_INDEX_SIZE;
typedef TExp#(LIVE_TOKEN_INDEX_SIZE) NUM_LIVE_TOKENS;
typedef Bit#(LIVE_TOKEN_INDEX_SIZE)  LIVE_TOKEN_INDEX;


function LIVE_TOKEN_INDEX liveTokenIdx(TOKEN_INDEX idx);
    return { idx.context_id, idx.token_id[valueOf(TOKEN_ID_SIZE) - 2 : 0] };
endfunction


//
// Order relationships between tokens.  When comparing the age of two tokens
// we depend on half the token index space being unused.  We thus know that
// the tokens may differ no more than NUM_LIVE_TOKENS.
//
// tokenIsOlderOrEQ returns true iff token "older" really is older than or equal
// to "younger".
//
function Bool tokenIsOlderOrEq(TOKEN_INDEX older, TOKEN_INDEX younger);
    return (younger.token_id - older.token_id)[valueOf(TOKEN_ID_SIZE) - 1] == 0;
endfunction


//
// Epoch is maintained by functional model and is available to timing models
// for read.  See struct TOKEN declaration below.
//
typedef Bit#(`TOKEN_BRANCH_EPOCH_BITS) TOKEN_BRANCH_EPOCH;
typedef Bit#(1) TOKEN_FAULT_EPOCH;

typedef struct 
{
    // Epoch changes tag the new stream of instructions following a branch
    // misprediction.  Epoch is set in the functional partition during
    // funcp_newInFlight and will not change.  The epoch counter that
    // is used to set epoch is updated as a side effect of funcp_rewindToToken.
    TOKEN_BRANCH_EPOCH branch;

    // Fault epoch lets a stage late in the timing pipeline detect a new stream of
    // instructions following a fault.  The fault handler is invoked only once
    // an instruction is ready and able to commit, so only one new stream can
    // be in flight at a time and a single bit is enough.  faultEpoch is set
    // during funcp_newInflight and will not change.  The fault epoch counter
    // used to set the value is updated as a side effect of funcp_handleFault.
    TOKEN_FAULT_EPOCH fault;
}
TOKEN_EPOCH
    deriving (Eq, Bits);


//
// Scratchpad space is temporary storage available for private use in
// timing models.
//
typedef Bit#(`TOKEN_TIMEP_SCRATCHPAD_BITS) TOKEN_TIMEP_SCRATCHPAD;

typedef struct
{

    TOKEN_TIMEP_SCRATCHPAD  scratchpad;
}
TOKEN_TIMEP_INFO 
    deriving (Eq, Bits);


//
// Token passed around the simulator.  Accessor functions are provided for most
// fields and should be used instead of using a token directly.  This will let
// us move fields around.
//
typedef struct
{
    TOKEN_INDEX       index;

    // Set by functional partition for instructions that would fault on commit.
    // Will not change during or after funcp_commitResults.
    Bool poison;

    // Initialized by the functional partition when a token is allocated.
    // Will not change after that.
    TOKEN_EPOCH epoch;

    // Initialized by the functional partition when a token is created.
    // Values set by the timing partition pass through the functional partition
    // unmodified.
    TOKEN_TIMEP_INFO timep_info;
}
TOKEN 
    deriving (Eq, Bits);


function CONTEXT_ID tokContextId(TOKEN tok) = tok.index.context_id;
function TOKEN_ID tokTokenId(TOKEN tok) = tok.index.token_id;

function Bool tokIsPoisoned(TOKEN tok) = tok.poison;

function TOKEN_BRANCH_EPOCH tokBranchEpoch(TOKEN tok) = tok.epoch.branch;
function TOKEN_FAULT_EPOCH tokFaultEpoch(TOKEN tok) = tok.epoch.fault;
function TOKEN_EPOCH tokEpoch(TOKEN tok) = tok.epoch;
    
function TOKEN_EPOCH initEpoch(TOKEN_BRANCH_EPOCH b, TOKEN_FAULT_EPOCH f) =
    TOKEN_EPOCH { branch: b, fault: f };


// ========================================================================
//
// Useful debugging functions.
//
// ========================================================================

instance FShow#(TOKEN_INDEX);
    function Fmt fshow(TOKEN_INDEX tokIdx);
        return $format("TOKEN (%0d, %0d)", tokIdx.context_id, tokIdx.token_id);
    endfunction
endinstance

instance FShow#(TOKEN);
    function Fmt fshow(TOKEN tok);
        Fmt s = fshow(tok.index);
        if (tokIsPoisoned(tok))
            s = s + fshow(" POISON");

        // For some reason the following line keeps "POISON" from appearing
        // improperly when the token is the last object printed.
        s = s + fshow("");

        return s;
    endfunction
endinstance


// ========================================================================
//
// Convenience modules for wrapping LIVE_TOKEN sized storage that is indexed
// by a TOKEN_INDEX.  These provide automatic conversion on the access methods
// from TOKEN_INDEX to LIVE_TOKEN_INDEX.
//
// ========================================================================

module mkLiveTokenBRAM
    // interface:
        (BRAM#(TOKEN_INDEX, data_T))
    provisos
        (Bits#(data_T, data_SZ));
    
    BRAM#(LIVE_TOKEN_INDEX, data_T) mem <- mkBRAM();

    method Action readReq(TOKEN_INDEX a);
        mem.readReq(liveTokenIdx(a));
    endmethod

    method ActionValue#(data_T) readRsp();
        data_T rsp <- mem.readRsp();
        return rsp;
    endmethod

    method Action write(TOKEN_INDEX a, data_T d);
        mem.write(liveTokenIdx(a), d);
    endmethod

endmodule


module mkLiveTokenBRAMInitialized#(data_T initval)
    // interface:
        (BRAM#(TOKEN_INDEX, data_T))
    provisos
        (Bits#(data_T, data_SZ));
    
    BRAM#(LIVE_TOKEN_INDEX, data_T) mem <- mkBRAMInitialized(initval);

    method Action readReq(TOKEN_INDEX a);
        mem.readReq(liveTokenIdx(a));
    endmethod

    method ActionValue#(data_T) readRsp();
        data_T rsp <- mem.readRsp();
        return rsp;
    endmethod

    method Action write(TOKEN_INDEX a, data_T d);
        mem.write(liveTokenIdx(a), d);
    endmethod

endmodule


//
// Allocate multiple real read ports when realPorts is True.  Otherwise allocate
// pseudo ports that share a single physical read port but route traffic correctly.
//
module mkLiveTokenBRAMMultiRead#(Bool realPorts)
    // interface:
        (BRAM_MULTI_READ#(n, TOKEN_INDEX, data_T))
    provisos
        (Bits#(data_T, data_SZ));
    
    BRAM_MULTI_READ#(n, LIVE_TOKEN_INDEX, data_T) mem;
    if (realPorts)
        mem <- mkBRAMMultiRead();
    else
        mem <- mkBRAMPseudoMultiRead();

    // readPorts

    Vector#(n, BROM#(TOKEN_INDEX, data_T)) portsLocal = newVector();

    for(Integer i = 0; i < valueOf(n); i = i + 1)
    begin
        portsLocal[i] = (interface BROM#(TOKEN_INDEX, data_T);
                             method Action readReq(TOKEN_INDEX a);
                                 mem.readPorts[i].readReq(liveTokenIdx(a));
                             endmethod

                             method ActionValue#(data_T) readRsp();
                                 data_T rsp <- mem.readPorts[i].readRsp();
                                 return rsp;
                             endmethod
                         endinterface);
    end

    interface readPorts = portsLocal;

    method Action write(TOKEN_INDEX a, data_T d);
        mem.write(liveTokenIdx(a), d);
    endmethod

endmodule


//
// Allocate multiple real read ports when realPorts is True.  Otherwise allocate
// pseudo ports that share a single physical read port but route traffic correctly.
//
module mkLiveTokenBRAMMultiReadInitialized#(Bool realPorts, data_T initval)
    // interface:
        (BRAM_MULTI_READ#(n, TOKEN_INDEX, data_T))
    provisos
        (Bits#(data_T, data_SZ));
    
    BRAM_MULTI_READ#(n, LIVE_TOKEN_INDEX, data_T) mem;
    if (realPorts)
        mem <- mkBRAMMultiReadInitialized(initval);
    else
        mem <- mkBRAMPseudoMultiReadInitialized(initval);

    // readPorts

    Vector#(n, BROM#(TOKEN_INDEX, data_T)) portsLocal = newVector();

    for(Integer i = 0; i < valueOf(n); i = i + 1)
    begin
        portsLocal[i] = (interface BROM#(TOKEN_INDEX, data_T);
                             method Action readReq(TOKEN_INDEX a);
                                 mem.readPorts[i].readReq(liveTokenIdx(a));
                             endmethod

                             method ActionValue#(data_T) readRsp();
                                 data_T rsp <- mem.readPorts[i].readRsp();
                                 return rsp;
                             endmethod
                         endinterface);
    end

    interface readPorts = portsLocal;

    method Action write(TOKEN_INDEX a, data_T d);
        mem.write(liveTokenIdx(a), d);
    endmethod

endmodule


module mkLiveTokenLUTRAM#(data_t init)
    // interface:
        (LUTRAM#(TOKEN_INDEX, data_t))
    provisos(Bits#(data_t, data_SZ));

    LUTRAM#(LIVE_TOKEN_INDEX, data_t) mem <- mkLUTRAM(init);

    method Action upd(TOKEN_INDEX addr, data_t d);
        mem.upd(liveTokenIdx(addr), d);
    endmethod

    method data_t sub(TOKEN_INDEX addr);
        return mem.sub(liveTokenIdx(addr));
    endmethod

endmodule


module mkLiveTokenLUTRAMU
    // interface:
        (LUTRAM#(TOKEN_INDEX, data_t))
    provisos(Bits#(data_t, data_SZ));

    LUTRAM#(LIVE_TOKEN_INDEX, data_t) mem <- mkLUTRAMU();

    method Action upd(TOKEN_INDEX addr, data_t d);
        mem.upd(liveTokenIdx(addr), d);
    endmethod

    method data_t sub(TOKEN_INDEX addr);
        return mem.sub(liveTokenIdx(addr));
    endmethod

endmodule


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

typedef `TOKEN_INDEX_BITS       TOKEN_INDEX_SIZE;
typedef TExp#(TOKEN_INDEX_SIZE) NUM_TOKENS;
typedef Bit#(TOKEN_INDEX_SIZE)  TOKEN_INDEX;

typedef TSub#(TOKEN_INDEX_SIZE, 1)   LIVE_TOKEN_INDEX_SIZE;
typedef TExp#(LIVE_TOKEN_INDEX_SIZE) NUM_LIVE_TOKENS;
typedef Bit#(LIVE_TOKEN_INDEX_SIZE)  LIVE_TOKEN_INDEX;


function LIVE_TOKEN_INDEX liveTokenIdx(TOKEN_INDEX idx);
    return truncate(idx);
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
    return (younger - older)[valueOf(TOKEN_INDEX_SIZE) - 1] == 0;
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


function Bool tokIsPoisoned(TOKEN tok) = tok.poison;

function TOKEN_BRANCH_EPOCH tokBranchEpoch(TOKEN tok) = tok.epoch.branch;
function TOKEN_FAULT_EPOCH tokFaultEpoch(TOKEN tok) = tok.epoch.fault;
function TOKEN_EPOCH tokEpoch(TOKEN tok) = tok.epoch;
    
function TOKEN_EPOCH initEpoch(TOKEN_BRANCH_EPOCH b, TOKEN_FAULT_EPOCH f) =
    TOKEN_EPOCH { branch: b, fault: f };

//
// Convenience modules for wrapping LIVE_TOKEN sized storage that is indexed
// by a TOKEN_INDEX.  These provide automatic conversion on the access methods
// from TOKEN_INDEX to LIVE_TOKEN_INDEX.
//

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


module mkLiveTokenBRAMMultiRead
    // interface:
        (BRAM_MULTI_READ#(n, TOKEN_INDEX, data_T))
    provisos
        (Bits#(data_T, data_SZ));
    
    BRAM_MULTI_READ#(n, LIVE_TOKEN_INDEX, data_T) mem <- mkBRAMMultiRead();

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


module mkLiveTokenBRAMMultiReadInitialized#(data_T initval)
    // interface:
        (BRAM_MULTI_READ#(n, TOKEN_INDEX, data_T))
    provisos
        (Bits#(data_T, data_SZ));
    
    BRAM_MULTI_READ#(n, LIVE_TOKEN_INDEX, data_T) mem <- mkBRAMMultiReadInitialized(initval);

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

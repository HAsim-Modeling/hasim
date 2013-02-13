//
// Copyright (C) 2008 Intel Corporation
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

// memstate_manager_macro_ops

// Tracks memory state with a store buffer,
// using the macro-operation design pattern.

// Library imports

import FIFO::*;
import FIFOF::*;
import Vector::*;
import FShow::*;

// Project Foundation Imports

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/provides/funcp_regstate_base_types.bsh"

// Memstate imports

`include "asim/provides/funcp_memstate_base_types.bsh"
`include "asim/provides/funcp_memstate_storebuffer.bsh"

//
// Store pipeline state machine.
//
typedef enum
{
    FUNCP_MEMSTATE_STORE_READY,
    FUNCP_MEMSTATE_STORE_TRYLOCK_REQ,
    FUNCP_MEMSTATE_STORE_TRYLOCK_RSP
}
FUNCP_MEMSTATE_STORE_STATE
    deriving (Eq, Bits);


// mkFUNCP_MemStateManager

// The module which encapsulates Loads and Stores,
// using macro-operations to refer to the store buffer and memory.

module [HASIM_MODULE] mkFUNCP_MemStateManager ();

    DEBUG_FILE debugLog <- mkDebugFile(`FUNCP_MEMSTATE_LOGFILE_NAME);

    // ***** Submodules ***** //

    // Instantiate the Store Buffer
    MEMSTATE_SBUFFER stBuffer <- mkFUNCP_StoreBuffer(debugLog);

    // ***** Soft Connections ***** //

    // Links to the functional partition register state.
    Connection_Server#(MEMSTATE_REQ, MEMSTATE_RESP) linkRegState <- mkConnection_Server("funcp_memstate");

    // Link to memory
    Connection_Client#(MEM_REQUEST, MEMSTATE_RESP) linkMemory <- mkConnection_Client("funcp_memory");

    // ***** Local data ***** //

    FIFOF#(Bool) writeBackQ <- mkFIFOF();
    FIFO#(MEM_LOAD_INFO) sbLookupQ <- mkFIFO();
    MEMSTATE_LOCK_LINE_MGR lockMgr <- mkFUNCPMemStateLockMgr(debugLog);

    // ***** Rules ***** //

    // memStore
    // 1-stage macro-operation.
    
    // When:   Any time we get a Store request from the register state.
    // Effect: We record the store in the store buffer but don't actually change memory.
    //         This change will take effect when the store is committed.
    // Parameters: MEMSTATE_REQ (MEMSTATE_REQ_STORE)
    // Returns:    None.
    
    Reg#(FUNCP_MEMSTATE_STORE_STATE) storeState <- mkReg(FUNCP_MEMSTATE_STORE_READY);

    rule memStore (linkRegState.getReq() matches tagged REQ_STORE .stInfo &&&
                   storeState == FUNCP_MEMSTATE_STORE_READY);

        if (! stInfo.reqExclusive)
        begin
            // Normal path: place the value in store buffer.
            stBuffer.insertReq(stInfo.tok.index, stInfo.addr, stInfo.value);

            // Pop the request from the register state.
            linkRegState.deq();

            debugLog.record($format("STORE: ") + fshow(stInfo.tok.index) + $format(", addr=0x%x, value=0x%x", stInfo.addr, stInfo.value));
        end
        else
        begin
            storeState <= FUNCP_MEMSTATE_STORE_TRYLOCK_REQ;

            debugLog.record($format("STORE wants EXCLUSIVE: ") + fshow(stInfo.tok.index) + $format(", addr=0x%x, value=0x%x", stInfo.addr, stInfo.value));
        end
    endrule

    //
    // memStoreTryLockReq --
    //   Look in store buffer to see whether a store may lock may obtain
    //   exlusive access to a line.
    //
    rule memStoreTryLockReq (linkRegState.getReq() matches tagged REQ_STORE .stInfo &&&
                             storeState == FUNCP_MEMSTATE_STORE_TRYLOCK_REQ);

        // We only permit the lock if no other context has a pending store
        // to the location.  This requirement is stronger than necessary,
        // but easier to implement.  Under the assumption that all stores to
        // the address of a lock will be conditional stores, the only
        // stores present in the store buffer should be successful conditional
        // stores already holding a lock.
        stBuffer.testForExclusiveReq(stInfo.tok.index, stInfo.addr);
        storeState <= FUNCP_MEMSTATE_STORE_TRYLOCK_RSP;
    endrule

    //
    // memStoreTryLockRsp --
    //   Determine whether a conditional store gets the requested lock.
    //
    rule memStoreTryLockRsp (linkRegState.getReq() matches tagged REQ_STORE .stInfo &&&
                             storeState == FUNCP_MEMSTATE_STORE_TRYLOCK_RSP);

        // Pop the request from the register state.
        linkRegState.deq();

        storeState <= FUNCP_MEMSTATE_STORE_READY;

        Bool got_lock = False;
        let not_in_sb <- stBuffer.testForExclusiveResp();
        if (not_in_sb)
        begin
            got_lock <- lockMgr.tryLock(tokContextId(stInfo.tok), stInfo.addr);
        end

        // Store permitted only if the lock was acquired
        if (got_lock)
        begin
            stBuffer.insertReq(stInfo.tok.index, stInfo.addr, stInfo.value);
        end

        // Respond with result
        linkRegState.makeResp(memStateRespStatus(stInfo.memRefToken, got_lock));

        debugLog.record($format("  STORE EXCLUSIVE: not in SB %0d, got lock %0d, ", not_in_sb, got_lock) + fshow(stInfo.memRefToken));
    endrule

    // memLoad
    
    // 2- or 3-stage macro-operation. (Depending on if store buffer hits.)
 
    // When:   Any time we get a Load request from the register state.
    // Effect: We check the store buffer and memory and return the response.
    // Parameters: MEMSTATE_REQ (MEMSTATE_REQ_LOAD)
    // Returns:    MEMSTATE_RSP (MEMSTATE_RSP_LOAD)

    // memLoad1

    // When:   Any time we get a Load request from the register state.
    //         Loads may not be processed while commit is in progress because
    //         values are removed from the store buffer before being written
    //         to memory.  There is a window in which earlier stores
    //         may be invisible.
    // Effect: Convert the address and send it to the store buffer.

    (* conservative_implicit_conditions *)
    rule memLoad (linkRegState.getReq() matches tagged REQ_LOAD .ldInfo &&&
                  ! writeBackQ.notEmpty());

        // Pop the request from the register state.
        linkRegState.deq();

        debugLog.record($format("LOAD: ") + fshow(ldInfo.tok.index) + $format(" / ") + fshow(ldInfo.memRefToken) + $format(", addr=0x%x", ldInfo.addr));

        // Load requesting a monitor for possible condititional store?
        if (ldInfo.reqLock)
        begin
            lockMgr.monStart(tokContextId(ldInfo.tok), ldInfo.addr);
        end

        // Check the store buffer.  The store buffer may either return an
        // immediate answer that the address is not present or may take
        // multiple cycles to respond.
        let check_sb <- stBuffer.lookupReq(ldInfo.tok.index, ldInfo.addr);

        let load_req = MEM_LOAD_INFO { contextId: tokContextId(ldInfo.tok),
                                       addr: ldInfo.addr,
                                       iStream: ldInfo.iStream,
                                       memRefToken: ldInfo.memRefToken };

        if (check_sb)
        begin
            // Value might be in store buffer.  Wait for the SB answer.
            sbLookupQ.enq(load_req);
        end
        else
        begin
            // Value is not in store buffer.  Request from memory.
            linkMemory.makeReq(tagged MEM_LOAD load_req);
            debugLog.record($format("  LOAD SB hash miss"));
        end

    endrule

    //
    // NOTE ON ORDER -- memLoadSB and memLoadMemory may return load results
    //                  out of order in order to improve performance!  The
    //                  register state manager is expected to deal with this.
    //

    //
    // memLoadSB --
    //   This path is taken when the address hits in the store buffer hash and
    //   the store buffer must look to see whether the value is present.
    //
    rule memLoadSB (True);
        let sb_rsp <- stBuffer.lookupResp();
        
        let load_req = sbLookupQ.first();
        sbLookupQ.deq();

        case (sb_rsp.mvalue) matches
          tagged Invalid:
          begin
              // A miss in the store buffer.  Look in memory.
              linkMemory.makeReq(tagged MEM_LOAD load_req);
              debugLog.record($format("  LOAD SB miss, ") + fshow(load_req.memRefToken));
          end
          tagged Valid .sb_val:
          begin
              // A hit in the store buffer.  No need to look in memory.
              linkRegState.makeResp(memStateRespLoad(load_req.memRefToken, sb_val));
              debugLog.record($format("  LOAD SB hit: value=0x%x, ", sb_val) + fshow(load_req.memRefToken));
          end
        endcase
    endrule


    //
    // memLoadMemory --
    //   Forward load response from memory back to the register state manager.
    //
    rule memLoadMemory (True);
        let mem_resp = linkMemory.getResp();
        linkMemory.deq();

        linkRegState.makeResp(mem_resp);
        debugLog.record($format("  LOAD from mem: value=0x%x, ", mem_resp.value) + fshow(mem_resp.memRefToken));
    endrule

  
    //
    // commitResults --
    //   Remap a commited store in the store buffer from the TOKEN index space
    //   to the STORE_TOKEN index space.
    rule commitResults (linkRegState.getReq() matches tagged REQ_COMMIT .req);
        linkRegState.deq();

        stBuffer.commitReq(req.tok.index, req.storeTok.index);
        debugLog.record($format("COMMIT: ") + fshow(req.tok.index) + $format(" is now ") + fshow(req.storeTok.index));
    endrule

    rule commitResultsResp (True);
        match {.addr, .done} <- stBuffer.commitResp();
        
        debugLog.record($format("  COMMIT addr=0x%x, done=%0d ", addr, done));
    endrule


    // writeBack
    
    // 2-stage macro-operation
    // When:   When the register state requests a write back for a store.
    // Effect: Remove a value from the store buffer and send it on to memory.
    // Parameters: TOKEN
    // Returns:    N/A
    
    // writeBack1

    // Effect: Retrieve the value from the store buffer.

    rule writeBack1 (linkRegState.getReq() matches tagged REQ_WRITE_BACK .req);
        // Get the input from the register state. Begin macro-operation.
        linkRegState.deq();

        debugLog.record($format("WRITEBACK: ") + fshow(req.storeTok.index));

        // Send the request on to the store buffer.
        stBuffer.writeBackReq(req.storeTok.index);

        // WriteBack queue is used just to lock out loads while the location
        // of stored data is unpredictable.
        writeBackQ.enq(?);
    endrule
    
    // writeBack2
    
    // When:   Some time after writeBack1
    // Effect: Send the store on to update memory.

    (* descending_urgency = "writeBack2, commitResultsResp, memLoadMemory, memLoadSB, memLoad, memStoreTryLockRsp, memStore" *)
    rule writeBack2 (True);
        // Get the response from the store buffer.
        let rsp <- stBuffer.writeBackResp();

        if (!rsp.hasMore)
            writeBackQ.deq();

        debugLog.record($format("  WRITEBACK resp: addr=0x%x, value=0x%x, more=%d", rsp.addr, rsp.value, rsp.hasMore));

        // Send the actual store to memory.
        linkMemory.makeReq(funcpMemStoreReq(rsp.tokIdx.context_id, rsp.addr, rsp.value));

        lockMgr.noteWriteback(rsp.tokIdx.context_id, rsp.addr);
    endrule

    // rewind
    
    // 1-stage macro-operation
    
    // When:   When the register state requests a rollback.
    // Effect: Pass the request on to the store buffer.
    // Parameters: TOKEN, TOKEN
    // Returns:    N/A

    rule rewind (linkRegState.getReq() matches tagged REQ_REWIND .rew);
      
        linkRegState.deq();

        debugLog.record($format("REWIND: rewind_to ") + fshow(rew.rewind_to) + $format(", rewind_from ") + fshow(rew.rewind_from));

        // Pass the request on to the store buffer.
        stBuffer.rewindReq(rew.rewind_to, rew.rewind_from);

    endrule

endmodule


// ========================================================================
//
//  Memory lock manager for enforcing exclusive access to a line for
//  load locked, store conditional.
//
// ========================================================================

interface MEMSTATE_LOCK_LINE_MGR;
    // Request monitoring of an address
    method Action monStart(CONTEXT_ID ctx, MEM_ADDRESS addr);

    // Note a write-back (movement from local store buffer to global memory)
    method Action noteWriteback(CONTEXT_ID ctx, MEM_ADDRESS addr);

    // Request exclusive access
    method ActionValue#(Bool) tryLock(CONTEXT_ID ctx, MEM_ADDRESS addr);
endinterface


module mkFUNCPMemStateLockMgr#(DEBUG_FILE debugLog)
    // Interface:
    (MEMSTATE_LOCK_LINE_MGR);

    // Track up to 8 monitor requests at a time
    Reg#(Vector#(8, Bool)) monValid <- mkReg(replicate(False));
    Reg#(Vector#(8, Tuple2#(CONTEXT_ID, MEM_ADDRESS))) monAddrs <- mkRegU();

    //
    // Start watching a memory location.  Only a relatively small number of
    // locations can be monitored at a time.  We seek here to monitor at least
    // as many locations as a reasonable timing model might expect.
    // New requests push out old (even valid) ones.
    //
    method Action monStart(CONTEXT_ID ctx, MEM_ADDRESS addr);
        monValid <= shiftInAtN(monValid, True);
        monAddrs <= shiftInAtN(monAddrs, tuple2(ctx, addr));

        debugLog.record($format("    Lock Mgr:  Monitor CTX %0d, Addr 0x%x", ctx, addr));
    endmethod

    method Action noteWriteback(CONTEXT_ID ctx, MEM_ADDRESS addr);
        //
        // Clear a monitor if it was valid and the written address matches
        // in a context other than the monitor's context.
        //
        function Bool clearConflicts(Bool isValid,
                                     Tuple2#(CONTEXT_ID, MEM_ADDRESS) monitor);
            match {.mon_ctx, .mon_addr} = monitor;
            return isValid && ((mon_ctx == ctx) || (mon_addr != addr));
        endfunction

        // Update all monitor valid bits
        let upd = zipWith(clearConflicts, monValid, monAddrs);
        monValid <= upd;

        // Debugging message loop
        for (Integer i = 0; i < 8; i = i + 1)
        begin
            if (monValid[i] && ! upd[i])
            begin
                debugLog.record($format("    Lock Mgr:  WB invalidates monitor CTX %0d, Addr 0x%x", tpl_1(monAddrs[i]), tpl_2(monAddrs[i])));
            end
        end
    endmethod

    method ActionValue#(Bool) tryLock(CONTEXT_ID ctx, MEM_ADDRESS addr);
        //
        // Lock is valid if some monitor matches the context and address.
        //
        function Bool validLock(Tuple2#(Bool, Tuple2#(CONTEXT_ID, MEM_ADDRESS)) arg);
            let isValid = tpl_1(arg);
            Tuple2#(CONTEXT_ID, MEM_ADDRESS) arg2 = tpl_2(arg);
            match {.mon_ctx, .mon_addr} = arg2;

            return isValid && (mon_ctx == ctx) && (mon_addr == addr);
        endfunction

        if (any(validLock, zip(monValid, monAddrs)))
        begin
            debugLog.record($format("    Lock Mgr:  Got lock CTX %0d, Addr 0x%x", ctx, addr));
            return True;
        end
        else
        begin
            return False;
        end
    endmethod
endmodule

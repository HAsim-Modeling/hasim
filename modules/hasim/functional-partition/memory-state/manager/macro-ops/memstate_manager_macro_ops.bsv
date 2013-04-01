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
import Clocks::*;

// Project Foundation Imports

`include "awb/provides/hasim_common.bsh"
`include "awb/provides/fpga_components.bsh"
`include "awb/provides/soft_connections.bsh"
`include "awb/provides/soft_services.bsh"
`include "awb/provides/soft_services_lib.bsh"
`include "awb/provides/soft_services_deps.bsh"
`include "awb/provides/common_services.bsh"
`include "awb/provides/librl_bsv.bsh"

`include "awb/provides/funcp_memory.bsh"
`include "awb/provides/funcp_regstate_base_types.bsh"

// Memstate imports

`include "awb/provides/funcp_memstate_base_types.bsh"
`include "awb/provides/funcp_memstate_storebuffer.bsh"

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
    STDIO#(Bit#(64)) stdio <- mkStdIO_Debug();

    let stdioLM <- mkStdIO_CondPrintf(ioMask_FUNCP_MEMSTATE, stdio);

    let msgLockResult <- getGlobalStringUID("MEMSTATE STORE EXCLUSIVE: CTX %d, Addr 0x%llx, not in SB %d, Lock Mgr %d, can store %d\n");

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
    MEMSTATE_LOCK_LINE_MGR lockMgr <- mkFUNCPMemStateLockMgr(debugLog, stdioLM);

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
        lockMgr.tryLockReq(tokContextId(stInfo.tok), stInfo.addr);
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

        let not_in_sb <- stBuffer.testForExclusiveResp();
        let got_lock <- lockMgr.tryLockResp();

        let can_store = not_in_sb && got_lock;

        if (can_store)
        begin
            // Success
            stBuffer.insertReq(stInfo.tok.index, stInfo.addr, stInfo.value);
        end

        // Respond with result
        linkRegState.makeResp(memStateRespStatus(stInfo.memRefToken, can_store));

        debugLog.record($format("  STORE EXCLUSIVE: not in SB %0d, Lock Mgr %0d, can store %0d, ", not_in_sb, got_lock, can_store) + fshow(stInfo.memRefToken));
        stdioLM.printf(msgLockResult, list(zeroExtend(tokContextId(stInfo.tok)), zeroExtend(stInfo.addr), zeroExtend(pack(not_in_sb)), zeroExtend(pack(got_lock)), zeroExtend(pack(can_store))));
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
    method Action tryLockReq(CONTEXT_ID ctx, MEM_ADDRESS addr);
    method ActionValue#(Bool) tryLockResp();
endinterface


//
// Each context may hold one lock on an address.
//
typedef struct
{
    // Unique tag associated with an address.  Called a "tag" instead of address
    // because addresses may be hashed.
    MEM_WORD_ADDRESS tag;

    // Chain of contexts within a hash table chain.
    Maybe#(CONTEXT_ID) prev;
    Maybe#(CONTEXT_ID) next;
}
FUNCP_MEMSTATE_CTX_LOCK
    deriving (Eq, Bits);

typedef enum
{
    FUNCP_MEMSTATE_LOCK_READY,
    FUNCP_MEMSTATE_LOCK_NEW_REQ,
    FUNCP_MEMSTATE_LOCK_UPD_PREV,
    FUNCP_MEMSTATE_LOCK_NOTE_WRITE,
    FUNCP_MEMSTATE_LOCK_CLEAR_MON
}
FUNCP_MEMSTATE_LOCK_STATE
    deriving (Eq, Bits);

//
// Incoming requests
//
typedef enum
{
    FUNCP_MEMSTATE_LOCK_REQ_MON_START,
    FUNCP_MEMSTATE_LOCK_REQ_NOTE_WRITE,
    FUNCP_MEMSTATE_LOCK_REQ_TRY_LOCK
}
FUNCP_MEMSTATE_LOCK_REQ
    deriving (Eq, Bits);


module [HASIM_MODULE] mkFUNCPMemStateLockMgr#(DEBUG_FILE debugLog,
                                              STDIO_COND_PRINTF#(Bit#(64)) stdio)
    // Interface:
    (MEMSTATE_LOCK_LINE_MGR)
    provisos (NumAlias#(n_HASH_BUCKETS, TAdd#(2, CONTEXT_ID_SIZE)),
              Alias#(t_HASH_IDX, Bit#(n_HASH_BUCKETS)));

    Reg#(FUNCP_MEMSTATE_LOCK_STATE) state <- mkReg(FUNCP_MEMSTATE_LOCK_READY);

    // Hash table
    MEMORY_IFC#(t_HASH_IDX, Maybe#(CONTEXT_ID)) hashTable <-
        mkBRAMInitialized(tagged Invalid);

    // Each context may monitor a single address.  When active, a context's
    // monitor is stored in the hash table.
    MEMORY_IFC#(CONTEXT_ID, Maybe#(FUNCP_MEMSTATE_CTX_LOCK)) monitors <-
        mkBRAMInitialized(tagged Invalid);

    Reg#(FUNCP_MEMSTATE_CTX_LOCK) curMon <- mkRegU();
    Reg#(Maybe#(CONTEXT_ID)) clearMonResumeMonIdx <- mkRegU();
    Reg#(FUNCP_MEMSTATE_LOCK_STATE) clearMonResumeState <- mkRegU();


    FIFO#(Tuple3#(FUNCP_MEMSTATE_LOCK_REQ, CONTEXT_ID, MEM_ADDRESS)) newReqQ <-
        mkFIFO();

    FIFO#(Tuple3#(FUNCP_MEMSTATE_LOCK_REQ, CONTEXT_ID, MEM_WORD_ADDRESS)) curQ <-
        mkFIFO();

    FIFO#(Bool) tryLockRspQ <- mkFIFO();

    // Debugging strings
    let msgMonNew <- getGlobalStringUID("MEMSTATE LockMgr: Lock Mgr:  Monitor CTX %d, tag 0x%llx\n");
    let msgWBInval <- getGlobalStringUID("MEMSTATE LockMgr: WB invalidates monitor idx %d, tag 0x%llx\n");
    let msgGotLock <- getGlobalStringUID("MEMSTATE LockMgr: Got lock CTX %d, tag 0x%llx\n");
    let msgFailedLock <- getGlobalStringUID("MEMSTATE LockMgr: Failed lock CTX %d, tag 0x%llx\n");

    //
    // startReq --
    //   All new requests begin here.  Compute the hash table entry and load
    //   the relevant current state.
    //
    rule startReq (state == FUNCP_MEMSTATE_LOCK_READY);
        match {.req, .ctx, .addr} = newReqQ.first();
        newReqQ.deq();

        // Monitor words, not bytes
        let word_addr = wordAddrFromByteAddr(addr);
        // Hash the address to generate a tag.  The hash function is reversible,
        // so there is a 1:1 mapping of input to output values.
        let tag = hashBits(word_addr);

        hashTable.readReq(truncate(tag));
        monitors.readReq(ctx);

        curQ.enq(tuple3(req, ctx, tag));
        state <= FUNCP_MEMSTATE_LOCK_NEW_REQ;

        debugLog.record($format("    Lock Mgr:  New req %0d, CTX %0d, Addr 0x%x, Tag 0x%x", req, ctx, addr, tag));
    endrule


    //
    // handleMonStart --
    //   Start monitoring an address for a specific context.
    //
    rule handleMonStart0 (state == FUNCP_MEMSTATE_LOCK_NEW_REQ &&
                          tpl_1(curQ.first) == FUNCP_MEMSTATE_LOCK_REQ_MON_START);
        match {.req, .ctx, .tag} = curQ.first();
        let cur_head <- hashTable.readRsp();
        let ctx_mon <- monitors.readRsp();

        if (ctx_mon matches tagged Valid .mon)
        begin
            // Context already has a monitor.  Start by clearing the old
            // monitor.  This rule will fire again once the monitor is cleared.
            state <= FUNCP_MEMSTATE_LOCK_CLEAR_MON;
            monitors.write(ctx, tagged Invalid);
            curMon <= mon;
            clearMonResumeMonIdx <= tagged Valid ctx;
            clearMonResumeState <= state;
        end
        else
        begin
            // Add context's monitor to the head of the tag's hash chain
            let new_mon = FUNCP_MEMSTATE_CTX_LOCK { tag: tag,
                                                    prev: tagged Invalid,
                                                    next: cur_head };
            monitors.write(ctx, tagged Valid new_mon);
            hashTable.write(truncate(tag), tagged Valid ctx);

            debugLog.record($format("    Lock Mgr:  Monitor CTX %0d, Tag 0x%x", ctx, tag));
            stdio.printf(msgMonNew, list(zeroExtend(ctx), zeroExtend(tag)));

            curMon <= new_mon;

            if (cur_head matches tagged Valid .old_head)
            begin
                monitors.readReq(old_head);
                state <= FUNCP_MEMSTATE_LOCK_UPD_PREV;
            end
            else
            begin
                curQ.deq();
                state <= FUNCP_MEMSTATE_LOCK_READY;
            end
        end
    endrule

    rule handleMonStart1 (state == FUNCP_MEMSTATE_LOCK_UPD_PREV);
        match {.req, .ctx, .tag} = curQ.first();
        let m_next_mon <- monitors.readRsp();
        let next_mon = validValue(m_next_mon);
        curQ.deq();

        // Update the next entry in the list to point back to the newly added
        // monitor.
        next_mon.prev = tagged Valid ctx;
        monitors.write(validValue(curMon.next), tagged Valid next_mon);

        state <= FUNCP_MEMSTATE_LOCK_READY;
    endrule


    //
    // handleNoteWrite0 --
    //   Watch all stores and kill any conflicting locks.
    //
    Reg#(CONTEXT_ID) monCtx <- mkRegU();

    rule handleNoteWrite0 (state == FUNCP_MEMSTATE_LOCK_NEW_REQ &&
                           tpl_1(curQ.first) == FUNCP_MEMSTATE_LOCK_REQ_NOTE_WRITE);
        match {.req, .ctx, .tag} = curQ.first();
        let cur_head <- hashTable.readRsp();
        let ctx_mon <- monitors.readRsp();

        if (! isValid(cur_head))
        begin
            // Nothing at the hash table entry.  Address is not present.
            curQ.deq();
            state <= FUNCP_MEMSTATE_LOCK_READY;
        end
        else
        begin
            // Start walking the entries, looking for possible matches.
            monitors.readReq(validValue(cur_head));
            state <= FUNCP_MEMSTATE_LOCK_NOTE_WRITE;
            monCtx <= validValue(cur_head);
        end
    endrule

    rule handleNoteWrite1 (state == FUNCP_MEMSTATE_LOCK_NOTE_WRITE);
        match {.req, .ctx, .tag} = curQ.first();
        let m_mon <- monitors.readRsp();
        // Must be valid since it was on a list
        let mon = validValue(m_mon);

        // Remove current entry if tag matches
        if (mon.tag == tag)
        begin
            debugLog.record($format("    Lock Mgr:  WB invalidates monitor CTX %0d, tag 0x%x", monCtx, tag));
            stdio.printf(msgWBInval, list(zeroExtend(monCtx), zeroExtend(tag)));

            state <= FUNCP_MEMSTATE_LOCK_CLEAR_MON;
            monitors.write(monCtx, tagged Invalid);
            curMon <= mon;

            clearMonResumeMonIdx <= mon.next;
            if (mon.next matches tagged Valid .next)
            begin
                // Chain continues.  Come back here.
                clearMonResumeState <= state;
                monCtx <= next;
            end
            else
            begin
                // End of chain.
                clearMonResumeState <= FUNCP_MEMSTATE_LOCK_READY;
            end
        end
        else if (mon.next matches tagged Valid .next)
        begin
            // List continues.  Check the next entry.
            monitors.readReq(next);
            monCtx <= next;
        end
        else
        begin
            // End of chain.  Done.
            curQ.deq();
            state <= FUNCP_MEMSTATE_LOCK_READY;
        end
    endrule


    //
    // handleTryLock --
    //   Store lock request has arrived.  Is the monitor still valid?
    //
    rule handleTryLock (state == FUNCP_MEMSTATE_LOCK_NEW_REQ &&
                        tpl_1(curQ.first) == FUNCP_MEMSTATE_LOCK_REQ_TRY_LOCK);
        match {.req, .ctx, .tag} = curQ.first();
        let cur_head <- hashTable.readRsp();
        let ctx_mon <- monitors.readRsp();

        Bool mon_valid = False;
        if (ctx_mon matches tagged Valid .mon)
        begin
            // Success if tag matches
            mon_valid = (mon.tag == tag);

            tryLockRspQ.enq(mon_valid);

            // Remove the monitor.  We could choose to remove all other monitors
            // for the address at the same time, but that would add logic and
            // it is not necessary, since the caller will also check the store
            // buffer for matches before returning a successful lock.  Thus,
            // this lock manager code may hand out more than one winner but
            // the store buffer will permit only one true winner.
            state <= FUNCP_MEMSTATE_LOCK_CLEAR_MON;
            monitors.write(ctx, tagged Invalid);
            curMon <= mon;
            clearMonResumeMonIdx <= tagged Invalid;
            clearMonResumeState <= FUNCP_MEMSTATE_LOCK_READY;
        end
        else
        begin
            // Failure: context no longer has a monitor.
            curQ.deq();
            tryLockRspQ.enq(False);
            state <= FUNCP_MEMSTATE_LOCK_READY;
        end

        if (mon_valid)
        begin
            debugLog.record($format("    Lock Mgr:  Got lock CTX %0d, tag 0x%x", ctx, tag));
            stdio.printf(msgGotLock, list(zeroExtend(ctx), zeroExtend(tag)));
        end
        else
        begin
            debugLog.record($format("    Lock Mgr:  Failed lock CTX %0d, tag 0x%x", ctx, tag));
            stdio.printf(msgFailedLock, list(zeroExtend(ctx), zeroExtend(tag)));
        end
    endrule


    //
    // removeMonitor --
    //   State machine for removing a monitor from a hash chain.
    //
    //   The monitor to remove is stored in curMon.  The state to set
    //   once done is stored in clearMonResumeState.
    //
    //   This rule assumes that the caller has already invalidated the
    //   monitor.
    //
    Reg#(Bit#(3)) removeState <- mkReg(0);

    rule removeMonitor (state == FUNCP_MEMSTATE_LOCK_CLEAR_MON);
        case (removeState)
            // Requests enter here.
            0:
            begin
                if (curMon.prev matches tagged Valid .prev)
                begin
                    // Entry is part of a chain.  Get the previous entry.
                    monitors.readReq(prev);
                    removeState <= 1;
                end
                else
                begin
                    // Monitor to remove was the head of the chain.  Make
                    // next entry the new head.
                    hashTable.write(truncate(curMon.tag), curMon.next);
                    removeState <= 2;
                end
            end

            // Previous entry is another monitor.  Update it to point around
            // the monitor being removed.
            1:
            begin
                let m_prev_mon <- monitors.readRsp();
                let prev_mon = validValue(m_prev_mon);

                prev_mon.next = curMon.next;
                monitors.write(validValue(curMon.prev), tagged Valid prev_mon);

                removeState <= 2;
            end

            // Is the next entry valid?  If so, the next monitor in the chain
            // has to be updated.
            2:
            begin
                if (curMon.next matches tagged Valid .next)
                begin
                    // Read the next monitor in the chain to update its prev
                    // pointer.
                    monitors.readReq(next);
                    removeState <= 3;
                end
                else
                begin
                    removeState <= 4;
                end
            end

            // Make next monitor point to prev of the one being removed.
            3:
            begin
                let m_next_mon <- monitors.readRsp();
                let next_mon = validValue(m_next_mon);

                next_mon.prev = curMon.prev;
                monitors.write(validValue(curMon.next), tagged Valid next_mon);

                removeState <= 4;
            end

            // Done
            4:
            begin
                match {.req, .ctx, .tag} = curQ.first();

                // Resume to ready state implies done with processing.
                if (clearMonResumeState == FUNCP_MEMSTATE_LOCK_READY)
                begin
                    curQ.deq();
                end

                // Re-read some state, as needed, and resume the original flow.
                if (clearMonResumeState == FUNCP_MEMSTATE_LOCK_NEW_REQ)
                begin
                    hashTable.readReq(truncate(tag));
                end
                if (clearMonResumeMonIdx matches tagged Valid .idx)
                begin
                    monitors.readReq(idx);
                end

                state <= clearMonResumeState;
                removeState <= 0;
            end
        endcase
    endrule


    //
    // Start watching a memory location.  Only a relatively small number of
    // locations can be monitored at a time.  We seek here to monitor at least
    // as many locations as a reasonable timing model might expect.
    // New requests push out old (even valid) ones.
    //
    method Action monStart(CONTEXT_ID ctx, MEM_ADDRESS addr);
        newReqQ.enq(tuple3(FUNCP_MEMSTATE_LOCK_REQ_MON_START, ctx, addr));
    endmethod

    method Action noteWriteback(CONTEXT_ID ctx, MEM_ADDRESS addr);
        newReqQ.enq(tuple3(FUNCP_MEMSTATE_LOCK_REQ_NOTE_WRITE, ctx, addr));
    endmethod

    method Action tryLockReq(CONTEXT_ID ctx, MEM_ADDRESS addr);
        newReqQ.enq(tuple3(FUNCP_MEMSTATE_LOCK_REQ_TRY_LOCK, ctx, addr));
    endmethod

    method ActionValue#(Bool) tryLockResp();
        let r = tryLockRspQ.first();
        tryLockRspQ.deq();

        return r;
    endmethod
endmodule

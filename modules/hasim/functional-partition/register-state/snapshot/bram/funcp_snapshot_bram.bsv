`include "hasim_common.bsh"
`include "hasim_isa.bsh"
`include "fpga_components.bsh"

import Vector::*;
import RegFile::*;

interface FUNCP_SNAPSHOT;

    method Action makeSnapshot(TOKEN_INDEX tokIndex, Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) newMap);
    method Action invalSnapshot(TOKEN_INDEX tokIndex);
    method Action requestSnapshot(FUNCP_SNAPSHOT_INDEX tokIndex);
    method Maybe#(FUNCP_SNAPSHOT_INDEX) hasSnapshot(TOKEN_INDEX tokIndex);
    method ActionValue#(Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) returnSnapshot();

endinterface

typedef Bit#(TLog#(`REGSTATE_NUM_SNAPSHOTS)) FUNCP_SNAPSHOT_INDEX;

module mkFUNCP_Snapshot
    //interface:
        (FUNCP_SNAPSHOT);

    Reg#(Vector#(NUM_TOKENS, Bool))             snapValids <- mkReg(replicate(False));

    // The IDs tell us which snapshot is in a given location.
    RegFile#(FUNCP_SNAPSHOT_INDEX, Maybe#(TOKEN_INDEX)) snapIDs <- mkRegFileFullInitialized(tagged Invalid);

    // The token table tells us the most recent snapshot associated with each token.
    RegFile#(TOKEN_INDEX, FUNCP_SNAPSHOT_INDEX) tokSnaps <- mkRegFileFullInitialized(0);
    
    // The next pointer points to the next location where we should write a snapshot.
    // (Possibly overwriting an old snapshot, which is okay, since we pull down snapValid for that token.)
    Reg#(FUNCP_SNAPSHOT_INDEX)                       snapNext <- mkReg(0);

    // The actual snapshots of the entire maptable.
    BRAM#(FUNCP_SNAPSHOT_INDEX, Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) snaps <- mkBRAM();

    method Action makeSnapshot(TOKEN_INDEX tokIndex, Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) newMap);

        let new_valids = snapValids;

        if (snapIDs.sub(snapNext) matches tagged Valid .old_tok)
        begin
           new_valids[old_tok] = False;
        end

        new_valids[tokIndex] = True;
        snapValids <= new_valids;
        tokSnaps.upd(tokIndex, snapNext);
        snapIDs.upd(snapNext, tagged Valid tokIndex);
        snaps.write(snapNext, newMap);
        snapNext <= snapNext + 1;

    endmethod

    method Action invalSnapshot(TOKEN_INDEX tokIndex);
        snapValids[tokIndex] <= False;
    endmethod

    method Maybe#(FUNCP_SNAPSHOT_INDEX) hasSnapshot(TOKEN_INDEX tokIndex);

        return (snapValids[tokIndex]) ? tagged Valid tokSnaps.sub(tokIndex) : tagged Invalid;

    endmethod
    
    method Action requestSnapshot(FUNCP_SNAPSHOT_INDEX idx);
    
        // Retrieve the snapshots.
        snaps.readReq(idx);

    endmethod

    method ActionValue#(Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) returnSnapshot();

        let snp_map <- snaps.readRsp();
        return snp_map;

    endmethod

endmodule

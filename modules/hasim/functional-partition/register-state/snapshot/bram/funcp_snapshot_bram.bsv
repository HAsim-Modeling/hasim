`include "hasim_common.bsh"
`include "hasim_isa.bsh"
`include "fpga_components.bsh"

import Vector::*;

interface Snapshot#(numeric type rname_SZ);

    method Action makeSnapshot(TOKEN_INDEX tokIndex, Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX) newMap);
    method Action requestSnapshot(FUNCP_SNAPSHOT_INDEX tokIndex);
    method Maybe#(FUNCP_SNAPSHOT_INDEX) hasSnapshot(TOKEN_INDEX tokIndex);
    method ActionValue#(Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX)) returnSnapshot();

endinterface

typedef Bit#(TLog#(`REGSTATE_NUM_SNAPSHOTS)) FUNCP_SNAPSHOT_INDEX;

module mkSnapshot
    //interface:
        (Snapshot#(rname_SZ))
    provisos
        (Bits#(TOKEN_INDEX, idx_SZ),
         Bits#(ISA_REG_INDEX, rname_SZ),
         Bits#(FUNCP_SNAPSHOT_INDEX, snapshotptr_SZ));

    // The valid bits tell us which location contains a valid snapshot.
    Reg#(Vector#(TExp#(idx_SZ), Bool))             snapValids <- mkReg(replicate(False));

    // The IDs tell us which snapshot is in a given location.
    Reg#(Vector#(TExp#(snapshotptr_SZ), TOKEN_INDEX)) snapIDs <- mkRegU();
    
    // The next pointer points to the next location where we should write a snapshot.
    // (Possibly overwriting an old snapshot, which is okay.)
    Reg#(FUNCP_SNAPSHOT_INDEX)                       snapNext <- mkReg(0);

    // The actual snapshots of the entire maptable.
    BRAM#(snapshotptr_SZ, Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX)) snaps <- mkBramInitialized(?);

    method Action makeSnapshot(TOKEN_INDEX tokIndex, Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX) newMap);
        snapValids[tokIndex] <= True;
        snapIDs[snapNext] <= tokIndex;
        snaps.write(snapNext, newMap);
        snapNext <= snapNext + 1;
    endmethod

    method Maybe#(FUNCP_SNAPSHOT_INDEX) hasSnapshot(TOKEN_INDEX tokIndex);

        Bool found = False;
        FUNCP_SNAPSHOT_INDEX idx = snapNext;

        if (snapValids[tokIndex]) // There's a chance we have a snapshot
        begin

            for (Integer x = 0; x < valueof(TExp#(snapshotptr_SZ)); x = x + 1)
            begin
                // We look the list at an offset from the oldest entry.
                let cur = snapNext + fromInteger(x);

                // If the entry we examine is of the appropriate token, we've found a candidate!
                match {.new_idx, .new_found} = (snapIDs[cur] == tokIndex) ? tuple2(cur, True) : tuple2(idx, found);
                found = new_found;
                idx = new_idx;
            end
        end

        return found ? tagged Valid idx : tagged Invalid;

    endmethod
    
    method Action requestSnapshot(FUNCP_SNAPSHOT_INDEX idx);
    
        // Retrieve the snapshots.
        snaps.readReq(idx);

    endmethod

    method ActionValue#(Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX)) returnSnapshot();

        let snp_map <- snaps.readResp();
        return snp_map;

    endmethod

endmodule

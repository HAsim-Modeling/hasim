`include "hasim_common.bsh"
`include "hasim_isa.bsh"
`include "fpga_components.bsh"

import Vector::*;

interface Snapshot#(numeric type rname_SZ);
    method Action makeSnapshot(TOKEN_INDEX tokIndex, Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX) newMap, FUNCP_PHYSICAL_REG_INDEX currPhysReg);
    method ActionValue#(Bool) hasSnapshot(TOKEN_INDEX tokIndex);
    method ActionValue#(Tuple2#(Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX), FUNCP_PHYSICAL_REG_INDEX)) returnSnapshot();
endinterface

typedef Bit#(TLog#(`REGSTATE_NUM_SNAPSHOTS)) FUNCP_SNAPSHOT_INDEX;

module mkSnapshot#(File debugLog, Bit#(32) fpgaCC)
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
    BRAM#(FUNCP_SNAPSHOT_INDEX, Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX)) snaps <- mkBRAM_Full();

    // An additional snapshot of the location of the freelist.
    BRAM#(FUNCP_SNAPSHOT_INDEX, FUNCP_PHYSICAL_REG_INDEX)                         snapsFL <- mkBRAM_Full();

    method Action makeSnapshot(TOKEN_INDEX tokIndex, Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX) newMap, FUNCP_PHYSICAL_REG_INDEX currPhysReg);
        $fdisplay(debugLog, "[%d]: TOKEN %0d: Snapshot: Making Snapshot (Number %0d).", fpgaCC, tokIndex, snapNext);
        snapValids[tokIndex] <= True;
        snapIDs[snapNext] <= tokIndex;
        snaps.write(snapNext, newMap);
        snapsFL.write(snapNext, currPhysReg);
        snapNext <= snapNext + 1;
    endmethod

    method ActionValue#(Bool) hasSnapshot(TOKEN_INDEX tokIndex);
        Bool found = False;
        if (snapValids[tokIndex]) // There's a chance we have a snapshot
        begin
            $fwrite(debugLog, "[%d]: Snapshot: Potential Fast Rewind", fpgaCC);

            FUNCP_SNAPSHOT_INDEX idx = snapNext;

            for (Integer x = 0; x < valueof(TExp#(snapshotptr_SZ)); x = x + 1)
            begin
                // We look the list at an offset from the oldest entry.
                let cur = snapNext + fromInteger(x);

                // If the entry we examine is of the appropriate token, we've found a candidate!
                match {.new_idx, .new_found} = (snapIDs[cur] == tokIndex) ? tuple2(cur, True) : tuple2(idx, found);
                found = new_found;
                idx = new_idx;
            end

            // Alright did we find anything?
            if (found)
            begin 
                // Log our success!
                $fwrite(debugLog, "[%d]: Snapshot: Fast Rewind confirmed with Snapshot %0d", fpgaCC, idx);

                // Retrieve the snapshots.
                snaps.read_req(idx);
                snapsFL.read_req(idx);
            end
        end
        return found;
    endmethod

    method ActionValue#(Tuple2#(Vector#(TExp#(rname_SZ), FUNCP_PHYSICAL_REG_INDEX), FUNCP_PHYSICAL_REG_INDEX)) returnSnapshot();
        let snp_map <- snaps.read_resp();
        let snp_fl  <- snapsFL.read_resp();

        return tuple2(snp_map, snp_fl);
    endmethod
endmodule

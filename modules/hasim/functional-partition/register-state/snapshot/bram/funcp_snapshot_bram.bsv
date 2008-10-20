`include "hasim_common.bsh"
`include "hasim_isa.bsh"
`include "fpga_components.bsh"

import Vector::*;

interface FUNCP_SNAPSHOT;

    method ActionValue#(FUNCP_SNAPSHOT_INDEX) makeSnapshot(TOKEN_INDEX tokIndex, Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) newMap);
    method Action invalSnapshot(TOKEN_INDEX tokIndex);
    method Action requestSnapshot(FUNCP_SNAPSHOT_INDEX tokIndex);
    method Maybe#(FUNCP_SNAPSHOT_INDEX) hasSnapshot(TOKEN_INDEX tokIndex);
    method ActionValue#(Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) returnSnapshot();

endinterface

typedef Bit#(TLog#(`REGSTATE_NUM_SNAPSHOTS)) FUNCP_SNAPSHOT_INDEX;

module mkFUNCP_Snapshot
    //interface:
        (FUNCP_SNAPSHOT);

    // The token table tells us the most recent snapshot associated with each token.
    LUTRAM#(TOKEN_INDEX, Maybe#(FUNCP_SNAPSHOT_INDEX)) tokSnaps <- mkLiveTokenLUTRAM(tagged Invalid);
    
    // The snapshot table tells us the most recent token associated with a snapshot.
    // It is the opposite of tokSnaps above.  In order to be valid snapToks must
    // point to the token and tokSnaps must point back to the snapshot.  This
    // lets us deal with running out of snapshot entries gracefully (resorting
    // to slow rewind) while not having to invalidate other snapshots during rewind.
    LUTRAM#(FUNCP_SNAPSHOT_INDEX, Maybe#(TOKEN_INDEX)) snapToks <- mkLUTRAM(tagged Invalid);

    // The next pointer points to the next location where we should write a snapshot.
    Reg#(FUNCP_SNAPSHOT_INDEX)                       snapNext <- mkReg(0);

    // The actual snapshots of the entire maptable.
    BRAM#(FUNCP_SNAPSHOT_INDEX, Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) snaps <- mkBRAM();

    //
    // makeSnapshot --
    //     Allocates a new snapshot.  If all snapshot slots are full it will
    //     overwrite and invalidate an old one.  The register state manager
    //     will then resort to slow rewind if necessary.
    //
    method ActionValue#(FUNCP_SNAPSHOT_INDEX) makeSnapshot(TOKEN_INDEX tokIndex, Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) newMap);

        let snap_idx = snapNext;

        snapToks.upd(snap_idx, tagged Valid tokIndex);
        tokSnaps.upd(tokIndex, tagged Valid snap_idx);
        snaps.write(snap_idx, newMap);
        snapNext <= snapNext + 1;
    
        return snap_idx;

    endmethod

    method Action invalSnapshot(TOKEN_INDEX tokIndex);
        tokSnaps.upd(tokIndex, tagged Invalid);
    endmethod

    method Maybe#(FUNCP_SNAPSHOT_INDEX) hasSnapshot(TOKEN_INDEX tokIndex);

        //
        // For a snapshot to be valid the tokSnaps and snapToks pointers must
        // point to each other.
        //
        if (tokSnaps.sub(tokIndex) matches tagged Valid .tok_snap &&&
            snapToks.sub(tok_snap) matches tagged Valid .snap_tok &&&
            snap_tok == tokIndex)
        begin
            return tagged Valid tok_snap;
        end
        else
        begin
            return tagged Invalid;
        end

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

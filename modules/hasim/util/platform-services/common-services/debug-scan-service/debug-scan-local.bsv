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


import FIFO::*;
import Counter::*;
import Vector::*;

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/debug_scan_device.bsh"

`include "asim/dict/RINGID.bsh"
`include "asim/dict/DEBUG_SCAN.bsh"

//
// Debug scan nodes accept any size data to scan out by breaking the data
// into chunks.  The mkDebugScanNode() module takes a wire as input, with
// the expectation that the wire will have no predicate (e.g. a mkBypassWire)
// and that it will have a meaningful value on any cycle.
//

//
// Debug scan nodes have no methods.
//
interface DEBUG_SCAN#(type t_DEBUG_DATA);
endinterface: DEBUG_SCAN


typedef 8 DEBUG_SCAN_VALUE_SZ;
typedef Bit#(DEBUG_SCAN_VALUE_SZ) DEBUG_SCAN_VALUE;

typedef union tagged
{
    void DS_DUMP;
    struct {DEBUG_SCAN_DICT_TYPE id; DEBUG_SCAN_VALUE value;} DS_VAL;
}
DEBUG_SCAN_DATA
    deriving (Eq, Bits);


typedef enum
{
    DS_IDLE,
    DS_DUMPING
}
DEBUG_SCAN_STATE
    deriving (Eq, Bits);


//
// mkDebugScanNode --
//
//   Scan out the data coming in on wire debugValue.  To avoid deadlocks
//   during scan the wire should have no predicates (e.g. mkBypassWire).
// 
module [CONNECTED_MODULE] mkDebugScanNode#(DEBUG_SCAN_DICT_TYPE myID,
                                           Wire#(t_DEBUG_DATA) debugValue)
    // interface:
    (DEBUG_SCAN#(t_DEBUG_DATA))
    provisos (Bits#(t_DEBUG_DATA, t_DEBUG_DATA_SZ),
              Div#(t_DEBUG_DATA_SZ, DEBUG_SCAN_VALUE_SZ, n_ENTRIES));

    Connection_Chain#(DEBUG_SCAN_DATA) chain <- mkConnection_Chain(`RINGID_DEBUG_SCAN);

    Reg#(DEBUG_SCAN_STATE) state <- mkReg(DS_IDLE);

    Reg#(Vector#(n_ENTRIES, DEBUG_SCAN_VALUE)) dbgVal <- mkRegU();
    Reg#(Bit#(TLog#(n_ENTRIES))) dbgValIdx <- mkRegU();


    //
    // sendDumpData --
    //     Forward dump data and token around the ring.
    //
    rule sendDumpData (state == DS_DUMPING);
        if ((valueOf(n_ENTRIES) == 1) || (dbgValIdx == 0))
        begin
            // Done with this node's data
            chain.sendToNext(tagged DS_DUMP);
            state <= DS_IDLE;
        end
        else
        begin
            // More data remains for this node
            let idx = dbgValIdx - 1;
            dbgValIdx <= idx;
            chain.sendToNext(tagged DS_VAL { id: myID, value: dbgVal[idx] });
        end
    endrule


    //
    // receiveCmd --
    //     Receive a command on the ring.
    //
    (* conservative_implicit_conditions *)
    rule receiveCmd (state == DS_IDLE);
        let ds <- chain.recvFromPrev();

        case (ds) matches 
            tagged DS_DUMP:
            begin
                // Capture the dump value as an array of scan-chain-sized data.
                Vector#(n_ENTRIES, DEBUG_SCAN_VALUE) val = unpack(zeroExtendNP(pack(debugValue)));
                dbgVal <= val;
                dbgValIdx <= fromInteger(valueOf(n_ENTRIES) - 1);

                // Send the first chunk of data on the chain
                chain.sendToNext(tagged DS_VAL { id: myID, value: val[valueOf(n_ENTRIES) - 1] });
                state <= DS_DUMPING;
            end

            default: chain.sendToNext(ds);
        endcase
    endrule
endmodule

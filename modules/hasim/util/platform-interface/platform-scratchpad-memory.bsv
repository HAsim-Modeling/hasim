//
// Copyright (C) 2009 Intel Corporation
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

//
// Interfaces to scratchpad memory.
//

`include "asim/provides/libfpga_bsv_base.bsh"
`include "asim/provides/scratchpad_memory.bsh"

`include "asim/dict/VDEV.bsh"
`ifndef VDEV_SCRATCH__BASE
`define VDEV_SCRATCH__BASE 0
`endif

//
// Scratchpad requests (either a load or a store).
//
typedef union tagged 
{
    SCRATCHPAD_MEM_ADDRESS SCRATCHPAD_MEM_READ;
    struct {SCRATCHPAD_MEM_ADDRESS addr; SCRATCHPAD_MEM_VALUE val;} SCRATCHPAD_MEM_WRITE;
}
SCRATCHPAD_MEM_REQUEST
    deriving (Eq, Bits);


//
// Construct the name of the soft connection to a scratchpad memory port.
// Ports are created dynamically using dictionaries in the VDEV.SCRATCH
// name space.
//
function String scratchPortName(Integer n) = "vdev_memory_" + integerToString(n - `VDEV_SCRATCH__BASE);
    
    
module [HASIM_MODULE] mkDirectScratchpad#(Integer scratchpadID)
    // interface:
    (MEMORY_IFC#(SCRATCHPAD_MEM_ADDRESS, SCRATCHPAD_MEM_VALUE));
    
    Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_MEM_VALUE) link_memory <- mkConnection_Client(scratchPortName(scratchpadID));

    method Action readReq(SCRATCHPAD_MEM_ADDRESS addr);
        link_memory.makeReq(tagged SCRATCHPAD_MEM_READ addr);
    endmethod

    method ActionValue#(SCRATCHPAD_MEM_VALUE) readRsp();
        let v = link_memory.getResp();
        link_memory.deq();
    
        return v;
    endmethod

    method Action write(SCRATCHPAD_MEM_ADDRESS addr, SCRATCHPAD_MEM_VALUE val);
        link_memory.makeReq(tagged SCRATCHPAD_MEM_WRITE { addr: addr, val: val });
    endmethod
endmodule

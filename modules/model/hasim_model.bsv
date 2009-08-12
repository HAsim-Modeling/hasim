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

import Clocks::*;

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/hasim_controller.bsh"
`include "asim/provides/software_system.bsh"
`include "asim/provides/hardware_system.bsh"
`include "asim/provides/virtual_platform.bsh"
`include "asim/provides/low_level_platform_interface.bsh"
`include "asim/provides/platform_interface.bsh"
`include "asim/provides/physical_platform.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/clocks_device.bsh"

module [HASIM_MODULE] mkModel (TOP_LEVEL_WIRES);
    
    // The Model is instantiated inside a NULL (noClock) clock domain,
    // so first instantiate the Platform Interface and get a clock and
    // reset from it.

    // name must be pi_llpint --- explain!!!
    VIRTUAL_PLATFORM vp <- mkVirtualPlatform();

    Clock clk = vp.llpint.physicalDrivers.clocksDriver.clock;
    Reset rst = vp.llpint.physicalDrivers.clocksDriver.reset;
    
    let pi <- mkPlatformInterface(vp, clocked_by clk, reset_by rst);

    // instantiate system and controller
    let system     <- mkSystem    (clocked_by clk, reset_by rst);
    let controller <- mkController(clocked_by clk, reset_by rst);
    
    // return top level wires interface
    return vp.llpint.topLevelWires;

endmodule

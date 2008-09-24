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
import hasim_common::*;
import soft_connections::*;
import rrr::*;

`include "streams.bsh"

module [HASIM_MODULE] mkConnectionTerminus();

    if (`TERM_FPGA_SWITCHES == 1)
        Connection_Receive#(FRONTP_SWITCHES) link_switches <- mkConnection_Receive("fpga_switches");

    if (`TERM_FPGA_BUTTONS == 1)
        Connection_Receive#(ButtonInfo) link_buttons <- mkConnection_Receive("fpga_buttons");

    if (`TERM_FPGA_LEDS == 1)
        Connection_Send#(FRONTP_MASKED_LEDS) link_leds <- mkConnection_Send("fpga_leds");

    if (`TERM_VDEV_MEMORY == 1)
        Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_MEM_VALUE) link_memory <- mkConnection_Client("vdev_memory");

    if (`TERM_VDEV_MEMORY_INVALIDATE == 1)
        Connection_Receive#(SCRATCHPAD_MEM_ADDRESS) link_memory_inval <- mkConnection_Receive("vdev_memory_invalidate");

    if (`TERM_VDEV_STREAMS == 1)
        Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

endmodule

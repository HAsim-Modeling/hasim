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


module [CONNECTED_MODULE] mkDebugScanService#(DEBUG_SCAN_DEVICE debugScan)
    // interface:
    ();

    // ****** State Elements ******

    // Communication link to the Stats themselves
    Connection_Chain#(DEBUG_SCAN_DATA) chain <- mkConnection_Chain(`RINGID_DEBUG_SCAN);
    

    // ****** Rules ******
  
    //
    // processReq --
    //     Receive a command requesting a scan dump.
    //
    rule processReq (True);
        let cmd <- debugScan.getCmd();

        // There is only one command:  start a scan
        chain.sendToNext(tagged DS_DUMP);
    endrule


    //
    // processResp --
    //
    // Process a response from an individual scan node.
    //  
    rule processResp (True);
        let ds <- chain.recvFromPrev();

        case (ds) matches
            // A value to dump
            tagged DS_VAL .info:
            begin
                debugScan.scanValue(info.id, info.value);
            end

            // Command came all the way around the loop.  Done dumping.
            tagged DS_DUMP:
            begin
                debugScan.finishCmd(DEBUG_SCAN_CMD_DOSCAN);
            end
        endcase
    endrule
endmodule

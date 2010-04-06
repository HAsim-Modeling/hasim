//
// Copyright (C) 2009 Massachusetts Institute of Technology
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

module [CONNECTED_MODULE] mkStatsService#(STATS statsDevice)
    // interface:
    ();

    // ****** State Elements ******

    // Communication link to the Stats themselves
    Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);

    // ****** Rules ******

    //
    // processReq --
    //     Receive a command from the statistics device and begin an action
    //     on the statistics ring.
    //
    rule processReq (True);
        let cmd <- statsDevice.getCmd();

        case (cmd)
            STATS_CMD_GETLENGTHS:
                chain.sendToNext(ST_GET_LENGTH);

            STATS_CMD_DUMP:
                chain.sendToNext(ST_DUMP);

            STATS_CMD_RESET:
                chain.sendToNext(ST_RESET);

            STATS_CMD_TOGGLE:
                chain.sendToNext(ST_TOGGLE);
        endcase
    endrule


    //
    // processResp --
    //
    //     Process responses returning on the statistics ring.
    //
    rule processResp (True);
        let st <- chain.recvFromPrev();

        case (st) matches
            tagged ST_VAL .stinfo: // A stat to dump
                statsDevice.reportStat(stinfo.statID, stinfo.index, stinfo.value);

            tagged ST_LENGTH .stinfo: // A stat vector length
                statsDevice.setVectorLength(stinfo.statID,
                                            stinfo.length,
                                            stinfo.buildArray);

            tagged ST_GET_LENGTH:  // We're done getting lengths
                statsDevice.finishCmd(STATS_CMD_GETLENGTHS);

            tagged ST_DUMP:  // We're done dumping
                statsDevice.finishCmd(STATS_CMD_DUMP);

            tagged ST_RESET:  // We're done reseting
                statsDevice.finishCmd(STATS_CMD_RESET);

            tagged ST_TOGGLE:  // We're done toggling
                statsDevice.finishCmd(STATS_CMD_TOGGLE);
        endcase
    endrule
endmodule

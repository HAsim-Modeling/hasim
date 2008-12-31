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

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// Commands.bsv                                                              //
//                                                                           //
// Non-ISA-specific datatypes required by Controller model                   //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

// Project imports

`include "asim/provides/hasim_common.bsh"


typedef union tagged
{
    void         COM_RunProgram;     // Begin running, allowed to slip
    void         COM_Synchronize;    // Start synchronizing the system
    void         COM_StartSyncQuery; // Start checking if you're synchronized
    void         COM_SyncQuery;      // Is the system synchronized yet?
    void         COM_Step;           // Run exactly one model CC.
    CONTEXT_ID   COM_EnableContext;  // Enable context
    CONTEXT_ID   COM_DisableContext; // Disable context
}
CONTROLLER_COMMAND 
    deriving (Eq, Bits);

                
typedef union tagged
{
    Bool RESP_DoneRunning; // Bool is run passed
    void RESP_Balanced;    // Response to query
    void RESP_UnBalanced;  // Response to query
}
CONTROLLER_RESPONSE
    deriving (Eq, Bits);

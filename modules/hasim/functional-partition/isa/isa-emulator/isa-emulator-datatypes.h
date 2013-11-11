//
// Copyright (C) 2013 Intel Corporation
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

#ifndef _ISA_EMULATOR_DATATYPES_
#define _ISA_EMULATOR_DATATYPES_

#include "awb/provides/hasim_isa.h"

typedef UINT16 REG_NAME_RRR;
typedef UINT32 INSTRUCTION_RRR;
typedef UINT32 ISA_INSTRUCTION;

#define AppendREG_NAME_RRR AppendUINT16
#define AppendINSTRUCTION_RRR AppendUINT32
#define AppendISA_INSTRUCTION AppendUINT32

#define ExtractREG_NAME_RRR ExtractUINT16
#define ExtractINSTRUCTION_RRR ExtractUINT32
#define ExtractISA_INSTRUCTION ExtractUINT32

#endif

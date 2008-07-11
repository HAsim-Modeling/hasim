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

#ifndef _STATS_CONTROLLER_
#define _STATS_CONTROLLER_

#include <bitset>

#include "platforms-module.h"
#include "asim/provides/rrr.h"

#include "asim/dict/STATS.h"

// this module handles gathering statistics. 
// Eventually this will interact with standard tools.

typedef class STATS_CONTROLLER_CLASS* STATS_CONTROLLER;
class STATS_CONTROLLER_CLASS: public RRR_SERVICE_CLASS,
                              public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static STATS_CONTROLLER_CLASS instance;

    // Running total of statistics as they are dumped incrementally
    UINT64 statValues[STATS_DICT_ENTRIES];

    // Check that each stat appears at most once
    bitset<STATS_DICT_ENTRIES> sawStat;

  public:
    STATS_CONTROLLER_CLASS();
    ~STATS_CONTROLLER_CLASS();

    void EmitFile();

    // static methods
    static STATS_CONTROLLER GetInstance() { return &instance; }

    // required RRR service methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    UMF_MESSAGE Request(UMF_MESSAGE);
    void Poll();
};


void StatsEmitFile();

#endif

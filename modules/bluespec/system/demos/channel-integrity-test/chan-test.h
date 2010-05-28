//
// Copyright (C) 2010 Intel Corporation
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

#ifndef __CHANTEST_SYSTEM__
#define __CHANTEST_SYSTEM__

#include "asim/provides/command_switches.h"
#include "asim/provides/virtual_platform.h"
#include "asim/rrr/client_stub_CHANTEST.h"
#include "asim/restricted/chan-test-server.h"

// Channel integrity test system

class TEST_ITERATIONS_SWITCH_CLASS : public COMMAND_SWITCH_INT_CLASS
{
  private:
    UINT32 testIter;

  public:
    ~TEST_ITERATIONS_SWITCH_CLASS() {};
    TEST_ITERATIONS_SWITCH_CLASS() :
        COMMAND_SWITCH_INT_CLASS("test-iterations"),
        testIter(10000)
    {};

    void ProcessSwitchInt(int arg) { testIter = arg; };
    bool ShowSwitch(char *buff)
    {
        strcpy(buff, "[--test-iterations=<n>] Channel test iterations");
        return true;
    };

    int Value(void) const { return testIter; }
};


typedef class HYBRID_APPLICATION_CLASS* HYBRID_APPLICATION;
class HYBRID_APPLICATION_CLASS
{
  private:

    // client stub
    CHANTEST_CLIENT_STUB clientStub;

    // Arguments
    TEST_ITERATIONS_SWITCH_CLASS testIterSwitch;

    // Server stub
    CHANTEST_SERVER server;

    void SendH2FMsg();

  public:

    HYBRID_APPLICATION_CLASS(VIRTUAL_PLATFORM vp);
    ~HYBRID_APPLICATION_CLASS();

    // main
    void Init();
    void Main();
};

#endif

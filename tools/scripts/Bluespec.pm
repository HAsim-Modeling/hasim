
# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

#
# Author:  Martha Mercaldi
#

package Bluespec;

use warnings;
use strict;

############################################################
# verilog_lib_files:
sub verilog_lib_files {
    my @files = ("BypassWire.v",
		 "ClockDiv.v", 
		 "ClockGen.v", 
		 "ClockInverter.v", 
		 "ClockMux.v", 
		 "ClockSelect.v", 
		 "ConvertFromZ.v", 
		 "ConvertToZ.v", 
		 "Counter.v", 
		 "DualPortRam.v", 
		 "FIFO1.v", 
		 "FIFO10.v", 
		 "FIFO2.v", 
		 "FIFO20.v", 
		 "FIFOL1.v", 
		 "FIFOL10.v", 
		 "FIFOL2.v", 
		 "FIFOL20.v", 
		 "FIFOLevel.v", 
		 "Fork.v", 
		 "GatedClock.v", 
		 "InitialReset.v", 
		 "MakeClock.v", 
		 "McpRegUN.v", 
		 "RWire.v", 
		 "RWire0.v", 
		 "RegA.v", 
		 "RegFile.v", 
		 "RegFileLoad.v", 
		 "RegN.v", 
		 "RegTwoA.v", 
		 "RegTwoN.v", 
		 "RegTwoUN.v", 
		 "RegUN.v", 
		 "ResolveZ.v", 
		 "SizedFIFO.v", 
		 "SizedFIFO0.v", 
		 "SizedFIFOL.v", 
		 "SizedFIFOL0.v", 
		 "SyncBit.v", 
		 "SyncBit1.v", 
		 "SyncBit15.v", 
		 "SyncFIFO.v", 
		 "SyncFIFOLevel.v", 
		 "SyncHandshake.v", 
		 "SyncPulse.v", 
		 "SyncRegister.v", 
		 "SyncReset.v", 
		 "SyncResetA.v", 
		 "SyncWire.v");
    return @files;
}

############################################################
# verilog_lib_dir:
sub verilog_lib_dir {
    if (! defined$ENV{'BLUESPEC_LIB'}) {
      Util::WARN_AND_DIE("BLUESPEC_LIB undefined in environment.");
    }

    return $ENV{'BLUESPEC_LIB'};
}

return 1;

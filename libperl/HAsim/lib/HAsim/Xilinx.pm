
# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

#
# Author:  Martha Mercaldi
#

package HAsim::Xilinx;

use Asim;

use HAsim::Build;
use HAsim::Util;

use warnings;
use strict;

############################################################
# package variables

our $tmp_xilinx_dir = ".xilinx";
our $part_num = "xc2vp30-7-ff896";

############################################################
# generate_files:
sub generate_files {
    my $model = shift;

    generate_v_file($model);
    generate_xst_file($model);
    generate_ucf_file($model);
    generate_ut_file($model);
    generate_download_file($model);

}

############################################################
# generate_xst_file:
sub generate_xst_file {
    my $model = shift;
    my $name = HAsim::Build::get_model_name($model);
    
    my $xst_file = HAsim::Util::path_append($model->build_dir(),$name . ".xst");

    CORE::open(FILE, "> $xst_file") || return undef;

    print FILE "run\n";
    print FILE "-ifmt VERILOG\n";
    my $verilog_lib = HAsim::Bluespec::verilog_lib_dir();
    print FILE "-vlgincdir " . $verilog_lib . "\n";
    print FILE "-ifn " . $name . ".v\n";
    print FILE "-ofn " . $name . "\n";
    print FILE "-ofmt NGC\n";
    print FILE "-p " . $part_num . "\n";
    print FILE "-top " . HAsim::Build::get_wrapper($model->modelroot()) . "\n";
    CORE::close(FILE);
}

############################################################
# generate_v_file:
sub generate_v_file {

    my $model = shift;
    my $name = HAsim::Build::get_model_name($model);
    
    my $v_file = HAsim::Util::path_append($model->build_dir(),$name . ".v");

    my $file;
    CORE::open($file, "> $v_file") || return undef;
    
    foreach my $v_file (HAsim::Bluespec::verilog_lib_files()) {
	print $file "`include \"" . $v_file . "\"\n";
    }

    __generate_v_file($model->modelroot(),"arch",$file);
    CORE::close($file);

    return 1;
}

############################################################
# __generate_v_file:
sub __generate_v_file {
    my $module = shift;
    my $parent_dir = shift;
    my $file = shift;

    my $my_dir = HAsim::Build::get_module_build_dir($module,$parent_dir);

    # recurse
    HAsim::Build::check_submodules_defined($module);
    foreach my $child ($module->submodules()) {
	__generate_v_file($child,$my_dir,$file);
    }

    if (HAsim::Build::is_synthesis_boundary($module)) {
	my $v_file = HAsim::Util::path_append($my_dir, HAsim::Build::get_wrapper($module) . ".v");
	print $file "`include \"$v_file\"\n";
    }


}

############################################################
# generate_ut_file:
sub generate_ut_file {
    my $model = shift;
    my $name = HAsim::Build::get_model_name($model);
    
    my $ut_file = HAsim::Util::path_append($model->build_dir(),$name . ".ut");

    CORE::open(FILE, "> $ut_file") || return undef;
    print FILE "-w\n";
    print FILE "-g DebugBitstream:No\n";
    print FILE "-g Binary:no\n";
    print FILE "-g CRC:Enable\n";
    print FILE "-g ConfigRate:4\n";
    print FILE "-g CclkPin:PullUp\n";
    print FILE "-g M0Pin:PullUp\n";
    print FILE "-g M1Pin:PullUp\n";
    print FILE "-g M2Pin:PullUp\n";
    print FILE "-g ProgPin:PullUp\n";
    print FILE "-g DonePin:PullUp\n";
    print FILE "-g PowerdownPin:PullUp\n";
    print FILE "-g TckPin:PullUp\n";
    print FILE "-g TdiPin:PullUp\n";
    print FILE "-g TdoPin:PullNone\n";
    print FILE "-g TmsPin:PullUp\n";
    print FILE "-g UnusedPin:PullDown\n";
    print FILE "-g UserID:0xFFFFFFFF\n";
    print FILE "-g DCMShutdown:Disable\n";
    print FILE "-g DisableBandgap:No\n";
    print FILE "-g DCIUpdateMode:AsRequired\n";
    print FILE "-g StartUpClk:CClk\n";
    print FILE "-g DONE_cycle:4\n";
    print FILE "-g GTS_cycle:5\n";
    print FILE "-g GWE_cycle:6\n";
    print FILE "-g LCK_cycle:NoWait\n";
    print FILE "-g Security:None\n";
    print FILE "-g DonePipe:No\n";
    print FILE "-g DriveDone:No\n";
    print FILE "-g Encrypt:No\n";
    CORE::close(FILE);
}

############################################################
# generate_ucf_file:
sub generate_ucf_file {
    my $model = shift;
    my $name = HAsim::Build::get_model_name($model);
    
    my $ucf_file = HAsim::Util::path_append($model->build_dir(),$name . ".ucf");

    CORE::open(FILE, "> $ucf_file") || return undef;

    print FILE "// CLOCK\n";
    print FILE "\n";
    print FILE "NET \"CLK\" LOC = \"AJ15\" | IOSTANDARD = LVCMOS25;\n";
    print FILE "\n";
    print FILE "// RST_N\n";
    print FILE "\n";
    print FILE "NET \"RST_N\" LOC=\"AD10\";\n";
    print FILE "// LEDS\n";
    print FILE "\n";
    print FILE "NET \"LED[0]\" LOC=\"AC4\" | IOSTANDARD=LVTTL | DRIVE=12 | SLEW=SLOW;\n";
    print FILE "NET \"LED[1]\" LOC=\"AC3\" | IOSTANDARD=LVTTL | DRIVE=12 | SLEW=SLOW;\n";
    print FILE "NET \"LED[2]\" LOC=\"AA6\" | IOSTANDARD=LVTTL | DRIVE=12 | SLEW=SLOW; \n";
    print FILE "NET \"LED[3]\" LOC=\"AA5\" | IOSTANDARD=LVTTL | DRIVE=12 | SLEW=SLOW;\n";
    print FILE "\n";
    print FILE "// SWITCHES\n";
    print FILE "\n";
    print FILE "NET \"SWITCH[0]\" LOC=\"AC11\" | IOSTANDARD=LVCMOS25;\n";
    print FILE "NET \"SWITCH[1]\" LOC=\"AD11\" | IOSTANDARD=LVCMOS25;\n";
    print FILE "NET \"SWITCH[2]\" LOC=\"AF8\" | IOSTANDARD=LVCMOS25;\n";
    print FILE "NET \"SWITCH[3]\" LOC=\"AF9\" | IOSTANDARD=LVCMOS25;\n";
    print FILE "\n";
    print FILE "// PUSHBUTTONS\n";
    print FILE "\n";
    print FILE "NET \"BUTTON_LEFT\" LOC=\"AH1\" | IOSTANDARD=LVTTL;\n";
    print FILE "NET \"BUTTON_RIGHT\" LOC=\"AH2\" | IOSTANDARD=LVTTL;\n";
    print FILE "NET \"BUTTON_UP\" LOC=\"AH4\" | IOSTANDARD=LVTTL;\n";
    print FILE "NET \"BUTTON_DOWN\" LOC=\"AG3\" | IOSTANDARD=LVTTL;\n";
    print FILE "NET \"BUTTON_CENTER\" LOC=\"AG5\" | IOSTANDARD=LVTTL;\n";

    CORE::close(FILE);
}


############################################################
# generate_download_file:
sub generate_download_file {
    my $model = shift;
    my $name = HAsim::Build::get_model_name($model);
    
    my $download_file = HAsim::Util::path_append($model->build_dir(),$name . ".download");

    CORE::open(FILE, "> $download_file") || return undef;

    print FILE "setMode -bscan\n";
    print FILE "setCable -p auto\n";
    print FILE "identify\n";    
    print FILE "assignfile -p 3 -file " . HAsim::Build::get_model_name($model) . "_par.bit\n";
    print FILE "program -p 3\n";
    print FILE "quit\n";
    
    CORE::close(FILE);
}


return 1;

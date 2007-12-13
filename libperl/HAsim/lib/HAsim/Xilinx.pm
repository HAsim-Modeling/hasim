
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
# find_all_files_with_suffix: search the model tree and 
#                               return all files which have 
#                               the given suffix.

sub find_all_files_with_suffix {
    my $module = shift;
    my $suffix = shift;
    
    my @res = ();
    
    foreach my $child ($module->submodules())
    {
      push(@res, find_all_files_with_suffix($child, $suffix));
    }
    
    my @files = ();
    #push(@files, $module->public());
    #push(@files, $module->private());
    push(@files, $module->sources("*", "*"));
    
    foreach my $file (@files)
    {
      if ($file =~ /(.*)$suffix$/)
      {
        push(@res, Asim::resolve(HAsim::Util::path_append($module->base_dir(), $file)));
      }
    }
    
    return @res;
}


############################################################
# package variables

our $tmp_xilinx_dir = ".xilinx";
our $xilinx_config_dir = "config";

############################################################
# generate_files:
sub generate_files {
    my $model = shift;

    my $config_dir = HAsim::Util::path_append($model->build_dir(), $xilinx_config_dir);
    system("mkdir -p $config_dir");

    generate_prj_file($model);
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
    my $final_xst_file = HAsim::Util::path_append($model->build_dir(),$xilinx_config_dir,$name . ".xst");

    CORE::open(XSTFILE, "> $final_xst_file") || return undef;

    my $replacements_r = HAsim::Util::empty_hash_ref();

    HAsim::Util::common_replacements($model, $replacements_r);
    
    HAsim::Util::hash_set($replacements_r,'@APM_NAME@',$name);
    HAsim::Util::hash_set($replacements_r,'@TMP_XILINX_DIR@',$tmp_xilinx_dir);

    my $bdir = bluespec_dir();
    HAsim::Util::hash_set($replacements_r,'@BLUESPECDIR@', $bdir);

    #Concatenate all found .xst files

    my @model_xst_files = find_all_files_with_suffix($model->modelroot(), ".xst");

    foreach my $model_xst_file (@model_xst_files)
    {
      HAsim::Templates::do_template_replacements($model_xst_file, *XSTFILE{IO}, $replacements_r);
    }
    
    CORE::close(XSTFILE);
}

############################################################
# generate_prj_file:
sub generate_prj_file {

    my $model = shift;
    my $name = HAsim::Build::get_model_name($model);
    
    my $final_prj_file = HAsim::Util::path_append($model->build_dir(),$xilinx_config_dir,$name . ".prj");
   
    #first add all local .prj files

    CORE::open(PRJFILE, "> $final_prj_file") || return undef;
    
    my @model_prj_files = find_all_files_with_suffix($model->modelroot(), ".prj");
    
    my $replacements_r = HAsim::Util::empty_hash_ref();
    
    HAsim::Util::common_replacements($model, $replacements_r);
    
    HAsim::Util::hash_set($replacements_r,'@APM_NAME@',$name);

    my $bdir = bluespec_dir();
    HAsim::Util::hash_set($replacements_r,'@BLUESPECDIR@', $bdir);

    foreach my $model_prj_file (@model_prj_files)
    {
      HAsim::Templates::do_template_replacements($model_prj_file, *PRJFILE{IO}, $replacements_r);
    }

    #now add all bsc-generated verilog files

    __generate_prj_file($model->modelroot(),"hw",*PRJFILE{IO});
    CORE::close(PRJFILE);

    return 1;
}

############################################################
# __generate_prj_file:
sub __generate_prj_file {
    my $module = shift;
    my $parent_dir = shift;
    my $file = shift;

    my $my_dir = HAsim::Build::get_module_build_dir($module,$parent_dir);

    # recurse
    HAsim::Build::check_submodules_defined($module);
    foreach my $child ($module->submodules()) {
	__generate_prj_file($child,$my_dir,$file);
    }

    if (HAsim::Build::is_synthesis_boundary($module)) {
	my $v_file = HAsim::Util::path_append($my_dir, $HAsim::Bluespec::tmp_bsc_dir, HAsim::Build::get_wrapper($module) . ".v");
	print $file "verilog work \"$v_file\"\n";
    }

    # Add includes of public and private bsc files
    my @l = ();
    push(@l, $module->private());

    #Add existing verilog/vhdl primitives unless a previous .prj file overrode this.

    foreach my $f (@l) {
      if ($f =~ /\.v$/) {
          my $v_file = HAsim::Util::path_append($my_dir, $f);
          print $file "verilog work \"$v_file\"\n";
      }
      if ($f =~ /\.vhd$/) {
          my $vhd_lib = get_vhdl_lib($module);
          my $vhd_file = HAsim::Util::path_append($my_dir, $f);
          print $file "vhdl $vhd_lib \"$vhd_file\"\n";
      }
    }

}

############################################################
# generate_ut_file:
sub generate_ut_file {
    my $model = shift;
    my $name = HAsim::Build::get_model_name($model);
    
    my $final_ut_file = HAsim::Util::path_append($model->build_dir(),$xilinx_config_dir,$name . ".ut");

    CORE::open(UTFILE, "> $final_ut_file") || return undef;

    #Concatenate all found .ut files
    
    my @model_ut_files = find_all_files_with_suffix($model->modelroot(), ".ut");
    
    my $replacements_r = HAsim::Util::empty_hash_ref();
    
    HAsim::Util::common_replacements($model, $replacements_r);
    
    HAsim::Util::hash_set($replacements_r,'@APM_NAME@',$name);    

    foreach my $model_ut_file (@model_ut_files)
    {
      HAsim::Templates::do_template_replacements($model_ut_file, *UTFILE{IO}, $replacements_r);
    }

    CORE::close(UTFILE);

}

############################################################
# generate_ucf_file:
sub generate_ucf_file {
    my $model = shift;

    my $replacements_r = HAsim::Util::empty_hash_ref();
    
    HAsim::Util::common_replacements($model, $replacements_r);
    
    my $apm = HAsim::Build::get_model_name($model);
    HAsim::Util::hash_set($replacements_r,'@APM_NAME@',$apm);

    my $final_ucf_file = HAsim::Util::path_append($model->build_dir(),$xilinx_config_dir,$apm . ".ucf");

    CORE::open(UCFFILE, "> $final_ucf_file") || return undef;

    #Concatenate all found .ucf files

    my @model_ucf_files = find_all_files_with_suffix($model->modelroot(), ".ucf");

    foreach my $model_ucf_file (@model_ucf_files)
    {
      HAsim::Templates::do_template_replacements($model_ucf_file, *UCFFILE{IO}, $replacements_r);
    
    }
    
    CORE::close(UCFFILE);
}


############################################################
# generate_download_file:
sub generate_download_file {
    my $model = shift;
    my $name = HAsim::Build::get_model_name($model);
    
    my $download_file = HAsim::Util::path_append($model->build_dir(),$xilinx_config_dir,$name . ".download");

    my $replacements_r = HAsim::Util::empty_hash_ref();
    
    HAsim::Util::common_replacements($model, $replacements_r);
    
    my $apm = HAsim::Build::get_model_name($model);
    HAsim::Util::hash_set($replacements_r,'@APM_NAME@',$apm);

    CORE::open(DWNFILE, "> $download_file") || return undef;

    #Concatenate all found .download files

    my @model_dwn_files = find_all_files_with_suffix($model->modelroot(), ".download");

    foreach my $model_dwn_file (@model_dwn_files)
    {
      HAsim::Templates::do_template_replacements($model_dwn_file, *DWNFILE{IO}, $replacements_r);
    
    }
    
    CORE::close(DWNFILE);
}

############################################################
# bluespec_dir:
sub bluespec_dir {
    if (! defined$ENV{'BLUESPECDIR'}) {
      HAsim::Util::WARN_AND_DIE("BLUESPECDIR undefined in environment.");
    }

    return $ENV{'BLUESPECDIR'};
}

############################################################
# vhdl_lib: returns a VHDL lib, or "work" by default

sub get_vhdl_lib {
    my $module = shift;

    foreach my $param_r ($module->parameters()) {
	my %param = %{$param_r};
	if ($param{'name'} eq "VHDL_LIB") {
	    return $param{'default'};
	}
    }
    return "work";
}
return 1;


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
# generate_files_xilinx:
# create the Xilinx-tool specific build files
sub generate_files_xilinx {
    my $model = shift;
    my $builddir = shift;
    my $name = HAsim::Build::get_model_name($model);

    my $config_dir = HAsim::Util::path_append($builddir, $xilinx_config_dir);
    system("mkdir -p $config_dir");

    # Files need to be opened here to share code with the synplify code path

    # PRJ file

    my $final_prj_file = HAsim::Util::path_append($builddir,$xilinx_config_dir,$name . ".prj");
    open(PRJFILE, "> $final_prj_file") || return undef;

    my $xilinx_processfile = sub {
        my $module = shift;
        my $stream = shift;
        my $dir = shift;
        my $file = shift;

        my $xilinx_printfunc = sub {
           my $stream = shift;
           my $type = shift;
           my $file = shift;

           print $stream "$type work $file\r\n";
        };

        my $relative_file = HAsim::Util::path_append($dir, $file);
        
        #crunch this down.
        if ($relative_file =~ /\.v$/ ) {
            $xilinx_printfunc->($stream,"verilog", $relative_file);
        }

        if ($relative_file =~ /\.vhd$/) {
            my $vhd_lib = get_vhdl_lib($module);
            $xilinx_printfunc->($stream,"vhdl", $relative_file);
        }
    };



    ## XST file

    my $final_xst_file = HAsim::Util::path_append($builddir,$xilinx_config_dir,$name . ".xst");

    open(XSTFILE, "> $final_xst_file") || return undef;

    my @model_xst_files = find_all_files_with_suffix($model->modelroot(), ".xst");

    ## UT File

    my $final_ut_file = HAsim::Util::path_append($builddir,$xilinx_config_dir,$name . ".ut");

    open(UTFILE, "> $final_ut_file") || return undef;

    #Concatenate all found .ut files
    
    my @model_ut_files = find_all_files_with_suffix($model->modelroot(), ".ut");

    ##PRJ Files    

    my @model_prj_files = find_all_files_with_suffix($model->modelroot(), ".prj");



    generate_prj_file($model, $builddir, *PRJFILE{IO}, $xilinx_processfile);
    generate_concatenation_file($model, $builddir, *PRJFILE{IO}, @model_prj_files);
    generate_concatenation_file($model, $builddir, *XSTFILE{IO}, @model_xst_files);
    generate_concatenation_file($model, $builddir, *UTFILE{IO}, @model_ut_files);
    generate_download_file($model, $builddir);

}


############################################################
# generate_files_synplify:
# create the Synplify specific build files
sub generate_files_synplify {
    my $model = shift;
    my $builddir = shift;
    my $name = HAsim::Build::get_model_name($model);

    my $config_dir = HAsim::Util::path_append($builddir, $xilinx_config_dir);
    system("mkdir -p $config_dir");

    # to generate the synplify tcl, we follow the same path as generate prj, 
    # but with a capability to produce different outputs.
    my $final_sdf_file = HAsim::Util::path_append($builddir,$xilinx_config_dir,$name . ".synplify.prj");

    open(SDFFILE, "> $final_sdf_file") || return undef;
   
    my $synplify_processfile = sub {
        my $module = shift;
        my $stream = shift;
        my $dir = shift;
        my $file = shift;

        my $synplify_printfunc = sub {
            my $stream = shift;
            my $type = shift;
            my $file = shift;
            #relative paths need to be filled out at make time
            if(File::Spec->file_name_is_absolute($file)) { 
                print $stream "add_file -$type $file\n";
       	    } else {
                print $stream "add_file -$type \"\$env(BUILD_DIR)/$file\"\n";
	    }
        };

        my $relative_file = HAsim::Util::path_append($dir, $file);
        
        #crunch this down.
        if ($relative_file =~ /\.v$/ ) {
            $synplify_printfunc->($stream,"verilog", $relative_file);
        }

        if ($relative_file =~ /\.vhd$/) {
            my $vhd_lib = get_vhdl_lib($module);
            $synplify_printfunc->($stream,"vhdl", $relative_file);
        }

        if ($relative_file =~ /\.ngc$/) {
            my $vhd_lib = get_vhdl_lib($module);
            $synplify_printfunc->($stream,"ngc", $relative_file);
        }
    };

    
    # User SDFs

    my @model_sdf_files = find_all_files_with_suffix($model->modelroot(), ".sdf");

    # User SDC
    my $final_sdc_file = HAsim::Util::path_append($builddir,$xilinx_config_dir,$name . ".sdc");
    my @model_sdc_files = find_all_files_with_suffix($model->modelroot(), ".sdc");
    open(SDCFILE, "> $final_sdc_file") || return undef;

    # Notice that the synplify file accumulates more things than its xilinx 
    # counterpart.

    generate_prj_file($model, $builddir, *SDFFILE{IO}, $synplify_processfile);    
    # add target of the global constraint file to the sdf
    print SDFFILE "add_file -constraint \"\$env(BUILD_DIR)/config/$name.sdc\"\n";
    print SDFFILE "set_option -constraint -clear\n";
    print SDFFILE "set_option -constraint -enable \"\$env(BUILD_DIR)/config/$name.sdc\"\n";

    # gather any user sdf files
    generate_concatenation_file($model, $builddir, *SDFFILE{IO}, @model_sdf_files);   
 
    #build global sdf file
    generate_concatenation_file($model, $builddir, *SDCFILE{IO}, @model_sdc_files);   


}

############################################################
# generate_concatentation_file:
# Build a single file from a group of similar files, while applying a 
# set of common substitutions to the files.

sub generate_concatenation_file {
    my $model = shift;
    my $builddir = shift;
    my $file = shift;
    my @files = shift;    

    my $name = HAsim::Build::get_model_name($model);

    my $replacements_r = HAsim::Util::empty_hash_ref();

    HAsim::Util::common_replacements($model, $replacements_r);
    
    HAsim::Util::hash_set($replacements_r,'@APM_NAME@',$name);
    HAsim::Util::hash_set($replacements_r,'@TMP_XILINX_DIR@',$tmp_xilinx_dir);

    my $bdir = bluespec_dir();
    HAsim::Util::hash_set($replacements_r,'@BLUESPECDIR@', $bdir);

    #Concatenate all found files

    foreach my $single_file (@files)
    {
      HAsim::Templates::do_template_replacements($single_file, $file, $replacements_r);
    }
    
}

############################################################
# generate_prj_file:
sub generate_prj_file {

    my $model = shift;
    my $builddir = shift;
    my $file = shift;
    my $processfile = shift;
    my $name = HAsim::Build::get_model_name($model);
    
    # Create replacements hash

    my $replacements_r = HAsim::Util::empty_hash_ref();
    
    HAsim::Util::common_replacements($model, $replacements_r);
    
    HAsim::Util::hash_set($replacements_r,'@APM_NAME@',$name);

    my $bdir = bluespec_dir();
    HAsim::Util::hash_set($replacements_r,'@BLUESPECDIR@', $bdir);

    __generate_prj_file($model->modelroot(),"hw",$file, $replacements_r, $processfile);

    return 1;
}

############################################################
# __generate_prj_file:
sub __generate_prj_file {
    my $module = shift;
    my $parent_dir = shift;
    my $file = shift;
    my $replacements_r = shift;
    my $processfile = shift;

    my $my_dir = HAsim::Build::get_module_build_dir($module,$parent_dir);

    # recurse
    HAsim::Build::check_submodules_defined($module);
    foreach my $child ($module->submodules()) {
	__generate_prj_file($child,$my_dir,$file,$replacements_r, $processfile);
    }

    if (HAsim::Build::is_synthesis_boundary($module)) {
	#my $v_file = HAsim::Util::path_append($my_dir, $HAsim::Bluespec::tmp_bsc_dir, HAsim::Build::get_wrapper($module) . ".v");
        $processfile->($module,$file,$my_dir . "/" . $HAsim::Bluespec::tmp_bsc_dir, HAsim::Build::get_wrapper($module) . ".v");
    }

    # Add includes of public and private bsc files
    my @l = ();
    push(@l, $module->private());

    #Add existing verilog/vhdl primitives unless a previous .prj file overrode this.
    #use a hash of processors to append the appropriate files

    foreach my $f (@l) {
        $processfile->($module,$file,$my_dir,$f);
    }

    # Add files from %include --type=verilog

    foreach my $f ($module->include("verilog")) {
	my $incdir = HAsim::Templates::do_line_replacements($f, $replacements_r);

	my @i = glob(HAsim::Util::path_append($incdir,"*.v"));

	foreach my $v_file (@i) {
	    # Don't include main in an include...
	    next if ($v_file =~ /main.v$/);
            next if ($v_file =~ /ConstrainedRandom.v$/);
            $processfile->($module,$file,"",$v_file);
	}
    }
	    
}


############################################################
# generate_download_file:
sub generate_download_file {
    my $model = shift;
    my $builddir = shift;
    my $name = HAsim::Build::get_model_name($model);
    
    my $download_file = HAsim::Util::path_append($builddir,$xilinx_config_dir,$name . ".download");

    my $replacements_r = HAsim::Util::empty_hash_ref();
    
    HAsim::Util::common_replacements($model, $replacements_r);
    
    my $apm = HAsim::Build::get_model_name($model);
    HAsim::Util::hash_set($replacements_r,'@APM_NAME@',$apm);

    # Concatenate all found .download files
    my @model_dwn_files = find_all_files_with_suffix($model->modelroot(), ".download");
    if (@model_dwn_files) {
        open(DWNFILE, "> $download_file") || return undef;

        print DWNFILE "#!/bin/sh\n";
        print DWNFILE "impact -batch <<EOF\n";

        foreach my $model_dwn_file (@model_dwn_files) {
            HAsim::Templates::do_template_replacements($model_dwn_file, *DWNFILE{IO}, $replacements_r);
        }

        print DWNFILE "EOF\n";

        close(DWNFILE);
        chmod(0755, $download_file);
    }
    else {
        unlink($download_file);
    }
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

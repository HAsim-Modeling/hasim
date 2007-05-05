
# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

#
# Author:  Martha Mercaldi
#

package HAsim::Build;

use Asim;

use HAsim::Util;

use warnings;
use strict;

############################################################
# get_makefile_include_template
sub get_makefile_include_template {
    my $model = shift;
    my $root = $model->modelroot();

    if (num_makefile_templates($root) != 3) {
	HAsim::Util::WARN_AND_DIE("In HAsim, root module must specify three Makefile templates.");
    }

    return get_makefile_template($root,0);
}

############################################################
# get_makefile_top_template
sub get_makefile_top_template {
    my $model = shift;
    my $root = $model->modelroot();

    if (num_makefile_templates($root) != 3) {
	HAsim::Util::WARN_AND_DIE("In HAsim, root module must specify three Makefile templates.");
    }

    return get_makefile_template($root,1);
}

############################################################
# get_makefile_sub_template
sub get_makefile_sub_template {
    my $model = shift;
    my $module = shift;
    my $root = $model->modelroot();

    # if $module is root, or has no override. . .
    if ($module->issame($root) || num_makefile_templates($module) == 0) {
	return get_makefile_template($root,2);
    }

    return get_makefile_template($module,0);
}

############################################################
# num_makefile_templates 
sub num_makefile_templates {
    my $module = shift;
    my @templates = $module->makefile();
    return ($#templates + 1);
}

############################################################
# get_makefile_template
sub get_makefile_template {
    my $module = shift;
    my $idx = shift;

    my @templates = $module->makefile();

    if ($idx >= num_makefile_templates($module)) {
	HAsim::Util::WARN_AND_DIE("No " . $idx . "-th makefile template in module with " . num_makefile_templates($module) . " templates.");
    }

    return Asim::resolve(HAsim::Util::path_append($module->base_dir(),$templates[$idx]));
}

############################################################
# get_wrapper
sub get_wrapper {
    my $module = shift;
    
    if (! is_synthesis_boundary($module)) {
	HAsim::Util::WARN_AND_DIE("Wrappers generated only for modules designated as synthesis boundaries.");
    }

    return ("mk_" . $module->provides . "_Wrapper");
}

############################################################
# check_submodules_defined: check that all submodules are
#                           defined in model
sub check_submodules_defined {
    my $module = shift;

    my @requires = $module->requires();
    my @submodules = $module->submodules();
    foreach my $index (0 .. $#requires) {
	my $m = $submodules[$index];
	my $r = $requires[$index];
	if (! defined($m)) {
	    HAsim::Util::WARN("No implementation specified for module of type $r...");
	}
    }

    return 1;
}

############################################################
# get_module_build_dir: identify module's directory in the
#                       build tree (given parent directory path)
sub get_module_build_dir {
    my $module = shift;
    my $parent_dir = shift;
    my $my_dir;
    if (is_synthesis_boundary($module)) {
	$my_dir = HAsim::Util::path_append($parent_dir, $module->provides());
    } else {
	$my_dir = $parent_dir;
    }
    return $my_dir;
}

############################################################
# is_synthesis_boundary: reads ab Asim module's parameters to
#                        see if any Bluespec module is designated
#                        a synthesis boundary
sub is_synthesis_boundary {
    my $module = shift;

    foreach my $param_r ($module->parameters()) {
	my %param = %{$param_r};
	if ($param{'name'} eq "SYNTH_BOUNDARY") {
	    return 1;
	}
    }
    return 0;
}

############################################################
# get_model_name
sub get_model_name {
    my $model = shift;
    my $apm_file = $model->filename();

    my @segments = split(/\//,$apm_file);
    my $name = $segments[$#segments];
    $name =~ s/.apm//g;

    return $name;
}

return 1;

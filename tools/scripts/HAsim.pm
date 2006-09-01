
# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

#
# Author:  Martha Mercaldi
#

package HAsim;

use Asim;

use Util;

use warnings;
use strict;

############################################################
# get_wrapper
sub get_wrapper {
    my $module = shift;
    
    if (! is_synthesis_boundary($module)) {
	Util::WARN_AND_DIE("Wrappers generated only for modules designated as synthesis boundaries.");
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
	    Util::WARN("No implementation specified for module of type $r...");
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
	$my_dir = Util::path_append($parent_dir, $module->provides());
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

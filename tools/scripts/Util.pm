
# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

#
# Author:  Martha Mercaldi
#

package Util;

use warnings;
use strict;

############################################################
# path_append: simple utility to directory paths
sub path_append {
    my @parts = @_;

    my $result = "";
    foreach my $part (@parts) {
	if ($result eq "") {
	    $result = $part;
	} elsif ($part eq "") {
	    # skip this part
	} else {
	    $result = $result . "/" . $part;
	}
    }

    return $result;
}

############################################################
# empty_hash_ref: produce a ref to a new empty hash table
sub empty_hash_ref {
    my %hash = ();
    return \%hash;
}

############################################################
# hash_set: set a key,value pair in given hash table
sub hash_set {
    my $hash_r = shift;
    my $key = shift;
    my $value = shift;

    $hash_r->{$key} = $value;

    return 1;
}

############################################################
# hash_append: append a value to the value already present
#              in the given hash table (using the given 
#              separator)
sub hash_append {
    my $hash_r = shift;
    my $separator = shift;
    my $key = shift;
    my $value = shift;
    
    if (exists $hash_r->{$key}) { 
	$hash_r->{$key} = $hash_r->{$key} . $separator . $value;
    } else {
	$hash_r->{$key} = $value;
    }

    return 1;
}

############################################################
# WARN
sub WARN {
    my $msg = shift;
    my ($package, $filename, $line, $subroutine, $hasargs, $wantarray, $evaltext, $is_require, $hints, $bitmask) = caller(1);
    print STDERR "$package::$subroutine: (called from $filename:$line): $msg\n";
}

############################################################
# WARN_AND_DIE
sub WARN_AND_DIE {
    my $msg = shift;
    WARN($msg);
    die;
}

return 1;

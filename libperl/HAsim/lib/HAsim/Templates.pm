
# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

#
# Author:  Martha Mercaldi
#

package HAsim::Templates;

use HAsim::Util;

use warnings;
use strict;


############################################################
# do_template_replacements: Given a template file, a 
#                           destination file, and a hash of
#                           replacement keys to values, 
#                           produce destination file = 
#                           template + replacements

sub do_template_replacements {
    my $template = shift;
    my $dstfile = shift;
    my $replacements_r = shift;

#    print "Generating... $dst\n" if $debug;
#    print "================================\n" if $debug;
#    print "$template --> $dst\n" if $debug;
#    print "--------------------------------\n" if $debug;
#    while ( my ($key, $value) = each %$replacements_r ) {
#	print "$key => $value\n" if $debug;
#    }
#    print "================================\n" if $debug;


    CORE::open(TEMPLATE, "< $template") || return undef;

    while (my $line = <TEMPLATE>) {
	# check for each possible substitution
	while ( my ($key, $value) = each %$replacements_r ) {
	    $line =~ s/$key/$value/g;
	}
	
	# remove any unmatched replacements
	while ($line =~ /@([\w\-]+)@/) {
	    $line =~ s/@[\w\-]+@//g;
	}
	print $dstfile $line;
    }
    CORE::close(TEMPLATE);

    return 1;
}

############################################################
# do_replacements: The function to create a new file

sub do_replacements {
    my $template = shift;
    my $dst = shift;
    my $replacements_r = shift;
    
    CORE::open(DST, "> $dst") || return undef;
    
    return do_template_replacements($template, *DST{IO}, $replacements_r);
    
    CORE::close(DST);
    
}


return 1;

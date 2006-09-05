
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
# merge_replacements
sub merge_replacements {
    my $r1 = shift;
    my $r2 = shift;
    
    HAsim::Util::WARN_AND_DIE("Templates::merge_replacements() not yet implemented.");

}

############################################################
# do_template_replacements: Given a template file, a 
#                           destination file, and a hash of
#                           replacement keys to values, 
#                           produce destination file = 
#                           template + replacements
sub do_replacements {
    my $template = shift;
    my $dst = shift;
    my $replacements_r = shift;

#    print "Generating... $dst\n" if $debug;
#    print "================================\n" if $debug;
#    print "$template --> $dst\n" if $debug;
#    print "--------------------------------\n" if $debug;
#    while ( my ($key, $value) = each %$replacements_r ) {
#	print "$key => $value\n" if $debug;
#    }
#    print "================================\n" if $debug;


    CORE::open(DST, "> $dst") || return undef;
    CORE::open(TEMPLATE, "< $template") || return undef;
    while (my $line = <TEMPLATE>) {
	# check for each possible substitution
	while ( my ($key, $value) = each %$replacements_r ) {
	    $line =~ s/$key/$value/g;
	}
	
	# remove any unmatched replacements
	while ($line =~ /@.+@/) {
	    $line =~ s/@.+@//g;
	}
	print DST $line;
    }
    CORE::close(TEMPLATE);
    CORE::close(DST);

    return 1;
}


return 1;

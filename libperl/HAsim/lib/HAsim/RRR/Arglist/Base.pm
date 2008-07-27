# *****************************************************************************
# * Base.pm
# *
# * Copyright (C) 2008 Intel Corporation
# *
# * This program is free software; you can redistribute it and/or
# * modify it under the terms of the GNU General Public License
# * as published by the Free Software Foundation; either version 2
# * of the License, or (at your option) any later version.
# *
# * This program is distributed in the hope that it will be useful,
# * but WITHOUT ANY WARRANTY; without even the implied warranty of
# * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# * GNU General Public License for more details.
# *
# * You should have received a copy of the GNU General Public License
# * along with this program; if not, write to the Free Software
# * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# *
# *****************************************************************************

#
# Author:  Angshuman Parashar
#

package HAsim::RRR::Arglist::Base;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Argument;
use HAsim::RRR::Arglist::BSV;

##
## constructor
##
sub new
{
    # get name of class
    my $class = shift;

    # get direction that we should look for while parsing
    my $direction = shift;

    # get string to parse
    my $string = shift;

    # parse string into an arglist
    my $obj = _parse($direction, $string);

    # typecast object
    bless ($obj, $class);

    # return object
    return $obj;
}

##
## accept a string and parse it into an arglist
##
sub _parse
{
    my $direction = shift;
    my $string    = shift;

    # create a new anonymous hash
    my $arglist;

    $arglist->{direction} = $direction;

    # split arg string using comma as a delimiter
    my @raw_args = split(/,/, $string);
    
    # process each split as a type
    foreach my $raw_arg (@raw_args)
    {
        my $arg = HAsim::RRR::Argument->new($raw_arg);
        
        # push into in or out list ONLY IF it has the direction
        # we're looking for
        if ($arg->direction() eq $direction)
        {
            push(@{ $arglist->{args} }, $arg);
        }
    }

    # return hash
    return $arglist;
}

##
## get the direction
##
sub direction
{
    my $self = shift;

    return $self->{direction};
}

##
## get the actual list of args
##
sub args
{
    my $self = shift;

    return $self->{args};
}

##
## return the number of args
##
sub num
{
    my $self = shift;

    return ($#{ $self->{args} } + 1);
}

##
## return the total size of args
##
sub size
{
    my $self = shift;

    my $size = 0;
    foreach my $arg (@{ $self->{args} })
    {
        $size = $size + $arg->type()->size();
    }

    return $size;
}

1;

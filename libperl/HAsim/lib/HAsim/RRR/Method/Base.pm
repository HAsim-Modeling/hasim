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

package HAsim::RRR::Method::Base;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Arglist::Base;

# regex
my $REGEX = qr/
                  \s*?
                  method
                  \s+
                  (\S+)
                  \s*?
                  \(
                  (.+?)
                  \)
                  \s*?
                  ;
                  \s*?
              /x;

##
## constructor: this is an unusual constructor. It returns
## not a single object of type Method, but a list of objects
## of type Method. All other member functions operate on a
## single object.
##
sub new
{
    # get name of class
    my $class = shift;

    # get string to parse
    my $string = shift;

    # parse string into multiple methods
    my @objlist = _parse($string);

    # typecast each entry in list
    foreach my $obj (@objlist)
    {
        bless ($obj, $class);
    }

    # return list of objects
    return @objlist;
}

##
## accept a string and parse it into a method
##
sub _parse
{
    # string
    my $string = shift;

    # create an empty list of methods
    my @methodlist = ();

    # parse for multiple methods
    while ($string =~ /$REGEX/g)
    {
        # create a new method object
        my $method;

        # assign name to hash
        $method->{name} = $1;

        # construct input and output arg lists from raw arg string
        $method->{inargs}  = HAsim::RRR::Arglist::Base->new("in", $2);
        $method->{outargs} = HAsim::RRR::Arglist::Base->new("out", $2);

        # add to list
        push(@methodlist, $method);
    }

    # return list
    return @methodlist;
}

##
## get the name
##
sub name
{
    my $self = shift;

    return $self->{name};
}

##
## get the input arglist
##
sub inargs
{
    my $self = shift;

    return $self->{inargs};
}

##
## get the output arglist
##
sub outargs
{
    my $self = shift;

    return $self->{outargs};
}

##
## generate a name for the input arg type
##
sub _intype_name
{
    my $self = shift;

    return "IN_TYPE_" . $self->{name};
}

##
## generate a name for the output arg type
##
sub _outtype_name
{
    my $self = shift;

    return "OUT_TYPE_" . $self->{name};
}

1;

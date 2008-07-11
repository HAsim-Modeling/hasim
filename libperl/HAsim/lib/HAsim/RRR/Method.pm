# *****************************************************************************
# * Method.pm
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

package HAsim::RRR::Method;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Argument;

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
        $method->{name}   = $1;
        $method->{inarg}  = undef;
        $method->{outarg} = undef;

        # split args string using comma as a delimiter
        my @raw_arglist = split(/,/, $2);

        # process each split as a type
        foreach my $raw_arg (@raw_arglist)
        {
            my $arg = HAsim::RRR::Argument->new($raw_arg);

            # push into in or out list
            if ($arg->direction() eq "in")
            {
                # multiple args not supported, so overwrite
                $method->{inarg} = $arg;
                # push(@{ $method->{inarglist} }, $arg);
            }
            else
            {
                # multiple args not supported, so overwrite
                $method->{outarg} = $arg;
                # push(@{ $method->{outarglist} }, $arg);
            }
        }

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
## get the input arg
##
sub inarg
{
    my $self = shift;

    return $self->{inarg};
}

##
## get the output arg
##
sub outarg
{
    my $self = shift;

    return $self->{outarg};
}

##
## get the input arg size
##
sub insize
{
    my $self = shift;

    return $self->{inarg}->type()->size();
}

##
## get the output arg size
##
sub outsize
{
    my $self = shift;

    if (defined($self->{outarg}))
    {
        return $self->{outarg}->type()->size();
    }
    else
    {
        return 0;
    }
}

#
# get the input arglist
#
# sub inarglist
# {
#     my $self = shift;
# 
#     return $self->{inarglist};
# }

#
# get the output arglist
#
# sub outarglist
# {
#     my $self = shift;
# 
#     return $self->{outarglist};
# }

#
# return the total size of input args
#
# sub insize
# {
#     my $self = shift;
# 
#     my $size = 0;
#     foreach my $arg (@{ $self->{inargs} })
#     {
#         $size = $size + $arg->type()->size();
#     }
# 
#     return $size;
# }

#
# return the total size of output args
#
# sub outsize
# {
#     my $self = shift;
# 
#     my $size = 0;
#     foreach my $arg (@{ $self->{outargs} })
#     {
#         $size = $size + $arg->type()->size();
#     }
# 
#     return $size;
# }

1;

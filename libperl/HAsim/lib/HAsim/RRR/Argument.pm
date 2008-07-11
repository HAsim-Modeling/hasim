# *****************************************************************************
# * Argument.pm
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

package HAsim::RRR::Argument;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Type;
use HAsim::RRR::Identifier;

### static constants

# regular expressions
my $REGEX =     qr/
                    (in|out)
                    \s+
                    (\S+)
                    \s+
                    (\S+)
                  /x;

### static methods

# constructor
sub new
{
    # get name of class
    my $class = shift;

    # get string to parse
    my $string = shift;

    # parse string
    my $self = _parse($string);

    # typecast
    bless ($self, $class);

    # return object
    return $self;
}

# accept a string and parse it into an argument
sub _parse
{
    # string
    my $string = shift;

    # parse
    if ($string =~ /$REGEX/)
    {
        my $direction = $1;
        my $type      = HAsim::RRR::Type->new($2);
        my $name      = HAsim::RRR::Identifier->new($3);

        return { direction => $direction,
                 type      => $type,
                 name      => $name };
    }
    else
    {
        return { direction => undef,
                 type      => undef,
                 name      => undef };
    }
}

### interface methods

# get the direction
sub direction
{
    my $self = shift;

    return $self->{direction};
}

# get the type
sub type
{
    my $self = shift;

    return $self->{type};
}

# get the name
sub name
{
    my $self = shift;

    return $self->{name};
}

# print an argument in BSV format
sub print_bsv
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # print into file
    if (defined($self->{type}) && defined($self->{name}))
    {
        $self->{type}->print_bsv($file);
        print $file " ";
        $self->{name}->print_bsv($file);
    }
    else
    {
        die ref($self) . ": invalid, cannot print.";
    }
}

# print an argument in CPP format
sub print_cpp
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # print into file
    if (defined($self->{type}) && defined($self->{name}))
    {
        $self->{type}->print_cpp($file);
        print $file " ";
        $self->{name}->print_cpp($file);
    }
    else
    {
        die ref($self) . ": invalid, cannot print.";
    }
}

1;

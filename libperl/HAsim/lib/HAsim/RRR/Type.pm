# *****************************************************************************
# * Type.pm
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

package HAsim::RRR::Type;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Identifier;
use HAsim::RRR::Numeral;

### static constants
my $regex_digit     = qr/\d/x;
my $regex_character = qr/[\w_\.]/x;

# regular expressions
my $REGEX       = qr/
                      ($regex_character+)
                      \[
                      ($regex_digit+)
                      \]
                      \s*?
                    /x;

### static methods

# construct a new type by parsing a string
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

# accept a string and parse it into a type
sub _parse
{
    # string
    my $string = shift;

    # parse
    if ($string =~ /$REGEX/)
    {
        my $name = HAsim::RRR::Identifier->new($1)->string();
        my $size = HAsim::RRR::Numeral->new($2)->value();

        return { name => $name,
                 size => $size };
    }
    else
    {
        return { name => undef,
                 size => undef };
    }
}

### interface methods

# return the name of the type
sub name
{
    my $self = shift;

    return $self->{name};
}

# return the size of the type in BSV
sub size_bsv
{
    my $self = shift;

    return $self->{size};
}

# return the size of the type in CPP
sub size_cpp
{
    my $self = shift;

    # FIXME: handle special cases. For now we assume all types
    # are divisible by 8
    return ($self->{size} / 8);
}

# return a string representing the type in BSV
sub string_bsv
{
    # get object
    my $self = shift;

    if (defined($self->name()))
    {
        return $self->name();
    }
    else
    {
        die ref($self) . ": invalid, cannot extract string.";
    }
}

# print a type in BSV format
sub print_bsv
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    if (defined($self->name()))
    {
        print $file $self->name();
    }
    else
    {
        die ref($self) . ": invalid, cannot print.";
    }
}

# return a string representing the type in CPP
sub string_cpp
{
    # get object
    my $self = shift;

    if (defined($self->name()))
    {
        return $self->name();
    }
    else
    {
        die ref($self) . ": invalid, cannot extract string.";
    }
}

# print a type in CPP format
sub print_cpp
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # print into file
    if (defined($self->name()))
    {
        print $file $self->name();
    }
    else
    {
        die ref($self) . ": invalid, cannot print.";
    }
}

1;

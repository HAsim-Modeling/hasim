# *****************************************************************************
# * Identifier.pm
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

package HAsim::RRR::Identifier;

use warnings;
use strict;
use re 'eval';

### static constants
my $regex_digit     = qr/\d/x;
my $regex_character = qr/[\w_\.]/x;

# regular expressions
my $REGEX      = qr/
                    (
                        \s*?
                        $regex_character+
                        \s*?
                    )
                   /x;

### static methods

# construct a new identifier by parsing a string
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

# accept a string and parse it into an identifier
sub _parse
{
    # string
    my $string = shift;

    # parse
    if ($string =~ /$REGEX/)
    {
        return { string => $1 };
    }
    else
    {
        return { string => undef };
    }
}

### interface methods

# return the identifier string
sub string
{
    # get object
    my $self = shift;

    # return string
    return $self->{string};
}

# print an identifier in BSV format
sub print_bsv
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # print string into file
    if (defined($self->string()))
    {
        print $file $self->string();
    }
    else
    {
        die ref($self) . ": invalid, cannot print.";
    }
}

# print an identifier in CPP format
sub print_cpp
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # print string into file
    if (defined($self->string()))
    {
        print $file $self->string();
    }
    else
    {
        die ref($self) . ": invalid, cannot print.";
    }
}

1;

# *****************************************************************************
# * Numeral.pm
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

package HAsim::RRR::Numeral;

use warnings;
use strict;
use re 'eval';

### static constants

# regular expressions
my $regex_digit     = qr/\d/x;
my $dec_numeral     = qr/
                        \s*?
                        $regex_digit+
                        \s*?
                      /x;
my $hex_numeral     = qr/
                        \s*?
                        0x
                        $regex_digit+
                        \s*?
                      /x;
my $REGEX = qr/($dec_numeral|$hex_numeral)/x;

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

# accept a string and parse it into a numeral
sub _parse
{
    # string
    my $string = shift;

    # parse
    if ($string =~ /$REGEX/)
    {
        # base
        if ($1 =~ /$hex_numeral/)
        {
            # hex
            return { base  => 16,
                     value => $1 };
        }
        else
        {
            # dec
            return { base  => 10,
                     value => $1 };
        }
    }
    else
    {
        # invalid
        return { base  => undef,
                 value => undef };
    }
}

### interface methods

# return the base of the numeral
sub base
{
    # get object
    my $self = shift;

    # return base
    return $self->{base};
}

# return the decoded value of the numeral
sub value
{
    # get object
    my $self = shift;

    # return value
    if ($self->{base} == 10)
    {
        return $self->{value};
    }
    else
    {
        # TODO
        return 0;
    }
}

# print a numeral in BSV format
sub print_bsv
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # print value into file
    if (defined($self->{value}))
    {
        if ($self->{value} == 10)
        {
            print $file $self->{value};
        }
        else
        {
            print $file "\'h" . $self->{value};
        }
    }
    else
    {
        die ref($self) . ": invalid, cannot print.";
    }
}

# print a numeral in CPP format
sub print_cpp
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # print value into file
    if (defined($self->{value}))
    {
        if ($self->{base} == 10)
        {
            print $file $self->{value};
        }
        else
        {
            print $file "0x" . $self->{value};
        }
    }
    else
    {
        die ref($self) . ": invalid, cannot print.";
    }
}

1;

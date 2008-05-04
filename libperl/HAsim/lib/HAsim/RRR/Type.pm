# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

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

# return the size of the type
sub size
{
    my $self = shift;

    return $self->{size};
}

# return a string representing the type in BSV
sub string_bsv
{
    # get object
    my $self = shift;
    my $pack = shift;

    if ($pack == 0)
    {
        if (defined($self->name()))
        {
            return $self->name();
        }
        else
        {
            die ref($self) . ": invalid, cannot extract string.";
        }
    }
    else
    {
        if (defined($self->size()))
        {
            return "Bit#(" . $self->size() . ")";
        }
        else
        {
            die ref($self) . ": invalid, cannot extract string.";
        }
    }
}

# print a type in BSV format
sub print_bsv
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;
    my $pack = shift;

    # print into file
    if ($pack == 0)
    {
        if (defined($self->name()))
        {
            print $file $self->name();
        }
        else
        {
            die ref($self) . ": invalid, cannot print.";
        }
    }
    else
    {
        if (defined($self->size()))
        {
            print $file "Bit#(" . $self->size() . ")";
        }
        else
        {
            die ref($self) . ": invalid, cannot print.";
        }
    }
}

1;

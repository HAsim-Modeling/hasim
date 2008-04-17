# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

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

1;

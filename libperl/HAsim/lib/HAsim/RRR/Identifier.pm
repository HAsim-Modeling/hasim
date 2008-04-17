# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

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

1;

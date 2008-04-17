# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

#
# Author:  Angshuman Parashar
#

package HAsim::RRR::Collection;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Method;

# regex: parsing a collection is relatively easy since
# it is at the leaf-level of a balanced-braces hierarchy
my $REGEX = qr/
                  \s*?
                  server \s*?
                  (\S+)
                  \s*? \( \s*?
                  (\S+)
                  \s*? , \s*?
                  (\S+)
                  \s*? \) \s*? <- \s*?
                  (\S+)
                  \s*? \( \s*?
                  (\S+)
                  \s*? , \s*?
                  (\S+)
                  \s*? \) \s*?
                  \{
                  (.*?)
                  \}
                  \s*?
                  ;
                  \s*?
              /x;

# constructor: this is an unusual constructor. It returns
# not a single object of type Collection, but a list of objects
# of type Collection. All other member functions operate on a
# single object.
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

# accept a string and parse it into a list of Collections
sub _parse
{
    # string
    my $string = shift;

    # create an empty list of collections
    my @collectionlist = ();

    # parse
    while ($string =~ /$REGEX/g)
    {
        # create a new collection
        my $collection;

        # set targets
        $collection->{server_target} = $1;
        $collection->{server_lang} = $2;
        $collection->{server_ifc}  = $3;

        $collection->{client_target} = $4;
        $collection->{client_lang} = $5;
        $collection->{client_ifc}  = $6;

        # parse body of collection into list of methods
        push(@{ $collection->{methodlist} }, HAsim::RRR::Method->new($7));

        # add collection to list
        push(@collectionlist, $collection);
    }
    
    # return list
    return @collectionlist;
}

# return the server target name
sub server_target
{
    my $self = shift;

    return $self->{server_target};
}

# return the server implementation language
sub server_lang
{
    my $self = shift;

    return $self->{server_lang};
}

# return the server interface type
sub server_ifc
{
    my $self = shift;

    return $self->{server_ifc};
}

# return the client target name
sub client_target
{
    my $self = shift;

    return $self->{client_target};
}

# return the client implementation language
sub client_lang
{
    my $self = shift;

    return $self->{client_lang};
}

# return the client interface type
sub client_ifc
{
    my $self = shift;

    return $self->{client_ifc};
}

# return the list of methods
sub methodlist
{
    my $self = shift;

    return $self->{methodlist};
}

1;

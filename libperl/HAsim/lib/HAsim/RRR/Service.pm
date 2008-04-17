# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

#
# Author:  Angshuman Parashar
#

package HAsim::RRR::Service;

use warnings;
use strict;
use re 'eval';

use Text::Balanced;

use HAsim::RRR::Collection;
use HAsim::RRR::Server;
# use HAsim::RRR::Client;

# regex
my $REGEX = qr/
                  .*?
                  service \s*
                  (\S+?)
                  \s*
                  (\{)
                  (.*)
              /x;

##
## constructor: this is an unusual constructor. It returns
## not a single object of type Service, but a list of objects
## of type Service. All other member functions operate on a
## single object.
##
sub new
{
    # get name of class
    my $class = shift;

    # get string to parse
    my $string = shift;

    # parse string into multiple services
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
## accept a string and parse it into a list of services
##
sub _parse
{
    # string
    my $string = shift;

    # create an empty list of services
    my @servicelist = ();

    # parse
    while ($string =~ /$REGEX/) # note: NOT /$REGEX/g
    {
        # create a new service
        my $service;

        # extract a service name
        $service->{name} = $1;

        # capture the remainder of the string
        my $remainder = $2 . $3;

        # extract one service body, destroying $remainder in the
        # process and leaving all code after the captured service
        # body in the new $remainder
        my $body = Text::Balanced::extract_bracketed($remainder, '{}');

        # parse body into a list of collections 
        my @collectionlist = HAsim::RRR::Collection->new($body);

        # re-arrange collections into a lists of clients and servers
        my @serverlist = HAsim::RRR::Server->new($service->{name}, @collectionlist);
        # my @clientlist = HAsim::RRR::Client->new(@collectionlist);

        # add client and server lists to service
        push (@{ $service->{serverlist} }, @serverlist);
        # push (@{ $service->{clientlist} }, @clientlist);

        # add service to service list
        push(@servicelist, $service);

        # set residue as the new string to parse, and continue
        $string = $remainder;
    }

    # return service list
    return @servicelist;
}

##
## return the name
##
sub name
{
    my $self = shift;

    return $self->{name};
}

##
## return the list of servers
##
sub serverlist
{
    my $self = shift;

    return $self->{serverlist};
}

# return the list of clients
# sub clientlist
# {
#     my $self = shift;
# 
#     return $self->{clientlist};
# }

##
## do we need to generate a server stub for this service and target?
##
sub needs_server_stub
{
    # capture params
    my $self   = shift;
    my $target = shift;

    # for each entry in my list of servers...
    foreach my $server (@{ $self->{serverlist} })
    {
        # look for the specified target name. It is guaranteed that
        # each server in this list will have a unique target name.
        if ($server->target() eq $target)
        {
            # return true
            return 1;
        }

        # NOTE: we are guaranteed to only print one stub
        # for a given target
    }

    # no match found, return false
    return 0;
}   

##
## print server stub for a given target into a given file
##
sub print_server_stub
{
    # capture params
    my $self   = shift;
    my $file   = shift;
    my $target = shift;

    # for each entry in my list of servers...
    foreach my $server (@{ $self->{serverlist} })
    {
        # look for the specified target name. It is guaranteed that
        # each server in this list will have a unique target name.
        if ($server->target() eq $target)
        {
            # ask the server to print out a stub
            $server->print_stub($file);
        }

        # NOTE: we are guaranteed to only print one stub
        # for a given target
    }
}

##
## do we need to generate a server connections for this service and target?
##
sub needs_server_connections
{
    # capture params
    my $self   = shift;
    my $target = shift;

    # for each entry in my list of servers...
    foreach my $server (@{ $self->{serverlist} })
    {
        # look for the specified target name. It is guaranteed that
        # each server in this list will have a unique target name.
        if ($server->target() eq $target)
        {
            # now check type of server interface
            if ($server->interface() eq "connection")
            {
                return 1;
            }
            else
            {
                return 0;
            }
        }

        # NOTE: we are guaranteed to only print one stub
        # for a given target
    }

    # no match found, return false
    return 0;
}   

##
## print server connections for a given target into a given file
##
sub print_server_connections
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;
    my $target = shift;

    # for each entry in my list of servers...
    foreach my $server (@{ $self->{serverlist} })
    {
        # look for the specified target name. It is guaranteed that
        # each server in this list will have a unique target name.
        if ($server->target() eq $target)
        {
            # ask the server to print out a connection instantiation
            $server->print_connections($file, $indent);
        }

        # NOTE: we are guaranteed to only print one connection
        # for a given target
    }
}

##
## print server link rules for a given target into a given file
##
sub print_server_link_rules
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;
    my $target = shift;

    # for each entry in my list of servers...
    foreach my $server (@{ $self->{serverlist} })
    {
        # look for the specified target name. It is guaranteed that
        # each server in this list will have a unique target name.
        if ($server->target() eq $target)
        {
            # ask the server to print out a wrapper rule
            $server->print_link_rules($file, $indent);
        }

        # NOTE: we are guaranteed to only print one rule
        # for a given target
    }
}

1;

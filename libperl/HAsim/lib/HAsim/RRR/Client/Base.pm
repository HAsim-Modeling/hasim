# *****************************************************************************
# * Base.pm
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

package HAsim::RRR::Client::Base;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Collection;
use HAsim::RRR::Client::BSV;
use HAsim::RRR::Client::CPP;

##
## constructor: this is an unusual constructor. It returns
## not a single object of type Client, but a list of objects
## of type Client. All other member functions operate on a
## single object.
##
sub new
{
    # get name of class
    my $class = shift;

    # get service name
    my $servicename = shift;

    # get list of collections
    my @collectionlist = @_;

    # extract methods from each collection and place them
    # into multiple client modules, each with a unique
    # target name
    my @objlist = _extract($servicename, @collectionlist);

    # do not typecast list entries, they are already typed
    # according to their implementation language

    # return list of objects
    return @objlist;
}

##
## extract methods from each collection and place them
## into multiple client modules, each with a unique
## target name
##
sub _extract
{
    my $servicename    = shift;
    my @collectionlist = @_;

    # create an empty list of clients
    my @clientlist = ();

    # for each collection in given list
    foreach my $collection (@collectionlist)
    {
        # create a new client: we are guaranteed to have
        # one new client target per collection
        my $client;

        # check for target name conflicts in existing
        # list of clients
        foreach my $s (@clientlist)
        {
            if ($s->{target} eq $collection->client_target())
            {
                die "client target name conflict: " . $s->{target};
            }
        }

        # no conflicts, fill in client details
        $client->{name}   = $servicename;
        $client->{target} = $collection->client_target();
        $client->{lang}   = $collection->client_lang();
        $client->{ifc}    = $collection->client_ifc();

        # now construct a target-language-specific client
        my $typed_client;
        if ($client->{lang} eq "bsv")
        {
            # pass in the method list
            $typed_client = HAsim::RRR::Client::BSV->new($client, @{ $collection->methodlist() });
        }
        elsif ($client->{lang} eq "cpp")
        {
            # pass in the method list
            $typed_client = HAsim::RRR::Client::CPP->new($client, @{ $collection->methodlist() });
        }
        else
        {
            die "invalid client language: " . $client->{lang};
        }

        # add typed client to list of clients
        push(@clientlist, $typed_client);
    }

    # return list
    return @clientlist;
}

##
## return the target name
##
sub target
{
    my $self = shift;

    return $self->{target};
}

##
## return the language
##
sub lang
{
    my $self = shift;

    return $self->{lang};
}

##
## return the interface type
##
sub interface
{
    my $self = shift;

    return $self->{ifc};
}

1;

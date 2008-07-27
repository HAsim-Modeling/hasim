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

package HAsim::RRR::Server::Base;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Collection;
use HAsim::RRR::Server::BSV;
use HAsim::RRR::Server::CPP;

#
# constructor: this is an unusual constructor. It returns
# not a single object of type Server, but a list of objects
# of type Server. All other member functions operate on a
# single object.
#
sub new
{
    # get name of class
    my $class = shift;

    # get service name
    my $servicename = shift;

    # get list of collections
    my @collectionlist = @_;

    # extract methods from each collection and place them
    # into multiple server modules, each with a unique
    # target name
    my @objlist = _extract($servicename, @collectionlist);

    # do not typecast list entries, they are already typed
    # according to their implementation language

    # return list of objects
    return @objlist;
}

#
# extract methods from each collection and place them
# into multiple server modules, each with a unique
# target name
#
sub _extract
{
    my $servicename    = shift;
    my @collectionlist = @_;

    # create an empty list of servers
    my @serverlist = ();

    # for each collection in given list
    foreach my $collection (@collectionlist)
    {
        # create a new server: we are guaranteed to have
        # one new server target per collection
        my $server;

        # check for target name conflicts in existing
        # list of servers
        foreach my $s (@serverlist)
        {
            if ($s->{target} eq $collection->server_target())
            {
                die "server target name conflict: " . $s->{target};
            }
        }

        # no conflicts, fill in server details
        $server->{name}   = $servicename;
        $server->{target} = $collection->server_target();
        $server->{lang}   = $collection->server_lang();
        $server->{ifc}    = $collection->server_ifc();

        # now construct a target-language-specific server
        my $typed_server;
        if ($server->{lang} eq "bsv")
        {
            # pass in the method list
            $typed_server = HAsim::RRR::Server::BSV->new($server, @{ $collection->methodlist() });
        }
        elsif ($server->{lang} eq "cpp")
        {
            # pass in the method list
            $typed_server = HAsim::RRR::Server::CPP->new($server, @{ $collection->methodlist() });
        }
        else
        {
            die "invalid server language: " . $server->{lang};
        }

        # add typed server to list of servers
        push(@serverlist, $typed_server);
    }

    # return list
    return @serverlist;
}

#
# return the target name
#
sub target
{
    my $self = shift;

    return $self->{target};
}

#
# return the language
#
sub lang
{
    my $self = shift;

    return $self->{lang};
}

#
# return the interface type
#
sub interface
{
    my $self = shift;

    return $self->{ifc};
}

1;

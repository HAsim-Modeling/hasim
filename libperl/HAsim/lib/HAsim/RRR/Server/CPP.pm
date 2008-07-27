# *****************************************************************************
# * CPP.pm
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

package HAsim::RRR::Server::CPP;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Server::Base;
use HAsim::RRR::Method::Base;
use HAsim::RRR::Method::CPP;

# inherit from Server
our @ISA = qw(HAsim::RRR::Server::Base);

##
## constructor
##
sub new
{
    # get name of class
    my $class = shift;

    # get pointer to untyped server
    my $server = shift;

    # get list of untyped methods
    my @methodlist = @_;

    # typecast and insert the method list
    _addmethods($server, @methodlist);

    # typecast the object itself
    bless ($server, $class);

    # return typed object
    return $server;
}

##
## take a method list, create a CPP-type method from each of these,
## and add the typed methods to the server's method list
##
sub _addmethods
{
    my $server     = shift;
    my @methodlist = @_;

    # initialize server's methodlist
    @{ $server->{methodlist} } = ();

    # for each method in given list
    foreach my $method (@methodlist)
    {
        # create a new CPP-type method
        my $cpp_method = HAsim::RRR::Method::CPP->new($method);

        # add the typed method to the server's list
        push(@{ $server->{methodlist} }, $cpp_method);
    }
}

#
# print stub into a given file in cpp
#
sub print_stub
{
    # capture params
    my $self   = shift;
    my $file   = shift;

    # make sure it's a Bluespec target
    if ($self->{lang} ne "cpp")
    {
        die "CPP server asked to print non-CPP stub: " . $self->{lang};
    }    

    # determine if we should write stub at all
    if ($#{ $self->{methodlist} } == -1)
    {
        return;
    }

    # not implemented
}

1;

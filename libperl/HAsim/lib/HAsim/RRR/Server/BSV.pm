# *****************************************************************************
# * BSV.pm
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

package HAsim::RRR::Server::BSV;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Server::Base;
use HAsim::RRR::Method::Base;
use HAsim::RRR::Method::BSV;

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
## take a method list, create a BSV-type method from each of these,
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
        # create a new BSV-type method
        my $bsv_method = HAsim::RRR::Method::BSV->new($method);

        # add the typed method to the server's list
        push(@{ $server->{methodlist} }, $bsv_method);
    }
}

##
## print stub into a given file in bsv
## this method prints direct and proxy stubs
##
sub print_stub
{
    # capture params
    my $self   = shift;
    my $file   = shift;

    # make sure it's a Bluespec target
    if ($self->{lang} ne "bsv")
    {
        die "BSV server asked to print non-BSV stub: " . $self->{lang};
    }

    # determine if we should write stub at all
    if ($#{ $self->{methodlist} } == -1)
    {
        return;
    }

    # header, defines and includes
    print $file "//\n";
    print $file "// Synthesized server stub file\n";
    print $file "//\n";
    print $file "\n";

    print $file "`ifndef _" . $self->{name} . "_SERVER_STUB_\n";
    print $file "`define _" . $self->{name} . "_SERVER_STUB_\n";
    print $file "\n";

    print $file "`include \"rrr.bsh\"\n";
    print $file "`include \"channelio.bsh\"\n";
    print $file "`include \"umf.bsh\"\n";
    print $file "\n";

    # compute max request and response bitwidths
    my $maxinsize = 0;
    my $maxoutsize = 0;
    my @list = @{ $self->{methodlist} };
    foreach my $method (@list)
    {
        if ($method->inargs()->size() > $maxinsize)
        {
            $maxinsize = $method->inargs()->size();
        }
        
        if ($method->outargs()->size() > $maxoutsize)
        {
            $maxoutsize = $method->outargs()->size();
        }
    }

    # helper definition for service ID
    print $file "`define SERVICE_ID `" . $self->{name} ."_SERVICE_ID\n";
    print $file "\n";

    # types for each method: only print for direct stubs
    if ($self->{ifc} eq "method")
    {
        foreach my $method (@{ $self->{methodlist} })
        {
            $method->print_types($file);
        }
    }

    # interface ...
    print $file "interface ServerStub_" . $self->{name} . ";\n";

    # indent
    my $indent = "    ";

    # interface entry for each method
    foreach my $method (@{ $self->{methodlist} })
    {
        if ($self->{ifc} eq "connection")
        {
            $method->print_proxy_accept_request_declaration($file, $indent);
            $method->print_proxy_send_response_declaration($file, $indent);
        }
        else
        {
            $method->print_direct_accept_request_declaration($file, $indent);
            $method->print_direct_send_response_declaration($file, $indent);
        }
    }
    
    # endinterface
    print $file "endinterface\n";
    print $file "\n";
    
    # module mk...
    print $file "module mkServerStub_" . $self->{name} . "#(RRR_SERVER server)";
    print $file " (ServerStub_" . $self->{name} . ");\n";
    print $file "\n";
    
    # global state
    $self->_print_state($file, $maxinsize, $maxoutsize);
    
    # per-method state and definitions
    my $methodID = 0;
    foreach my $method (@{ $self->{methodlist} })
    {
        $method->print_server_state($file, $indent, $methodID);
        $methodID = $methodID + 1;
    }
    print $file "\n";    

    # global (i.e., not RRR-method-specific) rules
    $self->_print_request_rules($file);
    if ($maxoutsize != 0)
    {
        $self->_print_response_rules($file);
    }

    # method definitions
    foreach my $method (@{ $self->{methodlist} })
    {
        if ($self->{ifc} eq "connection")
        {
            $method->print_proxy_accept_request_definition($file, $indent);
            $method->print_proxy_send_response_definition($file, $indent);
        }
        else
        {
            $method->print_direct_accept_request_definition($file, $indent);
            $method->print_direct_send_response_definition($file, $indent);
        }
    }
    
    # endmodule
    print $file "endmodule\n";
    print $file "\n";

    # closing stamements
    print $file "`endif\n";
    print $file "\n";
}

#
# print global stub module state
#
sub _print_state
{
    my $self = shift;
    my $file = shift;

    my $maxinsize  = shift;
    my $maxoutsize = shift;

    print $file "    DEMARSHALLER#(UMF_CHUNK, Bit#($maxinsize)) dem <- mkDeMarshaller();\n";
    print $file "    Reg#(UMF_METHOD_ID) mid <- mkReg(0);\n";
    print $file "\n";
    if ($maxoutsize != 0)
    {
        print $file "    MARSHALLER#(Bit#($maxoutsize), UMF_CHUNK) mar <- mkMarshaller();\n";
        print $file "\n";
    }
}

#
# print global request rules for a service module
#
sub _print_request_rules
{
    my $self = shift;
    my $file = shift;

    print $file "    rule startRequest (True);\n";
    print $file "        UMF_PACKET packet <- server.requestPorts[`SERVICE_ID].read();\n";
    print $file "        mid <= packet.UMF_PACKET_header.methodID;\n";
    print $file "        dem.start(packet.UMF_PACKET_header.numChunks);\n";
    print $file "    endrule\n";
    print $file "\n";
    
    print $file "    rule continueRequest (True);\n";
    print $file "        UMF_PACKET packet <- server.requestPorts[`SERVICE_ID].read();\n";
    print $file "        dem.insert(packet.UMF_PACKET_dataChunk);\n";
    print $file "    endrule\n";
    print $file "\n";
}

#
# print global response rules for a service module
#
sub _print_response_rules
{
    my $self = shift;
    my $file = shift;

    print $file "    rule continueResponse (True);\n";
    print $file "        UMF_CHUNK chunk = mar.first();\n";
    print $file "        mar.deq();\n";
    print $file "        server.responsePorts[`SERVICE_ID].write(tagged UMF_PACKET_dataChunk chunk);\n";
    print $file "    endrule\n";
    print $file "\n";
}

#
# print connection instantiations
#
sub print_connections
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # verify that RRR file requested a connection for this server
    if ($self->{ifc} eq "connection")
    {
        # first instantiate server stub module
        print $file $indent                            .
                    "let stub_server_" . $self->{name} .
                    " <- mkServerStub_"                .
                    $self->{name} . "(server);\n"      .
                    "\n";

        # for each method
        foreach my $method (@{ $self->{methodlist} })
        {
            # ask method to print out a connection instantiation
            $method->print_server_connection($file, $indent, $self->{name});
        }
    }
    else
    {
        # error
        die "cannot print connections for \"method\" type servers";
    }
}

#
# print link rules
#
sub print_link_rules
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # check if RRR file requested a connection for this server
    if ($self->{ifc} eq "connection")
    {
        # for each method
        foreach my $method (@{ $self->{methodlist} })
        {
            # ask method to print out a set of link rules
            $method->print_server_link_rules($file, $indent, $self->{name});
        }
    }
}

######################################
#           REMOTE STUBS             #
######################################

##
## print remote stub into a given file in bsv
##
sub print_remote_stub
{
    # capture params
    my $self   = shift;
    my $file   = shift;

    # make sure it's a Bluespec target
    if ($self->{lang} ne "bsv")
    {
        return;
    }    

    # determine if we should write stub at all
    if ($#{ $self->{methodlist} } == -1)
    {
        return;
    }

    # interface should be connection
    if ($self->{ifc} ne "connection")
    {
        die "remote stubs are valid only for connection-type interfaces";
    }

    # generate header
    print $file "//\n";
    print $file "// Synthesized remote server stub file\n";
    print $file "//\n";
    print $file "\n";

    print $file "`ifndef _" . $self->{name} . "_REMOTE_SERVER_STUB_\n";
    print $file "`define _" . $self->{name} . "_REMOTE_SERVER_STUB_\n";
    print $file "\n";

    print $file "`include \"hasim_common.bsh\"\n";
    print $file "`include \"soft_connections.bsh\"\n";
    print $file "`include \"rrr.bsh\"\n";
    print $file "`include \"channelio.bsh\"\n";
    print $file "`include \"umf.bsh\"\n";
    print $file "\n";

    # compute max request and response bitwidths
    my $maxinsize = 0;
    my $maxoutsize = 0;
    my @list = @{ $self->{methodlist} };
    foreach my $method (@list)
    {
        if ($method->inargs()->size() > $maxinsize)
        {
            $maxinsize = $method->inargs()->size();
        }
        
        if ($method->outargs()->size() > $maxoutsize)
        {
            $maxoutsize = $method->outargs()->size();
        }
    }

    # helper definition for service ID
    print $file "`define SERVICE_ID `" . $self->{name} ."_SERVICE_ID\n";
    print $file "\n";

    # types for each method
    foreach my $method (@{ $self->{methodlist} })
    {
        $method->print_types($file);
    }

    # interface ...
    print $file "interface ServerStub_" . $self->{name} . ";\n";

    # indent
    my $indent = "    ";

    # interface entry for each method
    foreach my $method (@{ $self->{methodlist} })
    {
        # use same methods as non-remote declarations
        $method->print_remote_accept_request_declaration($file, $indent);
        $method->print_remote_send_response_declaration($file, $indent);
    }
    
    # endinterface
    print $file "endinterface\n";
    print $file "\n";
    
    # module mk...
    print $file "module [HASim_Module] mkServerStub_" . $self->{name};
    print $file " (ServerStub_" . $self->{name} . ");\n";
    print $file "\n";
    
    # instantiate connections
    foreach my $method (@{ $self->{methodlist} })
    {
        # ask method to print out a connection instantiation
        $method->print_remote_server_connection($file, $indent, $self->{name});
    }
    print $file "\n";
    
    # method definitions
    foreach my $method (@{ $self->{methodlist} })
    {
        $method->print_remote_accept_request_definition($file, $indent);
        $method->print_remote_send_response_definition($file, $indent);
    }
    
    # endmodule
    print $file "endmodule\n";
    print $file "\n";

    # closing stamements
    print $file "`endif\n";
    print $file "\n";
}

1;
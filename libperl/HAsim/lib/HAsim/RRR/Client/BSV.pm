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

package HAsim::RRR::Client::BSV;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Client::Base;
use HAsim::RRR::Method::Base;

# inherit from Client
our @ISA = qw(HAsim::RRR::Client::Base);

##
## constructor
##
sub new
{
    # get name of class
    my $class = shift;

    # get pointer to untyped client
    my $client = shift;

    # get list of untyped methods
    my @methodlist = @_;

    # typecast and insert the method list
    _addmethods($client, @methodlist);

    # typecast the object itself
    bless ($client, $class);

    # return typed object
    return $client;
}

##
## take a method list, create a BSV-type method from each of these,
## and add the typed methods to the client's method list
##
sub _addmethods
{
    my $client     = shift;
    my @methodlist = @_;

    # initialize client's methodlist
    @{ $client->{methodlist} } = ();

    # for each method in given list
    foreach my $method (@methodlist)
    {
        # create a new BSV-type method
        my $bsv_method = HAsim::RRR::Method::BSV->new($method, $client->{name});

        # add the typed method to the client's list
        push(@{ $client->{methodlist} }, $bsv_method);
    }
}

##
## print stub into a given file in bsv
##
sub print_stub
{
    # capture params
    my $self   = shift;
    my $file   = shift;

    # make sure it's a Bluespec target
    if ($self->{lang} ne "bsv")
    {
        die "BSV client asked to print non-BSV stub: " . $self->{lang};
    }    

    # determine if we should write stub at all
    if ($#{ $self->{methodlist} } == -1)
    {
        return;
    }

    # generate header
    print $file "//\n";
    print $file "// Synthesized client stub file\n";
    print $file "//\n";
    print $file "\n";

    print $file "`ifndef _" . $self->{name} . "_CLIENT_STUB_\n";
    print $file "`define _" . $self->{name} . "_CLIENT_STUB_\n";
    print $file "\n";

    if ($self->{ifc} eq "connection")
    {
        print $file "`include \"asim/provides/soft_connections.bsh\"\n";
    }
    print $file "`include \"asim/provides/rrr.bsh\"\n";
    print $file "`include \"asim/provides/rrr_common.bsh\"\n";
    print $file "`include \"asim/provides/umf.bsh\"\n";
    print $file "\n";
    print $file "`include \"asim/rrr/service_ids.bsh\"\n";
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
    print $file "interface ClientStub_" . $self->{name} . ";\n";

    # indent
    my $indent = "    ";

    # interface entry for each method
    foreach my $method (@{ $self->{methodlist} })
    {
        $method->print_make_request_declaration($file, $indent, $self->{ifc});
        $method->print_get_response_declaration($file, $indent, $self->{ifc});
    }
    
    # endinterface
    print $file "endinterface\n";
    print $file "\n";
    
    # module mk...
    if ($self->{ifc} eq "connection")
    {
        print $file "module [CONNECTED_MODULE] mkClientStub_" . $self->{name};
    }
    else
    {
        print $file "module mkClientStub_" . $self->{name} . "#(RRR_CLIENT client)";
    }
    print $file " (ClientStub_" . $self->{name} . ");\n";
    print $file "\n";
    
    # global state
    $self->_print_state($file, $maxinsize, $maxoutsize);
    
    # per-method state and definitions
    my $methodID = 0;
    foreach my $method (@{ $self->{methodlist} })
    {
        $method->print_client_state($file, $indent, $methodID);
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
        $method->print_make_request_definition($file, $indent, $self->{ifc});
        $method->print_get_response_definition($file, $indent, $self->{ifc});
    }
    
    # endmodule
    print $file "endmodule\n";
    print $file "\n";

    # closing stamements
    print $file "`endif\n";
    print $file "\n";
}

##
## print global stub module state
##
sub _print_state
{
    my $self = shift;
    my $file = shift;

    my $maxinsize  = shift;
    my $maxoutsize = shift;

    if ($self->{ifc} eq "connection")
    {
        print $file "    Connection_Send#(UMF_PACKET)    link_req  <- mkConnection_Send(\"rrr_client_" .
            $self->{name} . "_req\");\n";
        print $file "    Connection_Receive#(UMF_PACKET) link_resp <- mkConnection_Receive(\"rrr_client_" .
            $self->{name} . "_resp\");\n";
        print $file "\n";
    }

    print $file "    MARSHALLER#(Bit#($maxinsize), UMF_CHUNK) mar <- mkMarshaller();\n";

    if ($maxoutsize != 0)
    {
        print $file "    DEMARSHALLER#(UMF_CHUNK, Bit#($maxoutsize)) dem <- mkDeMarshaller();\n";
        print $file "\n";
        print $file "    Reg#(UMF_METHOD_ID) mid <- mkReg(0);\n";
    }

    print $file "\n";
}

##
## print global request rules for a client module
##
sub _print_request_rules
{
    my $self = shift;
    my $file = shift;

    print $file "    rule continueRequest (True);\n";
    print $file "        UMF_CHUNK chunk = mar.first();\n";
    print $file "        mar.deq();\n";
    if ($self->{ifc} eq "connection")
    {
        print $file "        link_req.send(tagged UMF_PACKET_dataChunk chunk);\n";
    }
    else
    {
        print $file "        client.requestPorts[`SERVICE_ID].write(tagged UMF_PACKET_dataChunk chunk);\n";
    }
    print $file "    endrule\n";
    print $file "\n";
}

##
## print global response rules for a client module
##
sub _print_response_rules
{
    my $self = shift;
    my $file = shift;

    print $file "    rule startResponse (True);\n";
    if ($self->{ifc} eq "connection")
    {
        print $file "        UMF_PACKET packet = link_resp.receive();\n";
        print $file "        link_resp.deq();\n";
    }
    else
    {
        print $file "        UMF_PACKET packet <- client.responsePorts[`SERVICE_ID].read();\n";
    }
    print $file "        mid <= packet.UMF_PACKET_header.methodID;\n";
    print $file "        dem.start(packet.UMF_PACKET_header.numChunks);\n";
    print $file "    endrule\n";
    print $file "\n";
    
    print $file "    rule continueResponse (True);\n";
    if ($self->{ifc} eq "connection")
    {
        print $file "        UMF_PACKET packet = link_resp.receive();\n";
        print $file "        link_resp.deq();\n";        
    }
    else
    {
        print $file "        UMF_PACKET packet <- client.responsePorts[`SERVICE_ID].read();\n";
    }
    print $file "        dem.insert(packet.UMF_PACKET_dataChunk);\n";
    print $file "    endrule\n";
    print $file "\n";
}

##
## print connection instantiations for wrapper client_connections module
## in platform interface
##
sub print_connections
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # verify that RRR file requested a connection for this client
    if ($self->{ifc} eq "connection")
    {
        my $name = $self->{name};

        # request connection
        print $file "$indent Connection_Receive#(UMF_PACKET) link_client_$name\_req  <- " .
                    "mkConnection_Receive(\"rrr_client_$name\_req\");\n";

        # response connection
        print $file "$indent Connection_Send#(UMF_PACKET)    link_client_$name\_resp <- " .
                    "mkConnection_Send(\"rrr_client_$name\_resp\");\n";
    }
    else
    {
        # error
        die "cannot print connections for \"method\" type clients";
    }
}

##
## print link rules
##
sub print_link_rules
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # check if RRR file requested a connection for this client
    if ($self->{ifc} eq "connection")
    {
        my $name = $self->{name};

        print $file "$indent rule client_$name\_req (True);\n";
        print $file "$indent     client.requestPorts[`$name\_SERVICE_ID].write(link_client_$name\_req.receive());\n";
        print $file "$indent     link_client_$name\_req.deq();\n";
        print $file "$indent endrule\n";
        print $file "\n";

        print $file "$indent rule client_$name\_resp (True);\n";
        print $file "$indent     let chunk <- client.responsePorts[`$name\_SERVICE_ID].read();\n";
        print $file "$indent     link_client_$name\_resp.send(chunk);\n";
        print $file "$indent endrule\n";
        print $file "\n";
    }
}

1;

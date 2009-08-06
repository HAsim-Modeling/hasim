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
## this method prints direct and proxy stubs, depending on {ifc}
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

    print $file "`include \"asim/provides/rrr.bsh\"\n";
    print $file "`include \"asim/provides/channelio.bsh\"\n";
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

    # types for each method: only print them for direct stubs
    if ($self->{ifc} eq "method")
    {
        foreach my $method (@{ $self->{methodlist} })
        {
            $method->print_types($file);
        }
    }

    # interface ...
    print $file "interface ClientStub_" . $self->{name} . ";\n";

    # indent
    my $indent = "    ";

    # interface entry for each method
    foreach my $method (@{ $self->{methodlist} })
    {
        if ($self->{ifc} eq "connection")
        {
            $method->print_proxy_make_request_declaration($file, $indent);
            $method->print_proxy_get_response_declaration($file, $indent);
        }
        else
        {
            $method->print_direct_make_request_declaration($file, $indent);
            $method->print_direct_get_response_declaration($file, $indent);
        }
    }
    
    # endinterface
    print $file "endinterface\n";
    print $file "\n";
    
    # module mk...
    print $file "module mkClientStub_" . $self->{name} . "#(RRR_CLIENT client)";
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
        if ($self->{ifc} eq "connection")
        {
            $method->print_proxy_make_request_definition($file, $indent);
            $method->print_proxy_get_response_definition($file, $indent);
        }
        else
        {
            $method->print_direct_make_request_definition($file, $indent);
            $method->print_direct_get_response_definition($file, $indent);
        }
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

    print $file "    MARSHALLER#(Bit#($maxinsize), UMF_CHUNK) mar <- mkMarshaller();\n";
    print $file "\n";

    if ($maxoutsize != 0)
    {
        print $file "    DEMARSHALLER#(UMF_CHUNK, Bit#($maxoutsize)) dem <- mkDeMarshaller();\n";
        print $file "    Reg#(UMF_METHOD_ID) mid <- mkReg(0);\n";
        print $file "\n";
    }
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
    print $file "        client.requestPorts[`SERVICE_ID].write(tagged UMF_PACKET_dataChunk chunk);\n";
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
    print $file "        UMF_PACKET packet <- client.responsePorts[`SERVICE_ID].read();\n";
    print $file "        mid <= packet.UMF_PACKET_header.methodID;\n";
    print $file "        dem.start(packet.UMF_PACKET_header.numChunks);\n";
    print $file "    endrule\n";
    print $file "\n";
    
    print $file "    rule continueResponse (True);\n";
    print $file "        UMF_PACKET packet <- client.responsePorts[`SERVICE_ID].read();\n";
    print $file "        dem.insert(packet.UMF_PACKET_dataChunk);\n";
    print $file "    endrule\n";
    print $file "\n";
}

##
## print connection instantiations
##
sub print_connections
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # verify that RRR file requested a connection for this client
    if ($self->{ifc} eq "connection")
    {
        # first instantiate client stub module
        print $file $indent                       .
                    "let stub_client_" . $self->{name}   .
                    " <- mkClientStub_"          .
                    $self->{name} . "(client);\n" .
                    "\n";

        # for each method
        foreach my $method (@{ $self->{methodlist} })
        {
            # ask method to print out a connection instantiation
            $method->print_client_connection($file, $indent, $self->{name});
        }

        # print control ID link
        print $file $indent                                              .
                    "Connection_Receive#(UMF_METHOD_ID) control_client_" .
                    $self->{name}                                        .
                    " <- mkConnection_Receive(\"control_client_"         .
                    $self->{name}                                        .
                    "\");\n";
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
        # peek into current control ID for this service
        print $file $indent                    .
                    "UMF_METHOD_ID controlID_" .
                    $self->{name}              .
                    " = control_client_"       .
                    $self->{name}              .
                    ".receive();\n\n";

        # Hack:  give priority to low method ids.  This lets us guarantee
        # delivery of one method ahead of another.
        # Start by gathering method names...
        my @names = ();
        foreach my $method (@{ $self->{methodlist} })
        {
            push(@names, $method->client_link_rule_names($self->{name}));
        }
        if ($#names > 0)
        {
            print $file "${indent}(* descending_urgency= \"" . join(', ', @names) . "\" *)\n\n";
        }

        # for each method
        foreach my $method (@{ $self->{methodlist} })
        {
            # ask method to print out a set of link rules
            $method->print_client_link_rules($file, $indent, $self->{name});
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
    print $file "// Synthesized remote client stub file\n";
    print $file "//\n";
    print $file "\n";

    print $file "`ifndef _" . $self->{name} . "_REMOTE_CLIENT_STUB_\n";
    print $file "`define _" . $self->{name} . "_REMOTE_CLIENT_STUB_\n";
    print $file "\n";

    print $file "`include \"asim/provides/hasim_common.bsh\"\n";
    print $file "`include \"asim/provides/soft_connections.bsh\"\n";
    print $file "`include \"asim/provides/rrr.bsh\"\n";
    print $file "`include \"asim/provides/channelio.bsh\"\n";
    print $file "`include \"asim/provides/umf.bsh\"\n";
    print $file "\n";
    print $file "`include \"asim/rrr/service_ids.bsh\"\n";
    print $file "`include \"asim/rrr/client_control_ids.bsh\"\n";
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
        # use same methods as non-remote declarations
        $method->print_remote_make_request_declaration($file, $indent);
        $method->print_remote_get_response_declaration($file, $indent);
    }
    
    # endinterface
    print $file "endinterface\n";
    print $file "\n";
    
    # module mk...
    print $file "module [HASIM_MODULE] mkClientStub_" . $self->{name};
    print $file " (ClientStub_" . $self->{name} . ");\n";
    print $file "\n";
    
    # instantiate connections
    foreach my $method (@{ $self->{methodlist} })
    {
        # ask method to print out a connection instantiation
        $method->print_remote_client_connection($file, $indent, $self->{name});
    }
    print $file "\n";
    
    # print control ID link
    print $file $indent                                           .
                "Connection_Send#(UMF_METHOD_ID) control "        .
                "<- mkConnection_Send(\"control_client_"          .
                $self->{name}                                     .
                "\");\n\n";

    # method definitions
    foreach my $method (@{ $self->{methodlist} })
    {
        $method->print_remote_make_request_definition($file, $indent, $self->{name});
        $method->print_remote_get_response_definition($file, $indent, $self->{name});
    }
    
    # endmodule
    print $file "endmodule\n";
    print $file "\n";

    # closing stamements
    print $file "`endif\n";
    print $file "\n";
}

######################################
#    CONTROL IDS FOR REMOTE STUB     #
######################################

##
## print control IDs into a given file in bsv
##
sub print_control_ids
{
    # capture params
    my $self   = shift;
    my $file   = shift;

    # make sure it's a Bluespec target
    if ($self->{lang} ne "bsv")
    {
        return;
    }    

    # interface should be connection
    if ($self->{ifc} ne "connection")
    {
        die "control IDs are valid only for connection-type interfaces";
    }

    # print entry for each method
    my $controlID = 0;
    foreach my $method (@{ $self->{methodlist} })
    {
        print $file "\`define "     .
                    $self->{name}   .
                    "_"             .
                    $method->name() .
                    "_CONTROL_ID "  .
                    $controlID      .
                    "\n";
        $controlID = $controlID + 1;
    }
}

1;

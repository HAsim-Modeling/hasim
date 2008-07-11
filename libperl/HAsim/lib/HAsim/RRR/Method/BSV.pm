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

package HAsim::RRR::Method::BSV;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Method;

# inherit from Method
our @ISA = qw(HAsim::RRR::Method);

##
## constructor
##
sub new
{
    # get name of class
    my $class = shift;

    # get pointer to untyped method
    my $method = shift;

    # create a new typed method
    my $typed_method = _semi_deep_copy($method);

    # typecast the object
    bless ($typed_method, $class);

    # return typed object
    return $typed_method;
}

##
## create a new method hash by copying over the contents
## of the input hash
##
sub _semi_deep_copy
{
    my $source = shift;

    # copy all fields. Note that in many cases we are merely
    # copying the references to the objects in the original hash,
    # which is exactly what we want.
    my %target;

    $target{name}   = $source->{name};
    $target{inarg}  = $source->{inarg};
    $target{outarg} = $source->{outarg};

    return \%target;
}

######################################
#           SERVER STUBS             #
######################################

##### ACCEPT_REQUEST STUB PRINTING #####

##
## print accept_request declaration
##
sub print_accept_request_declaration
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # get indentation
    my $indent = shift;

    my $pack = shift;

    # print method header after indenting it
    print $file $indent;
    _print_accept_request_header($self, $file, $pack);
    print $file ";\n";
}

##
## print accept_request definition
##
sub print_accept_request_definition
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # get indentation
    my $indent = shift;

    # pack?
    my $pack = shift;

    # print method header after indenting it
    print $file $indent;
    _print_accept_request_header($self, $file, $pack);

    # conditions
    print $file " if (mid == fromInteger(mid_";
    print $file $self->{name};
    print $file "));\n";

    # body
    print $file $indent . "    let a <- dem.readAndDelete();\n";
    print $file $indent . "    ";

    # TODO: support multiple params
    print $file $self->inarg()->type()->string_bsv($pack);
    print $file " retval = unpack(truncate(a));\n";
    print $file $indent . "    return retval;\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##
## print remote accept_request definition that wraps the remote
## (module) end of a connection
##
sub print_remote_accept_request_definition
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # get indentation
    my $indent = shift;

    # force pack to 0
    my $pack = 0;

    # print method header after indenting it
    print $file $indent;
    _print_accept_request_header($self, $file, $pack);

    # no conditions
    print $file ";\n";

    # body
    if (defined($self->outarg()))
    {
        # server-type connection
        print $file $indent . "    let a = link_" . $self->{name} . ".getReq();\n";
        print $file $indent . "    link_" . $self->{name} . ".deq();\n";
        print $file $indent . "    return unpack(a);\n";
    }
    else
    {
        # receive-type connection
        print $file $indent . "    let a = link_" . $self->{name} . ".receive();\n";
        print $file $indent . "    link_" . $self->{name} . ".deq();\n";
        print $file $indent . "    return unpack(a);\n";
    }

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##
## print accept_request header
##
sub _print_accept_request_header
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # pack?
    my $pack = shift;

    # print into file
    print $file "method ActionValue#(" .
                $self->inarg()->type()->string_bsv($pack) .
                ") ";

    # method name
    print $file "acceptRequest_" .
                $self->{name}    .
                "()";
}

##### SEND_RESPONSE STUB PRINTING #####

##
## print send_response declaration
##
sub print_send_response_declaration
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;
    my $pack = shift;

    # return if we don't need a response
    if (!defined($self->outarg()))
    {
        return;
    }

    # print method header after indenting it
    print $file $indent;
    _print_send_response_header($self, $file, $pack);
    print $file ";\n";
}

##
## print send_response definition
##
sub print_send_response_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;
    my $pack = shift;

    # return if we don't need a response
    if (!defined($self->outarg()))
    {
        return;
    }

    # print method header after indenting it
    print $file $indent;
    _print_send_response_header($self, $file, $pack);
    print $file ";\n";

    # body
    print $file $indent . "    UMF_PACKET header = tagged UMF_PACKET_header\n";
    print $file $indent . "                        {\n";
    print $file $indent . "                            channelID: ?,\n";
    print $file $indent . "                            serviceID: `SERVICE_ID,\n";
    print $file $indent . "                            methodID : fromInteger(mid_";
    print $file $self->{name};
    print $file "),\n";
    print $file $indent . "                            numChunks: fromInteger(numChunks_";
    print $file $self->{name};
    print $file ")\n";
    print $file $indent . "                        };\n";
    print $file $indent . "    server.responsePorts[`SERVICE_ID].write(header);\n";
    print $file $indent . "    mar.enq(zeroExtend(pack(resp)), fromInteger(numChunks_";
    print $file $self->{name};
    print $file "));\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##
## print remote send_response definition that wraps the remote
## (module) end of a connection
##
sub print_remote_send_response_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # force pack to 0
    my $pack = 0;

    # return if we don't need a response
    if (!defined($self->outarg()))
    {
        return;
    }

    # print method header after indenting it
    print $file $indent;
    _print_send_response_header($self, $file, $pack);
    print $file ";\n";

    # body
    print $file $indent . "    link_" . $self->{name} . ".makeResp(pack(resp));\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##
## print send_response header
##
sub _print_send_response_header
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;
    my $pack = shift;

    # print into file
    print $file "method Action sendResponse_";

    # method name
    print $file $self->{name} . "("                   .
                $self->outarg()->type()->string_bsv($pack) .
                " resp)"; # std formal param
}

##### CONNECTION RULES #####

##
## print connection instantiation
##
sub print_server_connection
{
    my $self        = shift;
    my $file        = shift;
    my $indent      = shift;
    my $servicename = shift;

    # force pack to 1
    my $pack = 1;

    # print connection definitions
    if (!defined($self->outarg()))
    {
        # no return: create Send-type connection
        print $file $indent . "Connection_Send#("        .
                    $self->inarg()->type()->string_bsv($pack) .
                    ") link_server_$servicename" . "_"          .
                    $self->{name}                        .
                    " <- mkConnection_Send(\"rrr_server_$servicename\_" .
                    $self->{name} . "\");\n";
    }
    else
    {
        # need return: create Client-type connection
        print $file $indent . "Connection_Client#("       .
                    $self->inarg()->type()->string_bsv($pack)  .
                    ", "                                  .
                    $self->outarg()->type()->string_bsv($pack) .
                    ") link_server_$servicename" . "_"           .
                    $self->{name}                         .
                    " <- mkConnection_Client(\"rrr_server_$servicename\_" .
                    $self->{name} . "\");\n";
    }
}

##
## print rules to link method calls to a connection
##
sub print_server_link_rules
{
    my $self        = shift;
    my $file        = shift;
    my $indent      = shift;
    my $servicename = shift;

    # force pack to 1
    my $pack = 1;

    my $methodname = $self->{name};
    my $insize     = $self->insize();
    my $intype     = $self->inarg()->type()->string_bsv($pack);
    
    # print rules to transfer request and response to the connection
    if (!defined($self->outarg()))
    {
        # no return: create only request rule, use send()
        print $file $indent . "rule server_acceptRequest_$servicename\_$methodname (True);\n";
        print $file $indent . "    $intype req <- stub_server_$servicename.acceptRequest\_$methodname();\n";
        print $file $indent . "    link_server_$servicename\_$methodname.send(req);\n";
        print $file $indent . "endrule\n";
        print $file $indent . "\n";
    }
    else
    {
        my $outsize = $self->outsize();
        my $outtype = $self->outarg()->type()->string_bsv($pack);

        # need return: create request and response rules, use makeReq()
        print $file $indent . "rule server_acceptRequest_$servicename\_$methodname (True);\n";
        print $file $indent . "    $intype req <- stub_server_$servicename.acceptRequest\_$methodname();\n";
        print $file $indent . "    link_server_$servicename\_$methodname.makeReq(req);\n";
        print $file $indent . "endrule\n";
        print $file $indent . "\n";
        
        print $file $indent . "rule server_sendResponse_$servicename\_$methodname (True);\n";
        print $file $indent . "    $outtype resp = link_server_$servicename\_$methodname.getResp();\n";
        print $file $indent . "    link_server_$servicename\_$methodname.deq();\n";
        print $file $indent . "    stub_server_$servicename.sendResponse\_$methodname(resp);\n";
        print $file $indent . "endrule\n";
        print $file $indent . "\n";
    }
}

##
## print remote server connection wrapper instantiation
##
sub print_remote_server_connection
{
    my $self        = shift;
    my $file        = shift;
    my $indent      = shift;
    my $servicename = shift;

    # force pack to 1
    my $pack = 1;

    # print connection definitions
    if (!defined($self->outarg()))
    {
        # no return: create receive-type connection
        print $file $indent . "Connection_Receive#("          .
                    $self->inarg()->type()->string_bsv($pack) .
                    ") link_"                                 .
                    $self->{name}                             .
                    " <- mkConnection_Receive(\"rrr_server_$servicename\_" .
                    $self->{name} . "\");\n";
    }
    else
    {
        # need return: create server-type connection
        print $file $indent . "Connection_Server#("            .
                    $self->inarg()->type()->string_bsv($pack)  .
                    ", "                                       .
                    $self->outarg()->type()->string_bsv($pack) .
                    ") link_"                                  .
                    $self->{name}                              .
                    " <- mkConnection_Server(\"rrr_server_$servicename\_" .
                    $self->{name} . "\");\n";
    }
}

######################################
#           CLIENT STUBS             #
######################################

##### MAKE_REQUEST STUB PRINTING #####

##
## print make_request declaration
##
sub print_make_request_declaration
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # get indentation
    my $indent = shift;

    # pack?
    my $pack = shift;

    # print method header after indenting it
    print $file $indent;
    _print_make_request_header($self, $file, $pack);
    print $file ";\n";
}

##
## print make_request definition
##
sub print_make_request_definition
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # get indentation
    my $indent = shift;

    # pack?
    my $pack = shift;

    # print method header after indenting it
    print $file $indent;
    _print_make_request_header($self, $file, $pack);
    print $file ";\n";

    # body
    print $file $indent . "    UMF_PACKET header = tagged UMF_PACKET_header\n";
    print $file $indent . "                        {\n";
    print $file $indent . "                            channelID: ?,\n";
    print $file $indent . "                            serviceID: `SERVICE_ID,\n";
    print $file $indent . "                            methodID : fromInteger(mid_";
    print $file $self->{name};
    print $file "),\n";
    print $file $indent . "                            numChunks: fromInteger(numChunks_";
    print $file $self->{name};
    print $file ")\n";
    print $file $indent . "                        };\n";
    print $file $indent . "    client.requestPorts[`SERVICE_ID].write(header);\n";
    print $file $indent . "    mar.enq(zeroExtend(pack(req)), fromInteger(numChunks_";
     print $file $self->{name};
    print $file "));\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##
## print remote make_request definition that wraps the remote
## (module) end of a connection
##
sub print_remote_make_request_definition
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # get indentation
    my $indent = shift;

    # force pack to 0 for header printing
    my $pack = 0;

    # print method header after indenting it
    print $file $indent;
    _print_make_request_header($self, $file, $pack);
    print $file ";\n";

    # body
    if (defined($self->outarg()))
    {
        # client-type connection
        print $file $indent . "    link_" . $self->{name} . ".makeReq(pack(req));\n";
    }
    else
    {
        # send-type connection
        print $file $indent . "    link_" . $self->{name} . ".send(pack(req));\n";
    }

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##
## print make_request header
##
sub _print_make_request_header
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;
    my $pack = shift;

    # print into file
    print $file "method Action makeRequest_";

    # method name
    print $file $self->{name} . "("                  .
                $self->inarg()->type()->string_bsv($pack) .
                " req)"; # std formal param
}

##### GET_RESPONSE STUB PRINTING #####

##
## print get_response declaration
##
sub print_get_response_declaration
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # get indentation
    my $indent = shift;

    # pack?
    my $pack = shift;

    # return if we don't need a response
    if (!defined($self->outarg()))
    {
        return;
    }

    # print method header after indenting it
    print $file $indent;
    _print_get_response_header($self, $file, $pack);
    print $file ";\n";
}

##
## print get_response definition
##
sub print_get_response_definition
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # get indentation
    my $indent = shift;

    # pack?
    my $pack = shift;

    # return if we don't need a response
    if (!defined($self->outarg()))
    {
        return;
    }

    # print method header after indenting it
    print $file $indent;
    _print_get_response_header($self, $file, $pack);

    # conditions
    print $file " if (mid == fromInteger(mid_";
    print $file $self->{name};
    print $file "));\n";

    # body
    print $file $indent . "    let a <- dem.readAndDelete();\n";
    print $file $indent . "    ";
    print $file $self->outarg()->type()->string_bsv($pack);
    print $file " retval = unpack(truncate(a));\n";
    print $file $indent . "    return retval;\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##
## print remote get_response definition that wraps the remote
## (module) end of a connection
##
sub print_remote_get_response_definition
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;

    # get indentation
    my $indent = shift;

    # force pack to 0 for header printing
    my $pack = 0;

    # return if we don't need a response
    if (!defined($self->outarg()))
    {
        return;
    }

    # print method header after indenting it
    print $file $indent;
    _print_get_response_header($self, $file, $pack);

    # no conditions
    print $file ";\n";

    # body
    print $file $indent . "    let a = link_" . $self->{name} . ".getResp();\n";
    print $file $indent . "    link_" . $self->{name} . ".deq();\n";
    print $file $indent . "    return unpack(a);\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##
## print get_response header
##
sub _print_get_response_header
{
    # get object
    my $self = shift;

    # get file handle
    my $file = shift;
    my $pack = shift;

    # print into file
    print $file "method ActionValue#("                .
                $self->outarg()->type()->string_bsv($pack) .
                ") ";

    # method name
    print $file "getResponse_" .
                $self->{name}  .
                "()";
}

##### CONNECTION RULES #####

##
## print connection instantiation
##
sub print_client_connection
{
    my $self        = shift;
    my $file        = shift;
    my $indent      = shift;
    my $servicename = shift;

    # force pack to 1
    my $pack = 1;

    # print connection definitions
    if (!defined($self->outarg()))
    {
        # no return: create Receive-type connection
        print $file $indent . "Connection_Receive#("        .
                    $self->inarg()->type()->string_bsv($pack) .
                    ") link_client_$servicename" . "_"          .
                    $self->{name}                        .
                    " <- mkConnection_Receive(\"rrr_client_$servicename\_" .
                    $self->{name} . "\");\n";
    }
    else
    {
        # need return: create Server-type connection
        print $file $indent . "Connection_Server#("       .
                    $self->inarg()->type()->string_bsv($pack)  .
                    ", "                                  .
                    $self->outarg()->type()->string_bsv($pack) .
                    ") link_client_$servicename" . "_"           .
                    $self->{name}                         .
                    " <- mkConnection_Server(\"rrr_client_$servicename\_" .
                    $self->{name} . "\");\n";
    }
}

##
## return an array of names that will be emitted for client rules
##
sub client_link_rule_names
{
    my $self        = shift;
    my $servicename = shift;

    my $methodname = $self->{name};
    
    if (!defined($self->outarg()))
    {
        return ( "client_makeRequest_$servicename\_$methodname" );
    }
    else
    {
        return ( "client_makeRequest_$servicename\_$methodname",
                 "client_getResponse_$servicename\_$methodname" );
    }
}

##
## print rules to link method calls to a connection
##
sub print_client_link_rules
{
    my $self        = shift;
    my $file        = shift;
    my $indent      = shift;
    my $servicename = shift;

    # force pack to 1
    my $pack = 1;

    my $methodname = $self->{name};
    my $insize     = $self->insize();
    my $intype     = $self->inarg()->type()->string_bsv($pack);
    
    # print rules to transfer request and response to the connection
    if (!defined($self->outarg()))
    {
        # no return: create only request rule, use receive()
        print $file $indent . "rule client_makeRequest_$servicename\_$methodname (True);\n";
        print $file $indent . "    $intype req = link_client_$servicename\_$methodname.receive();\n";
        print $file $indent . "    link_client_$servicename\_$methodname.deq();\n";
        print $file $indent . "    stub_client_$servicename.makeRequest\_$methodname(req);\n";
        print $file $indent . "endrule\n";
        print $file $indent . "\n";
    }
    else
    {
        my $outsize = $self->outsize();
        my $outtype = $self->outarg()->type()->string_bsv($pack);

        # need return: create request and response rules, use getReq()
        print $file $indent . "rule client_makeRequest_$servicename\_$methodname (True);\n";
        print $file $indent . "    $intype req = link_client_$servicename\_$methodname.getReq();\n";
        print $file $indent . "    link_client_$servicename\_$methodname.deq();\n";
        print $file $indent . "    stub_client_$servicename.makeRequest\_$methodname(req);\n";
        print $file $indent . "endrule\n";
        print $file $indent . "\n";

        print $file $indent . "rule client_getResponse_$servicename\_$methodname (True);\n";
        print $file $indent . "    $outtype resp <- stub_client_$servicename.getResponse\_$methodname();\n";
        print $file $indent . "    link_client_$servicename\_$methodname.makeResp(resp);\n";
        print $file $indent . "endrule\n";
        print $file $indent . "\n";
    }
}

##
## print remote client connection wrapper instantiation
##
sub print_remote_client_connection
{
    my $self        = shift;
    my $file        = shift;
    my $indent      = shift;
    my $servicename = shift;

    # force pack to 1
    my $pack = 1;

    # print connection definitions
    if (!defined($self->outarg()))
    {
        # no return: create send-type connection
        print $file $indent . "Connection_Send#("             .
                    $self->inarg()->type()->string_bsv($pack) .
                    ") link_"                                 .
                    $self->{name}                             .
                    " <- mkConnection_Send(\"rrr_client_$servicename\_" .
                    $self->{name} . "\");\n";
    }
    else
    {
        # need return: create client-type connection
        print $file $indent . "Connection_Client#("            .
                    $self->inarg()->type()->string_bsv($pack)  .
                    ", "                                       .
                    $self->outarg()->type()->string_bsv($pack) .
                    ") link_"                                  .
                    $self->{name}                              .
                    " <- mkConnection_Client(\"rrr_client_$servicename\_" .
                    $self->{name} . "\");\n";
    }
}

######################################
#         OTHER STUB STATE           #
######################################

##
## print server state
##
sub print_server_state
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;
    my $id     = shift;

    print $file $indent . "Integer mid_" . $self->{name} . " = $id;\n";

    if ($self->outarg())
    {
        my $outsize = $self->outsize();

        print $file $indent . "Integer numChunks_" .
                              $self->{name}        .
                              " = ($outsize % `UMF_CHUNK_BITS) == 0 ?\n";
        print $file $indent . "    ($outsize / `UMF_CHUNK_BITS) :\n";
        print $file $indent . "    ($outsize / `UMF_CHUNK_BITS) + 1;\n";
    }
}

##
## print client state
##
sub print_client_state
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;
    my $id     = shift;

    print $file $indent . "Integer mid_" . $self->{name} . " = $id;\n";

    my $insize = $self->insize();
    
    print $file $indent . "Integer numChunks_" .
                $self->{name}        .
                " = ($insize % `UMF_CHUNK_BITS) == 0 ?\n";
    print $file $indent . "    ($insize / `UMF_CHUNK_BITS) :\n";
    print $file $indent . "    ($insize / `UMF_CHUNK_BITS) + 1;\n";
}

1;

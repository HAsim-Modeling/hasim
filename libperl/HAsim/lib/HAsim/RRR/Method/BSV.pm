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

use HAsim::RRR::Method::Base;

# inherit from Method
our @ISA = qw(HAsim::RRR::Method::Base);

##
## constructor
##
sub new
{
    # get name of class
    my $class = shift;

    # get pointer to untyped method
    my $method = shift;

    # service name
    my $servicename = shift;

    # create a new typed method
    my $typed_method = _semi_deep_copy($method, $servicename);

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
    my $servicename = shift;

    # copy all fields. Note that in many cases we are merely
    # copying the references to the objects in the original hash,
    # which is exactly what we want.
    my $target;

    $target->{name}        = $source->{name};
    $target->{servicename} = $servicename;

    # copy over the arg lists, but type case them into BSV
    $target->{inargs}  = HAsim::RRR::Arglist::BSV->new($source->{inargs});
    $target->{outargs} = HAsim::RRR::Arglist::BSV->new($source->{outargs});

    return $target;
}

######################################
#               TYPES                #
######################################

##
## print type definitions
##
sub print_types
{
    my $self = shift;
    my $file = shift;

    # input
    if ($self->inargs()->num() > 1)
    {
        # create a struct
        print $file "typedef "                     .
                    $self->inargs()->makestruct()  .
                    $self->_intype_name()          .
                    "\n    deriving (Bits, Eq);\n";
    }
    else
    {
        # use type of lone element in arg list
        print $file "typedef "                 .
                $self->inargs()->singletype()  .
                " "                            .
                $self->_intype_name()          .
                ";\n";
    }

    # output
    if ($self->outargs()->num() > 1)
    {
        # create a struct
        print $file "typedef "                     .
                    $self->outargs()->makestruct() .
                    $self->_outtype_name()         .
                    "\n    deriving (Bits, Eq);\n";
    }
    elsif ($self->outargs()->num() == 1)
    {
        print $file "typedef "                     .
                    $self->outargs()->singletype() .
                    " "                            .
                    $self->_outtype_name()         .
                    ";\n";
    }
    else
    {
        # no output args, don't print anything
    }

    print $file "\n";
}

######################################
#              GENERAL               #
######################################

##
## create a "get"-type header
##
sub _make_get_header
{
    my $self        = shift;
    my $methodclass = shift;
    my $typestring  = shift;
    
    my $string = "method ActionValue#(" .
                 $typestring            .
                 ") "                   .
                 $methodclass           .
                 "_"                    .
                 $self->{name}          .
                 "()";
}

##
## create a "put"-type header
##
sub _make_put_header
{
    my $self        = shift;
    my $methodclass = shift;
    my $argstring   = shift;
    
    my $string = "method Action " .
                 $methodclass     .
                 "_"              .
                 $self->{name}    .
                 "("              .
                 $argstring       .
                 ")";
}

######################################
#           SERVER STUBS             #
######################################

##### ACCEPT_REQUEST STUB PRINTING #####

##
## print direct accept_request declaration
##
sub print_direct_accept_request_declaration
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    print $file $indent                                      .
                $self->_make_get_header("acceptRequest",
                                        $self->_intype_name()) .
                ";\n";
}

##
## print proxy accept_request declaration
##
sub print_proxy_accept_request_declaration
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    print $file $indent                                      .
                $self->_make_get_header("acceptRequest",
                                        $self->inargs()->makebitvector()) .
                ";\n";
}

##
## print remote accept_request declaration
##
sub print_remote_accept_request_declaration
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    print $file $indent                                      .
                $self->_make_get_header("acceptRequest",
                                        $self->_intype_name()) .
                ";\n";
}

##
## print direct accept_request definition
##
sub print_direct_accept_request_definition
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # header
    print $file $indent                             .
                $self->_make_get_header("acceptRequest",
                                        $self->_intype_name());

    # conditions
    print $file " if (mid == fromInteger(mid_";
    print $file $self->{name};
    print $file "));\n";

    # body
    print $file $indent . "    let a <- dem.readAndDelete();\n";
    print $file $indent . "    ";

    # acceptRequest()s always return a struct/bitvector
    print $file $self->_intype_name();
    print $file " retval = unpack(truncate(a));\n";
    print $file $indent . "    return retval;\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##
## print proxy accept_request definition
##
sub print_proxy_accept_request_definition
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # header
    print $file $indent                             .
                $self->_make_get_header("acceptRequest",
                                        $self->inargs()->makebitvector());

    # conditions
    print $file " if (mid == fromInteger(mid_";
    print $file $self->{name};
    print $file "));\n";

    # body
    print $file $indent . "    let a <- dem.readAndDelete();\n";
    print $file $indent . "    ";

    # acceptRequest()s always return a struct/bitvector
    print $file $self->inargs()->makebitvector();
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
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # header
    print $file $indent                             .
                $self->_make_get_header("acceptRequest",
                                        $self->_intype_name());

    # no conditions
    print $file ";\n";

    # body
    if ($self->outargs()->num() != 0)
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


##### SEND_RESPONSE STUB PRINTING #####

##
## print direct send_response declaration
##
sub print_direct_send_response_declaration
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    # header
    print $file $indent                             .
                $self->_make_put_header("sendResponse",
                                        $self->outargs()->makelist());

    print $file ";\n";
}

##
## print proxy send_response declaration
##
sub print_proxy_send_response_declaration
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    # header
    print $file $indent                             .
                $self->_make_put_header("sendResponse",
                                        $self->outargs()->makebitvector() . " resp");

    print $file ";\n";
}

##
## print remote send_response declaration
##
sub print_remote_send_response_declaration
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    # header
    print $file $indent                             .
                $self->_make_put_header("sendResponse",
                                        $self->outargs()->makelist());

    print $file ";\n";
}

##
## print direct send_response definition
##
sub print_direct_send_response_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    # header
    print $file $indent                             .
                $self->_make_put_header("sendResponse",
                                        $self->outargs()->makelist());

    print $file ";\n";

    # pack all elements into a struct
    print $file $indent . "    let resp = " .
                $self->outargs()->fillstruct($self->_outtype_name()) . ";\n";

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
## print proxy send_response definition
##
sub print_proxy_send_response_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    # header
    print $file $indent                             .
                $self->_make_put_header("sendResponse",
                                        $self->outargs()->makebitvector() . " resp");

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

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    # header
    print $file $indent                             .
                $self->_make_put_header("sendResponse",
                                        $self->outargs()->makelist());

    print $file ";\n";

    # pack all elements into a struct
    print $file $indent . "    let resp = " .
                $self->outargs()->fillstruct($self->_outtype_name()) . ";\n";

    # body
    print $file $indent . "    link_" . $self->{name} . ".makeResp(pack(resp));\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
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

    # print connection definitions
    if ($self->outargs()->num() == 0)
    {
        # no return: create Send-type connection
        print $file $indent . "Connection_Send#("        .
                    $self->inargs()->makebitvector()     .
                    ") link_server_$servicename" . "_"   .
                    $self->{name}                        .
                    " <- mkConnection_Send(\"rrr_server_$servicename\_" .
                    $self->{name} . "\");\n";
    }
    else
    {
        # need return: create Client-type connection
        print $file $indent . "Connection_Client#("      .
                    $self->inargs()->makebitvector()     .
                    ", "                                 .
                    $self->outargs()->makebitvector()    .
                    ") link_server_$servicename" . "_"   .
                    $self->{name}                        .
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

    my $methodname = $self->{name};
    my $intype     = $self->inargs()->makebitvector();
    
    # print rules to transfer request and response to the connection
    if ($self->outargs()->num() == 0)
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
        my $outtype = $self->outargs()->makebitvector();

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

    # print connection definitions
    if ($self->outargs()->num() == 0)
    {
        # no return: create receive-type connection
        print $file $indent . "Connection_Receive#("  .
                    $self->inargs()->makebitvector()  .
                    ") link_"                         .
                    $self->{name}                     .
                    " <- mkConnection_Receive(\"rrr_server_$servicename\_" .
                    $self->{name} . "\");\n";
    }
    else
    {
        # need return: create server-type connection
        print $file $indent . "Connection_Server#("   .
                    $self->inargs()->makebitvector()  .
                    ", "                              .
                    $self->outargs()->makebitvector() .
                    ") link_"                         .
                    $self->{name}                     .
                    " <- mkConnection_Server(\"rrr_server_$servicename\_" .
                    $self->{name} . "\");\n";
    }
}

######################################
#           CLIENT STUBS             #
######################################

##### MAKE_REQUEST STUB PRINTING #####

##
## print direct make_request declaration
##
sub print_direct_make_request_declaration
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    print $file $indent                             .
                $self->_make_put_header("makeRequest",
                                        $self->inargs()->makelist());

    print $file ";\n";
}

##
## print proxy make_request declaration
##
sub print_proxy_make_request_declaration
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    print $file $indent                             .
                $self->_make_put_header("makeRequest",
                                        $self->inargs()->makebitvector() . " req");

    print $file ";\n";
}

##
## print remote make_request declaration
##
sub print_remote_make_request_declaration
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    print $file $indent                             .
                $self->_make_put_header("makeRequest",
                                        $self->inargs()->makelist());

    print $file ";\n";
}

##
## print direct make_request definition
##
sub print_direct_make_request_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # header
    print $file $indent                             .
                $self->_make_put_header("makeRequest",
                                        $self->inargs()->makelist());

    print $file ";\n";

    # pack all elements into a struct
    print $file $indent . "    let req = " .
                $self->inargs()->fillstruct($self->_intype_name()) . ";\n";

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
## print proxy make_request definition
##
sub print_proxy_make_request_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # header
    print $file $indent                             .
                $self->_make_put_header("makeRequest",
                                        $self->inargs()->makebitvector() . " req");

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
    my $self        = shift;
    my $file        = shift;
    my $indent      = shift;
    my $servicename = shift;

    # header
    print $file $indent                             .
                $self->_make_put_header("makeRequest",
                                        $self->inargs()->makelist());

    print $file ";\n";

    # pack all elements into a struct
    print $file $indent . "    let req = " .
                $self->inargs()->fillstruct($self->_intype_name()) . ";\n";

    # body
    if ($self->outargs()->num() != 0)
    {
        # client-type connection
        print $file $indent . "    link_" . $self->{name} . ".makeReq(pack(req));\n";
    }
    else
    {
        # send-type connection
        print $file $indent . "    link_" . $self->{name} . ".send(pack(req));\n";
    }

    # send control ID
    print $file $indent .
        "    control.send(\`$servicename\_" . $self->{name} . "_CONTROL_ID);\n";    

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##### GET_RESPONSE STUB PRINTING #####

##
## print direct get_response declaration
##
sub print_direct_get_response_declaration
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    print $file $indent                                      .
                $self->_make_get_header("getResponse",
                                        $self->_outtype_name()) .
                ";\n";
}

##
## print proxy get_response declaration
##
sub print_proxy_get_response_declaration
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    print $file $indent                                      .
                $self->_make_get_header("getResponse",
                                        $self->outargs()->makebitvector()) .
                ";\n";
}

##
## print remote get_response declaration
##
sub print_remote_get_response_declaration
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    print $file $indent                                      .
                $self->_make_get_header("getResponse",
                                        $self->_outtype_name()) .
                ";\n";
}

##
## print direct get_response definition
##
sub print_direct_get_response_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    # header
    print $file $indent                                      .
                $self->_make_get_header("getResponse",
                                        $self->_outtype_name());

    # conditions
    print $file " if (mid == fromInteger(mid_";
    print $file $self->{name};
    print $file "));\n";

    # body
    print $file $indent . "    let a <- dem.readAndDelete();\n";
    print $file $indent . "    ";
    print $file $self->_outtype_name();
    print $file " retval = unpack(truncate(a));\n";
    print $file $indent . "    return retval;\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##
## print proxy get_response definition
##
sub print_proxy_get_response_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    # header
    print $file $indent                                      .
                $self->_make_get_header("getResponse",
                                        $self->outargs()->makebitvector());

    # conditions
    print $file " if (mid == fromInteger(mid_";
    print $file $self->{name};
    print $file "));\n";

    # body
    print $file $indent . "    let a <- dem.readAndDelete();\n";
    print $file $indent . "    ";
    print $file $self->outargs()->makebitvector();
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
    my $self = shift;
    my $file = shift;
    my $indent = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    # header
    print $file $indent                                      .
                $self->_make_get_header("getResponse",
                                        $self->_outtype_name());

    # no conditions
    print $file ";\n";

    # body
    print $file $indent . "    let a = link_" . $self->{name} . ".getResp();\n";
    print $file $indent . "    link_" . $self->{name} . ".deq();\n";
    print $file $indent . "    return unpack(a);\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
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

    # print connection definitions
    if ($self->outargs()->num() == 0)
    {
        # no return: create Receive-type connection
        print $file $indent . "Connection_Receive#("    .
                    $self->inargs()->makebitvector()    .
                    ") link_client_$servicename" . "_"  .
                    $self->{name}                       .
                    " <- mkConnection_Receive(\"rrr_client_$servicename\_" .
                    $self->{name} . "\");\n";
    }
    else
    {
        # need return: create Server-type connection
        print $file $indent . "Connection_Server#("     .
                    $self->inargs()->makebitvector()    .
                    ", "                                .
                    $self->outargs()->makebitvector()   .
                    ") link_client_$servicename" . "_"  .
                    $self->{name}                       .
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
    
    if ($self->outargs()->num() == 0)
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

    my $methodname = $self->{name};
    my $intype     = $self->inargs()->makebitvector();
    
    # print rules to transfer request and response to the connection
    if ($self->outargs()->num() == 0)
    {
        # no return: create only request rule, use receive()
        print $file $indent . "rule client_makeRequest_$servicename\_$methodname (controlID_$servicename == \`$servicename\_$methodname\_CONTROL_ID);\n";
        print $file $indent . "    control_client_$servicename.deq();\n";
        print $file $indent . "    $intype req = link_client_$servicename\_$methodname.receive();\n";
        print $file $indent . "    link_client_$servicename\_$methodname.deq();\n";
        print $file $indent . "    stub_client_$servicename.makeRequest\_$methodname(req);\n";
        print $file $indent . "endrule\n";
        print $file $indent . "\n";
    }
    else
    {
        my $outtype = $self->outargs()->makebitvector();

        # need return: create request and response rules, use getReq()
        print $file $indent . "rule client_makeRequest_$servicename\_$methodname (controlID_$servicename == \`$servicename\_$methodname\_CONTROL_ID);\n";
        print $file $indent . "    control_client_$servicename.deq();\n";
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

    # print connection definitions
    if ($self->outargs()->num() == 0)
    {
        # no return: create send-type connection
        print $file $indent . "Connection_Send#("             .
                    $self->inargs()->makebitvector()          .
                    ") link_"                                 .
                    $self->{name}                             .
                    " <- mkConnection_Send(\"rrr_client_$servicename\_" .
                    $self->{name} . "\");\n";
    }
    else
    {
        # need return: create client-type connection
        print $file $indent . "Connection_Client#("           .
                    $self->inargs()->makebitvector()          .
                    ", "                                      .
                    $self->outargs()->makebitvector()         .
                    ") link_"                                 .
                    $self->{name}                             .
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

    if ($self->outargs()->num() != 0)
    {
        my $outsize = $self->outargs()->size();

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

    my $insize = $self->inargs()->size();
    
    print $file $indent . "Integer numChunks_" .
                $self->{name}        .
                " = ($insize % `UMF_CHUNK_BITS) == 0 ?\n";
    print $file $indent . "    ($insize / `UMF_CHUNK_BITS) :\n";
    print $file $indent . "    ($insize / `UMF_CHUNK_BITS) + 1;\n";
}

1;

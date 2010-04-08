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

sub _make_get_noaction_header
{
    my $self        = shift;
    my $methodclass = shift;
    my $typestring  = shift;
    
    my $string = "method " .
                 $typestring            .
                 " "                    .
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
## print accept_request declaration
##
sub print_accept_request_declaration
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    print $file $indent                                      .
                $self->_make_get_header("acceptRequest",
                                        $self->_intype_name()) .
                ";\n";
    print $file $indent                                      .
                $self->_make_get_noaction_header("peekRequest",
                                                 $self->_intype_name()) .
                ";\n";
}

##
## print accept_request definition
##
sub print_accept_request_definition
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;
    my $ifc    = shift;

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

    # header
    print $file $indent                             .
                $self->_make_get_noaction_header("peekRequest",
                                        $self->_intype_name());

    # conditions
    print $file " if (mid == fromInteger(mid_";
    print $file $self->{name};
    print $file "));\n";

    # body
    print $file $indent . "    let a = dem.peek();\n";
    print $file $indent . "    ";

    # acceptRequest()s always return a struct/bitvector
    print $file $self->_intype_name();
    print $file " retval = unpack(truncate(a));\n";
    print $file $indent . "    return retval;\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##### SEND_RESPONSE STUB PRINTING #####

##
## print send_response declaration
##
sub print_send_response_declaration
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
## print send_response definition
##
sub print_send_response_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;
    my $ifc = shift;

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
    print $file $indent . "    UMF_PACKET header = tagged UMF_PACKET_header UMF_PACKET_HEADER\n";
    print $file $indent . "                        {\n";
    print $file $indent . "                            filler: ?,\n";
    print $file $indent . "                            phyChannelPvt: ?,\n";
    print $file $indent . "                            channelID: ?,\n";
    print $file $indent . "                            serviceID: `SERVICE_ID,\n";
    print $file $indent . "                            methodID : fromInteger(mid_";
    print $file $self->{name};
    print $file "),\n";
    print $file $indent . "                            numChunks: fromInteger(numChunks_";
    print $file $self->{name};
    print $file ")\n";
    print $file $indent . "                        };\n";
    if ($ifc eq "connection")
    {
        print $file $indent . "    link_resp.send(header);\n";
    }
    else
    {
        print $file $indent . "    server.responsePorts[`SERVICE_ID].write(header);\n";
    }
    print $file $indent . "    mar.enq(zeroExtend(pack(resp)), fromInteger(numChunks_";
    print $file $self->{name};
    print $file "));\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
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
    my $self = shift;
    my $file = shift;
    my $indent = shift;
    my $ifc = shift;

    print $file $indent                             .
                $self->_make_put_header("makeRequest",
                                        $self->inargs()->makelist());

    print $file ";\n";
}

##
## print make_request definition
##
sub print_make_request_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;
    my $ifc = shift;

    # header
    print $file $indent                             .
                $self->_make_put_header("makeRequest",
                                        $self->inargs()->makelist());

    print $file ";\n";

    # pack all elements into a struct
    print $file $indent . "    let req = " .
                $self->inargs()->fillstruct($self->_intype_name()) . ";\n";

    # body
    print $file $indent . "    UMF_PACKET header = tagged UMF_PACKET_header UMF_PACKET_HEADER\n";
    print $file $indent . "                        {\n";
    print $file $indent . "                            filler: ?,\n";
    print $file $indent . "                            phyChannelPvt: ?,\n";
    print $file $indent . "                            channelID: ?,\n";
    print $file $indent . "                            serviceID: `SERVICE_ID,\n";
    print $file $indent . "                            methodID : fromInteger(mid_";
    print $file $self->{name};
    print $file "),\n";
    print $file $indent . "                            numChunks: fromInteger(numChunks_";
    print $file $self->{name};
    print $file ")\n";
    print $file $indent . "                        };\n";
    if ($ifc eq "connection")
    {
        print $file $indent . "    link_req.send(header);\n";
    }
    else
    {
        print $file $indent . "    client.requestPorts[`SERVICE_ID].write(header);\n";
    }
    print $file $indent . "    mar.enq(zeroExtend(pack(req)), fromInteger(numChunks_";
    print $file $self->{name};
    print $file "));\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
}

##### GET_RESPONSE STUB PRINTING #####

##
## print get_response declaration
##
sub print_get_response_declaration
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;
    my $ifc = shift;

    # return if we don't need a response
    if ($self->outargs()->num() == 0)
    {
        return;
    }

    print $file $indent                                      .
                $self->_make_get_header("getResponse",
                                        $self->_outtype_name()) .
                ";\n";

    print $file $indent                                      .
                $self->_make_get_noaction_header("peekResponse",
                                                 $self->_outtype_name()) .
                ";\n";
}

##
## print get_response definition
##
sub print_get_response_definition
{
    my $self = shift;
    my $file = shift;
    my $indent = shift;
    my $ifc = shift;

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
    
    ##
    ## Equivalent peekResponse method (everything but the deq)
    ##

    # header
    print $file $indent                                      .
                $self->_make_get_noaction_header("peekResponse",
                                                 $self->_outtype_name());

    # no conditions
    print $file ";\n";

    # body
    print $file $indent . "    let a = dem.peek();\n";
    print $file $indent . "    ";
    print $file $self->_outtype_name();
    print $file " retval = unpack(truncate(a));\n";
    print $file $indent . "    return retval;\n";

    # endmethod
    print $file $indent . "endmethod\n\n";
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
                              " = ($outsize % valueOf(UMF_CHUNK_BITS)) == 0 ?\n";
        print $file $indent . "    ($outsize / valueOf(UMF_CHUNK_BITS)) :\n";
        print $file $indent . "    ($outsize / valueOf(UMF_CHUNK_BITS)) + 1;\n";
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
                " = ($insize % valueOf(UMF_CHUNK_BITS)) == 0 ?\n";
    print $file $indent . "    ($insize / valueOf(UMF_CHUNK_BITS)) :\n";
    print $file $indent . "    ($insize / valueOf(UMF_CHUNK_BITS)) + 1;\n";
}

1;

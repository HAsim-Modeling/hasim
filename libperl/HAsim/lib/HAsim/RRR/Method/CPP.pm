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

package HAsim::RRR::Method::CPP;

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

    # get service name
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

    # copy over the arg lists, but type case them into CPP
    $target->{inargs}  = HAsim::RRR::Arglist::CPP->new($source->{inargs});
    $target->{outargs} = HAsim::RRR::Arglist::CPP->new($source->{outargs});

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
                    ";\n";
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
                    ";\n";
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
}

#######################################
##             CLIENT                ##
#######################################

##
## print client method definition
##
sub print_client_definition
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # sizes
    my $insize  = $self->inargs()->size();
    my $outsize = $self->outargs()->size();
    
    # header
    if ($self->outargs()->num() > 0)
    {
        print $file $indent . $self->_outtype_name();
    }
    else
    {
        print $file $indent . "void";
    }
    print $file " "                               .
                $self->name()                     .
                "("                               .
                $self->inargs()->makelist()       .
                ")\n";

    # body
    print $file $indent . "{\n";
    print $file $indent . "    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS;\n";
    print $file $indent . "    msg->SetLength($insize);\n";
    print $file $indent . "    msg->SetServiceID(" . $self->{servicename} . "_SERVICE_ID);\n";
    print $file $indent . "    msg->SetMethodID(" . $self->{servicename} . "_METHOD_ID_" . $self->{name} . ");\n";

    # marshall args, BUT use reverse order!
    my @reverseinlist = reverse(@{ $self->inargs()->args() });
    foreach my $arg (@reverseinlist)
    {
        print $file $indent                    .
                    "    msg->Append"          .
                    $arg->type()->string_cpp() .
                    "("                        .
                    $arg->name()->string()     .
                    ");\n";
    }
    print $file $indent . "    \n";

    # do we need a response?
    if ($self->outargs()->num() == 0)
    {
        # no response
        print $file $indent . "    RRRClient->MakeRequestNoResponse(msg);\n";
    }
    else
    {
        # need response
        print $file $indent . "    UMF_MESSAGE resp = RRRClient->MakeRequest(msg);\n";
        print $file $indent . "    \n";

        # demarshall return value(s)
        print $file $indent . "    " . $self->_outtype_name() . " retval;\n";

        if ($self->outargs()->num() == 1)
        {
            # only one return value
            my ($arg, @null) = @{ $self->outargs()->args() };
            print $file $indent                      .
                        "    retval = resp->Extract" .
                        $arg->type()->string_cpp()   .
                        "();\n";
        }
        else
        {
            # multiple return values, demarshall into struct BUT use reversed list
            my @reverseoutlist = reverse(@{ $self->outargs()->args() });
            foreach my $arg (@reverseoutlist)
            {
                print $file $indent                    .
                            "    retval."              .
                            $arg->name()->string()     .
                            " = resp->Extract"         .
                            $arg->type()->string_cpp() .
                            "();\n";
            }
        }

        # cleanup and return
        print $file $indent . "    delete resp;\n";
        print $file $indent . "    return retval;\n";
    }

    # end method
    print $file $indent . "}\n";
}

#######################################
##             SERVER                ##
#######################################

##
## print server case block
##
sub print_server_case_block
{
    my $self   = shift;
    my $file   = shift;
    my $indent = shift;

    # sizes
    my $insize  = $self->inargs()->size();
    my $outsize = $self->outargs()->size();
    
    # start printing case statement
    print $file $indent . "case " . $self->{servicename} . "_METHOD_ID_" . $self->name() . ":\n";
    print $file $indent . "{\n";

    # some versions of GCC aren't very happy if the first statement
    # after a case label is a variable declaration, so we add an
    # empty statement here
    # print $file $indent . "    ;\n";

    # demarshall in args from UMF msg, BUT use reversed list
    my @reverseinlist = reverse(@{ $self->inargs()->args() });
    foreach my $arg (@reverseinlist)
    {
        print $file $indent            .
            "    "                     .
            $arg->string_cpp()         .
            " = req->Extract"          .
            $arg->type()->string_cpp() .
            "();\n";
    }

    # call server
    if ($self->outargs()->num() == 0)
    {
        # void
        print $file $indent . "    server->" . $self->name() . "(" .
                              $self->inargs()->makecalllist() . ");\n";

        # de-allocate request message
        print $file $indent . "    delete req;\n";
    }
    else
    {
        # has return
        print $file $indent . "    " . $self->_outtype_name() .
            " retval = server->" . $self->name() . "(" .
            $self->inargs()->makecalllist() . ");\n";

        # de-allocate request message
        print $file $indent . "    delete req;\n";
        print $file "\n";

        # create response message
        print $file $indent . "    resp = new UMF_MESSAGE_CLASS;\n";
        print $file $indent . "    resp->SetLength($outsize);\n";
        print $file $indent . "    resp->SetServiceID(" . $self->{servicename} . "_SERVICE_ID);\n";
        print $file $indent . "    resp->SetMethodID(methodID);\n";
        print $file "\n";

        # we have to treat single and multiple args differently
        if ($self->outargs()->num() == 1)
        {
            my ($arg, @null) = @{ $self->outargs()->args() };
            print $file $indent            .
                "    resp->Append"         .
                $arg->type()->string_cpp() .
                "(retval);\n";
        }
        else
        {
            # marshall return values, in REVERSE!
            my @reverseoutlist = reverse(@{ $self->outargs()->args() });
            foreach my $arg (@reverseoutlist)
            {
                print $file $indent            .
                    "    resp->Append"         .
                    $arg->type()->string_cpp() .
                    "(retval."                 .
                    $arg->name()->string()     .
                    ");\n";
            }
        }
    }

    # end of case block
    print $file $indent . "    break;\n";
    print $file $indent . "}\n";
}

1;

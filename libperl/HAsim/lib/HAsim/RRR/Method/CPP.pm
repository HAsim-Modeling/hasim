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
    my $target;

    $target->{name} = $source->{name};

    # copy over the arg lists, but type case them into CPP
    $target->{inargs}  = HAsim::RRR::Arglist::CPP->new($source->{inargs});
    $target->{outargs} = HAsim::RRR::Arglist::CPP->new($source->{outargs});

    return $target;
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
    print $file $indent . "    UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();\n";
    print $file $indent . "    msg->SetLength($insize);\n";
    print $file $indent . "    msg->SetServiceID(SERVICE_ID);\n";
    print $file $indent . "    msg->SetMethodID(METHOD_ID_" . $self->{name} . ");\n";

    # marshall args
    foreach my $arg ( @{ $self->inargs()->args() })
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
            # multiple return values, demarshall into struct
            foreach my $arg ( @{ $self->outargs()->args() })
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
        print $file $indent . "    resp->Delete();\n";
        print $file $indent . "    return retval;\n";
    }

    # end method
    print $file $indent . "}\n";
}

1;

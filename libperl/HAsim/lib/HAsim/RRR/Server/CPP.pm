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
        my $cpp_method = HAsim::RRR::Method::CPP->new($method, $server->{name});

        # add the typed method to the server's list
        push(@{ $server->{methodlist} }, $cpp_method);
    }
}

##
## print stub into a given file in cpp
##
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

    #
    # Types section
    #

    print $file "//\n";
    print $file "// Types\n";
    print $file "//\n";
    print $file "\n";

    # gate type-printing if only stub was requested
    print $file "#ifndef STUB_ONLY\n";
    print $file "\n";

    # primary types gate
    print $file "#ifndef __" . $self->name() . "_SERVER_TYPES__\n";
    print $file "#define __" . $self->name() . "_SERVER_TYPES__\n";
    print $file "\n";

    # for TEMPORARY backwards-compatibility, we allow servers to bypass the stub and
    # process the UMF_MESSAGE directly, in which case we shouldn't print the types
    print $file "#ifndef BYPASS_SERVER_STUB\n";
    print $file "\n";

    # print types
    foreach my $method (@{ $self->{methodlist} })
    {
        $method->print_types($file);
    }
    print $file "\n";

    # close the #ifndef for the TEMPORARY stub-bypass trick
    print $file "#endif\n";
    print $file "\n";

    # close primary types gate
    print $file "#endif\n";
    print $file "\n";

    # close stub-only gate
    print $file "#endif\n";
    print $file "\n";

    #
    # Stub section
    #

    print $file "//\n";
    print $file "// Stub\n";
    print $file "//\n";
    print $file "\n";

    # gate stub-printing if only types were requested
    print $file "#ifndef TYPES_ONLY\n";
    print $file "\n";

    # defines and includes
    print $file "#ifndef __" . $self->name() . "_SERVER_STUB__\n";
    print $file "#define __" . $self->name() . "_SERVER_STUB__\n";
    print $file "\n";

    print $file "#include \"asim/provides/low_level_platform_interface.h\"\n";
    print $file "#include \"asim/provides/rrr.h\"\n";
    print $file "#include \"asim/rrr/service_ids.h\"\n";
    print $file "\n";
    
    # assign method IDs
    my $methodID = 0;
    foreach my $method (@{ $self->{methodlist} })
    {
        print $file "#define METHOD_ID_" . $method->name() . " $methodID\n";
        $methodID = $methodID + 1;
    }
    print $file "\n";

    # other generic stuff
    print $file "using namespace std;\n";
    print $file "\n";

    # start creating the server class
    print $file "typedef class " . $self->name() . "_SERVER_STUB_CLASS* " . $self->name() . "_SERVER_STUB;\n";
    print $file "class " . $self->name() . "_SERVER_STUB_CLASS: public RRR_SERVER_STUB_CLASS,\n" .
                "    public PLATFORMS_MODULE_CLASS\n";
    print $file "{\n";
    print $file "\n";

    print $file "  private:\n";
    print $file "\n";
    print $file "    " . $self->name() . "_SERVER server;\n";
    print $file "\n";

    print $file "  public:\n";
    print $file "\n";

    # constructor
    print $file "    " . $self->name() . "_SERVER_STUB_CLASS(" . $self->name() . "_SERVER s)\n";
    print $file "    {\n";
    print $file "        parent = PLATFORMS_MODULE(s);\n";
    print $file "        server = s;\n";
    print $file "        RRR_SERVER_MONITOR_CLASS::RegisterServer(" . $self->name() . "_SERVICE_ID, this);\n";
    print $file "    }\n";
    print $file "\n";

    # destructor
    print $file "    ~" . $self->name() . "_SERVER_STUB_CLASS()\n";
    print $file "    {\n";
    print $file "    }\n";
    print $file "\n";

    # generic methods (pass-through to server)
    print $file "    void Init(PLATFORMS_MODULE p)\n";
    print $file "    {\n";
    print $file "        server->Init(p);\n";
    print $file "    }\n";
    print $file "\n";

    print $file "    void Poll()\n";
    print $file "    {\n";
    print $file "        server->Poll();\n";
    print $file "    }\n";    
    print $file "\n";

    # main Request method
    print $file "    UMF_MESSAGE Request(UMF_MESSAGE req)\n";
    print $file "    {\n";

    # for TEMPORARY backwards-compatibility, we allow servers to bypass the stub and
    # process the UMF_MESSAGE directly
    print $file "#ifdef BYPASS_SERVER_STUB\n";
    print $file "\n";

    print $file "        return server->Request(req);\n";
    print $file "\n";

    print $file "#else\n";
    print $file "\n";

    # Response (optional)
    print $file "        UMF_MESSAGE resp = NULL;\n";
    print $file "\n";

    # extract methodID
    print $file "        UINT32 methodID = req->GetMethodID();\n";
    print $file "\n";

    # case statement based on methodID
    print $file "        switch(methodID)\n";
    print $file "        {\n";

    # switch statement for each server method
    foreach my $method (@{ $self->{methodlist} })
    {
        $method->print_server_case_block($file, "            ");
        print $file "\n";
    }

    # default case
    print $file "            default:\n";
    print $file "                cerr << \"" . $self->name() .
                " server: invalid methodID: \" << methodID << endl;\n";
    print $file "                parent->CallbackExit(1);\n";
    print $file "                break;\n";
    print $file "        }\n";
    print $file "\n";

    # finish up the Request method
    print $file "        return resp;\n";

    # close the #else for the TEMPORARY stub-bypass trick
    print $file "\n";
    print $file "#endif\n";
    print $file "\n";

    print $file "    }\n";

    # close the class
    print $file "};\n";
    print $file "\n";

    # end the stub, close primary gate
    print $file "#endif\n";
    print $file "\n";

    # print types-only gate
    print $file "#endif\n";
}

1;

# *****************************************************************
# *                                                               *
# *   Copyright (c) (Fill in here)                                *
# *                                                               *
# *****************************************************************

#
# Author:  Angshuman Parashar
#

package HAsim::RRR::Client;

use warnings;
use strict;
use re 'eval';

use HAsim::RRR::Collection;

##
## constructor: this is an unusual constructor. It returns
## not a single object of type Client, but a list of objects
## of type Client. All other member functions operate on a
## single object.
##
sub new
{
    # get name of class
    my $class = shift;

    # get service name
    my $servicename = shift;

    # get list of collections
    my @collectionlist = @_;

    # extract methods from each collection and place them
    # into multiple client modules, each with a unique
    # target name
    my @objlist = _extract($servicename, @collectionlist);

    # typecast each entry in list
    foreach my $obj (@objlist)
    {
        bless ($obj, $class);
    }

    # return list of objects
    return @objlist;
}

##
## extract methods from each collection and place them
## into multiple client modules, each with a unique
## target name
##
sub _extract
{
    my $servicename    = shift;
    my @collectionlist = @_;

    # create an empty list of clients
    my @clientlist = ();

    # for each collection in given list
    foreach my $collection (@collectionlist)
    {
        # create a new client: we are guaranteed to have
        # one new client target per collection
        my $client;

        # check for target name conflicts in existing
        # list of clients
        foreach my $s (@clientlist)
        {
            if ($s->{target} eq $collection->client_target())
            {
                die "client target name conflict: " . $s->{target};
            }
        }

        # no conflicts, fill in client details
        $client->{name}   = $servicename;
        $client->{target} = $collection->client_target();
        $client->{lang}   = $collection->client_lang();
        $client->{ifc}    = $collection->client_ifc();
        push(@{ $client->{methodlist} }, @{ $collection->methodlist() });

        # add client to list
        push(@clientlist, $client);
    }

    # return list
    return @clientlist;
}

##
## return the target name
##
sub target
{
    my $self = shift;

    return $self->{target};
}

##
## return the language
##
sub lang
{
    my $self = shift;

    return $self->{lang};
}

##
## return the interface type
##
sub interface
{
    my $self = shift;

    return $self->{ifc};
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
        return;
    }    

    # determine if we should write stub at all
    if ($#{ $self->{methodlist} } == -1)
    {
        return;
    }

    # compute max request and response bitwidths
    my $maxinsize = 0;
    my $maxoutsize = 0;
    my @list = @{ $self->{methodlist} };
    foreach my $method (@list)
    {
        if ($method->insize() > $maxinsize)
        {
            $maxinsize = $method->insize();
        }
        
        if ($method->outsize() > $maxoutsize)
        {
            $maxoutsize = $method->outsize();
        }
    }

    # helper definition for service ID
    print $file "`define SERVICE_ID `" . $self->{name} ."_SERVICE_ID\n";
    print $file "\n";

    # interface ...
    print $file "interface ClientStub_" . $self->{name} . ";\n";

    # indent
    my $indent = "    ";

    # interface entry for each method
    foreach my $method (@{ $self->{methodlist} })
    {
        $method->print_make_request_declaration($file, $indent);
        $method->print_get_response_declaration($file, $indent);
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
        $method->print_state($file, $indent, $methodID);
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
        $method->print_make_request_definition($file, $indent, $self->{name});
        print $file "\n";
        $method->print_get_response_definition($file, $indent, $self->{name});
        print $file "\n";
    }
    
    # endmodule
    print $file "endmodule\n";
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
        # for each method
        foreach my $method (@{ $self->{methodlist} })
        {
            # ask method to print out a set of link rules
            $method->print_client_link_rules($file, $indent, $self->{name});
        }
    }
}

1;

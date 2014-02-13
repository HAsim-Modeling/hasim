//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

// Miscellaneous useful utilities

// Library imports

import Vector::*;

`include "asim/provides/soft_connections.bsh"


// Eventually a HASIM_MODULE will be more complex than this.
// For now it's just a module with soft connections.

typedef Connected_Module HASIM_MODULE;


// The side-effects to using Bit#(0) can be unexpected, so 
// use Bit#(1) instead.

typedef Bit#(1) VOID;


// updateRange

// A rollover vector-updating function.

function Vector#(TExp#(width), dataT) updateRange(dataT data, Bit#(width) lo, Bit#(width) hi, Vector#(TExp#(width), dataT) oldVec);
    Vector#(TExp#(width), dataT) newVec = newVector();
    for(Integer i = 0; i < valueOf(TExp#(width)); i = i + 1)
    begin
        if(lo < hi)
            newVec[i] = (fromInteger(i) > lo && fromInteger(i) < hi)? data: oldVec[i];
        else
            newVec[i] = (fromInteger(i) > lo || fromInteger(i) < hi)? data: oldVec[i];
    end
    return newVec;
endfunction

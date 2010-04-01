//
// Copyright (C) 2008 Massachusetts Institute of Technology
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

import Vector::*;
import ModuleCollect::*;
import List::*;

import soft_connections::*;

//The interface of a module with Connections
interface WITH_CONNECTIONS#(parameter numeric type numIn,
                           parameter numeric type numOut,
                           parameter type orig_T);

  interface Vector#(numIn, PHYSICAL_CONNECTION_IN)  incoming;
  interface Vector#(numOut, PHYSICAL_CONNECTION_OUT) outgoing;
  interface Vector#(CON_NUM_CHAINS, PHYSICAL_CONNECTION_INOUT) chains;
  interface orig_T device;

endinterface

// Backwards compatability:
typedef WITH_CONNECTIONS#(nI, nO, Empty) WithConnections#(parameter numeric type nI, parameter numeric type nO);

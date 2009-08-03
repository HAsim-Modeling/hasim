//
// Copyright (C) 2008 Intel Corporation
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

//
// Base types for functional partition
//

//
// Integer register size.  For now also used as the fundamental access size
// for functional hybrid memory.
//
typedef Bit#(`FUNCP_ISA_INT_REG_SIZE) FUNCP_INT_REG;

//
// Virtual address size.
//
typedef Bit#(`FUNCP_ISA_V_ADDR_SIZE) FUNCP_VADDR;

//
// Physical address size.
//
typedef `FUNCP_ISA_P_ADDR_SIZE FUNCP_PADDR_SIZE;
typedef Bit#(`FUNCP_ISA_P_ADDR_SIZE) FUNCP_PADDR;

//
// Page of memory (FUNCP_PAGE) and offset in a page.
//
typedef Bit#(TSub#(`FUNCP_ISA_V_ADDR_SIZE, `FUNCP_ISA_PAGE_SHIFT)) FUNCP_V_PAGE;
typedef Bit#(TSub#(`FUNCP_ISA_P_ADDR_SIZE, `FUNCP_ISA_PAGE_SHIFT)) FUNCP_P_PAGE;
typedef Bit#(`FUNCP_ISA_PAGE_SHIFT) FUNCP_PAGE_OFFSET;


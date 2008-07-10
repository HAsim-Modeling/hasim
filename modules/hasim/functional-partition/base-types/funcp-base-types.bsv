//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
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
typedef Bit#(`FUNCP_ISA_P_ADDR_SIZE) FUNCP_PADDR;

//
// Page of memory
//
typedef Bit#(`FUNCP_ISA_PAGE_SHIFT) FUNCP_PAGE;

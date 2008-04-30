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
#if (FUNCP_ISA_INT_REG_SIZE == 32)
typedef UINT32 FUNCP_INT_REG;
#elif (FUNCP_ISA_INT_REG_SIZE == 64)
typedef UINT64 FUNCP_INT_REG;
#else
#error "invalid int reg value size"
#endif

//
// Address size.  Also defines the PC size.
//
#if (FUNCP_ISA_ADDR_SIZE == 32)
typedef UINT32 FUNCP_ADDR;
#elif (FUNCP_ISA_ADDR_SIZE == 64)
typedef UINT64 FUNCP_ADDR;
#else
#error "invalid memory address size"
#endif

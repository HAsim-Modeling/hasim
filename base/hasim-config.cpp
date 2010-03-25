/*
 * *****************************************************************
 * *                                                               *
 * *    Copyright (c) Digital Equipment Corporation, 1994          *
 * *                                                               *
 * *   All Rights Reserved.  Unpublished rights  reserved  under   *
 * *   the copyright laws of the United States.                    *
 * *                                                               *
 * *   The software contained on this media  is  proprietary  to   *
 * *   and  embodies  the  confidential  technology  of  Digital   *
 * *   Equipment Corporation.  Possession, use,  duplication  or   *
 * *   dissemination of the software and media is authorized only  *
 * *   pursuant to a valid written license from Digital Equipment  *
 * *   Corporation.                                                *
 * *                                                               *
 * *   RESTRICTED RIGHTS LEGEND   Use, duplication, or disclosure  *
 * *   by the U.S. Government is subject to restrictions  as  set  *
 * *   forth in Subparagraph (c)(1)(ii)  of  DFARS  252.227-7013,  *
 * *   or  in  FAR 52.227-19, as applicable.                       *
 * *                                                               *
 * *****************************************************************
 */

/**
 * @file
 * @author Artur Klauser
 * @brief Register simulator configuration as stats so the configuration
 * will end up being put into the stats output file.
 */

//
//
// This is a dumbed down version of the Asim simcore base/config.cpp, with
// registration removed.  When HAsim gets more statistics infrastructure
// we can put some of the code back.
//

// ASIM core
#include "asim/syntax.h"
#include "asim/config.h"

ASIM_CONFIG_CLASS::ASIM_CONFIG_CLASS()
{
  // nada
}

void
ASIM_CONFIG_CLASS::RegisterSimulatorConfiguration(void)
{
  // the following include is an automatically generated file and
  // contains the specific configuration of a simulator; here we
  // instatiate the initialization and registration of each parameter;
#define Register(NAME,DESC,TYPE,VAR,VAL) \
  VAR = VAL;
#define RegisterDyn(NAME,DESC,TYPE,VAR)
#define Declare(DECL)
//  RegisterState(&VAR, NAME, DESC);
//
// ASIM public modules - This is OK here, since AWB guarantees that
// this header file is synthesized for all configurations.
#include "asim/provides/sim_config.h"
#undef Register
#undef RegisterDyn
#undef Declare

}


void
ASIM_CONFIG_CLASS::EmitStats(std::ofstream &statsFile)
{
#define Register(NAME,DESC,TYPE,VAR,VAL) \
    statsFile << NAME << ",\"" << DESC << "\"," << VAR << std::endl;
#define RegisterDyn(NAME,DESC,TYPE,VAR) \
    statsFile << NAME << ",\"" << DESC << "\"," << VAR << std::endl;
#define Declare(DECL)

#include "asim/provides/sim_config.h"
#undef Register
#undef RegisterDyn
#undef Declare
}

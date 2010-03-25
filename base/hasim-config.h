// *****************************************************************
// *                                                               *
// *    Copyright (c) Compaq Computer Corporation, 2000            *
// *                                                               *
// *   All Rights Reserved.  Unpublished rights  reserved  under   *
// *   the copyright laws of the United States.                    *
// *                                                               *
// *   The software contained on this media  is  proprietary  to   *
// *   and  embodies  the  confidential  technology  of  Compaq    *
// *   Computer Corporation.  Possession, use,  duplication  or    *
// *   dissemination of the software and media is authorized only  *
// *   pursuant to a valid written license from Compaq Computer    *
// *   Corporation.                                                *
// *                                                               *
// *   RESTRICTED RIGHTS LEGEND   Use, duplication, or disclosure  *
// *   by the U.S. Government is subject to restrictions  as  set  *
// *   forth in Subparagraph (c)(1)(ii)  of  DFARS  252.227-7013,  *
// *   or  in  FAR 52.227-19, as applicable.                       *
// *                                                               *
// *****************************************************************

/**
 * @file
 * @author Artur Klauser
 * @brief Register simulator configuration as stats so the configuration
 * will end up being put into the stats output file.
 */

#ifndef _CONFIG_
#define _CONFIG_

// generic C++
#include <string>
#include <iostream>
#include <fstream>

// ASIM core
#include "asim/syntax.h"

// setup extern declarations of dynamic parameters
#define Register(NAME,DESC,TYPE,VAR,VAL)
#define RegisterDyn(NAME,DESC,TYPE,VAR)
#define Declare(DECL) \
  DECL;
//
// ASIM public modules - This is OK here, since AWB guarantees that
// this header file is synthesized for all configurations.
#include "asim/provides/sim_config.h"
#undef Register
#undef RegisterDyn
#undef Declare

/*
 * Class ASIM_CONFIG
 *
 * ASIM simulator configuration module
 *
 */
typedef class ASIM_CONFIG_CLASS *ASIM_CONFIG;
class ASIM_CONFIG_CLASS
{
  private:
#define Register(NAME,DESC,TYPE,VAR,VAL) \
      TYPE VAR;
#define RegisterDyn(NAME,DESC,TYPE,VAR)
#define Declare(DECL)
//
// ASIM public modules - This is OK here, since AWB guarantees that
// this header file is synthesized for all configurations.
#include "asim/provides/sim_config.h"
#undef Register
#undef RegisterDyn
#undef Declare

  public:
    ASIM_CONFIG_CLASS();
    void RegisterSimulatorConfiguration(void);

    // An EmitStats() method is provided here to support writing the model
    // configuration to a statistics file.  This module is not a member
    // of STATS_EMITTER_CLASS because it may be used in models that have
    // no support for statistics.  The EmitStats() here will be called
    // explicitly by a statistics emitter.    
    void EmitStats(std::ofstream &statsFile);
};

#endif

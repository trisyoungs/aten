/*
        *** Common functions for MOPAC71 plugins
        *** src/plugins/method_mopac71/common.h
        Copyright T. Youngs 2016-2017

        This file is part of Aten.
    
        Aten is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or 
        (at your option) any later version.
                               
        Aten is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.

        You should have received a copy of the GNU General Public License
        along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef ATEN_MOPAC71PLUGINCOMMON_H
#define ATEN_MOPAC71PLUGINCOMMON_H

#include "base/kvmap.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations
/* none */

// MOPAC71 Plugin Common Functions
class MOPAC71Common
{
	public:
	// Initialise Plugin Options
	static void initialiseOptions(KVMap& options);
};

ATEN_END_NAMESPACE

#endif

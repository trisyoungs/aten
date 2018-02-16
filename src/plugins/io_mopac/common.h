/*
        *** Common functions for MOPAC plugins
        *** src/plugins/io_mopac/common.h
        Copyright T. Youngs 2016-2018

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

#ifndef ATEN_MOPACFILEPLUGINCOMMON_H
#define ATEN_MOPACFILEPLUGINCOMMON_H

#include "base/kvmap.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations
class Model;
class FileParser;

// MOPAC Plugin Common Functions
class MOPACFilePluginCommon
{
	public:
	// Read single MOPAC model from file
	static bool readMOPACModel(FilePluginInterface* plugin, FileParser& parser, const FilePluginStandardImportOptions standardOptions, Model* targetModel);
};

ATEN_END_NAMESPACE

#endif

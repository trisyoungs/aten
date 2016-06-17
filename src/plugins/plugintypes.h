/*
        *** Plugin Types
        *** src/plugins/plugintypes.h
        Copyright T. Youngs 2007-2016

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

#ifndef ATEN_PLUGINTYPES_H
#define ATEN_PLUGINTYPES_H

#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations
/* none */

// Plugin Types
class PluginTypes
{
	public:
	// File plugin category
	enum FilePluginCategory { ModelFilePlugin, TrajectoryFilePlugin, ExpressionFilePlugin, GridFilePlugin, nFilePluginCategories };
	// Return single-word name of file plugin category
	static const char* filePluginCategory(FilePluginCategory category);
	// File plugin type
	enum FilePluginType { ImportPlugin, ExportPlugin };
	// Return single-word name of file plugin type
	static const char* filePluginType(FilePluginType type);
};

ATEN_END_NAMESPACE

#endif
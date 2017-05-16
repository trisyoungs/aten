/*
        *** Plugin Types
        *** src/plugins/plugintypes.h
        Copyright T. Youngs 2007-2017

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
	// Basic Plugin types
	enum PluginType { FilePlugin, MethodPlugin, ToolPlugin, nPluginTypes };
	// Return single-word name of plugin type
	static const char* pluginType(PluginType type);


	/*
	 * File Plugin
	 */
	public:
	// File plugin category
	enum FilePluginCategory { ModelFilePlugin, TrajectoryFilePlugin, ExpressionFilePlugin, GridFilePlugin, nFilePluginCategories };
	// Return capitalised single-word name of file plugin category
	static const char* niceFilePluginCategory(FilePluginCategory category);
	// Return single-word name of file plugin category
	static const char* filePluginCategory(FilePluginCategory category);
	// File plugin type
	enum FilePluginType { ImportPlugin, ExportPlugin };
	// Return single-word name of file plugin type
	static const char* filePluginType(FilePluginType type);


	/*
	 * Method Plugin
	 */
	public:
	// Method plugin category
	enum MethodPluginCategory { ChargeMethodPlugin, GeneralMethodPlugin, OptimisationMethodPlugin, nMethodPluginCategories };
	// Return capitalised single-word name of method plugin category
	static const char* niceMethodPluginCategory(MethodPluginCategory category);
	// Return single-word name of method plugin category
	static const char* methodPluginCategory(MethodPluginCategory category);


	/*
	 * Tool Plugin
	 */
	public:
	// Tool plugin category
	enum ToolPluginCategory { GeneralToolPlugin, nToolPluginCategories };
	// Return capitalised single-word name of tool plugin category
	static const char* niceToolPluginCategory(ToolPluginCategory category);
	// Return single-word name of tool plugin category
	static const char* toolPluginCategory(ToolPluginCategory category);
};

ATEN_END_NAMESPACE

#endif
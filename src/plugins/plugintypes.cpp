/*
        *** Plugin Types
        *** src/plugins/plugintypes.cpp
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

#include "plugins/plugintypes.h"
#include <QString>

ATEN_USING_NAMESPACE

// Plugin type
const char* PluginTypeStrings[] = { "File (Import/Export)", "Method (Enhancements to built-in methods)", "Tool (General Functionality Extension)" };

// Return name of file type
const char* PluginTypes::pluginType(PluginTypes::PluginType type)
{
	return PluginTypeStrings[type];
}

/*
 * File Plugins
 */

// File plugin categories
const char* FilePluginCategories[] = { "Model", "Trajectory", "Expression", "Grid" };

// Return capitalised single-word name of file plugin category
const char* PluginTypes::niceFilePluginCategory(PluginTypes::FilePluginCategory category)
{
	return FilePluginCategories[category];
}

// Return single-word name of file plugin category
const char* PluginTypes::filePluginCategory(PluginTypes::FilePluginCategory category)
{
	return qPrintable(QString(FilePluginCategories[category]).toLower());
}

// File plugin type
const char* FilePluginTypes[] = { "import", "export" };

// Return single-word name of file plugin type
const char* PluginTypes::filePluginType(PluginTypes::FilePluginType type)
{
	return FilePluginTypes[type];
}

/*
 * Method Plugins
 */

// Method plugin categories
const char* MethodPluginCategories[] = { "Charge", "General", "Optimisation" };

// Return capitalised single-word name of method plugin category
const char* PluginTypes::niceMethodPluginCategory(MethodPluginCategory category)
{
	return MethodPluginCategories[category];
}

// Return single-word name of method plugin category
const char* PluginTypes::methodPluginCategory(MethodPluginCategory category)
{
	return qPrintable(QString(MethodPluginCategories[category]).toLower());
}

/*
 * Tool Plugins
 */

// Tool plugin categories
const char* ToolPluginCategories[] = { "General" };

// Return capitalised single-word name of tool plugin category
const char* PluginTypes::niceToolPluginCategory(ToolPluginCategory category)
{
	return ToolPluginCategories[category];
}

// Return single-word name of tool plugin category
const char* PluginTypes::toolPluginCategory(ToolPluginCategory category)
{
	return qPrintable(QString(ToolPluginCategories[category]).toLower());
}

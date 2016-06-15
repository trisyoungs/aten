/*
        *** CIF Model Plugin Functions
        *** src/plugins/io_cif/cif_funcs.cpp
        Copyright T. Youngs 2016-2016

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

#include "plugins/io_cif/cif.hui"
#include "model/model.h"

// Constructor
CIFModelPlugin::CIFModelPlugin()
{
}

// Destructor
CIFModelPlugin::~CIFModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* CIFModelPlugin::makeCopy()
{
	return new CIFModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory CIFModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString CIFModelPlugin::name() const
{
	return QString("CIF (dlputils) 3D probability density");
}

// Nickname of plugin
QString CIFModelPlugin::nickname() const
{
	return QString("cif");
}

// Description (long name) of plugin
QString CIFModelPlugin::description() const
{
	return QString("Import/export for dlputils CIF files");
}

// Related file extensions
QStringList CIFModelPlugin::extensions() const
{
	return QStringList() << "cif";
}

// Exact names
QStringList CIFModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool CIFModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool CIFModelPlugin::importData()
{
	return true;
}

// Return whether this plugin can export data
bool CIFModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool CIFModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool CIFModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool CIFModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool CIFModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool CIFModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool CIFModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool CIFModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

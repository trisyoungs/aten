/*
        *** PDens (dlputils) Grid Plugin Functions
        *** src/plugins/io_pdens/pdens_funcs.cpp
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

#include "plugins/io_pdens/pdens.hui"
#include "model/model.h"

// Constructor
PDensGridPlugin::PDensGridPlugin()
{
}

// Destructor
PDensGridPlugin::~PDensGridPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* PDensGridPlugin::makeCopy()
{
	return new PDensGridPlugin;
}

/*
 * AKF Model Import / Export Plugin
 */

// Return category of plugin
PluginTypes::FilePluginCategory PDensGridPlugin::category() const
{
	return PluginTypes::GridFilePlugin;
}

// Name of plugin
QString PDensGridPlugin::name() const
{
	return QString("PDens (dlputils) 3D probability density");
}

// Nickname of plugin
QString PDensGridPlugin::nickname() const
{
	return QString("pdens");
}

// Description (long name) of plugin
QString PDensGridPlugin::description() const
{
	return QString("Import/export for dlputils PDens files");
}

// Related file extensions
QStringList PDensGridPlugin::extensions() const
{
	return QStringList() << "pdens";
}

// Exact names
QStringList PDensGridPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool PDensGridPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool PDensGridPlugin::importData()
{
	return true;
}

// Return whether this plugin can export data
bool PDensGridPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool PDensGridPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool PDensGridPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool PDensGridPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool PDensGridPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool PDensGridPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool PDensGridPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool PDensGridPlugin::showExportOptionsDialog()
{
	return false;
}
 

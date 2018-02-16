/*
        *** MOPAC Arc Model Plugin Functions
        *** src/plugins/io_mopac/mopacarc_funcs.cpp
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

#include "plugins/io_mopac/mopacarc.hui"
#include "plugins/io_mopac/common.h"
#include "model/model.h"

// Constructor
MOPACArcModelPlugin::MOPACArcModelPlugin()
{
}

// Destructor
MOPACArcModelPlugin::~MOPACArcModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* MOPACArcModelPlugin::makeCopy() const
{
	return new MOPACArcModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType MOPACArcModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int MOPACArcModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString MOPACArcModelPlugin::name() const
{
	return QString("MOPAC Arc file");
}

// Nickname of plugin
QString MOPACArcModelPlugin::nickname() const
{
	return QString("mopacarc");
}

// Return whether the plugin is enabled
bool MOPACArcModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString MOPACArcModelPlugin::description() const
{
	return QString("Basic import for MOPAC arc files");
}

// Related file extensions
QStringList MOPACArcModelPlugin::extensions() const
{
	return QStringList() << "arc";
}

// Exact names
QStringList MOPACArcModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool MOPACArcModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool MOPACArcModelPlugin::importData()
{
	// Search for geometry data in file...
	if (!fileParser_.find("FINAL GEOMETRY OBTAINED") == 0)
	{
		Messenger::error("No geometry found in arc file.");
		return false;
	}

	if (!MOPACFilePluginCommon::readMOPACModel(this, fileParser_, standardOptions_, createModel())) return false;

	return true;
}

// Return whether this plugin can export data
bool MOPACArcModelPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool MOPACArcModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool MOPACArcModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool MOPACArcModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool MOPACArcModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool MOPACArcModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool MOPACArcModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool MOPACArcModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}
 

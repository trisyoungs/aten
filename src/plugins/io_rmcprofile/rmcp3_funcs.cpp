/*
        *** RMCProfile V3 Model Plugin Functions
        *** src/plugins/io_rmcprofile/rmcp3_funcs.cpp
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

#include "plugins/io_rmcprofile/rmcp3.hui"
#include "plugins/io_rmcprofile/common.h"
#include "model/model.h"

// Constructor
RMCProfile3ModelPlugin::RMCProfile3ModelPlugin()
{
	// Setup option keywords
	/* none */
}

// Destructor
RMCProfile3ModelPlugin::~RMCProfile3ModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* RMCProfile3ModelPlugin::makeCopy() const
{
	return new RMCProfile3ModelPlugin;
}

/*
 * XYZ Model Import / Export Plugin
 */

// Return type of plugin
PluginTypes::PluginType RMCProfile3ModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int RMCProfile3ModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString RMCProfile3ModelPlugin::name() const
{
	return QString("RMCProfile v3 ??? Files");
}

// Nickname of plugin
QString RMCProfile3ModelPlugin::nickname() const
{
	return QString("rmcp3???");
}

// Return whether the plugin is enabled
bool RMCProfile3ModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString RMCProfile3ModelPlugin::description() const
{
	return QString("Import/export? for RMCProfile v3 coordinate files");
}

// Related file extensions
QStringList RMCProfile3ModelPlugin::extensions() const
{
	return QStringList() << "rmc3";
}

// Exact names
QStringList RMCProfile3ModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool RMCProfile3ModelPlugin::canImport() const
{
	return false;
}

// Import data from the specified file
bool RMCProfile3ModelPlugin::importData()
{
	// Create new model to read into
	createModel();
	bool result = RMCProfileFilePluginCommon::readModel(this, fileParser_, targetModel());
	if (!result) discardModel(targetModel());
	return result;
}

// Return whether this plugin can export data
bool RMCProfile3ModelPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool RMCProfile3ModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool RMCProfile3ModelPlugin::importNextPart()
{
	return RMCProfileFilePluginCommon::readModel(this, fileParser_, targetModel());
}

// Skip next partial data chunk
bool RMCProfile3ModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool RMCProfile3ModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool RMCProfile3ModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool RMCProfile3ModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool RMCProfile3ModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

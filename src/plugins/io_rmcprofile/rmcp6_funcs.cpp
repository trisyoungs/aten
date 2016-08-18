/*
        *** RMCProfile V6 Plugin Functions
        *** src/plugins/io_rmcprofile/rmcp6_funcs.cpp
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

#include "plugins/io_rmcprofile/rmcp6.hui"
#include "plugins/io_rmcprofile/common.h"
// #include "plugins/io_rmcprofile/xyzexportoptions.h"
// #include "plugins/io_rmcprofile/xyzimportoptions.h"
#include "model/model.h"

// Constructor
RMCProfile6ModelPlugin::RMCProfile6ModelPlugin()
{
	// Setup plugin options
	pluginOptions_.add("myOption", "false");
}

// Destructor
RMCProfile6ModelPlugin::~RMCProfile6ModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* RMCProfile6ModelPlugin::makeCopy()
{
	return new RMCProfile6ModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType RMCProfile6ModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int RMCProfile6ModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString RMCProfile6ModelPlugin::name() const
{
	return QString("RMCProfile v6 ??? Files");
}

// Nickname of plugin
QString RMCProfile6ModelPlugin::nickname() const
{
	return QString("rmcp6???");
}

// Description (long name) of plugin
QString RMCProfile6ModelPlugin::description() const
{
	return QString("Import/export? for RMCProfile v6 coordinate files");
}

// Related file extensions
QStringList RMCProfile6ModelPlugin::extensions() const
{
	return QStringList() << "rmc6";
}

// Exact names
QStringList RMCProfile6ModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool RMCProfile6ModelPlugin::canImport()
{
	return false;
}

// Import data from the specified file
bool RMCProfile6ModelPlugin::importData()
{
	// Create new model to read into
	createModel();
	bool result = RMCProfileFilePluginCommon::readModel(this, fileParser_, targetModel());
	if (!result) discardModel(targetModel());
}

// Return whether this plugin can export data
bool RMCProfile6ModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool RMCProfile6ModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool RMCProfile6ModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool RMCProfile6ModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool RMCProfile6ModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool RMCProfile6ModelPlugin::showImportOptionsDialog()
{
// 	RMCProfile6ImportOptionsDialog optionsDialog(pluginOptions_);

// 	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}

// Return whether the plugin has export options
bool RMCProfile6ModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool RMCProfile6ModelPlugin::showExportOptionsDialog()
{
// 	RMCProfile6ExportOptionsDialog optionsDialog(pluginOptions_);

// 	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}

/*
        *** DL_POLY_4 Plugin Functions
        *** src/plugins/io_dlpoly/dlp4_funcs.cpp
        Copyright T. Youngs 2016-2016
        Copyright A. M. Elena 2016-2016

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

#include "plugins/io_dlpoly/dlp4.hui"
#include "plugins/io_dlpoly/common.h"
#include "model/model.h"
#include "plugins/io_dlpoly/dlp4importoptions.h"
#include "plugins/io_dlpoly/dlp4exportoptions.h"

// Constructor
DLP4ModelPlugin::DLP4ModelPlugin()
{
	// Setup plugin options
	pluginOptions_.add("shiftCell", "true");
	pluginOptions_.add("levcfg","0");
	pluginOptions_.add("useTypeNames", "false");
}

// Destructor
DLP4ModelPlugin::~DLP4ModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* DLP4ModelPlugin::makeCopy()
{
    return new DLP4ModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory DLP4ModelPlugin::category() const
{
    return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString DLP4ModelPlugin::name() const
{
    return QString ( "DL_POLY CONFIG Files (version 4)" );
}

// Nickname of plugin
QString DLP4ModelPlugin::nickname() const
{
    return QString ( "dlpoly4" );
}

// Description (long name) of plugin
QString DLP4ModelPlugin::description() const
{
    return QString ( "Import/export for DL_POLY_4 CONFIG coordinate files" );
}

// Related file extensions
QStringList DLP4ModelPlugin::extensions() const
{
    return QStringList() << "CONFIG";
}

// Exact names
QStringList DLP4ModelPlugin::exactNames() const
{
    return QStringList() << "CONFIG";
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool DLP4ModelPlugin::canImport()
{
    return true;
}

// Import data from the specified file
bool DLP4ModelPlugin::importData()
{
	// Create a new model
	createModel();

	// Read in model data
	bool result = DLPOLYPluginCommon::readCONFIGModel ( this, fileParser_, targetModel(), DLPOLYPluginCommon::DLPOLY4 );
	if ( !result ) {
		discardModel ( targetModel() );
		return false;
	}

	return true;
}

// Return whether this plugin can export data
bool DLP4ModelPlugin::canExport()
{
    return true;
}

// Export data to the specified file
bool DLP4ModelPlugin::exportData()
{
   return DLPOLYPluginCommon::writeCONFIGModel ( this, fileParser_, targetModel(), DLPOLYPluginCommon::DLPOLY4 );
}

// Import next partial data chunk
bool DLP4ModelPlugin::importNextPart()
{
    return false;
}

// Skip next partial data chunk
bool DLP4ModelPlugin::skipNextPart()
{
    return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool DLP4ModelPlugin::hasImportOptions()
{
    return true;
}

// Show import options dialog
bool DLP4ModelPlugin::showImportOptionsDialog()
{
    DLP4ImportOptionsDialog optionsDialog ( pluginOptions_ );
    return ( optionsDialog.updateAndExecute() == QDialog::Accepted );
}

// Return whether the plugin has export options
bool DLP4ModelPlugin::hasExportOptions()
{
    return true;
}

// Show export options dialog
bool DLP4ModelPlugin::showExportOptionsDialog()
{
    DLP4ExportOptionsDialog optionsDialog ( pluginOptions_ );
    return ( optionsDialog.updateAndExecute() == QDialog::Accepted );
}

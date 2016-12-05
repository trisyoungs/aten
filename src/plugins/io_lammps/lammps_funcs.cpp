/*
        *** LAMMPS Plugin Functions
        *** src/plugins/io_lammps/lammps_funcs.cpp
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

#include "plugins/io_lammps/lammps.hui"
#include "plugins/io_lammps/lammpsexportoptions.h"
#include "model/model.h"

// Constructor
LAMMPSModelPlugin::LAMMPSModelPlugin()
{
	// Setup plugin options
	pluginOptions_.add("useTypeNames", "false");
}

// Destructor
LAMMPSModelPlugin::~LAMMPSModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* LAMMPSModelPlugin::makeCopy() const
{
	return new LAMMPSModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType LAMMPSModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int LAMMPSModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString LAMMPSModelPlugin::name() const
{
	return QString("LAMMPS Files");
}

// Nickname of plugin
QString LAMMPSModelPlugin::nickname() const
{
	return QString("lammps");
}

// Description (long name) of plugin
QString LAMMPSModelPlugin::description() const
{
	return QString("Import/export for LAMMPS input files");
}

// Related file extensions
QStringList LAMMPSModelPlugin::extensions() const
{
	return QStringList() << "lammps";
}

// Exact names
QStringList LAMMPSModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool LAMMPSModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool LAMMPSModelPlugin::importData()
{
	// Create a new Model to put our data in
	createModel();

	// Read in model data
	// TODO!

	return true;
}

// Return whether this plugin can export data
bool LAMMPSModelPlugin::canExport() const
{
	return true;
}

// Export data to the specified file
bool LAMMPSModelPlugin::exportData()
{
	return true;
}

// Import next partial data chunk
bool LAMMPSModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool LAMMPSModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool LAMMPSModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool LAMMPSModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool LAMMPSModelPlugin::hasExportOptions() const
{
	return true;
}

// Show export options dialog
bool LAMMPSModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	LAMMPSExportOptionsDialog optionsDialog(targetOptions);

	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}

/*
        *** XYZ Plugin Functions
        *** src/plugins/io_xyz/xyz_funcs.cpp
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

#include "plugins/io_xyz/xyz.hui"
#include "plugins/io_xyz/common.h"
#include "plugins/io_xyz/xyzexportoptions.h"
#include "plugins/io_xyz/xyzimportoptions.h"
#include "model/model.h"

// Constructor
XYZModelPlugin::XYZModelPlugin()
{
	// Setup plugin options
	pluginOptions_.add("readMultipleAsTrajectory", "false");
	pluginOptions_.add("useTypeNames", "false");
}

// Destructor
XYZModelPlugin::~XYZModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* XYZModelPlugin::makeCopy() const
{
	return new XYZModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType XYZModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int XYZModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString XYZModelPlugin::name() const
{
	return QString("XYZ Files (XMol Style)");
}

// Nickname of plugin
QString XYZModelPlugin::nickname() const
{
	return QString("xyz");
}

// Description (long name) of plugin
QString XYZModelPlugin::description() const
{
	return QString("Import/export for XMol-style XYZ coordinate files");
}

// Related file extensions
QStringList XYZModelPlugin::extensions() const
{
	return QStringList() << "xyz";
}

// Exact names
QStringList XYZModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool XYZModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool XYZModelPlugin::importData()
{
	int nModels = 0;
	bool result, readAsTrajectory = false;

	// Read data - first model in file is always the parent (regardless of whether we're reading it as a trajectory or not)
	do
	{
		// Do we need to switch to reading the file as a trajectory?
		if ((nModels == 1) && (pluginOptions_.value("readMultipleAsTrajectory") == "true"))
		{
			readAsTrajectory = true;

			// Copy the parent model into the first frame of the trajectory
			createFrame();
			targetModel()->copy(parentModel());
		}

		// Prepare new model
		if (readAsTrajectory)
		{
			// Check parent model pointer
			if (!parentModel())
			{
				Messenger::error("Error in XYZ plugin - parentModel pointer is NULL.");
				return false;
			}
			createFrame();
		}
		else
		{
			// Reading as individual models, so just create a new one
			// Make a copy of it in 'parentModel', in case we switch to trajectory creation
			createModel();
		}

		// Read in model data
		result = XYZFilePluginCommon::readXYZModel(this, fileParser_, targetModel());

		if (!result)
		{
			// Failed, so remove model (or frame)
			if (readAsTrajectory) discardFrame(targetModel());
			else discardModel(parentModel());

			return false;
		}

		// Increase number of models
		++nModels;
	} while (!fileParser_.eofOrBlank());

	return true;
}

// Return whether this plugin can export data
bool XYZModelPlugin::canExport() const
{
	return true;
}

// Export data to the specified file
bool XYZModelPlugin::exportData()
{
	return XYZFilePluginCommon::writeXYZModel(this, fileParser_, targetModel());
}

// Import next partial data chunk
bool XYZModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool XYZModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool XYZModelPlugin::hasImportOptions() const
{
	return true;
}

// Show import options dialog
bool XYZModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	XYZImportOptionsDialog optionsDialog(targetOptions);

	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}

// Return whether the plugin has export options
bool XYZModelPlugin::hasExportOptions() const
{
	return true;
}

// Show export options dialog
bool XYZModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	XYZExportOptionsDialog optionsDialog(targetOptions);

	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}

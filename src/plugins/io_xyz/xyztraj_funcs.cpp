/*
        *** XYZ Trajectory Plugin Functions
        *** src/plugins/io_xyz/xyztraj_funcs.cpp
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

#include "plugins/io_xyz/xyztraj.hui"
#include "plugins/io_xyz/common.h"
#include "model/model.h"

// Constructor
XYZTrajectoryPlugin::XYZTrajectoryPlugin()
{
	// Setup option keywords
	/* none */
}

// Destructor
XYZTrajectoryPlugin::~XYZTrajectoryPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* XYZTrajectoryPlugin::makeCopy() const
{
	return new XYZTrajectoryPlugin;
}

/*
 * XYZ Model Import / Export Plugin
 */

// Return type of plugin
PluginTypes::PluginType XYZTrajectoryPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int XYZTrajectoryPlugin::category() const
{
	return PluginTypes::TrajectoryFilePlugin;
}

// Name of plugin
QString XYZTrajectoryPlugin::name() const
{
	return QString("XYZ Files (XMol Style)");
}

// Nickname of plugin
QString XYZTrajectoryPlugin::nickname() const
{
	return QString("xyz");
}

// Description (long name) of plugin
QString XYZTrajectoryPlugin::description() const
{
	return QString("Import/export for XMol-style XYZ multiple coordinate files");
}

// Related file extensions
QStringList XYZTrajectoryPlugin::extensions() const
{
	return QStringList() << "xyz";
}

// Exact names
QStringList XYZTrajectoryPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool XYZTrajectoryPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool XYZTrajectoryPlugin::importData()
{
	// Read header information from trajectory file, if there is any
	/* none */

	// Read the first trajectory frame.
	// The model where we should put the frame data will have been set in the FileParser (in parentModel()).
	// Calling FilePluginInterface::importPart(0) will set the file positions we need, and read in the first frame.
	Model* frame = createFrame();
	if (!importPart(0))
	{
		discardFrame(frame);
		return false;
	}

	if (standardOptions_.isSetAndOn(FilePluginStandardImportOptions::CacheAllSwitch))
	{
		Messenger::print("Caching all frames from XYZ trajectory (%i %s)...", nDataParts(), isNPartialDataEstimated() ? "estimated" : "actual");
		int count = 0;
		bool frameResult;
		do
		{
			// Add a new trajectory frame
			Model* frame = createFrame();

			// Attempt to read in the next data part in the file
			frameResult = importPart(++count);
			if (!frameResult) discardFrame(frame);
			
		} while (frameResult && (!fileParser_.eofOrBlank()));

		closeFiles();
	}

	return true;
}

// Return whether this plugin can export data
bool XYZTrajectoryPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool XYZTrajectoryPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool XYZTrajectoryPlugin::importNextPart()
{
	return XYZFilePluginCommon::readXYZModel(this, fileParser_, targetModel());
}

// Skip next partial data chunk
bool XYZTrajectoryPlugin::skipNextPart()
{
	return XYZFilePluginCommon::skipXYZModel(this, fileParser_);
}

/*
 * Options
 */

// Return whether the plugin has import options
bool XYZTrajectoryPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool XYZTrajectoryPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool XYZTrajectoryPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool XYZTrajectoryPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

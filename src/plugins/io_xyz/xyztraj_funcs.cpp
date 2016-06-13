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
	pluginOptionKeywords_ = QStringList();

	// Default values for local variables
	/* none */
}

// Destructor
XYZTrajectoryPlugin::~XYZTrajectoryPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* XYZTrajectoryPlugin::duplicate()
{
	return new XYZTrajectoryPlugin;
}

/*
 * XYZ Model Import / Export Plugin
 */

// Return category of plugin
PluginTypes::FilePluginCategory XYZTrajectoryPlugin::category() const
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
bool XYZTrajectoryPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool XYZTrajectoryPlugin::importData(const KVMap standardOptions)
{
	// Read header information from trajectory file, if there is any
	/* none */

	// Read the first trajectory frame.
	// The model where we should put the frame data will have been set in the FileParser (in targetModel()).
	// Calling FilePluginInterface::importPart(0) will set the file positions we need, and read in the first frame.
	if (!importPart(0, standardOptions)) return false;

	if (standardOptions.value(standardOption(FilePluginInterface::CacheAllOption)) == "true")
	{
		Messenger::print("Caching all frames from XYZ trajectory (%i %s)...", nDataParts(), isNPartialDataEstimated() ? "estimated" : "actual");
		int count = 0;
		bool frameResult;
		do
		{
			// Add a new trajectory frame
			Model* frame = targetModel()->addTrajectoryFrame();
			setTargetFrame(frame);

			// Attempt to read in the next data part in the file
			frameResult = importPart(count, standardOptions);
			if (frameResult) ++count;
			else
			{
				// Delete unused frame
				targetModel()->removeTrajectoryFrame(frame);
			}
			
		} while (frameResult && (!fileParser_.eofOrBlank()));

		closeFiles();
	}

	return true;
}

// Return whether this plugin can export data
bool XYZTrajectoryPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool XYZTrajectoryPlugin::exportData(const KVMap standardOptions)
{
	return false;
}

// Import next partial data chunk
bool XYZTrajectoryPlugin::importNextPart(const KVMap standardOptions)
{
	return XYZFilePluginCommon::readXYZModel(fileParser_, standardOptions, targetFrame());
}

// Skip next partial data chunk
bool XYZTrajectoryPlugin::skipNextPart(const KVMap standardOptions)
{
	return XYZFilePluginCommon::skipXYZModel(fileParser_, standardOptions);
}

/*
 * Local Functions / Data
 */

// Return enum'd plugin option from supplied keyword
int XYZTrajectoryPlugin::pluginOption(QString optionName)
{
	for (int n=0; n<pluginOptionKeywords_.count(); ++n) if (pluginOptionKeywords_.at(n) == optionName) return n;

	return nPluginOptions;
}

// Set option for plugin
bool XYZTrajectoryPlugin::setOption(QString optionName, QString optionValue)
{
	int option = pluginOption(optionName);

	switch (option)
	{
		default:
			Messenger::error("Unrecognised plugin option '" + optionName + "'");
			return false;
			break;
	}

	return true;
}


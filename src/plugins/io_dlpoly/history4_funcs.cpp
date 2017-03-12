/*
        *** DL_POLY_4 Trajectory Plugin Functions
        *** src/plugins/io_dlpoly/history4_funcs.cpp
        Copyright T. Youngs 2016-2017
        Copyright A.M. Elena 2016-2016

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

#include "plugins/io_dlpoly/history4.hui"
#include "plugins/io_dlpoly/common.h"
#include "plugins/io_dlpoly/dlp4importoptions.h"
#include "model/model.h"

// Constructor
DLP4TrajectoryPlugin::DLP4TrajectoryPlugin()
{
	// Setup option keywords and standard options
	pluginOptions_.add("shiftCell", "true");
	standardOptions_.setZMappingType(ElementMap::FirstAlphaZMap);
	standardOptions_.setSwitch(FilePluginStandardImportOptions::PreventFoldingSwitch, true);
	standardOptions_.setSwitch(FilePluginStandardImportOptions::PreventPackingSwitch, true);
	standardOptions_.setSwitch(FilePluginStandardImportOptions::PreventRebondingSwitch, true);
}

// Destructor
DLP4TrajectoryPlugin::~DLP4TrajectoryPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
BasePluginInterface* DLP4TrajectoryPlugin::makeCopy() const
{
	return new DLP4TrajectoryPlugin;
}

/*
 * DLPOLY4 Model Import / Export Plugin
 */

// Return type of plugin
PluginTypes::PluginType DLP4TrajectoryPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int DLP4TrajectoryPlugin::category() const
{
	return PluginTypes::TrajectoryFilePlugin;
}

// Name of plugin
QString DLP4TrajectoryPlugin::name() const
{
	return QString("DL_POLY HISTORY Files (version 4)");
}

// Nickname of plugin
QString DLP4TrajectoryPlugin::nickname() const
{
	return QString("dlpoly4his");
}

// Description (long name) of plugin
QString DLP4TrajectoryPlugin::description() const
{
	return QString("Import for DL_POLY_4 HISTORY files");
}

// Related file extensions
QStringList DLP4TrajectoryPlugin::extensions() const
{
	return QStringList() << "HISTORY" << "HISu" << "HISf"<<"dlpolyhist";
}

// Exact names
QStringList DLP4TrajectoryPlugin::exactNames() const
{
	return QStringList() << "HISTORY";
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool DLP4TrajectoryPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool DLP4TrajectoryPlugin::importData()
{
	// Read header information from trajectory file, if there is any
	QString name;
	if ( !fileParser_.readLine ( name ) ) {
		return false;
	}
	targetModel()->setName ( name );
	if ( !fileParser_.skipLines ( 1 ) ) {
		return false;
	}

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
		Messenger::print("Caching all frames from HISTORY file (%i %s)...", nDataParts(), isNPartialDataEstimated() ? "estimated" : "actual");
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
bool DLP4TrajectoryPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool DLP4TrajectoryPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool DLP4TrajectoryPlugin::importNextPart()
{
	return DLPOLYPluginCommon::readCONFIGModel(this, fileParser_, targetModel(), DLPOLYPluginCommon::DLPOLY4, true);
}

// Skip next partial data chunk
bool DLP4TrajectoryPlugin::skipNextPart()
{
	return DLPOLYPluginCommon::skipFrameModel(this, fileParser_,DLPOLYPluginCommon::DLPOLY4);
}

/*
 * Options
 */

// Return whether the plugin has import options
bool DLP4TrajectoryPlugin::hasImportOptions() const
{
	return true;
}

// Show import options dialog
bool DLP4TrajectoryPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	DLP4ImportOptionsDialog optionsDialog(targetOptions);
	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}

// Return whether the plugin has export options
bool DLP4TrajectoryPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool DLP4TrajectoryPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

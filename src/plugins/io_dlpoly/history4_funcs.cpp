/*
        *** DL_POLY_4 Trajectory Plugin Functions
        *** src/plugins/io_dlpoly/history4_funcs.cpp
        Copyright T. Youngs 2016-2016
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
#include "model/model.h"
#include "plugins/io_dlpoly/history4importoptions.h"

// Constructor
DLP4TrajectoryPlugin::DLP4TrajectoryPlugin()
{
	// Setup option keywords
	/* none */
}

// Destructor
DLP4TrajectoryPlugin::~DLP4TrajectoryPlugin()
{
	pluginOptions_.add("ioTraj", "false");
	standardOptions_.setZMappingType(ElementMap::SingleAlphaZMap);
	standardOptions_.setPreventFolding(false);
	standardOptions_.setPreventPacking(true);
	standardOptions_.setPreventRebonding(true);
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* DLP4TrajectoryPlugin::makeCopy()
{
	return new DLP4TrajectoryPlugin;
}

/*
 * DLPOLY4 Model Import / Export Plugin
 */

// Return category of plugin
PluginTypes::FilePluginCategory DLP4TrajectoryPlugin::category() const
{
	return PluginTypes::TrajectoryFilePlugin;
}

// Name of plugin
QString DLP4TrajectoryPlugin::name() const
{
	return QString("HISTORY Files (DL_POLY_4 Style)");
}

// Nickname of plugin
QString DLP4TrajectoryPlugin::nickname() const
{
	return QString("dlpoly4his");
}

// Description (long name) of plugin
QString DLP4TrajectoryPlugin::description() const
{
	return QString("Import/export for DL_POLY_4 HISTORY files");
}

// Related file extensions
QStringList DLP4TrajectoryPlugin::extensions() const
{
	return QStringList() << "HISTORY" << "HISu" << "HISf";
}

// Exact names
QStringList DLP4TrajectoryPlugin::exactNames() const
{
	return QStringList()<< "HISTORY";
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool DLP4TrajectoryPlugin::canImport()
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

	if (standardOptions_.cacheAll())
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
bool DLP4TrajectoryPlugin::canExport()
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
	return DLPOLYPluginCommon::readCONFIGModel(this, fileParser_, targetModel(),DLPOLYPluginCommon::DLPOLY4,true);
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
bool DLP4TrajectoryPlugin::hasImportOptions()
{
	return true;
}

// Show import options dialog
bool DLP4TrajectoryPlugin::showImportOptionsDialog()
{
  HISTORY4ImportOptionsDialog optionsDialog ( pluginOptions_ );
  return ( optionsDialog.updateAndExecute() == QDialog::Accepted );
}

// Return whether the plugin has export options
bool DLP4TrajectoryPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool DLP4TrajectoryPlugin::showExportOptionsDialog()
{
	return false;
}

/*
        *** DL_POLY_4 Defects Plugin Functions
        *** src/plugins/io_dlpoly/defects4_funcs.cpp
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

#include "plugins/io_dlpoly/defects4.hui"
#include "plugins/io_dlpoly/common.h"
#include "plugins/io_dlpoly/defects4importoptions.h"
#include "model/model.h"

// Constructor
DLP4DefectsPlugin::DLP4DefectsPlugin()
{
	// Setup option keywords and standard options
	pluginOptions_.add("shiftCell", "true");
	pluginOptions_.add("vacancy", "true");
	pluginOptions_.add("interstitial", "true");
	standardOptions_.setZMappingType(ElementMap::FirstAlphaZMap);
	standardOptions_.setSwitch(FilePluginStandardImportOptions::PreventFoldingSwitch, true);
	standardOptions_.setSwitch(FilePluginStandardImportOptions::PreventPackingSwitch, true);
	standardOptions_.setSwitch(FilePluginStandardImportOptions::PreventRebondingSwitch, true);
}

// Destructor
DLP4DefectsPlugin::~DLP4DefectsPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
BasePluginInterface* DLP4DefectsPlugin::makeCopy() const
{
	return new DLP4DefectsPlugin;
}

/*
 * DLPOLY4 Model Import / Export Plugin
 */

// Return type of plugin
PluginTypes::PluginType DLP4DefectsPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int DLP4DefectsPlugin::category() const
{
	return PluginTypes::TrajectoryFilePlugin;
}

// Name of plugin
QString DLP4DefectsPlugin::name() const
{
	return QString("DL_POLY DEFECTS Files (version 4)");
}

// Nickname of plugin
QString DLP4DefectsPlugin::nickname() const
{
	return QString("dlpoly4def");
}

// Description (long name) of plugin
QString DLP4DefectsPlugin::description() const
{
	return QString("Import for DL_POLY_4 DEFECTS files");
}

// Related file extensions
QStringList DLP4DefectsPlugin::extensions() const
{
	return QStringList() << "DEFECTS";
}

// Exact names
QStringList DLP4DefectsPlugin::exactNames() const
{
	return QStringList() << "DEFECTS";
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool DLP4DefectsPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool DLP4DefectsPlugin::importData()
{
	// Read header information from trajectory file, if there is any
	QString name;
	if ( !fileParser_.readLine ( name ) ) {
		return false;
	}
	targetModel()->setName ( name );
  if ( !fileParser_.parseLine() ) {
    return false;
  }
  setNDataParts(fileParser_.argi(1));

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
		Messenger::print("Caching all frames from DEFECTS file (%i %s)...", nDataParts(), isNPartialDataEstimated() ? "estimated" : "actual");
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
bool DLP4DefectsPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool DLP4DefectsPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool DLP4DefectsPlugin::importNextPart()
{
	return DLPOLYPluginCommon::readDEFECTSModel(this, fileParser_, targetModel());
}

// Skip next partial data chunk
bool DLP4DefectsPlugin::skipNextPart()
{
	return DLPOLYPluginCommon::skipFrameDefects(this, fileParser_);
}

/*
 * Options
 */

// Return whether the plugin has import options
bool DLP4DefectsPlugin::hasImportOptions() const
{
	return true;
}

// Show import options dialog
bool DLP4DefectsPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	DLP4DefImportOptionsDialog optionsDialog(targetOptions);
	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}

// Return whether the plugin has export options
bool DLP4DefectsPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool DLP4DefectsPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

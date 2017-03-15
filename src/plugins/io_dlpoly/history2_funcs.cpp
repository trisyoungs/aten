/*
	*** DL_POLY_2 Trajectory Plugin Functions
	*** src/plugins/io_dlpoly/history2_funcs.cpp
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

#include "plugins/io_dlpoly/history2.hui"
#include "plugins/io_dlpoly/common.h"
#include "plugins/io_dlpoly/dlp2importoptions.h"
#include "model/model.h"

// Constructor
DLP2TrajectoryPlugin::DLP2TrajectoryPlugin()
{
	// Setup option keywords and standard options
	pluginOptions_.add("shiftCell", "true");
	pluginOptions_.add("integerSize", "4");
	pluginOptions_.add("realSize", "8");
	standardOptions_.setZMappingType(ElementMap::FirstAlphaZMap);
	standardOptions_.setSwitch(FilePluginStandardImportOptions::PreventFoldingSwitch, true);
	standardOptions_.setSwitch(FilePluginStandardImportOptions::PreventPackingSwitch, true);
	standardOptions_.setSwitch(FilePluginStandardImportOptions::PreventRebondingSwitch, true);
}

// Destructor
DLP2TrajectoryPlugin::~DLP2TrajectoryPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* DLP2TrajectoryPlugin::makeCopy() const
{
	return new DLP2TrajectoryPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType DLP2TrajectoryPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int DLP2TrajectoryPlugin::category() const
{
	return PluginTypes::TrajectoryFilePlugin;
}

// Name of plugin
QString DLP2TrajectoryPlugin::name() const
{
	return QString("DL_POLY HISTORY Files (version 2)");
}

// Nickname of plugin
QString DLP2TrajectoryPlugin::nickname() const
{
	return QString("dlpoly2his");
}

// Return whether the plugin is enabled
bool DLP2TrajectoryPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString DLP2TrajectoryPlugin::description() const
{
	return QString("Import for DL_POLY_2 HISTORY files");
}

// Related file extensions
QStringList DLP2TrajectoryPlugin::extensions() const
{
	return QStringList() << "HISTORY" << "HISu" << "HISf";
}

// Exact names
QStringList DLP2TrajectoryPlugin::exactNames() const
{
	return QStringList() << "HISTORY";
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool DLP2TrajectoryPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool DLP2TrajectoryPlugin::importData()
{
	// Determine format of file (formatted or unformatted)
	if (!DLPOLYPluginCommon::determineHISTORYFormat(this, fileParser_, unformatted_, hasHeader_, DLPOLYPluginCommon::DLPOLY2)) return false;

	// Set recor sizes for unformatted trajectory
	if (unformatted_)
	{
		integerSize_ = pluginOptions_.value("integerSize").toInt();
		realSize_ = pluginOptions_.value("realSize").toInt();

	}

	// Read header information from trajectory file, if there is any
	unformattedAtomNames_.clear();
	if (hasHeader_)
	{
		if (unformatted_)
		{
			int recordLength;

			// Frame / model title
			if (!fileParser_.readRawInteger(recordLength, integerSize_)) return false;
			if (recordLength != 80) return false;
			targetModel()->setName(fileParser_.readChars(80));
			if (!fileParser_.readRawInteger(recordLength, integerSize_)) return false;

			// Next, number of atoms in the system (as a double)
			if (!fileParser_.readRawInteger(recordLength, integerSize_)) return false;
			double realNAtoms;
			if (!fileParser_.readRawDouble(realNAtoms, realSize_)) return false;
			int nAtoms = int(realNAtoms);
			if (!fileParser_.readRawInteger(recordLength, integerSize_)) return false;

			// Atom names - store for later use
			if (!fileParser_.readRawInteger(recordLength, integerSize_)) return false;
			QString atomName;
			for (int n=0; n<nAtoms; ++n) unformattedAtomNames_ << fileParser_.readChars(recordLength/nAtoms);
			if (!fileParser_.readRawInteger(recordLength, integerSize_)) return false;

			// Atomic masses
			if (!fileParser_.readRawInteger(recordLength, integerSize_)) return false;
			fileParser_.skipChars(recordLength);
			if (!fileParser_.readRawInteger(recordLength, integerSize_)) return false;

			// Atom charges
			if (!fileParser_.readRawInteger(recordLength, integerSize_)) return false;
			if (!fileParser_.readRawDoubleArray(unformattedCharges_, nAtoms)) return false;
			if (!fileParser_.readRawInteger(recordLength, integerSize_)) return false;
		}
		else
		{
			// First line is name of model
			QString name;
			if (!fileParser_.readLine(name)) return false;
			targetModel()->setName(name);

			// Second line is keytrj, imcon, nFrames (ignored, since it is repeated)
			if (!fileParser_.skipLines(1)) return false;
		}
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
bool DLP2TrajectoryPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool DLP2TrajectoryPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool DLP2TrajectoryPlugin::importNextPart()
{
	if (unformatted_) return DLPOLYPluginCommon::readUnformattedFrame(this, fileParser_, targetModel(), DLPOLYPluginCommon::DLPOLY2, integerSize_, realSize_, unformattedAtomNames_, unformattedCharges_);
	else return DLPOLYPluginCommon::readCONFIGModel(this, fileParser_, targetModel(), DLPOLYPluginCommon::DLPOLY2, true);
}

// Skip next partial data chunk
bool DLP2TrajectoryPlugin::skipNextPart()
{
	if (unformatted_) return DLPOLYPluginCommon::skipUnformattedFrame(this, fileParser_, DLPOLYPluginCommon::DLPOLY2, integerSize_, realSize_);
	else return DLPOLYPluginCommon::skipFrameModel(this, fileParser_, DLPOLYPluginCommon::DLPOLY2);
}

/*
 * Options
 */

// Return whether the plugin has import options
bool DLP2TrajectoryPlugin::hasImportOptions() const
{
	return true;
}

// Show import options dialog
bool DLP2TrajectoryPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	DLP2ImportOptionsDialog optionsDialog(targetOptions);
	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}

// Return whether the plugin has export options
bool DLP2TrajectoryPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool DLP2TrajectoryPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

/*
        *** MOPAC7.1 Method Plugin Functions
        *** src/plugins/method_mopac71/mopac71_funcs.cpp
        Copyright T. Youngs 2016-2017

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

#include "plugins/method_mopac71/mopac71.hui"
#include "plugins/interfaces/fileplugin.h"
#include "plugins/method_mopac71/common.h"
#include "plugins/pluginstore.h"
#include "model/model.h"

// Constructor
MOPAC71MethodPlugin::MOPAC71MethodPlugin()
{
	// Plugin options
	MOPAC71Common::initialiseOptions(pluginOptions_);
}

// Destructor
MOPAC71MethodPlugin::~MOPAC71MethodPlugin()
{
}

/*
 * Instance Handling
 */
// Return a copy of the plugin object
BasePluginInterface* MOPAC71MethodPlugin::makeCopy() const
{
	return new MOPAC71MethodPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType MOPAC71MethodPlugin::type() const
{
	return PluginTypes::MethodPlugin;
}

// Return category of plugin
int MOPAC71MethodPlugin::category() const
{
	return PluginTypes::GeneralMethodPlugin;
}

// Name of plugin
QString MOPAC71MethodPlugin::name() const
{
	return QString("MOPAC7.1 Plugin");
}

// Nickname of plugin
QString MOPAC71MethodPlugin::nickname() const
{
	return QString("mopac71");
}

// Description (long name) of plugin
QString MOPAC71MethodPlugin::description() const
{
	return QString("MOPAC functionality (v7.1 public domain version)");
}

/*
 * Method
 */

// Run method on the current target model
bool MOPAC71MethodPlugin::runMethod()
{
	// Get the name of a temporary file prefix - this will let us delete all MOPAC-generated files afterwards
	QString jobBaseName = addTemporaryFilePrefix("AtenMOPAC71Method");
	int jobBaseNameLength = jobBaseName.length();

	// Save the input file - use MOPAC71ControlModelPlugin ('mopac71control') to do it...
	// -- First, find the plugin
	const FilePluginInterface* controlPluginMaster = pluginStore_->findFilePluginByNickname(PluginTypes::ModelFilePlugin, PluginTypes::ExportPlugin, "mopac71control");
	if (!controlPluginMaster)
	{
		Messenger::error("Error: MOPAC71MethodPlugin also requires the MOPAC71ControlModelPlugin.");
		return false;
	}
	// -- Now, save the file. Pass our own pluginOptions_ on to the FilePluginInterface, since they map onto each other
	FilePluginInterface* plugin = (FilePluginInterface*) controlPluginMaster->duplicate();
	QString mopacInput = jobBaseName + ".mop";
	if (!plugin->openOutput(mopacInput))
	{
		Messenger::error("Couldn't open temporary file '" + mopacInput + "' for MOPAC job.");
		return false;
	}
	plugin->setParentModel(targetModel());
	plugin->setOptions(pluginOptions_);
	if (!plugin->exportData())
	{
		Messenger::error("Couldn't export data for MOPAC71 optimisation job.");
		return false;
	}
	plugin->closeFiles();

	// Input file is ready, so run MOPAC...
	printf("HERE.........\n");
	bool result = runmopac71_(qPrintable(jobBaseName), jobBaseNameLength);
	if (!result) Messenger::error("Failed to perform MOPAC calculation.");
	printf("HERE.........\n");

	// Dump output file to Messenger...
	QFile file(jobBaseName + ".out");
	if (file.open(QIODevice::ReadOnly))
	{
		QTextStream stream(&file);
		while (!stream.atEnd()) Messenger::print(stream.readLine());
		file.close();
	}
	else Messenger::error("Couldn't retrieve MOPAC output '" + file.fileName()+ "' for display.");
	printf("HERE.........\n");

	return true;
}

/*
 * Options
 */

// Return whether the plugin has options
bool MOPAC71MethodPlugin::hasOptions()
{
	return false;
}

// Show options dialog
bool MOPAC71MethodPlugin::showOptionsDialog()
{
	return false;
}

/*
 * Auxiliary Functions
 */

// Run MOPAC using the base jobname specified
bool MOPAC71MethodPlugin::runMopac(QString jobBaseName)
{
	// Run MOPAC
	int jobBaseNameLength = jobBaseName.length();
	bool result = runmopac71_(qPrintable(jobBaseName), jobBaseNameLength);
	if (!result) Messenger::error("Failed to perform MOPAC calculation.");

	// Dump output file to Messenger...
	QFile file(jobBaseName + ".out");
	if (file.open(QIODevice::ReadOnly))
	{
		QTextStream stream(&file);
		while (!stream.atEnd()) Messenger::print(stream.readLine());
		file.close();
	}
	else Messenger::error("Couldn't retrieve MOPAC log '" + file.fileName()+ "' for display.");

	return result;
}
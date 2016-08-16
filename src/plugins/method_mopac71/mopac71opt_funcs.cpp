/*
        *** MOPAC7.1 Optimisation Plugin Functions
        *** src/plugins/method_mopac71/mopac71opt_funcs.cpp
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

#include "plugins/method_mopac71/mopac71opt.hui"
#include "plugins/method_mopac71/mopac71control.hui"
#include "plugins/method_mopac71/mopac71.hui"
#include "model/model.h"

// Constructor
MOPAC71OptimisationPlugin::MOPAC71OptimisationPlugin()
{
	// Plugin options
// 	pluginOptions_.add("GEO-OK", 
}

// Destructor
MOPAC71OptimisationPlugin::~MOPAC71OptimisationPlugin()
{
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType MOPAC71OptimisationPlugin::type() const
{
	return PluginTypes::MethodPlugin;
}

// Return category of plugin
int MOPAC71OptimisationPlugin::category() const
{
	return PluginTypes::OptimisationMethodPlugin;
}

// Name of plugin
QString MOPAC71OptimisationPlugin::name() const
{
	return QString("MOPAC7.1 Optimisation Plugin");
}

// Nickname of plugin
QString MOPAC71OptimisationPlugin::nickname() const
{
	return QString("mopac71opt");
}

// Description (long name) of plugin
QString MOPAC71OptimisationPlugin::description() const
{
	return QString("MOPAC7.1 geometry optimiser (v7.1 public domain version)");
}

/*
 * Method
 */

// Run method on the current target model
bool MOPAC71OptimisationPlugin::runMethod()
{
	// Get the name of a temporary file prefix - this will let us delete all MOPAC-generated files afterwards
	QString jobBaseName = addTemporaryFilePrefix("AtenMOPAC71OptimisationPlugin");
	int jobBaseNameLength = jobBaseName.length();

	// Save the input file - use MOPAC71ControlMethodPlugin to do it...
	MOPAC71ControlModelPlugin controlPlugin;
	QString mopacInput = jobBaseName + ".mop";
	if (!controlPlugin.openOutput(mopacInput))
	{
		Messenger::error("Couldn't open temporary file '" + mopacInput + "' for MOPAC job.");
		return false;
	}
	KVMap mopacOptions;
// 	mopacOptions.add();
	controlPlugin.setOptions(mopacOptions);
	controlPlugin.setParentModel(targetModel());

	// Run MOPAC
	MOPAC71MethodPlugin mopacPlugin;
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
	else Messenger::error("Couldn't retrieve MOAPC output '" + file.fileName()+ "' for display.");

	return result;
}

/*
 * Options
 */

// Return whether the plugin has options
bool MOPAC71OptimisationPlugin::hasOptions()
{
	return false;
}

// Show options dialog
bool MOPAC71OptimisationPlugin::showOptionsDialog()
{
	return false;
}

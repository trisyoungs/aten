/*
        *** MOPAC7.1 Method Plugin Functions
        *** src/plugins/method_mopac71/mopac71_funcs.cpp
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

#include "plugins/method_mopac71/mopac71.hui"
#include "model/model.h"

// Constructor
MOPAC71MethodPlugin::MOPAC71MethodPlugin()
{
	// Plugin options
// 	pluginOptions_.add("GEO-OK", 
}

// Destructor
MOPAC71MethodPlugin::~MOPAC71MethodPlugin()
{
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
	return PluginTypes::OptimisationMethodPlugin;
}

// Name of plugin
QString MOPAC71MethodPlugin::name() const
{
	return QString("MOPAC7 Plugin");
}

// Nickname of plugin
QString MOPAC71MethodPlugin::nickname() const
{
	return QString("mopac71");
}

// Description (long name) of plugin
QString MOPAC71MethodPlugin::description() const
{
	return QString("MOPAC7 functionality (v7.1 public domain version)");
}

/*
 * Method
 */

// Run method on the current target model
bool MOPAC71MethodPlugin::runMethod()
{
	Messenger:: error("Don't try to use runMethod() from MOPAC71MethodPlugin.");
	Messenger:: error("Use it's auxiliary functions instead.");

	return false;
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
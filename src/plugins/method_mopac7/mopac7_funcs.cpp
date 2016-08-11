/*
        *** MOPAC7 Method Plugin Functions
        *** src/plugins/method_mopac7/mopac7_funcs.cpp
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

#include "plugins/method_mopac7/mopac7.hui"
#include "model/model.h"

// Constructor
Mopac7MethodPlugin::Mopac7MethodPlugin()
{
	// Plugin options
// 	pluginOptions_.add("GEO-OK", 
}

// Destructor
Mopac7MethodPlugin::~Mopac7MethodPlugin()
{
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType Mopac7MethodPlugin::type() const
{
	return PluginTypes::MethodPlugin;
}

// Return category of plugin
int Mopac7MethodPlugin::category() const
{
	return PluginTypes::OptimisationMethodPlugin;
}

// Name of plugin
QString Mopac7MethodPlugin::name() const
{
	return QString("MOPAC7 Plugin");
}

// Nickname of plugin
QString Mopac7MethodPlugin::nickname() const
{
	return QString("mopac7");
}

// Description (long name) of plugin
QString Mopac7MethodPlugin::description() const
{
	return QString("MOPAC7 functionality (v1.15)");
}

/*
 * Method
 */

// Run method on the current target model
bool Mopac7MethodPlugin::runMethod()
{
	// Get the name of a temporary file prefix - this will let us delete all MOPAC-generated files afterwards
	QString jobBaseName = addTemporaryFilePrefix("AtenMopac7MethodPlugin");
	int jobBaseNameLength = jobBaseName.length();

	// Open the input file...
	QFile inputFile(jobBaseName + ".mop");
	if (!inputFile.open(QIODevice::WriteOnly))
	{
		Messenger::error("Failed to open temporary file for MOPAC input");
		return false;
	}

	// Create input deck
	QTextStream textStream(&inputFile);
	textStream << QString("%1").arg("PM3 GEO-OK", -80, QChar(' ')) << endl;
	textStream << QString("%1").arg("Water", -80, QChar(' ')) << endl;
	textStream << QString("%1").arg("Coordinates churned out by Aten.", -80, QChar(' ')) << endl;
	textStream << QString("%1").arg("  O     0.000000 0     0.000000 0     0.000000 0  0  0  0", -80, QChar(' ')) << endl;
	textStream << QString("%1").arg("  H     1.000000 1     0.000000 0     0.000000 0  1  0  0", -80, QChar(' ')) << endl;
	textStream << QString("%1").arg("  H     1.000000 1   108.000000 1     0.000000 0  1  2  0", -80, QChar(' ')) << endl;
	inputFile.close();

	// Run MOPAC
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
	else Messenger::error("Couldn't retrieve MOAPC output for display.");

	return result;
}

/*
 * Options
 */

// Return whether the plugin has options
bool Mopac7MethodPlugin::hasOptions()
{
	return false;
}

// Show options dialog
bool Mopac7MethodPlugin::showOptionsDialog()
{
	return false;
}

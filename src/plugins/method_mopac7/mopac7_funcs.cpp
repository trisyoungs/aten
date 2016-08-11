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
	// We will redirect all output from the fortran code to a temporary file (from stdout)
	// All write() statements that were unit 6 will reference the unit number we provide here.
	QString outputFile("/home/tris/src/aten/output.txt");
	int unit = 6;
	if (!om7prep_(&unit, qPrintable(outputFile), outputFile.length()))
	{
		Messenger::error("Failed to prepare OpenMOPAC7 for calculation.");
		return false;
	}

	// Create character array of input lines
	QString inputFile = QString("%1").arg("PM3 GEO-OK", -80, QChar(' '))
		+ QString("%1").arg("Water", -80, QChar(' '))
		+ QString("%1").arg("Coordinates churned out by Aten.", -80, QChar(' '))
// 		+ QString("%1").arg("  O     0.000000", -80, QChar(' '))
// 		+ QString("%1").arg("  H     1.0  1", -80, QChar(' '))
// 		+ QString("%1").arg("  H     1.0  1  108.0  2", -80, QChar(' '));
		+ QString("%1").arg("  O     0.000000 0     0.000000 0     0.000000 0  0  0  0", -80, QChar(' '))
		+ QString("%1").arg("  H     1.000000 1     0.000000 0     0.000000 0  1  0  0", -80, QChar(' '))
		+ QString("%1").arg("  H     1.000000 1   108.000000 1     0.000000 0  1  2  0", -80, QChar(' '));

	// Setup MOPAC calculation - pass input deck
	if (!om7setup_(qPrintable(inputFile), inputFile.length()))
	{
		Messenger::error("Failed to setup MOPAC7 calculation.");
		return false;
	}

	// Perform MOPAC calculation
	bool result = om7calc_();
	if (!result) Messenger::error("MOPAC7 calculation failed - see output for details.");

	// Finalise MOPAC7
	om7finalise_(&unit);
	
	// Dump output file to Messenger...
	QFile file(outputFile);
	if (file.open(QIODevice::ReadOnly))
	{
		QTextStream stream(&file);
		while (!stream.atEnd()) Messenger::print(stream.readLine());
		file.close();
	}
	else Messenger::error("Couldn't retrieve OpenMOPAC output for display.");

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

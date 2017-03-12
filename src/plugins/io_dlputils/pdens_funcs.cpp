/*
        *** PDens (dlputils) Grid Plugin Functions
        *** src/plugins/io_dlputils/pdens_funcs.cpp
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

#include "plugins/io_dlputils/pdens.hui"
#include "model/model.h"

// Constructor
PDensGridPlugin::PDensGridPlugin()
{
}

// Destructor
PDensGridPlugin::~PDensGridPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* PDensGridPlugin::makeCopy() const
{
	return new PDensGridPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType PDensGridPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int PDensGridPlugin::category() const
{
	return PluginTypes::GridFilePlugin;
}

// Name of plugin
QString PDensGridPlugin::name() const
{
	return QString("PDens (dlputils) 3D probability density");
}

// Nickname of plugin
QString PDensGridPlugin::nickname() const
{
	return QString("pdens");
}

// Description (long name) of plugin
QString PDensGridPlugin::description() const
{
	return QString("Import/export for dlputils PDens files");
}

// Related file extensions
QStringList PDensGridPlugin::extensions() const
{
	return QStringList() << "pdens";
}

// Exact names
QStringList PDensGridPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool PDensGridPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool PDensGridPlugin::importData()
{
	// Create new grid in the target model
	Grid* grid = createGrid(targetModel());
	grid->setName(fileParser_.filename());

	// First line contains number of gridpoints in each direction x,y,z
	if (!fileParser_.parseLine(Parser::SkipBlanks)) return false;
	Messenger::print("GridXYZ from file = %i %i %i\n", fileParser_.argi(0), fileParser_.argi(1), fileParser_.argi(2));
 	grid->initialise(Grid::RegularXYZData, fileParser_.arg3i(0));

	// Second line contains axis system
	if (!fileParser_.parseLine(Parser::SkipBlanks)) return false;
	Matrix axes;
	axes.setColumn(0, fileParser_.arg3d(0), 0.0);
	axes.setColumn(1, fileParser_.arg3d(3), 0.0);
	axes.setColumn(2, fileParser_.arg3d(6), 0.0);
	grid->setAxes(axes);
 
	// Third line contains grid origin (lower left-hand corner)
	if (!fileParser_.parseLine(Parser::SkipBlanks)) return false;
	grid->setOrigin(fileParser_.arg3d(0));
 
	// Fourth line is loop order
	if (!fileParser_.parseLine(Parser::SkipBlanks)) return false;
	grid->setLoopOrder(fileParser_.argc(0));

	// Read in grid data
	int nPoints = grid->nPoints();
	for (int n=0; n<nPoints; n++)
	{
		if (!fileParser_.parseLine(Parser::SkipBlanks))
		{
			Messenger::error("Failed to read grid data at point %i\n", n);
			break;
		}

		grid->setNextData(fileParser_.argd(0));
	}

	return true;
}

// Return whether this plugin can export data
bool PDensGridPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool PDensGridPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool PDensGridPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool PDensGridPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool PDensGridPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool PDensGridPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool PDensGridPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool PDensGridPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}
 

/*
        *** Surface (dlputils) Grid Plugin Functions
        *** src/plugins/io_dlputils/surface_funcs.cpp
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

#include "plugins/io_dlputils/surface.hui"
#include "model/model.h"

// Constructor
SurfaceGridPlugin::SurfaceGridPlugin()
{
}

// Destructor
SurfaceGridPlugin::~SurfaceGridPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* SurfaceGridPlugin::makeCopy()
{
	return new SurfaceGridPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType SurfaceGridPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int SurfaceGridPlugin::category() const
{
	return PluginTypes::GridFilePlugin;
}

// Name of plugin
QString SurfaceGridPlugin::name() const
{
	return QString("Surface (dlputils) 2D grid data");
}

// Nickname of plugin
QString SurfaceGridPlugin::nickname() const
{
	return QString("dlpsurf");
}

// Description (long name) of plugin
QString SurfaceGridPlugin::description() const
{
	return QString("Import for dlputils 2D surface files");
}

// Related file extensions
QStringList SurfaceGridPlugin::extensions() const
{
	return QStringList() << "surf";
}

// Exact names
QStringList SurfaceGridPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool SurfaceGridPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool SurfaceGridPlugin::importData()
{
	// Create new grid in the target model
	Grid* grid = createGrid(targetModel());
	grid->setName(fileParser_.filename());

	// First line contains number of gridpoints in each direction x,y,z
	if (!fileParser_.parseLine(Parser::SkipBlanks)) return false;
	Messenger::print("GridXY from file = %i %i\n", fileParser_.argi(0), fileParser_.argi(1));
 	grid->initialise(Grid::RegularXYData, Vec3<int>(fileParser_.argi(0),fileParser_.argi(1),0));

	// Second line contains axis system
	if (!fileParser_.parseLine(Parser::SkipBlanks)) return false;
	Matrix axes;
	axes.setColumn(0, fileParser_.arg3d(0), 0.0);
	axes.setColumn(1, fileParser_.arg3d(3), 0.0);
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
bool SurfaceGridPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool SurfaceGridPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool SurfaceGridPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool SurfaceGridPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool SurfaceGridPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool SurfaceGridPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool SurfaceGridPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool SurfaceGridPlugin::showExportOptionsDialog()
{
	return false;
}
 

/*
        *** Surface Model Plugin Functions
        *** src/plugins/io_surface/surface_funcs.cpp
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

#include "plugins/io_surface/surface.hui"
#include "model/model.h"

// Constructor
SurfaceModelPlugin::SurfaceModelPlugin()
{
}

// Destructor
SurfaceModelPlugin::~SurfaceModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* SurfaceModelPlugin::makeCopy()
{
	return new SurfaceModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory SurfaceModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString SurfaceModelPlugin::name() const
{
	return QString("Surface (dlputils) 3D probability density");
}

// Nickname of plugin
QString SurfaceModelPlugin::nickname() const
{
	return QString("surface");
}

// Description (long name) of plugin
QString SurfaceModelPlugin::description() const
{
	return QString("Import/export for dlputils Surface files");
}

// Related file extensions
QStringList SurfaceModelPlugin::extensions() const
{
	return QStringList() << "surface";
}

// Exact names
QStringList SurfaceModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool SurfaceModelPlugin::canImport()
{
	return true;
}

// Import data from the spesurfaceied file
bool SurfaceModelPlugin::importData()
{
//	# Variable declarations
//	int n,nx,ny,nz,npoints;
//	vector origin, vx, vy;
//	double data;
//	string order;
//
//	# Create new grid
//	Grid g = newGrid(filterFilename());
//
//	# First line contains number of gridpoints in each direction x,y
//	readLine(nx,ny);
//	initGrid("regularxy",nx,ny,0);
//
//	# Second line contains vectors between gridpoints
//	readLine(vx.x, vx.y, vx.z, vy.x, vy.y, vy.z);
//	gridAxes(vx.x, vx.y, vx.z, vy.x, vy.y, vy.z, 0, 0, 1);
//
//	# Third line contains grid origin (lower left-hand corner) and optional data minima and maxima
//	readLine(g.origin.x, g.origin.y, g.origin.z, g.dataMinimum.x, g.dataMinimum.y, g.dataMinimum.z, g.dataMaximum.x, g.dataMaximum.y, g.dataMaximum.z);
//	g.axisPositionX = { 0.0, g.dataMinimum.y, g.dataMinimum.z };
//	g.axisPositionY = { g.dataMinimum.x, 0.0, g.dataMinimum.z };
//	g.axisPositionZ = { g.dataMinimum.x, g.dataMinimum.y, 0.0 };
//
//	# Fourth line contains loop order
//	readLine(order);
//	gridLoopOrder(order);
//
//	# Read in grid data
//	npoints = nx*ny;
//	for (n=0; n<npoints; ++n)
//	{
//		if (!readLine(data)) error("Couldn't read point %i from file.\n", n+1);
//		addNextGridPoint(data);
//	}
//	finaliseGrid();
	return true;
}

// Return whether this plugin can export data
bool SurfaceModelPlugin::canExport()
{
	return false;
}

// Export data to the spesurfaceied file
bool SurfaceModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool SurfaceModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool SurfaceModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool SurfaceModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool SurfaceModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool SurfaceModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool SurfaceModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

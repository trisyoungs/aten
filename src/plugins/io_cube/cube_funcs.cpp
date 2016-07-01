/*
        *** CUBE Model Plugin Functions
        *** src/plugins/io_cube/cube_funcs.cpp
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

#include "plugins/io_cube/cube.hui"
#include "model/model.h"

// Constructor
CUBEModelPlugin::CUBEModelPlugin()
{
}

// Destructor
CUBEModelPlugin::~CUBEModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* CUBEModelPlugin::makeCopy()
{
	return new CUBEModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType CUBEModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int CUBEModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString CUBEModelPlugin::name() const
{
	return QString("CUBE (dlputils) 3D probability density");
}

// Nickname of plugin
QString CUBEModelPlugin::nickname() const
{
	return QString("cube");
}

// Description (long name) of plugin
QString CUBEModelPlugin::description() const
{
	return QString("Import/export for dlputils CUBE files");
}

// Related file extensions
QStringList CUBEModelPlugin::extensions() const
{
	return QStringList() << "cube";
}

// Exact names
QStringList CUBEModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool CUBEModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool CUBEModelPlugin::importData()
{
//filter(type="importmodel", name="Gaussian CUBE", nickname="cube", extension="cube", glob="*.cube", zmap="numeric")
//{
//	# Variable declaration
//	string e,title;
//	int natoms,nx,ny,nz,i,j,k;
//	vector origin, x,y,z;
//	double rx,ry,rz,data;
//
//	# Since we must create the model first, and then set all grid date afterwards,
//	# store all grid-related values for later until we've finalised the model.
//
//	# Lines 1-2: Comments (use first as title)
//	getLine(title);
//	skipLine();
//
//	# Line 3: Number of atoms and coordinate origin of surface
//	readLine(natoms, origin.x, origin.y, origin.z);
//
//	# Lines 4-6: Number of voxels and axis vector
//	readLine(nx, x.x, x.y, x.z);
//	readLine(ny, y.x, y.y, y.z);
//	readLine(nz, z.x, z.y, z.z);
//
//	# Lines 7-(7+natoms) Molecule Definition
//	newModel(title);
//	for (i=0; i<natoms; ++i)
//	{
//		readLineF("%s%*%f%f%f", e, rx ,ry ,rz);
//		newAtom(e, rx, ry, rz);
//	}
//	finaliseModel();
//	rebond();
//
//	# Create a grid associated to this model, and set it up with data we read previously
//	newGrid(title);
//	initGrid("regularxyz",nx,ny,nz);
//	printf("Grid size (points) is : {%i,%i,%i}\n", nx, ny, nz);
//	printf("Grid origin is : {%f,%f,%f}\n", origin.x, origin.y, origin.z);
//	gridOrigin(origin.x, origin.y, origin.z);
//	gridAxes(x.x, x.y, x.z, y.x, y.y, y.z, z.x, z.y, z.z);
//
//	# Volumetric data (five points per line)
//	for (i=1; i<=nx; ++i)
//	{
//		for (j=1; j<=ny; ++j)
//		{
//			for (k=1; k<=nz; ++k)
//			{
//				if (!nextArg(data)) error("Error reading point {%i,%i,%i} from file.\n", i,j,k);
//				addGridPoint(i, j, k, data);
//			}
//		}
//	}
//	finaliseGrid();
//}
//
//filter(type="importgrid", name="Gaussian CUBE", nickname="cube", extension="cube", glob="*.cube", zmap="numeric", id=1)
//{
//	# Variable declaration
//	string e,title;
//	int natoms,nx,ny,nz,i,j,k;
//	vector origin, x,y,z;
//	double data;
//
//	# Lines 1-2: Comments (use first as title)
//	getLine(title);
//	skipLine();
//
//	# Line 3: Number of atoms and coordinate origin of surface
//	readLine(natoms, origin.x, origin.y, origin.z);
//
//	# Lines 4-6: Number of voxels and axis vector
//	readLine(nx, x.x, x.y, x.z);
//	readLine(ny, y.x, y.y, y.z);
//	readLine(nz, z.x, z.y, z.z);
//
//	# Lines 7-(7+natoms) Molecule Definition
//	skipLine(natoms);
//
//	# Create a grid associated to this model, and set it up with data we read previously
//	newGrid(title);
//	initGrid("regularxyz",nx,ny,nz);
//	printf("Grid size (points) is : {%i,%i,%i}\n", nx, ny, nz);
//	printf("Grid origin is : {%f,%f,%f}\n", origin.x, origin.y, origin.z);
//	gridOrigin(origin.x, origin.y, origin.z);
//	gridAxes(x.x, x.y, x.z, y.x, y.y, y.z, z.x, z.y, z.z);
//
//	# Volumetric data (five points per line)
//	for (i=1; i<=nx; ++i)
//	{
//		for (j=1; j<=ny; ++j)
//		{
//			for (k=1; k<=nz; ++k)
//			{
//				if (!nextArg(data)) error("Error reading point {%i,%i,%i} from file.\n", i,j,k);
//				addGridPoint(i, j, k, data);
//			}
//		}
//	}
//	finaliseGrid();
//}
//
	return true;
}

// Return whether this plugin can export data
bool CUBEModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool CUBEModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool CUBEModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool CUBEModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool CUBEModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool CUBEModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool CUBEModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool CUBEModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

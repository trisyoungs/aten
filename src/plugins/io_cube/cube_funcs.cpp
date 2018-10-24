/*
        *** Cube Plugin Functions
        *** src/plugins/io_cube/cube_funcs.cpp
        Copyright T. Youngs 2016-2018

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
CubeGridPlugin::CubeGridPlugin()
{
}

// Destructor
CubeGridPlugin::~CubeGridPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* CubeGridPlugin::makeCopy() const
{
	return new CubeGridPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType CubeGridPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int CubeGridPlugin::category() const
{
	return PluginTypes::GridFilePlugin;
}

// Name of plugin
QString CubeGridPlugin::name() const
{
	return QString("Gaussian cube");
}

// Nickname of plugin
QString CubeGridPlugin::nickname() const
{
	return QString("cube");
}

// Return whether the plugin is enabled
bool CubeGridPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString CubeGridPlugin::description() const
{
	return QString("Import for Gaussian cube files");
}

// Related file extensions
QStringList CubeGridPlugin::extensions() const
{
	return QStringList() << "cube";
}

// Exact names
QStringList CubeGridPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool CubeGridPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool CubeGridPlugin::importData()
{
	// Lines 1-2: Comments (use first as title)
	QString name;
	if (!fileParser_.readLine(name)) return false;
	fileParser_.skipLines(1);

	// Line 3: Number of atoms and coordinate origin of surface
	if (!fileParser_.parseLine()) return false;
	int nAtoms = fileParser_.argi(0);
	Vec3<double> origin = fileParser_.arg3d(1);

	// Lines 4-6: Number of voxels and axis vector
	Vec3<int> nXYZ;
	if (!fileParser_.parseLine()) return false;
	nXYZ.x = fileParser_.argi(0);
	double xFactor = nXYZ.x < 0 ? 1.0 : ANGBOHR;
	Vec3<double> xAxis = fileParser_.arg3d(1);
	nXYZ.y = fileParser_.argi(0);
	double yFactor = nXYZ.y < 0 ? 1.0 : ANGBOHR;
	Vec3<double> yAxis = fileParser_.arg3d(1);
	nXYZ.z  = fileParser_.argi(0);
	double zFactor = nXYZ.z < 0 ? 1.0 : ANGBOHR;
	Vec3<double> zAxis = fileParser_.arg3d(1);

	// Lines 7-(7+natoms) Molecule Definition (SKIP)
	fileParser_.skipLines(nAtoms);

	// Create new grid in the target model
	Grid* grid = createGrid(targetModel());
	grid->setName(fileParser_.filename());

	grid->initialise(Grid::RegularXYZData, nXYZ);
	Messenger::print("Grid size (points) is : {%i,%i,%i}\n", nXYZ.x, nXYZ.y, nXYZ.z);
	Messenger::print("Grid origin is : {%f,%f,%f}\n", origin.x, origin.y, origin.z);

	grid->setOrigin(origin);
	Matrix axes;
	axes.setColumn(0, xAxis * xFactor, 0.0);
	axes.setColumn(1, yAxis * yFactor, 0.0);
	axes.setColumn(2, zAxis * zFactor, 0.0);
	grid->setAxes(axes);

	// Read in volumetric data
	QString arg;
	grid->setLoopOrder("zyx");
	for (int i=0; i<nXYZ.x; ++i)
	{
		for (int j=0; j<nXYZ.y; ++j)
		{
			for (int k=0; k<nXYZ.z; ++k)
			{
				if (!fileParser_.readNextArg(arg))
				{
					Messenger::error("Failed to read grid point (%i,%i,%i) from cube file.\n", i+1, j+1, k+1);
					return false;
				}

				grid->setNextData(arg.toDouble());
			}
		}
	}

	return true;
}

// Return whether this plugin can export data
bool CubeGridPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool CubeGridPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool CubeGridPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool CubeGridPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool CubeGridPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool CubeGridPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool CubeGridPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool CubeGridPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}
 

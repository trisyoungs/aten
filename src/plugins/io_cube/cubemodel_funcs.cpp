/*
        *** Cube Model Plugin Functions
        *** src/plugins/io_cube/cubemodel_funcs.cpp
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

#include "plugins/io_cube/cubemodel.hui"
#include "model/model.h"

// Constructor
CubeModelPlugin::CubeModelPlugin()
{
}

// Destructor
CubeModelPlugin::~CubeModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* CubeModelPlugin::makeCopy() const
{
	return new CubeModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType CubeModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int CubeModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString CubeModelPlugin::name() const
{
	return QString("Gaussian cube file (including atomic coordinate data)");
}

// Nickname of plugin
QString CubeModelPlugin::nickname() const
{
	return QString("cube");
}

// Return whether the plugin is enabled
bool CubeModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString CubeModelPlugin::description() const
{
	return QString("Import/export for Gaussian-format cube files");
}

// Related file extensions
QStringList CubeModelPlugin::extensions() const
{
	return QStringList() << "cube";
}

// Exact names
QStringList CubeModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool CubeModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool CubeModelPlugin::importData()
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
	if (!fileParser_.parseLine()) return false;
	nXYZ.y = fileParser_.argi(0);
	double yFactor = nXYZ.y < 0 ? 1.0 : ANGBOHR;
	Vec3<double> yAxis = fileParser_.arg3d(1);
	if (!fileParser_.parseLine()) return false;
	nXYZ.z  = fileParser_.argi(0);
	double zFactor = nXYZ.z < 0 ? 1.0 : ANGBOHR;
	Vec3<double> zAxis = fileParser_.arg3d(1);

	// Create a new model and set its name
	Model* targetModel = createModel();
	targetModel->setName(name);

	// Lines 7-(7+natoms) Molecule Definition
	Vec3<double> r;
	for (int n=0; n<nAtoms; ++n)
	{
		if (!fileParser_.parseLine()) return false;

		r = fileParser_.arg3d(2);
		r.x *= xFactor;
		r.y *= yFactor;
		r.z *= zFactor;
		
		createAtom(targetModel, fileParser_.argc(0), r);
	}

	// Rebond the model
	if (!standardOptions().isSetAndOn(FilePluginStandardImportOptions::PreventRebondingSwitch)) targetModel->calculateBonding(true);

	// Create new grid in the target model
	Grid* grid = createGrid(targetModel);
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
	axes.print();

	// Read in volumetric data
	QString arg;
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
bool CubeModelPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool CubeModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool CubeModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool CubeModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool CubeModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool CubeModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool CubeModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool CubeModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}
 

/*
        *** XYZ Plugin Functions
        *** src/plugins/io_xyz/xyz_funcs.cpp
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

#include "plugins/io_xyz/xyz.h"
#include "model/model.h"

// Constructor
XYZModelPlugin::XYZModelPlugin()
{
}

// Destructor
XYZModelPlugin::~XYZModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
IOPluginInterface* XYZModelPlugin::duplicate()
{
	return new XYZModelPlugin;
}

/*
 * XYZ Model Import / Export Plugin
 */

// Return category of plugin
PluginTypes::IOPluginCategory XYZModelPlugin::category() const
{
	return PluginTypes::IOModelPlugin;
}

// Name of plugin
QString XYZModelPlugin::name() const
{
	return QString("XYZ Files (XMol Style)");
}

// Nickname of plugin
QString XYZModelPlugin::nickName() const
{
	return QString("xyz");
}

// Description (long name) of plugin
QString XYZModelPlugin::description() const
{
	return QString("Import/export for XMol-style XYZ coordinate files");
}

// Related file extensions
QStringList XYZModelPlugin::extensions() const
{
	return QStringList() << "xyz";
}

// Exact names
QStringList XYZModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool XYZModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool XYZModelPlugin::importData(FileParser& parser)
{
	int nAtoms, n;
	QString e, name;
	Vec3<double> r;
	Model* targetModel = NULL;

	// Read data
	while (!parser.eofOrBlank())
	{
		// Read number of atoms from file
		if (!parser.readLineAsInteger(nAtoms)) return false;

		// Next line is name of model
		if (!parser.readLine(name)) return false;

		// Create a new model now....
		targetModel = createModel();
		targetModel->setName(name);

		// Load atoms for model
		for (n=0; n<nAtoms; ++n)
		{
			if (!parser.parseLine()) break;

			// Create the new atom
			r.set(parser.argd(1), parser.argd(2), parser.argd(3));
			targetModel->addAtom(parser.argi(0), r);
		}

		// Rebond the model
		targetModel->calculateBonding(true);
	}
  return true;
}

// Return whether this plugin can export data
bool XYZModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool XYZModelPlugin::exportData(FileParser& parser)
{
	return false;
}

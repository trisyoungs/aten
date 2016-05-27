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

// Return type of plugin
PluginTypes::PluginType XYZModelPlugin::type() const
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

// Return whether this plugin can load data
bool XYZModelPlugin::canLoad()
{
	return true;
}

// Load data from the specified file
bool XYZModelPlugin::load(FileParser& parser)
{
	int nAtoms, n;
	QString e, name;
	double rx, ry, rz, q;
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
// 			Atom* i = targetModel->addAtom();
			// How many arguments did we get?
// 			if (parser.nArgs() == 4) 
// 			readLine(e,rx,ry,rz,q);
// 			i = newAtom(e, rx, ry, rz);
// 			i.q = q;
		}

		// Rebond the model
		targetModel->calculateBonding(true);
	}
}

// Return whether this plugin can save data
bool XYZModelPlugin::canSave()
{
	return false;
}

// Save data to the specified file
bool XYZModelPlugin::save(FileParser& parser)
{
	return true;
}

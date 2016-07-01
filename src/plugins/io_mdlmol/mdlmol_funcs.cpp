/*
        *** MDL Model Plugin Functions
        *** src/plugins/io_mdlmol/mdlmol_funcs.cpp
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

#include "plugins/io_mdlmol/mdlmol.hui"
#include "model/model.h"

// Constructor
MOLModelPlugin::MOLModelPlugin()
{
}

// Destructor
MOLModelPlugin::~MOLModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* MOLModelPlugin::makeCopy()
{
	return new MOLModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType MOLModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int MOLModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString MOLModelPlugin::name() const
{
	return QString("MDL Mol File");
}

// Nickname of plugin
QString MOLModelPlugin::nickname() const
{
	return QString("mol");
}

// Description (long name) of plugin
QString MOLModelPlugin::description() const
{
	return QString("Basic import for MDL Mol files");
}

// Related file extensions
QStringList MOLModelPlugin::extensions() const
{
	return QStringList() << "mol";
}

// Exact names
QStringList MOLModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool MOLModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool MOLModelPlugin::importData()
{
	// Read title line and create model
	QString title;
	if (!fileParser_.readLine(title)) return false;
	Model* model = createModel(title);

	// Skip two lines (???)
	if (!fileParser_.skipLines(2)) return false;

	// Read number of atoms and number of bonds
	if (!fileParser_.parseLine()) return false;
	int nAtoms = fileParser_.argi(0);
	int nBonds = fileParser_.argi(1);

	// Read in atoms
	for (int n=0; n<nAtoms; ++n)
	{
		if (!fileParser_.parseLine()) return false;
		createAtom(model, fileParser_.argc(3), fileParser_.arg3d(0));
	}

	// Read in bonds
	for (int n=0; n<nBonds; ++n)
	{
		if (!fileParser_.parseLine()) return false;
		model->bondAtoms(fileParser_.argi(0)-1, fileParser_.argi(1)-1, (Bond::BondType) fileParser_.argi(2));
	}

	return true;
}

// Return whether this plugin can export data
bool MOLModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool MOLModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool MOLModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool MOLModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool MOLModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool MOLModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool MOLModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool MOLModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

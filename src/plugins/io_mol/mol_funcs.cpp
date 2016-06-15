/*
        *** MOL Model Plugin Functions
        *** src/plugins/io_mol/mol_funcs.cpp
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

#include "plugins/io_mol/mol.hui"
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

// Return category of plugin
PluginTypes::FilePluginCategory MOLModelPlugin::category() const
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
	return QString("Import/export for MDL Mol files");
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

// Import data from the spemolied file
bool MOLModelPlugin::importData()
{
//	# Variable declaration
//	string line,e,title;
//	int n,natoms,nbonds,ii,jj,bondtype;
//	double rx,ry,rz;
//	
//	# Create default model
//	getLine(title);
//	newModel(title);
//	
//	skipLine(2);
//	
//	readLine(natoms,nbonds);
//	
//	for (n=0; n<natoms; ++n)
//	{
//		readLine(rx,ry,rz,e);
//		newAtom(e,rx,ry,rz);
//	}
//	
//	for (n=0; n<nbonds; ++n)
//	{
//		readLine(ii,jj,bondtype);
//		newBond(ii,jj,bondtype);
//	}
//	
//	finaliseModel();
//}
	return true;
}

// Return whether this plugin can export data
bool MOLModelPlugin::canExport()
{
	return false;
}

// Export data to the spemolied file
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
 

/*
        *** RMCProfile V6 Bonds File Plugin Functions
        *** src/plugins/io_rmcprofile/rmcp6bonds_funcs.cpp
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

#include "plugins/io_rmcprofile/rmcp6bonds.hui"
#include "model/model.h"

// Constructor
RMCProfile6BondsModelPlugin::RMCProfile6BondsModelPlugin()
{
	// Setup plugin options
	pluginOptions_.add("myOption", "false");
}

// Destructor
RMCProfile6BondsModelPlugin::~RMCProfile6BondsModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* RMCProfile6BondsModelPlugin::makeCopy()
{
	return new RMCProfile6BondsModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory RMCProfile6BondsModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString RMCProfile6BondsModelPlugin::name() const
{
	return QString("RMCProfile v6 Bonds");
}

// Nickname of plugin
QString RMCProfile6BondsModelPlugin::nickname() const
{
	return QString("rmc6bonds");
}

// Description (long name) of plugin
QString RMCProfile6BondsModelPlugin::description() const
{
	return QString("Import for RMCProfile v6 bonds files");
}

// Related file extensions
QStringList RMCProfile6BondsModelPlugin::extensions() const
{
	return QStringList() << "bonds";
}

// Exact names
QStringList RMCProfile6BondsModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool RMCProfile6BondsModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool RMCProfile6BondsModelPlugin::importData()
{
	// Don't create a new model - instead, we'll use the current targetModel();

	int nAtoms = -1;

	// Read in metadata lines until we find the delimiting line of dots....
	QString line;
	do
	{
		// Read a line from the file and split it
		if (!fileParser_.readAndParseLine(line)) return false;

		if (line.startsWith("Number of atoms = "))
		{
			nAtoms = fileParser_.argi(4);
			Messenger::print("Number of atoms specified in file = %i\n", nAtoms);
			if (nAtoms != targetModel()->nAtoms())
			{
				Messenger::error("NUmber of atoms in file (%i) differs from that in the current model (%i).", nAtoms, targetModel()->nAtoms());
				return false;
			}
		}
		else if (line.startsWith("...")) break;
		
	} while (!fileParser_.eofOrBlank());

	// Clear current bonding in the model
	targetModel()->clearBonding();

	// Now in bonds section, so read them in until the end of the file
	do
	{
		// Read in a line and parse it
		if (!fileParser_.parseLine()) return false;

		// First three items on line are atom index, chemical synbol, and bond type (not used)
		// Fourth item is the double-colon
		int indexI = fileParser_.argi(0) - 1;
		if (indexI > targetModel()->nAtoms())
		{
			Messenger::error("Line in bonds file references an atom index (%i) that is out of range in the current model.", indexI+1);
			continue;
		}
		// Check element
		Atom* i = targetModel()->atom(indexI);
		QString nameI = fileParser_.argc(1) == "D" ? "H" : fileParser_.argc(1);
		if (i->element() != ElementMap::find(nameI, ElementMap::AlphaZMap))
		{
			Messenger::error("Element symbol (%s) for atom %i in bonds file does not match that in the current model.", qPrintable(nameI), indexI+1, qPrintable(ElementMap::symbol(i)));
			continue;
		}

		// Last item on line contains the number of bonds to this atom
		// Leading up to that are sets of 'indexJ elementJ ;', with the indices at args 4, 7, 10 etc.
		int nBonds = fileParser_.argi(fileParser_.nArgs()-1);
		for (int n=0; n<nBonds; ++n)
		{
			int indexJ = fileParser_.argi(4+n*3) - 1;
			Atom* j = targetModel()->atom(indexJ);
			QString nameJ = fileParser_.argc(4+n*3) == "D" ? "H" : fileParser_.argc(4+n*3);

			if (j->element() != ElementMap::find(nameJ, ElementMap::AlphaZMap))
			{
				Messenger::error("Element symbol (%s) for bond partner atom %i in bonds file does not match that in the current model.", qPrintable(nameJ), indexJ+1, qPrintable(ElementMap::symbol(j)));
				continue;
			}

			targetModel()->bondAtoms(i, j, Bond::Single);
		}

	} while (!fileParser_.eofOrBlank());

	return true;
}

// Return whether this plugin can export data
bool RMCProfile6BondsModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool RMCProfile6BondsModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool RMCProfile6BondsModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool RMCProfile6BondsModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool RMCProfile6BondsModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool RMCProfile6BondsModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool RMCProfile6BondsModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool RMCProfile6BondsModelPlugin::showExportOptionsDialog()
{
	return false;
}

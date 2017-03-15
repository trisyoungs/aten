/*
        *** QuantumEspresso Output Import Plugin Functions
        *** src/plugins/io_espresso/espressoout_funcs.cpp
        Copyright T. Youngs 2016-2017

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

#include "plugins/io_espresso/espressoout.hui"
#include "model/model.h"

// Constructor
QEOutModelPlugin::QEOutModelPlugin()
{
}

// Destructor
QEOutModelPlugin::~QEOutModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* QEOutModelPlugin::makeCopy()
{
	return new QEOutModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory QEOutModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString QEOutModelPlugin::name() const
{
	return QString("QuantumEspresso output files");
}

// Nickname of plugin
QString QEOutModelPlugin::nickname() const
{
	return QString("espresso");
}

// Return whether the plugin is enabled
bool QEOutModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString QEOutModelPlugin::description() const
{
	return QString("Basic import for QuantumEspresso output");
}

// Related file extensions
QStringList QEOutModelPlugin::extensions() const
{
	return QStringList() << "out";
}

// Exact names
QStringList QEOutModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool QEOutModelPlugin::canImport()
{
	return false;
}

// Import data from the specified file
bool QEOutModelPlugin::importData()
{
//filter(type="importmodel", name="Quantum Espresso Output (PWSCF)", nickname="qepwscf", extension="out", glob="*.out", search="PWSCF")
//{
//	# Variable declaration
//	int nAtoms, nConfigs = 1, atomForces = FALSE;
//	string keywd, discard, line, alatKeywd, el;
//	UnitCell ucell = new UnitCell;
//	double alat, x, y, z;
//	removeReadOption("skipblanks");
//
//	# Locate lattice coordinate and number of atoms
//	if (find("lattice parameter", line))
//	{
//		addReadOption("stripbrackets");
//		readVar(line, discard, discard, alatKeywd, discard, alat);
//		removeReadOption("stripbrackets");
//		alat *= 0.52917720859;
//		printf("Lattice parameter (%s) is %f Angstroms\n", alatKeywd, alat);
//	}
//	else printf("Failed to find lattice parameter.\n");
//
//	# Find number of atoms per basic cell
//	if (find("number of atoms/cell", line))
//	{
//		readVar(line,discard,discard,discard,discard,nAtoms);
//		printf("Number of atoms in cell = %i\n", nAtoms); 
//	}
//	else error("Failed to find number of atoms in cell.");
//
//	# Read initial cell and coordinates
//	if (!readPWCell(ucell, alat, alatKeywd)) error("Failed to find any cell parameters");
//	Model m = newModel(filterFilename());
//	m.cell.copy(ucell);
//	if (!readPWAtoms(m, nAtoms, alat, alatKeywd)) error("No atomic coordinates found.");
//	rebond();
//	# Atomic forces?
//	atomForces = readPWForces(m);
//
//	# Any other configurations, e.g. from geometry optimisation or MD?
//	while (!eof())
//	{
//		# Attempt to find new cell information
//		if (readPWCellFrame(ucell, alat, alatKeywd))
//		{
//			++nConfigs;
//			m = addFrame(toa("Frame %i",nConfigs));
//			m.cell.copy(ucell);
//			if (!readPWAtomsFrame(m, nAtoms))
//			{
//				printf("Failed to find atom coordinates for frame %i\n", nConfigs);
//				finaliseFrame();
//				break;
//			}
//			# Atomic forces?
//			if (atomForces) readPWForces(m);
//		}
//		else if (find("ATOMIC_POSITIONS", line))
//		{
//			m = addFrame(toa("Frame %i",nConfigs));
//			m.cell.copy(ucell);
//
//			// Parse 'line' to find how the coordinates are specified
//			readVar(line, discard, keywd);
//			for (int n=0; n<nAtoms; ++n)
//			{
//				readLine(el, x, y, z);
//				if (keywd == "(crystal)") newAtomFrac(el, x, y, z);
//				else
//				{
//					printf("Unrecognised coordinate system in ATOMIC_POSITIONS : %s\n", keywd);
//					newAtom(el, x, y, z);
//				}
//			}
//		}
//		else break;
//
//		// Rebond and finalise frame
//		rebond();
//		finaliseFrame();
//	}
//	finaliseModel();
//}
}

// Return whether this plugin can export data
bool QEOutModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool QEOutModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool QEOutModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool QEOutModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool QEOutModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool QEOutModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool QEOutModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool QEOutModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

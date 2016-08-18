/*
        *** MOL2 Model Plugin Functions
        *** src/plugins/io_sybylmol2/mol2_funcs.cpp
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

#include "plugins/io_sybylmol2/mol2.hui"
#include "model/model.h"

// Constructor
Mol2ModelPlugin::Mol2ModelPlugin()
{
}

// Destructor
Mol2ModelPlugin::~Mol2ModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* Mol2ModelPlugin::makeCopy()
{
	return new Mol2ModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType Mol2ModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int Mol2ModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString Mol2ModelPlugin::name() const
{
	return QString("Tripos (Sybyl) MOL2 model");
}

// Nickname of plugin
QString Mol2ModelPlugin::nickname() const
{
	return QString("mol2");
}

// Description (long name) of plugin
QString Mol2ModelPlugin::description() const
{
	return QString("Basic import/export for Tripos (Sybyl) Mol2 files");
}

// Related file extensions
QStringList Mol2ModelPlugin::extensions() const
{
	return QStringList() << "mol2";
}

// Exact names
QStringList Mol2ModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool Mol2ModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool Mol2ModelPlugin::importData()
{
	// Read in lines, looking for the sections we're interested in.
	// All sections begin with '@<TRIPOS>', e.g. '@<TRIPOS>MOLECULE'

	int nAtoms = 0, nBonds = 0;
	while (!fileParser_.eofOrBlank())
	{
		if (!fileParser_.parseLine()) return false;

		if (fileParser_.argc(0) == "@<TRIPOS>MOLECULE")
		{
			// Molecule information
			QString title;
			if (!fileParser_.readLine(title)) return false;
			createModel(title);
			if (!fileParser_.parseLine()) return false;
			nAtoms = fileParser_.argi(0);
			nBonds = fileParser_.argi(1);

			// Skip the next four lines, which are type of molecule, charges used, and user comment
			fileParser_.skipLines(4);
		}
		else if (fileParser_.argc(0) == "@<TRIPOS>ATOM")
		{
			for (int n=0; n<nAtoms; ++n)
			{
				if (!fileParser_.parseLine()) return false;
				createAtom(targetModel(), fileParser_.argc(1), fileParser_.arg3d(2));
			}
		}
		else if (fileParser_.argc(0) == "@<TRIPOS>BOND")
		{
			for (int n=0; n<nBonds; ++n)
			{
				if (!fileParser_.parseLine()) return false;
				targetModel()->bondAtoms(fileParser_.argi(1)-1, fileParser_.argi(2)-1, (Bond::BondType) fileParser_.argi(3));
			}
		}
		else if (fileParser_.argc(0) == "@<TRIPOS>CRYSIN")
		{
			// Crystal cell information
			// Single line, format is ' a b c alpha beta gamma spgrp spgrp_setting'
			if (!fileParser_.parseLine()) return false;
			targetModel()->setCell(fileParser_.arg3d(0), fileParser_.arg3d(3));
			targetModel()->cell().setSpacegroup(fileParser_.argc(6), standardOptions_.forceRhombohedral());
		}
	}

	// Perform post-load operations
	if (!standardOptions_.preventPacking() && (targetModel()->cell().spacegroupId() != 0)) targetModel()->pack();
	if (!standardOptions_.preventFolding() && targetModel()->isPeriodic()) targetModel()->foldAllAtoms();

	return true;
}

// Return whether this plugin can export data
bool Mol2ModelPlugin::canExport()
{
	return true;
}

// Export data to the specified file
bool Mol2ModelPlugin::exportData()
{
	// Write title section
	if (!fileParser_.writeLine("@<TRIPOS>MOLECULE")) return false;
	if (!fileParser_.writeLine(targetModel()->name())) return false;
	if (!fileParser_.writeLineF("%i   %i\n", targetModel()->nAtoms(), targetModel()->nBonds())) return false;
	if (!fileParser_.writeLine("SMALL")) return false;
	if (!fileParser_.writeLine("NO_CHARGES")) return false;
	if (!fileParser_.writeLine("Coordinates churned out by Aten")) return false;
	if (!fileParser_.writeLine()) return false;

	// Write atoms section
	if (!fileParser_.writeLine("@<TRIPOS>ATOM")) return false;
	for (Atom* i = targetModel()->atoms(); i != NULL; i - i->next)
	{
		if (!fileParser_.writeLineF("%6i  %5s  %10.5f  %10.5f  %10.5f  %5s\n", i->id()+1, ElementMap().symbol(i), i->r().x, i->r().y, i->r().z, ElementMap().symbol(i))) return false;
	}

	// Write bonds section
	if (targetModel()->nBonds() > 0)
	{
		if (!fileParser_.writeLine("@<TRIPOS>BOND")) return false;
		int n = 0;
		for (Bond* b = targetModel()->bonds(); b != NULL; b = b->next)
		{
			if (!fileParser_.writeLineF("%5i  %5i  %5i  %s\n", ++n, b->atomI()->id()+1, b->atomJ()->id()+1, b->type())) return false;
		}
	}

	// Write cell section (if the model has one)
	if (targetModel()->isPeriodic())
	{
		if (!fileParser_.writeLine("@<TRIPOS>CRYSIN")) return false;
		Vec3<double> lengths = targetModel()->cell().lengths();
		Vec3<double> angles = targetModel()->cell().angles();
		if (!fileParser_.writeLineF("%10.4f   %10.4f   %10.4f    %10.4f   %10.4f   %10.4f    %i  %i", lengths.x, lengths.y, lengths.x, angles.x, angles.y, angles.z, targetModel()->cell().spacegroupId(), 0)) return false;
	}

	return true;
}

// Import next partial data chunk
bool Mol2ModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool Mol2ModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool Mol2ModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool Mol2ModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool Mol2ModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool Mol2ModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

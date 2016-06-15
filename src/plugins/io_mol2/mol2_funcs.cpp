/*
        *** MOL2 Model Plugin Functions
        *** src/plugins/io_mol2/mol2_funcs.cpp
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

#include "plugins/io_mol2/mol2.hui"
#include "model/model.h"

// Constructor
MOL2ModelPlugin::MOL2ModelPlugin()
{
}

// Destructor
MOL2ModelPlugin::~MOL2ModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* MOL2ModelPlugin::makeCopy()
{
	return new MOL2ModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory MOL2ModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString MOL2ModelPlugin::name() const
{
	return QString("Tripos (Sybyl) MOL2 model");
}

// Nickname of plugin
QString MOL2ModelPlugin::nickname() const
{
	return QString("mol2");
}

// Description (long name) of plugin
QString MOL2ModelPlugin::description() const
{
	return QString("Import/export for Tripos (Sybyl) Mol2 files");
}

// Related file extensions
QStringList MOL2ModelPlugin::extensions() const
{
	return QStringList() << "mol2";
}

// Exact names
QStringList MOL2ModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool MOL2ModelPlugin::canImport()
{
	return true;
}

// Import data from the spemol2ied file
bool MOL2ModelPlugin::importData()
{
//
//filter(type="importmodel", name="Tripos (Sybyl) Mol2", nickname="mol2", extension="mol2", glob="*.mol2", id=6)
//{
//	# Variable declaration
//	int n,natoms,nbonds,id,ii,jj,spgrp,setting;
//	string tripos,keywd,e,bondtype,title,discard;
//	double rx,ry,rz,ca,cb,cc,alpha,beta,gamma;
//
//	# Read in lines, looking for the sections we're interested in.
//	# All sections begin with '@<TRIPOS>', e.g. '@<TRIPOS>MOLECULE'
//	# Read a line and parse it into '@<TRIPOS>' and keyword parts
//	while (!eof())
//	{
//		readLineF("%9s%s",tripos,keywd);
//		if (keywd == "MOLECULE")
//		{
//			# Molecule information
//			getLine(title);
//			newModel(title);
//			readLine(natoms,nbonds);
//			# Skip the next four lines, which are type of molecule, charges used, and user comment
//			skipLine(4);
//		}
//		else if (keywd == "ATOM")
//		{
//			for (n=0; n<natoms; ++n)
//			{
//				readLine(id,e,rx,ry,rz);
//				newAtom(e,rx,ry,rz);
//			}
//		}
//		else if (keywd == "BOND")
//		{
//			for (n=0; n<nbonds; ++n)
//			{
//				readLine(discard,ii,jj,bondtype);
//				newBond(ii,jj,bondtype);
//			}
//		}
//		else if (keywd == "CRYSIN")
//		{
//			# Crystal cell information
//			# Single line, format is ' a b c alpha beta gamma spgrp spgrp_setting'
//			readLine(ca,cb,cc,alpha,beta,gamma,spgrp,setting);
//			cell(ca,cb,cc,alpha,beta,gamma);
//			spacegroup(spgrp);
//		}
//	}
//
//	# Perform post-load operations
//	pack();
//	fold();
//	finaliseModel();
//}
//
//filter(type="exportmodel", name="Tripos (Sybyl) Mol2", nickname="mol2", extension="mol2", glob="*.mol2", id=6)
//{
//	# Variables
//	atom i;
//	bond b;
//	int n;
//	model m = aten.frame;
//
//	# Write title section
//	writeLine("@<TRIPOS>MOLECULE");
//	writeLine(m.name);
//	writeLineF("%i   %i\n", m.nAtoms, m.nBonds);
//	writeLine("SMALL");
//	writeLine("NO_CHARGES");
//	writeLine("Coordinates churned out by Aten");
//	writeLine("");
//	
//	# Write atoms section
//	writeLine("@<TRIPOS>ATOM");
//	for (i = m.atoms; i != 0; ++i) writeLineF("%6i  %5s  %10.5f  %10.5f  %10.5f  %5s\n",i.id,i.symbol,i.rx,i.ry,i.rz, i.symbol);
//
//	# Write bonds section
//	if (m.nBonds > 0)
//	{
//		writeLine("@<TRIPOS>BOND");
//		n = 0;
//		for (b=m.bonds; b != 0; ++b) writeLineF("%5i  %5i  %5i  %s\n", ++n, b.i.id, b.j.id, b.type);
//	}
//
//	# Write cell section (if the model has one)
//	if (m.cell.type <> "none")
//	{
//		writeLine("@<TRIPOS>CRYSIN");
//		writeLine(m.cell.a,m.cell.b,m.cell.c,m.cell.alpha,m.cell.beta,m.cell.gamma,m.cell.sgId,0);
//	}
//}
//
	return true;
}

// Return whether this plugin can export data
bool MOL2ModelPlugin::canExport()
{
	return false;
}

// Export data to the spemol2ied file
bool MOL2ModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool MOL2ModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool MOL2ModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool MOL2ModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool MOL2ModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool MOL2ModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool MOL2ModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

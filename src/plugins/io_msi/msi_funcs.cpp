/*
        *** MSI Model Plugin Functions
        *** src/plugins/io_msi/msi_funcs.cpp
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

#include "plugins/io_msi/msi.hui"
#include "model/model.h"

// Constructor
MSIModelPlugin::MSIModelPlugin()
{
}

// Destructor
MSIModelPlugin::~MSIModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* MSIModelPlugin::makeCopy() const
{
	return new MSIModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType MSIModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int MSIModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString MSIModelPlugin::name() const
{
	return QString("MSI (dlputils) 3D probability density");
}

// Nickname of plugin
QString MSIModelPlugin::nickname() const
{
	return QString("msi");
}

// Return whether the plugin is enabled
bool MSIModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString MSIModelPlugin::description() const
{
	return QString("Import/export for dlputils MSI files");
}

// Related file extensions
QStringList MSIModelPlugin::extensions() const
{
	return QStringList() << "msi";
}

// Exact names
QStringList MSIModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool MSIModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool MSIModelPlugin::importData()
{
//filter(type="importmodel", name="MSI Datafile v3.8", nickname="msi", extension="msi", glob="*.msi", id=12)
//{
//	# Variable declarations
//	int n,cellvar,id,bondi,bondj,bondsymm;
//	string block,newblock,type,line,ident,data,a,b,c,el,name,sg;
//	vector r, f, axes[3];
//	double q;
//	Atom i, j;
//	Model m;
//
//	# Request that parentheses are removed as we read lines
//	addReadOption("stripbrackets");
//	removeReadOption("usequotes");
//
//	# Read in each line as six arguments.
//	# If the first is not 'A' then it is the start of some data block.
//	# We will use a variable 'block' to store the current block we're in (N=none, M=model, A=atom, B=bond)
//	block = "N";
//	cellvar = 0;
//	while (!eof())
//	{
//		# Read in whole lines and work with them instead of always using readLine
//		getLine(line);
//		# Parse the stored $line
//		readVar(line,ident,type,data,a,b,c);
//		# If the 'c' variable is '\' then we must read in the next line to get the real 'c'
//		if (c == "\\") { getLine(line); readVar(line,c); }
//		# Check for block specifiers
//		if (ident <> "A")
//		{
//			if (type == "Model") newblock = "M";
//			else if (type == "Atom") { newblock = "A"; id = atoi(ident); el = ""; }
//			else if (type == "Bond") { newblock = "B"; bondsymm = FALSE; }
//			else newblock = "N";
//			# Check the old 'block' value and perform end tasks
//			if (block == "A")
//			{
//				if (el == "") printf("Skipped atom (probably a symmetry copy)...\n");
//				else
//				{
//					i = newAtom(el, r.x, r.y, r.z);
//					i.bit = id;
//					setForces(f.x, f.y, f.z);
//					setCharge(q);
//				}
//			}
//			else if (block == "B")
//			{
//				if (bondsymm) printf("Skipped bond (probably a symmetry copy)...\n");
//				else
//				{
//					# Find atoms involved
//					i = m.atomWithBit(bondi);
//					j = m.atomWithBit(bondj);
//					if (i && j) newBond(i,j);
//					else printf("Couldn't find one or both atoms with temporary IDs %i and %i.\n", bondi, bondj);
//				}
//			}
//			else if (block == "M") m =newModel(name);
//			block = newblock;
//		}
//		else
//		{
//			# Not a block specifier, so must be data for a block we're already in...
//			if (block == "M")
//			{
//				# Model block
//				if (data == "Label") name = a;
//				else if (data == "A3") { axes[1].x = atof(a); axes[1].y = atof(b); axes[1].z = atof(c); ++cellvar; }
//				else if (data == "B3") { axes[2].x = atof(a); axes[2].y = atof(b); axes[2].z = atof(c); ++cellvar; }
//				else if (data == "C3") { axes[3].x = atof(a); axes[3].y = atof(b); axes[3].z = atof(c); ++cellvar; }
//				else if (data == "SpaceGroup") sg = a;
//			}
//			else if (block == "A")
//			{
//				# Atom block
//				if (data == "Charge") q = atof(a);
//				else if (data == "XYZ") { r.x = atof(a); r.y = atof(b); r.z = atof(c); }
//				else if (data == "Force") { f.x = atof(a); f.y = atof(b); f.z = atof(c); }
//				else if (data == "ACL") el = b;
//			}
//			else if (block == "B")
//			{
//				# Bond block
//				if (data == "Atom1") bondi = atoi(a);
//				else if (data == "Atom2") bondj = atoi(a);
//				else if ((data == "SymType") && (a == "Symmetry")) bondsymm = TRUE;
//			}
//		}
//	}
//
//	# Apply unit cell if one was specified
//	if (cellvar == 3)
//	{
//		cellAxes(axes[1].x, axes[1].y, axes[1].z, axes[2].x, axes[2].y, axes[2].z, axes[3].x, axes[3].y, axes[3].z);
//		fold();
//		if (sg != "")
//		{
//			spacegroup(sg);
//			pack();
//		}
//	}
//	else if (cellvar != 0) printf("Warning: Incomplete / mangled cell definition in file.");
//
//	# Finalise model
//	finaliseModel();
//}
//
	return true;
}

// Return whether this plugin can export data
bool MSIModelPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool MSIModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool MSIModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool MSIModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool MSIModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool MSIModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool MSIModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool MSIModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}
 

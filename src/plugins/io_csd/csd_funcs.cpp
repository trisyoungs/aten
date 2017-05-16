/*
        *** CSD Model Plugin Functions
        *** src/plugins/io_csd/csd_funcs.cpp
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

#include "plugins/io_csd/csd.hui"
#include "model/model.h"

// Constructor
CSDModelPlugin::CSDModelPlugin()
{
}

// Destructor
CSDModelPlugin::~CSDModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* CSDModelPlugin::makeCopy() const
{
	return new CSDModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType CSDModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int CSDModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString CSDModelPlugin::name() const
{
	return QString("CSD (dlputils) 3D probability density");
}

// Nickname of plugin
QString CSDModelPlugin::nickname() const
{
	return QString("csd");
}

// Return whether the plugin is enabled
bool CSDModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString CSDModelPlugin::description() const
{
	return QString("Import/export for dlputils CSD files");
}

// Related file extensions
QStringList CSDModelPlugin::extensions() const
{
	return QStringList() << "csd";
}

// Exact names
QStringList CSDModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool CSDModelPlugin::canImport() const
{
	return true;
}

// Import data from the specsdied file
bool CSDModelPlugin::importData()
{
//	# Variables
//	string spgrp,discard,ea,eb,ec,e,title;
//	int nf,nr,nd,ne,nsymm,natm,nsatm,nrad,ncon,hascell,atfor,pa,pb,pc,pal,pbe,pga;
//	int n,spgrpn,nlines,nextra,mod,natoms,ii,jj,total;
//	double xa,ya,za,xb,yb,zb,xc,yc,zc,rx,ry,rz;
//	double a,b,c,al,be,ga,cella,cellb,cellc,alpha,beta,gamma;
//
//	# Set the zmapping style to only consider the first alphabetic characters in atom symbols (i.e. trimming numbers)
//	aten.prefs.zMap = "firstalpha";
//	removeReadOption("skipblanks");
//	
//	# Begin loop over entries in file
//	while (!eof())
//	{
//		# Record 1: Directory information (single line)
//		# Contained information (first col = 0):
//		# 0 (#), 1-8 (refcode), 9 (crys.sys), 10 (category), 11-16 (accession date), 17-22 (unused), 23-25 (ncards),
//		# 26-28 (nrfrac), 29-31 (nrem), 32-34 (ndis), 35-37 (nerr), 38-40 (number of symm pos), 41-43 (nrad),
//		# 44-46 (natoms), 47-49 (nsatoms), 50-52 (unused), 53-55 (ncon), 56 (cell), 57 (intf), 58 (atom 'r' format),
//		# 59 (centre of symmetry), 60 (err), 61 (rpa), 62 (td), 63 (pd), 64 (unused), 65 (cbl), 66 (esd of C-C),
//		# 67 (polymeric structure), 68-77 (irrelevant), 78-79 (year)
//		readLineF(" %8s                 %3i%3i%3i%3i%3i%3i%3i%3i   %3i%1i%1i",title,nf,nr,nd,ne,nsymm,nrad,natm,nsatm,ncon,hascell,atfor);
//		model m = newModel(title);
//		printf("Found entry '%s'\n", title);
//		printf("Number of symmetry-unique atoms = %i, symmetry-related atoms = %i\n",natm,nsatm);
//		# Record 2: Unit cell definition (if %cell == 1)
//		# Contained information (first col = 0):
//		# 0-35 (A,B,C,a,b,c), 36-41 (precision digits), 42-53 (cesd), 54-59 (dens), 60-62 (spgrpn), 63-70 (spgrp),
//		# 71-75 (irrelevant)
//		if (hascell == 1)
//		{
//			readLineF("%6f%6f%6f%6f%6f%6f%1i%1i%1i%1i%1i%1i%18*%3i%8s",a,b,c,al,be,ga,pa,pb,pc,pal,pbe,pga,spgrpn,spgrp);
//			# Cell lengths and angles are divided by (10^n)...
//			cell(a/10.0^pa, b/10.0^pb, c/10.0^pc, al/10.0^pal, be/10.0^pbe, ga/10.0^pga);
//			spacegroup(spgrpn);
//		}
//		# Record 3: Text information (ignored)
//		# Total number of chars is the sum of nf, nr, nd, and ne read in earlier.
//		# Since the format rigidly defined 80 chars per line, skip by number of lines...
//		nlines = (nf+nr+nd+ne+79)/80;
//		skipLine(nlines);
//		if (nlines > 0) printf("Skipping %i line(s) of text information...\n", nlines);
//		# Record 4: Equivalent positions of spacegroup (ignored) - there would be 5 per line...
//		nlines = (nsymm+4)/5;
//		skipLine(nlines);
//		if (nlines > 0) printf("Skipping %i line(s) of spacegroup generator definitions...\n", nlines);
//		# Record 5: Atomic radii (ignored) - normally, there are up to 16 per line
//		nlines = (nrad+15)/16;
//		skipLine(nlines);
//		if (nlines > 0) printf("Skipping %i line(s) of atomic radii...\n", nlines);
//		# Record 6: Atomic coordinates
//		# Coordinates are given 3 per line, so determine number of lines to read in.
//		natoms = natm+nsatm;
//		printf("Expecting %i atoms in file...\n", natoms);
//		for (n=0; n<natoms; ++n)
//		{
//			# Read in new line if mod(n,3) is zero
//			mod = n%3;
//			if (mod == 0) readLineF("%5s%7f%7f%7f %5s%7f%7f%7f %5s%7f%7f%7f",ea,xa,ya,za,eb,xb,yb,zb,ec,xc,yc,zc);
//			verbose("Atom pass %i : Elements are %s, %s, and %s...", n, ea, eb, ec);
//			# Use variable data based on the $mod value...
//			if (mod == 0) newAtom(ea, xa/100000, ya/100000, za/100000);
//			else if (mod == 1) newAtom(eb, xb/100000, yb/100000, zb/100000);
//			else newAtom(ec, xc/100000, yc/100000, zc/100000);
//		}
//		# Record 7: Connectivity
//		# Connectivities are given in the format 40I2 (if natm+nsatm < 100) or 26I3,2x (if natm+nsatm >= 100)
//		# First (natm+nsatm) integers are paired to the atoms 1-(natm+nsatm) in that order. Any other integers
//		# are pairs of atoms involved in additional bonds.
//		nextra = (ncon-natoms)/2;
//		printf("Expecting %i connectivity integers (which includes %i extra bonding pairs)...\n", ncon, nextra);
//		for (n=1; n<=ncon; ++n)
//		{
//			jj = 0;
//			if (natoms < 100)
//			{
//				if (n <= natoms) { ii = atoi(readChars(2)); jj = n; }
//				else
//				{
//					if ((n-natoms)%2 == 1) ii = atoi(readChars(2));
//					else jj = atoi(readChars(2));
//				}
//			}
//			else
//			{
//				if (n <= natoms) { ii = atoi(readChars(3)); jj = n; }
//				else
//				{
//					if ((n-natoms)%2 == 1) ii = atoi(readChars(3));
//					else jj = atoi(readChars(3));
//				}
//				if (n%26 == 0) getLine(discard);
//			}
//			# Check for dummy bond (if ii = 0 don't create a bond)
//			if ((ii > 0) && (jj > 0))
//			{
//				verbose("Bonding atoms %i and %i (model atom indices)",ii,jj);
//				newBond(ii,jj);
//			}
//		}
//		# Read an extra 'line' to get to the proper end of the connectivity data if necessary
//		if (peekChar() != "\#") getLine(discard);
//		# Convert the stored fractional coordinates into real coordinates
//		fracToReal();
//		# Finalise the packing in the model
//		pack();
//		#fold();
//		# Perform post-load operations
//		finaliseModel();
//	}
	return true;
}

// Return whether this plugin can export data
bool CSDModelPlugin::canExport() const
{
	return false;
}

// Export data to the specsdied file
bool CSDModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool CSDModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool CSDModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool CSDModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool CSDModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool CSDModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool CSDModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}
 

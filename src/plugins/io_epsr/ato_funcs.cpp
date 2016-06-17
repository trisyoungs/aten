/*
        *** EPSRAto Model Plugin Functions
        *** src/plugins/io_epsr/ato_funcs.cpp
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

#include "plugins/io_epsr/ato.hui"
#include "model/model.h"

// Constructor
EPSRAtoModelPlugin::EPSRAtoModelPlugin()
{
}

// Destructor
EPSRAtoModelPlugin::~EPSRAtoModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* EPSRAtoModelPlugin::makeCopy()
{
	return new EPSRAtoModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory EPSRAtoModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString EPSRAtoModelPlugin::name() const
{
	return QString("EPSR Ato file");
}

// Nickname of plugin
QString EPSRAtoModelPlugin::nickname() const
{
	return QString("epsrato");
}

// Description (long name) of plugin
QString EPSRAtoModelPlugin::description() const
{
	return QString("Import/export for Empirical Potential Structure Refinement (EPSR) Ato models");
}

// Related file extensions
QStringList EPSRAtoModelPlugin::extensions() const
{
	return QStringList() << "ato";
}

// Exact names
QStringList EPSRAtoModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool EPSRAtoModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool EPSRAtoModelPlugin::importData()
{
//filter(type="importmodel",name="EPSR ATO File", nickname="ato", extension="ato", glob="*.ato", id=18)
//{
//	// Variables
//	Atom i;
//	int nmols,m,n,l,natoms,nres,nrot,count,done,id,nrotatoms,ncons;
//	Element el;
//	double boxsize, temp, eps, sigma, mass, q;
//	String atomname, elname, line, dummy;
//	Vector com, r, phi;
//	FFAtom ffa;
//
//	addReadOption("skipblanks");
//
//	// File header:
//	// Either  1   : nmols, box length, temperature   (for cubic systems)
//	//    or   2   : nmols,   temperature             (for non-cubic systems)
//	// followed by : A, B, C
//	//             : phib, thetac, phic
//	if (!readLine(nmols, boxsize, dummy)) error("Failed to read header information from file.\n");
//	newModel("ATO file");
//	if (dummy == "")
//	{
//		printf("File contains specific unit cell information.\n");
//		double a, b, c, phib, thetac, phic;
//		if (!readLine(a, b, c)) error("Failed to read unit cell lengths from file.\n");
//		if (!readLine(phib, thetac, phic)) error("Failed to read unit cell lengths from file.\n");
//		// phib = angle between a and b (== gamma)
//		// thetac = angle deviation from cartesian z (== 90-beta)  ?? CHECK
//		// phic = angle deviation from cartesian z (== 90-alpha)  ?? CHECK
//		cell(a, b, c, 90-phic, 90-thetac, phib);
//	}
//	else cell(boxsize,boxsize,boxsize,90,90,90);
//
//	// 2 : step sizes etc. (ignore)
//	skipLine();
//
//	// Molecule/atom specifications are in the form:
//	// n  : natoms, comx, comy, comz, phix, phiy, phiz
//	// n+1: atom name 1
//	// n+2: x,y,z (offsets from com)
//	// n+3: nrestraints, res1, res2... (number of distance restraints, 5 per line)
//	// n+4: ...resN-1, resN
//	// n+5: nrot (number of defined molecular rotations)
//	// n+6: atom1, atom2 (bonds of rotation 'axis')
//	// n+7: list of headgroup atoms that are rotated
//	count = 0;
//	for (m=0; m<nmols; m++)
//	{
//		if (!readLine(natoms, com.x, com.y, com.z, phi.x, phi.y, phi.z)) error("Error reading information for molecule %i.\n", m+1);
//
//		for (n=0; n<natoms; n++)
//		{
//			// Basic atom information
//			if (!readLine(atomname)) error("Error reading atom name %i for molecule %i\n", n+1, m+1);
//			if (!readLine(r.x, r.y, r.z)) error("Error reading atom coordinates %i for molecule %i\n", n+1, m+1);
//			// Adjust for molecule centre of mass, and also apply a half-cell translation since EPSR uses -0.5 -> 0.5 coordinates
//			r += com;
//			//r += aten.model.cell.centre;
//			// Create a new atom with element 0 - it will be set to a proper element later on
//			i = newAtom(0, r.x, r.y, r.z);
//			i.data = atomname;
//
//			// Apply restraint information as bonds
//			if (!nextArg(nres)) error("Error reading nrestraints for atom %i of molecule %i.\n", n+1, m+1);
//			for (l=0; l<nres; ++l)
//			{
//				if (!nextArg(id) || !nextArg(temp)) error("Error reading restraint information %i for atom %i of molecule %i.\n", l+1, n+1, m+1);
//
//				// If the relative atom id is greater than the current atom number, ignore it (it will be repeated the other way around for the second atom)
//				if (id > (n+1)) continue;
//
//				// Check for bonding the atom to itself (never a good idea)
//				if ((n+1) == id)
//				{
//					printf("Warning: Ato file specifies a distance restraint to the same atom.\n");
//				}
//				else newBond(count+n+1, count+id);
//			}
//		}
//
//		// Discard molecular rotations and dihedrals
//		// There are 14 atoms per line - first line contains number of atoms followed by (up to) 13 indices
//		if (!readLine(nrot)) error("Error reading nrotations for atom %i of molecule %i.\n", n+1, m+1);
//		for (n=0; n<nrot; ++n)
//		{
//			// Read line to find out which type of definition this is...
//			if (!readLine(dummy)) error("Error reading type of rotation group %i.\n", n+1);
//			// Skip axis line
//			skipLine(1);
//			// If a DIHedral, we expect an integer which defines the number of constraints? Skip this number of lines
//			if (dummy == "DIH")
//			{
//				readLine(ncons);
//				skipLine(ncons);
//			}
//			// Finally, read in number of atoms affected by rotation and calcualte next number of lines to discard
//			readLine(nrotatoms);
//			skipLine(nrotatoms/14);
//		}
//
//		count += natoms;
//	}
//
//	// Atomtype specifications follow
//	// Read in until we find don't find an element symbol (i.e. a number).  
//	done = FALSE;
//	Forcefield ff = newFF("Atomtypes from ATO file");
//	count = 0;
//	do
//	{
//		++count;
//		// Two lines per atomtype
//		//   name, symbol, 0/isomass
//		//   epsilon  sigma  mass  charge
//		if (!readLine(atomname, elname, dummy)) error("Error reading atomtype information %i.\n", count);
//		// If 'dummy' is empty, chances are we've gone on to the fmole numbers (which has no third data)
//		if (dummy == "") break;
//		if (!readLine(eps, sigma, mass, q)) error("Error reading atomtype LJ information %i.\n", count);
//		el = aten.findElement(elname);
//		if (el == 0) 
//		{
//			printf("Failed to map element name '%s' to an element.  Attempting to use atomname (%s) instead...\n", elname, atomname);
//			el = aten.findElement(atomname);
//		}
//		if (el != 0) printf("       : Mapping atom name %s to %s.\n", atomname, el.symbol);
//		else printf("       : Mapping atom name %s to XXX (unknown).\n", atomname);
//		ffa = ff.addType(count, atomname, atomname, el, "", "");
//		for (i in aten.model.atoms) if (i.data == atomname) { i.element = el; i.type = ffa; }
//		ff.addInter("lj", count, q, eps, sigma);
//	} while (!done);
//	ff.finalise();
//
//	finaliseModel();
//}
//
//filter(type="exportmodel",name="EPSR ATO File", nickname="ato", extension="ato", glob="*.ato", id=18)
//{
//	# Variable declaration
//	Model srcmodel = aten.frame;
//	Pattern p;
//	int nmols, n, m, o, mol, nresb, nresa, nrest, nrot, idj, count;
//	double rij, mass, boxsize;
//	Vector com, r;
//	Atom i, j, k, l;
//	Bound b;
//	Bond b1, b2, b3;
//	String rotText, resText, s;
//
//	# Main dialog creation function
//	void createDefaultDialog(Dialog ui)
//	{
//		Widget w, group;
//		ui.title = "ATO File Export Options";
//		ui.verticalFill = TRUE;
//
//		ui.addDoubleSpin("temperature", "Temperature", 0.0, 100000.0, 10.0, 300.0);
//		ui.addDoubleSpin("epsr_tol", "Tolerance", 0.0, 1.0, 0.1, 0.0);
//
//		# Step Sizes
//		group = ui.addGroup("stepsizes", "Step Sizes", -1, -1, 1);
//		group.verticalFill = TRUE;
//		group.addDoubleSpin("epsr_itrans", "Intramolecular Translations", 0.0, 100.0, 1.0, 1.5);
//		group.addDoubleSpin("epsr_hrot", "Headgroup Rotations", 0.0, 10.0, 0.1, 2.5);
//		group.addDoubleSpin("epsr_mrot", "Whole Molecule Rotations", 0.0, 10.0, 0.1, 1.0);
//		group.addDoubleSpin("epsr_mtrans", "Whole Molecule Translations", 0.0, 10.0, 0.01, 0.1);
//		ui.addDoubleSpin("epsr_vtemp", "Vibrational Temperature", 0.0, 1500.0, 10.0, 65.0);
//
//		# Output Options
//		group = ui.addGroup("outputoptions", "Output Options", -1, -1, 1);
//		group.verticalFill = TRUE;
//		group.addCheck("outputrotations", "Write Rotational Groups", 1);
//
//		# Restraints
//		group = ui.addGroup("restraints", "Restraints", -1, -1, 1);
//		ui.addRadioGroup("restraintgroup");
//		group.addRadioButton("restrain_con", "Restrain Based on Connectivity", "restraintgroup", 1);
//		group.addRadioButton("restrain_dist", "Restrain Based on Distance", "restraintgroup", 0);
//		# -- by Connectivity
//		w = group.addFrame("restrain_con_frame", 1, 2);
//		ui.addRadioGroup("connectivitygroup");
//		w.verticalFill = TRUE;
//		w.addRadioButton("restrain_bonds", "Across Bonds", "connectivitygroup", 1);
//		w.addRadioButton("restrain_angles", "Across Bonds and Angles","connectivitygroup",  0);
//		w.addRadioButton("restrain_torsions", "Across Bonds, Angles and Torsions", "connectivitygroup", 0);
//		w.addDoubleSpin("restrain_con_max", "Maximum Distance", 0.0, 10.0, 0.1, 2.4);
//
//		w = group.addFrame("restrain_dist_frame", 2, 2);
//		w.enabled = FALSE;
//		w.addDoubleSpin("restrain_dist_max", "Maximum Distance", 0.0, 10.0, 0.1, 2.4);
//
//		ui.widget("restrain_dist").onInteger(0, 1, "sendbool", "restrain_dist_frame", "enabled");
//		ui.widget("restrain_con").onInteger(0, 1, "sendbool", "restrain_con_frame", "enabled");
//		ui.widget("restrain_dist").onInteger(0, 1, "sendbool", "restrain_con_frame", "disabled");
//		ui.widget("restrain_con").onInteger(0, 1, "sendbool", "restrain_dist_frame", "disabled");
//	}
//	if (!showDefaultDialog()) error("Canceled through dialog.\n");
//	Dialog ui = defaultDialog();
//
//	// Grab some dialog values
//	int resType = ui.asInteger("restrain_bonds");
//	if (ui.asInteger("restrain_angles")) resType = 2;
//	else if (ui.asInteger("restrain_torsions")) resType = 3;
//	double rijmax = ui.asDouble("restrain_con_max");
//
//	// First, some checks. We need valid patterns and a cubic cell.
//	if (!createExpression(FALSE,TRUE)) error("Error: Can't write ATO file without valid forcefield types assigned to all atoms.\n");
//
//	// Line 1 : nmols, box dimension, temperature
//	// First determine total number of molecules
//	nmols = 0;      
//	for (p in srcmodel.patterns) nmols += p.nMols;
//	if (srcmodel.cell.type == "none")
//	{
//		// Set boxsize here to be zero (it is used elsewhere to shift by half-cell amounts, so this will prevent it)
//		boxsize = 0.0;
//		// Determine maximum extent of system
//		printf("Model has no cell definition - determining molecular extent...\n");
//		double maxrij = 0.0;
//		for (i = srcmodel.atoms; i; ++i)
//		{
//			for (j = ++i; j; ++j)
//			{
//				rij = geometry(i,j);
//				if (rij > maxrij) maxrij = rij;
//			}
//		}
//		writeLineF("  %4i %13.6e  %13.6e\n", nmols, maxrij*2.0, ui.asDouble("temperature"));
//	}
//	else if (srcmodel.cell.type == "cubic")
//	{
//		boxsize = srcmodel.cell.a;
//		writeLineF("  %4i %13.6e  %13.6e\n",nmols,srcmodel.cell.a, ui.asDouble("temperature"));
//	}
//	else
//	{
//		writeLineF("  %4i %13.6e\n", nmols, ui.asDouble("temperature"));
//		writeLineF(" %12.6e  %12.6e  %12.6e\n", srcmodel.cell.a, srcmodel.cell.b, srcmodel.cell.c);
//		// Angles must be determined:
//		// thetac = angle deviation from cartesian z (== 90-beta)  ?? CHECK
//		// phic = angle deviation from cartesian z (== 90-alpha)  ?? CHECK
//		writeLineF(" %12.6e  %12.6e  %12.6e\n", srcmodel.cell.gamma, 90.0-srcmodel.cell.beta, 90.0-srcmodel.cell.alpha);
//	}
//
//	// Line 2 : Tol, step sizes (intra trans, headgroup rot, mol rot, mol trans), vibrational temp
//	writeLineF("  %10.3e  %10.3e  %10.3e  %10.3e  %10.3e  %10.3e\n", ui.asDouble("epsr_tol"), ui.asDouble("epsr_itrans"), ui.asDouble("epsr_hrot"), ui.asDouble("epsr_mrot"), ui.asDouble("epsr_mtrans"), ui.asDouble("epsr_vtemp"));
//
//	// Atom/molecule loop
//	mol = 0;
//	i = srcmodel.atoms;
//	for (p in srcmodel.patterns)
//	{
//		for (m=1; m<=p.nMols; ++m)
//		{
//			++mol;
//			// Write centre of mass
//			com = p.com(m) - boxsize*0.5;
//			writeLineF("   %-2i %13.6e %13.6e %13.6e %13.6e %13.6e %13.6e F %5i\n",p.nMolAtoms,com.x,com.y,com.z,0.0,0.0,0.0,mol);
//
//			// Write atom information
//			for (n=1; n<=p.nMolAtoms; ++n)
//			{
//				if (i.type == 0) writeLineF(" %-3s     %5i\n", i.symbol, n);
//				else writeLineF(" %-3s     %5i\n", i.type.name, n);
//
//				// Get mim vector to COM
//				r = srcmodel.cell.mimVector(com, i.r - boxsize*0.5);
//				writeLineF(" %12.5e %12.5e %12.5e\n", r.x, r.y, r.z);
//
//				// Restraint information
//				count = 0;
//				resText = "";
//				if (ui.asInteger("restrain_con"))
//				{
//					// Loop over bonds to this atom
//					for (b1 in i.bonds)
//					{
//						j = b1.partner(i);
//
//						// Always restrain bond distance (if within max distance allowed)....
//						rij = geometry(b1.i.id, b1.j.id);
//						if (rij < rijmax)
//						{
//							++count;
//							sprintf(s, "%4i %9.3e ", j.id - p.firstAtomId + 1, rij);
//							resText += s;
//							if (count%5 == 0) resText += "\n";
//						}
//
//						// Restrain across angles?
//						if (resType > 1)
//						{
//							// Loop over bonds on j
//							for (b2 in j.bonds)
//							{
//								if (b1 == b2) continue;
//								k = b2.partner(j);
//								rij = geometry(k.id, i.id);
//								if (rij < rijmax)
//								{
//									++count;
//									sprintf(s, "%4i %9.3e ", k.id - p.firstAtomId + 1, rij);
//									resText += s;
//									if (count%5 == 0) resText += "\n";
//								}
//
//								// Restrain across torsions?
//								if (resType > 2)
//								{
//									// Loop over bonds on k
//									for (b3 in k.bonds)
//									{
//										if (b2 == b3) continue;
//										l = b3.partner(k);
//										rij = geometry(l.id, i.id);
//										if (rij < rijmax)
//										{
//											++count;
//											sprintf(s, "%4i %9.3e ", l.id - p.firstAtomId + 1, rij);
//											resText += s;
//											if (count%5 == 0) resText += "\n";
//										}
//		
//									}
//								}
//							}
//						}
//
//						
//					}
//					
//					writeLineF(" %2i %s\n", count, resText);
//				}
//				else
//				{
//					nresb = 0;
//					for (idj = 1; idj<=p.nMolAtoms; ++idj) if ((n != idj) && (geometry(p.atoms[n],p.atoms[idj]) < rijmax)) ++nresb;
//					writeLineF(" %4i", nresb);
//					if (nresb > 0) for (idj = 1; idj<=p.nMolAtoms; ++idj)
//					{
//						if (n == idj) continue;
//						rij = geometry(p.atoms[n],p.atoms[idj]);
//						if (rij < rijmax) writeLineF("%4i %9.3e ", idj, rij);
//					}
//					writeLineF("\n");
//				}
//
//				++i;
//			}
//
//			// Write headgroup rotations
//			// We will construct a list of rotations and count them up as we go. A string will be created containing all the info to be written out.
//			nrot = 0;
//			rotText = "";
//			if (ui.asInteger("outputrotations")) for (b in p.bonds)
//			{
//				if (p.atomsInRing(b.id[1],b.id[2])) continue;
//				if (p.atoms[b.id[1]].nBonds == 1) continue;
//				if (p.atoms[b.id[2]].nBonds == 1) continue;
//				b1 = p.atoms[b.id[1]].findBond(p.atoms[b.id[2]]);
//				srcmodel.selectNone();
//				srcmodel.selectTree(p.atoms[b.id[1]], b1);
//				// Check here the number of selected atoms - if greater than half the atoms in the molecule then we're better off with the inverse selection!
//				if (srcmodel.nSelected > 0.5*p.nMolAtoms)
//				{
//					for (o=1; o<=p.nMolAtoms; ++o)
//					{
//						if (p.atoms[o].selected) p.atoms[o].selected = FALSE;
//						else p.atoms[o].selected = TRUE;
//					}
//				}
//
//				// If the two atoms which form the rotation axis are both fixed, ignore this bond
//				if (p.atoms[b.id[1]].fixed && p.atoms[b.id[2]].fixed) continue;
//
//				// Remove the two atoms which form the rotation axis from the selection
//				srcmodel.deselect(p.atoms[b.id[1]], p.atoms[b.id[2]]);
//
//				// Loop over selected atoms now, deselecting all those which have their positions fixed
//				for (o=1; o<=p.nMolAtoms; ++o)
//				{
//					if (p.atoms[o].selected && p.atoms[o].fixed) p.atoms[o].selected = FALSE;
//				}
//				
//				// Still anything selected?
//				if (srcmodel.nSelected == 0) continue;
//				
//				// There are some suitable atoms to rotate, so create the necessary text
//				++nrot;
//				rotText += " ROT\n";
//				sprintf(s, "%5i%5i\n", b.id[1], b.id[2]);
//				rotText += s;
//				// List of rotating atoms should be given 14 per line (13 + natoms on the first line)
//				count = 1;
//				sprintf(s, " %4i", srcmodel.nSelected);
//				rotText += s;
//				for (j in srcmodel.selection)
//				{
//					if (count%14 == 0) writeLineF("\n");
//					sprintf(s, " %4i", j.id-p.firstAtomId+1);
//					rotText += s;
//					++count;
//				}
//				rotText += "\n";
//			}
//
//			if (nrot > 0)
//			{
//				writeLineF(" %5i\n", nrot);
//				writeLineF("%s", rotText);
//			}
//			else writeLineF("   %-2i\n", 0);
//		}
//	}
//
//	// Write the forcefield info
//        // Energy unit must be kj/mol, so set automatic conversion of ff energy parameters to kj
//        autoConversionUnit("kj");
//	for (ffatom ffa in srcmodel.ffTypes)
//	{
//		if ((ffa.form != "lj") && (ffa.form != "ljgeom")) error("Error: Atom type '%s' contains short-range parameters of an incompatible type with EPSR (%s).\n", ffa.name, ffa.form);
//		if (ffa.z == 1) writeLineF(" %-3s %-3s %1i\n", ffa.name, aten.elements[ffa.z].symbol, 1);
//		else writeLineF(" %-3s %-3s %1i\n", ffa.name, aten.elements[ffa.z].symbol, 0);
//
//		// Masses - for H atoms, write deuterium mass instead
//		if (ffa.z == 1) mass = 2.0;
//		else mass = aten.elements[ffa.z].mass;
//		writeLineF(" %10.4e  %10.4e  %10.4e  %10.4e  %10.4e\n", ffa.parameter("epsilon"), ffa.parameter("sigma"), mass, ffa.charge, 0.0);
//	}
//	
//	// Extra data
//
//	// Used by fmole to keep non-bonded atoms apart
//	writeLineF(" %10.4e  %10.4e\n", 1.0, 1.0);
//
//	// Random numbers for restart purposes
//	for (n=0; n<15; ++n) writeLineF(" %i", randomI());
//	writeLineF("\n");
//	
//}
//
	return true;
}

// Return whether this plugin can export data
bool EPSRAtoModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool EPSRAtoModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool EPSRAtoModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool EPSRAtoModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool EPSRAtoModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool EPSRAtoModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool EPSRAtoModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool EPSRAtoModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

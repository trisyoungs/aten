/*
        *** GROMACS Model Plugin Functions
        *** src/plugins/io_gromacs/gromacs_funcs.cpp
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

#include "plugins/io_gromacs/gromacs.hui"
#include "model/model.h"

// Constructor
GROMACSModelPlugin::GROMACSModelPlugin()
{
}

// Destructor
GROMACSModelPlugin::~GROMACSModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* GROMACSModelPlugin::makeCopy() const
{
	return new GROMACSModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType GROMACSModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int GROMACSModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString GROMACSModelPlugin::name() const
{
	return QString("GROMACS (dlputils) 3D probability density");
}

// Nickname of plugin
QString GROMACSModelPlugin::nickname() const
{
	return QString("gromacs");
}

// Return whether the plugin is enabled
bool GROMACSModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString GROMACSModelPlugin::description() const
{
	return QString("Import/export for dlputils GROMACS files");
}

// Related file extensions
QStringList GROMACSModelPlugin::extensions() const
{
	return QStringList() << "gromacs";
}

// Exact names
QStringList GROMACSModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool GROMACSModelPlugin::canImport() const
{
	return false;
}

// Import data from the specified file
bool GROMACSModelPlugin::importData()
{
//filter(type="importmodel",name="GROMACS coordinates", nickname="gro", extension="gro", glob="*.gro", id=14)
//{
//
//	# Variable declaration
//	int natoms,n,m,resno;
//	string e,title,res, stripped;
//	double rx,ry,rz,a,b,c;
//	atom i;
//
//	# Read data
//	getLine(title);
//	newModel(title);
//	readLine(natoms);
//	for (n=1; n<=natoms; ++n)
//	{
//		readLineF("%5i%-5s%-5s%5i%8.3f%8.3f%8.3f",resno,res,e,m,rx,ry,rz);
//		# Re-read element name to get rid of spaces
//		readVar(e,stripped);
//		i = newAtom(stripped, rx*10.0, ry*10.0, rz*10.0);
//	}
//	readLine(a,b,c);
//	if ((a < 0.0001) || (b < 0.0001) || (c < 0.0001)) printf("No valid cell found.\n");
//	else cell(a*10.0,b*10.0,c*10.0,90,90,90);
//	rebond(FALSE);
//	finaliseModel();
//}
//
//filter(type="exportmodel",name="GROMACS coordinates", nickname="gro", extension="gro", glob="*.gro", id=14)
//{
//
//	# Variable declaration
//	int n, mol, molcount, count;
//	atom i;
//	model m = aten.frame;
//
//	# Write data
//	writeLine(m.name);
//	writeLine(m.nAtoms);
//
//	count = 1;
//	molcount = 1;
//	for (pattern p = m.patterns; p; ++p)
//	{
//		i = p.firstAtom;
//		for (mol=0; mol<p.nMols; ++mol)
//		{
//			for (n=0; n<p.nMolAtoms; ++n)
//			{
//				if (i.type == 0) writeLineF("%5i%-5s%5s%5i%8.3f%8.3f%8.3f\n",molcount,substr(p.name,1,5),i.symbol,count,i.rx/10.0,i.ry/10.0,i.rz/10.0);
//				else writeLineF("%5i%-5s%5s%5i%8.3f%8.3f%8.3f\n",molcount,substr(p.name,1,5),i.type.name,count,i.rx/10.0,i.ry/10.0,i.rz/10.0);
//				++i;
//				++count;
//			}
//			++molcount;
//		}
//	}
//	if (m.cell.type == "none") writeLineF("%9.5f%9.5f%9.5f\n",0.0,0.0,0.0);
//	else writeLineF("%9.5f%9.5f%9.5f\n",m.cell.a/10.0,m.cell.b/10.0,m.cell.c/10.0);
//}
//
//filter(type="exportexpression",name="Gromacs .top File", extension="top", glob="*.top",nickname="top", id=14)
//{
//	# Variable declaration
//        Pattern p;
//	Atom i, j;
//	Bound b, b2;
//	FFAtom fi, fj;
//	double escale = 1.0, vscale = 1.0;
//	int uselj = TRUE, n, nconstraints, nfailed[3] = 0, nub, hasub, chgrp;
//
//	# Main dialog creation function
//	void createDefaultDialog(Dialog ui)
//	{
//		ui.title = "Gromacs Topology Export Options";
//		widget group, w;
//		ui.verticalFill = TRUE;
//		group = ui.addGroup("group", "Charge Group Definitions");
//		ui.addRadioGroup("chargegroup");
//		w = group.addRadioButton("radio1", "One charge group per molecule", "chargegroup", 0);
//		w = group.addRadioButton("radio2", "One charge group per atom", "chargegroup", 0);
//		w = group.addRadioButton("radio3", "One charge group per chemical unit", "chargegroup", 1);
//	}
//	# Execute dialog
//	if (!showDefaultDialog()) error("Options dialog canceled.\n");
//	Dialog ui = defaultDialog();
//
//	int chgrptype = ui.asInteger("chargegroup");
//
//	# Get current model
//	Model m = aten.frame;
//
//	# Note: All distances divided by 10 to convert to nm. Some force constant terms multiplied by 100 to convert to nm**2 (from A**2)
//	# Energy unit must be kj/mol, so set automatic conversion of ff energy parameters to kj
//	autoConversionUnit("kj");
//
//	# Assign charge groups to atoms in patterns
//	for (p=m.patterns; p; ++p)
//	{
//		if (chgrptype == 1) for (n=1; n<=p.nMolAtoms; ++n) p.atoms[n].bit = 1;
//		else if (chgrptype == 2) for (n=1; n<=p.nMolAtoms; ++n) p.atoms[n].bit = n;
//		else
//		{
//			# Zero atom 'bits' first
//			for (n=1; n<=p.nMolAtoms; ++n) p.atoms[n].bit = 0;
//			# Loop over atoms - if 'bit' is non-zero, continue. If only one bond, continue. If no bonds, assign chgrp.
//			chgrp = 1;
//			for (n=1; n<=p.nMolAtoms; ++n)
//			{
//				i = p.atoms[n];
//				i.bit = chgrp;
//				# All atoms attached to this atom which have only one bond will be part of the same charge group
//				for (Bond bnd = i.bonds; bnd; ++bnd)
//				{
//					j = bnd.partner(i);
//					if ((j.nBonds != 1) || (j.bit != 0)) continue;
//					j.bit = chgrp;
//				}
//				++chgrp;
//			}
//		}
//	}
//		
//	# ---------------
//	# PARAMETER LEVEL
//	# ---------------
//	writeLine(";Parameter level");
//
//	# *** [ defaults ]
//	writeLine("[ defaults ]");
//	writeLine("; nbfunc    comb-rule     gen-pairs       fudgeLJ  fudgeQQ");
//	# Attempt to grab scaling factors from first torsion in model
//	if (m.nFFTorsions > 0)
//	{
//		escale = m.ffTorsions[1].eScale;
//		vscale = m.ffTorsions[1].vScale;
//	}
//	# Only possible nbfunc values are 1 (LJ) or 2 (Buckingham), comb-rules are 2 (geom/arith) or 3 (geom/geom)
//	fi = m.atoms[1].type;
//	if (fi.form == "lj") writeLineF("    1           2            no         %8.4f %8.4f\n",vscale,escale);
//	else if (fi.form == "ljgeom") writeLineF("    1           3            no         %8.4f %8.4f\n",vscale,escale);
//	else if (fi.form == "buck") { uselj = FALSE; writeLineF("    2           1            no         %8.4f %8.4f\n",vscale,escale); }
//	else error("Short-range functional form '%s' is not supported by GROMACS.\n", fi.form);
//	
//	# *** [ atomtypes ]
//	writeLine("\n[ atomtypes ]");
//	if (uselj) writeLine(";type     mass        charge   ptype     param1     param2");
//	#else writeLine("
//	for (fi = m.ffTypes; fi; ++fi) writeLineF("%-6s %10.5f %10.6f     A    %10.6f %10.6f\n",fi.name,fi.mass,fi.charge,fi.parameter("sigma")/10,fi.parameter("epsilon"));
//
//	# ---------------
//	# MOLECULAR LEVEL
//	# ---------------
//	writeLineF("\n;Molecular level");
//
//	for (p=m.patterns; p; ++p)
//	{
//		# *** [ moleculetype ]
//		writeLine("\n[ moleculetype ]");
//		writeLine("; moleculename   nrexcl");
//		writeLineF("%15s      3\n", p.name);
//
//		# *** [ atoms ]
//		writeLine("\n[ atoms ]");
//		writeLine("; atomnr  type   resnr  residue");
//		for (n=1; n<=p.nMolAtoms; ++n) writeLineF(" %6i   %-6s  1   %-6s   %-6s  %2i  %8.4f  %8.4f\n", n, p.atoms[n].type.name, p.name, p.atoms[n].type.name, p.atoms[n].bit, p.atoms[n].q, p.atoms[n].type.mass);
//
//		nconstraints = 0;
//		nub = 0;
//		
//		# *** [ bonds ]
//		writeLineF("\n[ bonds ]\n");
//		writeLine(";    ai     aj   func   param1   param2   param3");
//		for (b = p.bonds; b; ++b)
//		{
//			if (b.type == "ub") ++nub;
//			else if (b.form == "harmonic") writeLineF("%6i %6i      1   %9.5f %9.5f\n", b.id[1], b.id[2], b.parameter("eq")/10,b.parameter("k")*100);
//			else if (b.form == "morse") writeLineF("%6i %6i      3   %9.5f %9.5f %9.5f\n", b.id[1], b.id[2], b.parameter("eq")/10,b.parameter("k")*100,b.parameter("beta")*10);
//			else if (b.form == "constraint") ++nconstraints;
//			else
//			{
//				++nfailed[1];
//				printf("Bond potential form '%s' not supported by GROMACS.\n", b.form);
//				writeLineF("%6i %6i   0   %9.5f %9.5f %9.5f\n", b.id[1], b.id[2], b.data[1], b.data[2], b.data[3]);
//			}
//		}
//
//		# *** [ angles ]
//		writeLineF("\n[ angles ]\n");
//		writeLine(";    ai     aj     ak    func   param1   param2   param3");
//		for (b = p.angles; b; ++b)
//		{
//			if (b.form == "harmonic")
//			{
//				# Does this angle have a corresponding Urey-Bradley term?
//				if (nub == 0) hasub = FALSE;
//				else for (b2=p.bonds; b2; ++b2) if ((b2.type == "ub") && (b.id[1] == b.id[1]) && (b.id[2] == b.id[2]) && (b.id[3] == b.id[3]))
//				{
//					hasub = TRUE;
//					writeLineF("%6i %6i %6i      5   %9.5f %9.5f %9.5f %9.5f\n", b.id[1], b.id[2], b.id[3], b.parameter("eq"),b.parameter("k"),b2.parameter("eq")*0.1, b2.parameter("k"));
//					break;
//				}
//				if (!hasub) writeLineF("%6i %6i %6i      1   %9.5f %9.5f\n", b.id[1], b.id[2], b.id[3], b.parameter("eq"),b.parameter("k"));
//			}
//			else if (b.form == "harmcos") writeLineF("%6i %6i %6i      2   %9.5f %9.5f\n", b.id[1], b.id[2], b.id[3], b.parameter("eq"),b.parameter("k"));
//			else if (b.form == "bondconstraint") ++nconstraints;
//			else
//			{
//				++nfailed[2];
//				printf("Angle potential form '%s' not supported by GROMACS.\n", b.form);
//				writeLineF("%6i %6i %6i      0   %9.5f %9.5f %9.5f\n", b.id[1], b.id[2], b.id[3], b.data[1], b.data[2], b.data[3]);
//			}
//		}
//
//		# *** [ dihedrals ]
//		writeLineF("\n[ dihedrals ]\n");
//		writeLine(";    ai     aj     ak     al    func   param1   param2   param3   param4");
//		for (b = p.torsions; b; ++b)
//		{
//			if (b.form == "cos")
//			{
//				# Aten allows variable 's' prefactor, but GROMACS assumes no prefactor (i.e. +1)...
//				double s = abs(b.parameter("s") - 1.0);
//				if (s < 0.01) writeLineF("%6i %6i %6i %6i      1   %9.5f %9.5f %9.5f\n", b.id[1], b.id[2], b.id[3], b.id[4], b.parameter("eq"),b.parameter("k"),b.parameter("n"));
//				else
//				{
//					++nfailed[3];
//					printf("Cosine torsion potential form must have a prefactor 's' of 1.0 in GROMACS, but this term has %f.\n", b.parameter("s"));
//					writeLineF("%6i %6i %6i %6i      0   %9.5f %9.5f %9.5f\n", b.id[1], b.id[2], b.id[3], b.id[4], b.parameter("eq"),b.parameter("k"),b.parameter("n"));
//				}
//			}
//			else if (b.form == "cos3") writeLineF("%6i %6i %6i %6i      5   %9.5f %9.5f %9.5f 0.0\n", b.id[1], b.id[2], b.id[3], b.id[4], b.parameter("k1"),b.parameter("k2"), b.parameter("k3"));
//			else if (b.form == "cos4") writeLineF("%6i %6i %6i %6i      5   %9.5f %9.5f %9.5f %9.5f\n", b.id[1], b.id[2], b.id[3], b.id[4], b.parameter("k1"),b.parameter("k2"), b.parameter("k3"), b.parameter("k4"));
//			else
//			{
//				++nfailed[3];
//				printf("Torsion potential form '%s' not supported by GROMACS.\n", b.form);
//				writeLineF("%6i %6i %6i %6i      0   %9.5f %9.5f %9.5f %9.5f\n", b.id[1], b.id[2], b.id[3], b.id[4], b.data[1], b.data[2], b.data[3], b.data[4]);
//			}
//		}
//
//		# *** [ constraints ]
//		if (nconstraints != 0)
//		{
//			writeLineF("\n[ constraints ]\n");
//			writeLine(";    ai    aj   func   param1   param2   param3");
//			for (b = p.bonds; b; ++b) if (b.form == "constraint") writeLineF("%6i %6i      1   %9.5f\n", b.id[1], b.id[2], b.parameter("eq")/10);
//			for (b = p.angles; b; ++b) if (b.form == "bondconstraint") writeLineF("%6i %6i      1   %9.5f\n", b.id[1], b.id[3], b.parameter("eq")/10);
//		}
//	}	
//
//	# ------------
//	# SYSTEM LEVEL
//	# ------------
//	writeLine("\n;System level");
//
//	# *** [ system ]
//	writeLine("[ system ]");
//	writeLine(m.name);
//
//	# *** [ molecules ]
//	writeLine("[ molecules ]");
//	writeLine("; moleculename     number");
//	for (p = m.patterns; p; ++p) writeLineF("%15s      %i\n", p.name, p.nMols);
//
//	# ----
//	# Done
//	# ----
//
//	# Raise any warnings...
//	if ((nfailed[1]+nfailed[2]+nfailed[3]) != 0)
//	{
//		string text = "Several forcefield terms could not be written:\n";
//		if (nfailed[1] > 0) text += toa("\t%i bond term(s)\n", nfailed[1]);
//		if (nfailed[2] > 0) text += toa("\t%i angle term(s)\n", nfailed[2]);
//		if (nfailed[3] > 0) text += toa("\t%i torsion term(s)\n", nfailed[3]);
//		error(text);
//	}
//
//}
//
//#
//# OLD EXPRESSION EXPORT FILTER BEGINS HERE
//#
//filter(type="exportexpression",name="Gromacs .rtp file Specification", extension="rtp", glob="*.rtp",nickname="rtp")
//{
//	# Variable declaration
//        pattern p;
//	atom a;
//	bound b;
//	ffatom at1, at2;
//	int nvdw, n, nc, n2, nconstraints, i,j,k,l,f;
//	
//	# Get current model
//	model m = aten.frame;
//	
//	typeModel();
//
//	#should make sure there is at least one pattern here
//	if (m.patterns)
//	{
//	printf("a pattern exists\n");
//	}
//	else
//	{
//	printf("noone here but us chickens\n");
//	p = newPattern("NUN", 1, m.nAtoms);
//	}
//     
//	# Loop over patterns (molecule types)
//	k=0;
//	for (p = m.patterns; p; ++p)
//	{
//		nconstraints = 0;
//
//		k++;
//
//		if (p.name) # actually want if p.name is 4 chars in length and/or not the same as another potential p.name
//		{
//		writeLineF("[ %4s ]\n", p.name);
//		} 
//		else  
//		{
//		writeLineF("[ MOL%i ]\n", k);
//		}
//
//	writeLine(" [ atoms ]");
//	a = p.firstAtom;
//		for (n=1; n<=p.nMolAtoms; ++n)
//		{
//
//			printf("Using : %s\n", a.type.ff.name);
//			if(a.type.ff.name == "Canongia Lopes & Padua Ionic Liquids (version 01/06/2006)"){
//			writeLineF("   %3s%-4i    cldp_%-3i    %9.6f      1\n",a.symbol,n,a.type.id,a.q);
//			}
//			else {
//			writeLineF("   %3s%-4i    opls_%-3i    %9.6f      1\n",a.symbol,n,a.type.id,a.q);
//			}
//
//
//			++a;
//		}
//
//    	# Bonds in pattern
//	writeLineF(" [ bonds ]\n");
//	nc = 0;
//		for (n=1; n<=p.nBonds; ++n) if (p.bonds[n].form == "constraint") ++nc;
//
//		for (n=1; n<=p.nBonds; ++n)
//		{
//			# Grab bound pointer
//			b = p.bonds[n];
//			if (b.form == "constraint") nconstraints++;
//			else 
//			{
//			i=b.id[1];
//			j=b.id[2];
//
//#			writeLineF("   %5s%i %5s%i\n",p.atoms[i].type.name,i,p.atoms[j].type.name,j);
//			writeLineF("   %5s%i %5s%i\n",p.atoms[i].symbol,i,p.atoms[j].symbol,j);
//
//
//			}
//                   }
//
//    	# Torsions in pattern
//	writeLineF("\n  [dihedrals]\n");
//		# Grab bound pointer
//		for (b = p.torsions; b; ++b)
//		{
//			i=b.id[1];
//			j=b.id[2];
//			k=b.id[3];
//			l=b.id[4];			
//#			writeLineF("   %5s%i %5s%i %5s%i %5s%i   \n",p.atoms[i].type.name,i,p.atoms[j].type.name,j,p.atoms[k].type.name,k,p.atoms[l].type.name,l);
//			writeLineF("   %5s%i %5s%i %5s%i %5s%i   \n",p.atoms[i].symbol,i,p.atoms[j].symbol,j,p.atoms[k].symbol,k,p.atoms[l].symbol,l);
//
//
//		}
//
//
//          }
//}
//
	return false;
}

// Return whether this plugin can export data
bool GROMACSModelPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool GROMACSModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool GROMACSModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool GROMACSModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool GROMACSModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool GROMACSModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool GROMACSModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool GROMACSModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}
 

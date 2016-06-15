/*
        *** QuantumEspresso Model Plugin Functions
        *** src/plugins/io_espresso/espresso_funcs.cpp
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

#include "plugins/io_espresso/espresso.hui"
#include "model/model.h"

// Constructor
QuantumEspressoModelPlugin::QuantumEspressoModelPlugin()
{
}

// Destructor
QuantumEspressoModelPlugin::~QuantumEspressoModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* QuantumEspressoModelPlugin::makeCopy()
{
	return new QuantumEspressoModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory QuantumEspressoModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString QuantumEspressoModelPlugin::name() const
{
	return QString("QuantumEspresso (dlputils) 3D probability density");
}

// Nickname of plugin
QString QuantumEspressoModelPlugin::nickname() const
{
	return QString("espresso");
}

// Description (long name) of plugin
QString QuantumEspressoModelPlugin::description() const
{
	return QString("Import/export for dlputils QuantumEspresso files");
}

// Related file extensions
QStringList QuantumEspressoModelPlugin::extensions() const
{
	return QStringList() << "espresso";
}

// Exact names
QStringList QuantumEspressoModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool QuantumEspressoModelPlugin::canImport()
{
	return true;
}

// Import data from the speespressoied file
bool QuantumEspressoModelPlugin::importData()
{
//filter(type="exportmodel", name="Quantum Espresso Input", nickname="qe", extension="in", glob="*.in", id=15)
//{
//	# Pseudopotential Data (Basic set from supplied pseudos)
//	string pseudoData[aten.nElements];
//	pseudoData[Al] = "'Al.pz-vbc.UPF'";
//	pseudoData[As] = "'As.pz-bhs.UPF'";
//	pseudoData[Au] = "'Au.rel-pz-kjpaw.UPF'";
//	pseudoData[C] = "'C.pbe-rrkjus.UPF', 'C.pbe-van_bm.UPF', 'C.pz-kjpaw.UPF', 'C.pz-rrkjus.UPF', 'C.pz-vbc.UPF', 'C.tpss-mt.UPF'";
//	pseudoData[Cu] = "'Cu.pbe-kjpaw.UPF', 'Cu.pz-d-rrkjus.UPF'";
//	pseudoData[Fe] = "'Fe.pz-nd-rrkjus.UPF', 'Fe.rel-pbe-kjpaw.UPF'";
//	pseudoData[Ge] = "'Ge.pbe-kjpaw.UPF'";
//	pseudoData[H] = "'H.blyp-vbc.UPF', 'H.coulomb-ae.UPF', 'H.pbe-kjpaw.UPF', 'H.pbe-rrkjus.UPF', 'H.pz-kjpaw.UPF', 'H.pz-vbc.UPF', 'H.tpss-mt.UPF', 'H_US.van'";
//	pseudoData[Mg] = "'Mg.pz-n-vbc.UPF'";
//	pseudoData[N] = "'N.blyp-mt.UPF', 'N.pbe-kjpaw.UPF'";
//	pseudoData[Ni] = "'Ni.pbe-nd-rrkjus.UPF', 'Ni.pz-nd-rrkjus.UPF', 'Ni.rel-pbe-nd-rrkjus.UPF'";
//	pseudoData[O] = "'O.blyp-mt.UPF', 'O.pbe-kjpaw.UPF', 'O.pbe-rrkjus.UPF', 'O.pz-kjpaw.UPF', 'O.pz-rrkjus.UPF', 'O.pz-van_ak.UPF', 'O_US.van'";
//	pseudoData[Pb] = "'Pb.pz-d-van.UPF'";
//	pseudoData[Pt] = "'Pt.rel-pbe-n-rrkjus.UPF', 'Pt.rel-pz-n-rrkjus.UPF'";
//	pseudoData[Rh] = "'Rh.pbe-rrkjus_lb.UPF'"; # 'Rhs.pbe-rrkjus_lb.UPF'
//	pseudoData[Si] = "'Si.bhs', 'Si.pbe-rrkj.UPF', 'Si.pz-vbc.UPF', 'Si.rel-pbe-rrkj.UPF'";
//	pseudoData[Ti] = "'Ti.pz-sp-van_ak.UPF'";
//
//	# Grab model pointer
//	Model m = aten.frame;
//
//	# Determine number of species (elements) used in model, and create element->species map
//	int n, nSpecies = 0, elInSystem[aten.nElements];
//	Atom i;
//	for (i = m.atoms; i; ++i) if (elInSystem[i.z] == 0) elInSystem[i.z] = ++nSpecies;
//	
//	# General Options dialog
//	Dialog ui = createDialog("Quantum Espresso Export Options");
//	#ui.title = "Quantum Espresso Export Options";
//	Widget group, w, w2;
//
//	# General control options group (&CONTROL)
//	group = ui.addGroup("controlgroup", "&CONTROL", 1, 1);
//	w = group.addCombo("control_calculation", "Calculation", "'scf', 'nscf', 'bands', 'relax', 'md', 'vc-relax', 'vc-md'", 4, 1, 1);
//
//	group.addCombo("control_restart", "Restart Mode", "none,from_scratch,restart,reset_counters,upto", 1, 3, 1);
//	group.addEdit("control_title", "Title", "Quantum Espresso input exported from Aten", 1, 2, 2);
//	group.addEdit("control_pseudo", "Pseudo Dir", "/home/user/src/espresso-4.3.2/pseudo", 1, 3, 2);
//	group.addIntegerSpin("control_nstep", "NStep", 1, 100000, 1, 50, 1, 4);
//	group.addDoubleSpin("control_dt", "dt", 0.0, 10.0, 0.1, 1.0, 3, 4);
//	group.addCheck("control_tstress", "tstress", 0, 1, 5);
//	w.onInteger(6, 7, "sendbool", "control_tstress", "value");
//
//	# System definition (&SYSTEM)
//	group = ui.addGroup("systemgroup", "&SYSTEM", 1, 2);
//	w = group.addCombo("system_ibrav", "ibrav", "'0 - free', '1 - cubic P (sc)', '2 - cubic F (fcc)', '3 - cubic I (bcc)', '4 - Hexagonal and Trigonal P', '5 - Trigonal R (3-fold axis c)', '-5 - Trigonal R (3-fold axis <111>)', '6 - Tetragonal P (st)', '7 - Tetragonal I (bct)', '8 - Orthorhombic P', '9 - Orthorhombic base-centered(bco)', '10 - Orthorhombic face-centered', '11 - Orthorhombic body-centered', '12 - Monoclinic P (unique axis c)', '-12 - Monoclinic P (unique axis b)', '13 - Monoclinic base-centered', '14 - Triclinic'", 1, 1, 1, 1);
//	group.addDoubleSpin("system_totcharge", "tot_charge", -100.0, 100.0, 1.0, 0.0, 1, 2);
//	group.addDoubleSpin("system_ecutwfc", "ecutwfc", 0.0, 100000.0, 10.0, 50.0, 1, 3);
//	group.addDoubleSpin("system_ecutrho", "ecutrho", 0.0, 100000.0, 10.0, 200.0, 1, 4);
//
//	# Cell definition (&CELL)
//	group = ui.addGroup("cellgroup", "&CELL", 2, 1);
//	group.enabled = FALSE;
//	group.addCombo("cell_dynamics", "Cell Dynamics", "'none', 'sd', 'damp-pr', 'damp-w', 'bfgs'", 5, 1, 1);
//	group.addDoubleSpin("cell_press", "P (kbar)", -1000.0, 1000.0, 0.001, 0.0, 1, 2);
//	group.addDoubleSpin("cell_conv", "Converge", -10.0, 10.0, 0.01, 0.5, 1, 3);
//	group.addCombo("cell_dofree", "DoFree", "'all', 'x', 'y', 'z', 'xy', 'xz', 'yz', 'xyz', 'shape'", 1, 1, 4);
//	# -- Widget relationships
//	w = ui.widget("control_calculation");
//	w.onInteger(6, 7, "sendbool", "cellgroup", "enabled");
//	w.onInteger(6, 6, "set", "cell_dynamics", "items", "'none', 'sd', 'damp-pr', 'damp-w', 'bfgs'");
//	w.onInteger(7, 7, "set", "cell_dynamics", "items", "'none', 'pr', 'w'");
//
//	# Ions (&IONS)
//	group = ui.addGroup("iongroup", "&IONS", 3, 1);
//	group.enabled = FALSE;
//	group.addCombo("ion_dynamics", "Ion Dynamics", "'bfgs', 'damp'", 1, 1, 1);
//	group.addCombo("ion_positions", "Ion Positions", "'default', 'from_input'", 1, 1, 1);
//	# -- Widget relationships
//	w = ui.widget("control_calculation");
//	w.onInteger(4, 7, "sendbool", "iongroup", "enabled");
//	w.onInteger(4, 4, "set", "cell_dynamics", "items", "'bfgs', 'damp'");
//	w.onInteger(5, 5, "set", "cell_dynamics", "items", "'verlet', 'langevin'");
//	w.onInteger(6, 6, "set", "cell_dynamics", "items", "'bfgs', 'damp'");
//	w.onInteger(7, 7, "set", "cell_dynamics", "items", "'beeman'");
//
//	# Pseudopotential list selector (dynamically created)
//	group = ui.addGroup("pseudogroup", "Pseudopotentials", 3, 1);
//	group.verticalFill = TRUE;
//	for (n=1; n<aten.nElements; ++n) if (elInSystem[n] != 0)
//	{
//		if (pseudoData[n] != "") group.addCombo(toa("pseudo%s",aten.elements[n].symbol), aten.elements[n].symbol, pseudoData[n], 1);
//		else group.addCombo(toa("pseudo%s",aten.elements[n].symbol), aten.elements[n].symbol, "'None Defined'", 1);
//	}
//	# Execute dialog
//	if (!ui.show()) error("Options dialog canceled.\n");
//
//	# Variable declaration
//	string e;
//	double rx,ry,rz;
//
//	# &CONTROL Section
//	writeLine("&CONTROL");
//	writeLineF("   title='%s',\n", ui.asString("control_title"));
//	writeLineF("   calculation='%s',\n", ui.asString("control_calculation"));
//	writeLineF("   tstress=%s,\n", ui.asInteger("control_tstress") ? ".TRUE." : ".FALSE.");
//	writeLineF("   pseudo_dir='%s',\n", ui.asString("control_pseudo"));
//	if (ui.asString("control_restart") != "none") writeLineF("   restart_mode='%s',\n", ui.asString("control_restart"));
//	writeLineF("   nstep=%i,\n", ui.asInteger("control_nstep"));
//	writeLineF("   dt=%f\n", ui.asDouble("control_dt"));
//	writeLine("/");
//
//	# &SYSTEM Section
//	writeLine("&SYSTEM");
//	int ibrav;
//	readVarF(ui.asString("system_ibrav"), "%i", ibrav);
//	writeLineF("   ibrav=%i,\n", ibrav);
//	switch (ibrav)
//	{
//		case (0):
//		case (1):
//		case (2):
//		case (3):
//			writeLineF("   celldm(1)=%f,\n", m.cell.a/ANGBOHR);
//			break;
//		case (4):
//		case (6):
//		case (7):
//			writeLineF("   celldm(1)=%f,\n", m.cell.a/ANGBOHR);
//			writeLineF("   celldm(3)=%f,\n", (m.cell.c/m.cell.a)/ANGBOHR);
//			break;
//		case (5):
//		case (-5):
//			writeLineF("   celldm(1)=%f,\n", m.cell.a/ANGBOHR);
//			writeLineF("   celldm(4)=%f,\n", cos(m.cell.alpha));
//			break;
//		case (8):
//		case (9):
//		case (10):
//		case (11):
//			writeLineF("   celldm(2)=%f,\n", (m.cell.b/m.cell.a)/ANGBOHR);
//			writeLineF("   celldm(3)=%f,\n", (m.cell.c/m.cell.a)/ANGBOHR);
//			break;
//		case (12):
//		case (13):
//			writeLineF("   celldm(2)=%f,\n", (m.cell.b/m.cell.a)/ANGBOHR);
//			writeLineF("   celldm(3)=%f,\n", (m.cell.c/m.cell.a)/ANGBOHR);
//			writeLineF("   celldm(4)=%f,\n", cos(m.cell.gamma));
//			break;
//		case (-12):
//			writeLineF("   celldm(2)=%f,\n", (m.cell.b/m.cell.a)/ANGBOHR);
//			writeLineF("   celldm(3)=%f,\n", (m.cell.c/m.cell.a)/ANGBOHR);
//			writeLineF("   celldm(5)=%f,\n", cos(m.cell.beta));
//			break;
//		case (14):
//			writeLineF("   celldm(2)=%f,\n", (m.cell.b/m.cell.a)/ANGBOHR);
//			writeLineF("   celldm(3)=%f,\n", (m.cell.c/m.cell.a)/ANGBOHR);
//			writeLineF("   celldm(4)=%f,\n", cos(m.cell.alpha));
//			writeLineF("   celldm(5)=%f,\n", cos(m.cell.beta));
//			writeLineF("   celldm(6)=%f,\n", cos(m.cell.gamma));
//			break;
//	}
//	writeLineF("   nat=%i,\n",m.nAtoms);
//	writeLineF("   ntyp=%i,\n",nSpecies);
//	writeLineF("   ecutwfc=%f,\n", ui.asDouble("system_ecutwfc"));
//	writeLineF("   ecutrho=%f,\n", ui.asDouble("system_ecutrho"));
//	writeLine("/");
//
//	# &ELECTRONS Section
//	writeLine("&ELECTRONS");
//	writeLine("  mixing_beta=0.25,");
//	writeLine("  conv_thr=1.0e-8");
//	writeLine("/");
//
//	# &IONS Section (only if calculation type requires it)
//	int calcType = ui.asInteger("control_calculation");
//	if (calcType >= 4)
//	{
//		writeLine("&IONS");
//		writeLineF("  ion_dynamics='%s',\n", ui.asString("ion_dynamics"));
//		writeLineF("  ion_positions='%s'\n", ui.asString("ion_positions"));
//		writeLine("/");
//	}
//
//	# &CELL Section (only if calculation type requires it)
//	if (calcType > 5)
//	{
//		writeLine("&CELL");
//		writeLineF("  cell_dynamics='%s',\n", ui.asString("cell_dynamics"));
//		writeLineF("  press=%f,\n", ui.asDouble("cell_press"));
//		writeLineF("  press_conv_thr=%f,\n", ui.asDouble("cell_conv"));
//		writeLineF("  cell_dofree='%s'\n", ui.asString("cell_dofree"));
//		writeLine("/");
//	}
//
//	# Write cell data
//	if (m.cell.type != "none")
//	{
//		writeLine("CELL_PARAMETERS");
//		double alat = m.cell.a;
//		writeLineF("    %f %f %f\n", m.cell.ax/alat, m.cell.ay/alat, m.cell.az/alat);
//		writeLineF("    %f %f %f\n", m.cell.bx/alat, m.cell.by/alat, m.cell.bz/alat);
//		writeLineF("    %f %f %f\n", m.cell.cx/alat, m.cell.cy/alat, m.cell.cz/alat);
//	}
//
//	# Write atomic species data
//	writeLine("ATOMIC_SPECIES");
//	for (n=1; n<=aten.nElements; n++) if (elInSystem[n] != 0)
//	{
//		String pp = ui.asString(toa("pseudo%s", aten.elements[n].symbol));
//		if (pp == "None Defined") pp = toa("%s-undefined.UPF", aten.elements[n].symbol);
//		writeLineF("%-5s  %10.4f  %s\n", aten.elements[n].symbol, aten.elements[n].mass, pp);
//	}
//
//	# Write atomic positions data
//	writeLine("ATOMIC_POSITIONS {angstrom}");
//	for (i = m.atoms; i; ++i) writeLineF("%-5s   %14.8f %14.8f %14.8f\n",i.symbol,i.rx,i.ry,i.rz);
//}
//
//# Subroutines for output-file reading
//
//# Read next occurrence of unit cell (PWSCF)
//int readPWCell(UnitCell ucell, double alat, string alatKeywd)
//{
//	string s = "crystal axes: (cart. coord. in units of " + alatKeywd + ")";
//	if (!find(s)) return FALSE;
//	if (!readLineF("%23* %f %f %f", ucell.ax,ucell.ay,ucell.az)) return 0;
//	if (!readLineF("%23* %f %f %f", ucell.bx,ucell.by,ucell.bz)) return 0;
//	if (!readLineF("%23* %f %f %f", ucell.cx,ucell.cy,ucell.cz)) return 0;
//	for (int n=1; n<=9; ++n) ucell.matrix[n] *= alat;
//	return TRUE;
//}
//
//# Read next occurrence of unit cell (CP)
//int readCPCell(UnitCell ucell)
//{
//	if (!find(" alat  = ")) return FALSE;
//	if (!readLineF("%* $* %f %f %f", ucell.ax,ucell.ay,ucell.az)) return 0;
//	if (!readLineF("%* $* %f %f %f", ucell.bx,ucell.by,ucell.bz)) return 0;
//	if (!readLineF("%* $* %f %f %f", ucell.cx,ucell.cy,ucell.cz)) return 0;
//	return TRUE;
//}
//
//# Read atomic coordinates (PWSCF)
//int readPWAtoms(Model m, int nAtoms, double alat, string alatKeywd)
//{
//	string s = "site n.     atom                  positions (" + alatKeywd + " units)";
//	if (!find(s)) return FALSE;
//	string el;
//	double x, y, z;
//	for (int n=0; n<nAtoms; ++n)
//	{
//		readLineF("%12*%10s%16*%f %f %f", el, x, y, z);
//		m.newAtom(el, x*alat, y*alat, z*alat);
//	}
//	return TRUE;
//}
//
//# Read atomic forces (PWSCF)
//int readPWForces(Model m)
//{
//	if (!find("Forces acting on atoms (Ry/au):") ) return FALSE;
//	skipLine();
//	Vector f;
//	double conv = (13.60569253*96.4853365)/ANGBOHR;
//	for (int n=1; n<=m.nAtoms; ++n)
//	{
//		readLineF("%*%*%*%*%*%*%f%f%f",f.x, f.y, f.z);
//		# Forces in Ry/au, so convert to kJ/mol/Angstrom
//		f *= conv;
//		m.atoms[n].f = f;
//	}
//}
//
//# Read next occurrence of unit cell (PWSCF frame)
//int readPWCellFrame(UnitCell ucell, double alat, string alatKeywd)
//{
//	string s = "new lattice vectors (" + alatKeywd + " unit)";
//	if (!find(s)) return FALSE;
//	if (!readLine(ucell.ax, ucell.ay, ucell.az)) return 0;
//	if (!readLine(ucell.bx, ucell.by, ucell.bz)) return 0;
//	if (!readLine(ucell.cx, ucell.cy, ucell.cz)) return 0;
//	for (int n=1; n<=9; ++n) ucell.matrix[n] *= alat;
//	return TRUE;
//}
//
//# Read atomic coordinates (PWSCF frame)
//int readPWAtomsFrame(Model m, int nAtoms)
//{
//	if (!find("new positions in cryst coord")) return FALSE;
//	string el;
//	double x, y, z;
//	for (int n=0; n<nAtoms; ++n)
//	{
//		readLine(el, x, y, z);
//		m.newAtomFrac(el, x, y, z);
//	}
//	return TRUE;
//}
//
//int readAtoms(int nAtoms)
//{
//	double x,y,z;
//	string el;
//	for (int n=0; n<nAtoms; ++n)
//	{
//		if (!readLine(el,x,y,z)) return 0;
//		newAtom(el,x,y,z);
//	}
//	return 1;
//}
//
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

	return true;
}

// Return whether this plugin can export data
bool QuantumEspressoModelPlugin::canExport()
{
	return false;
}

// Export data to the speespressoied file
bool QuantumEspressoModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool QuantumEspressoModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool QuantumEspressoModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool QuantumEspressoModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool QuantumEspressoModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool QuantumEspressoModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool QuantumEspressoModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

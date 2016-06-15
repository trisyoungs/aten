/*
        *** GAMESS Model Plugin Functions
        *** src/plugins/io_gamess/gamess_funcs.cpp
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

#include "plugins/io_gamess/gamess.hui"
#include "model/model.h"

// Constructor
GAMESSModelPlugin::GAMESSModelPlugin()
{
}

// Destructor
GAMESSModelPlugin::~GAMESSModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* GAMESSModelPlugin::makeCopy()
{
	return new GAMESSModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory GAMESSModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString GAMESSModelPlugin::name() const
{
	return QString("GAMESS (dlputils) 3D probability density");
}

// Nickname of plugin
QString GAMESSModelPlugin::nickname() const
{
	return QString("gamess");
}

// Description (long name) of plugin
QString GAMESSModelPlugin::description() const
{
	return QString("Import/export for dlputils GAMESS files");
}

// Related file extensions
QStringList GAMESSModelPlugin::extensions() const
{
	return QStringList() << "gamess";
}

// Exact names
QStringList GAMESSModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool GAMESSModelPlugin::canImport()
{
	return true;
}

// Import data from the spegamessied file
bool GAMESSModelPlugin::importData()
{
//filter(type="importtrajectory", name="GAMESS-US Trj File", nickname="gamustrj", extension="trj", glob="*.trj", zmap="name", search="GAMESS VERSION", id=11)
//{
//        int readHeader()
//        {
//                return TRUE;
//        }
//
//        int readFrame()
//        {
//		int n, natm, nfrag, nqmmm, packettype, skip = 0;
//		string discard, symbol, line;
//		double ano, x, y, z;
//		model m = aten.frame;
//
//		# MD data packets have three skippable lines at the start
//		# IRC data packets have 
//		if (find("DATA PACKET",line))
//		{
//			# Found a data packet - determine which type it is...
//			if (contains(line, "===== MD")) { packettype = 1; skip = 3; }
//			else if (contains(line, "===== IRC")) { packettype = 2; skip = 1; }
//			else
//			{
//				printf("Unrecognised DATA PACKET type encountered in trj file.\n");
//				return FALSE;
//			}
//		}
//		else packettype = 0;
//
//		# Did we find some data?
//		if (packettype != 0)
//		{
//			# Read in number of atoms
//			readLine(discard, natm, discard, nfrag, discard, nqmmm);
//			skipLine(skip);
//			# Read QM atom data (if present)
//			if (natm > 0)
//			{
//				skipLine();
//				for (n=0; n<natm; ++n)
//				{
//					readLine(symbol,ano,x,y,z);
//					newAtom(symbol,x,y,z);
//				}
//			}
//			# Read fragment data (if present)
//			if (nfrag > 0)
//			{
//				error("Reading of fragment data from trajectory not implemented yet.");
//				skipLine();
//				for (n=0; n<nfrag; ++n)
//				{
//				}
//			}
//			# Read QMMM data (if present)
//			if (nqmmm > 0)
//			{
//				error("Reading of QMMM data form trajectory not implemented yet.");
//				skipLine();
//				for (n=0; n<nqmmm; ++n)
//				{
//				}
//			}
//			# Specific reads for DATA PACKET types
//			if (packettype == 1)
//			{
//				# Read in velocities for atoms
//				if (find("TVELQM(1)"))
//				{
//					for (n=1; n<=natm; ++n)
//					{
//						readLine(x, y, z);
//						m.atoms[n].v = { x, y, z};
//					}
//				}
//			}
//			else if (packettype == 2)
//			{
//				# Discard mass-weighted gradient
//				skipLine(natm+1);
//			}
//			rebond();
//			finaliseModel();
//		}
//		else return FALSE;
//		return TRUE;
//	}
//}
//
//filter(type="importmodel", name="GAMESS-US Log File", nickname="gamuslog", extension="log", glob="*.log", zmap="name", search="GAMESS VERSION", id=11)
//{
//	# Variable declaration
//	int result,nstructures,natoms,n,l,lastshell,shell,count,orb_no[5];
//	Atom i;
//	BasisShell bas;
//	BasisPrimitive prim;
//	EigenVector orbs[5];
//	Vibration vibs[5];
//	string line,e,name,discard,orb_symm[5],type, vib_imag[5], vib_symm[5];
//	double rx,ry,rz, exponent,coeff,orb_nrg[5], orb_coeff[5];
//	double vib_freq[5], vib_dx[5], vib_dy[5], vib_dz[5], vib_rmass[5], vib_ir[5];
//
//	# Create a new model, and store its pointer for use later
//	model m = newModel(filterFilename());
//
//	# Find initial coordinates (in Bohr)
//	if (find("ATOM      ATOMIC                      COORDINATES (BOHR)"))
//	{
//		skipLine();
//		while (readLine(e,discard,rx,ry,rz) != 0) newAtom(e,rx,ry,rz);
//		bohr(aten.model);
//		rebond();
//	}
//	else error("No coordinates in file?");
//
//	# Read in basis set information
//	if (find("ATOMIC BASIS SET"))
//	{
//		skipLine(6);
//		# Loop over basis shell definitions, stopping when we find the text 'TOTAL'
//		readLineF("%s %s %i %f %r", e, name, n, exponent, line);
//		#printf("%s %s %i %f [%s]\n", e, name, n, exponent, line);
//		lastshell = -1;
//		count = 0;
//		while (e != "TOTAL")
//		{
//			# Start of a new atom?
//			if (n == 0)
//			{
//				count ++;
//				lastshell = 0;
//				#printf("Found start of atom %i, which is element %s\n", count, e);
//			}
//			else
//			{
//				# Create new basis shell?
//				shell = atoi(e);
//				#if (lastshell != shell) printf("Lastn (%i) is != shell (%i), so creating a new shell...\n", lastshell, shell);
//				if (lastshell != shell) bas = m.newBasisShell(count, name);
//				lastshell = shell;
//			}
//
//			# Store primitive's exponent and coefficient(s)
//			if (lastshell != 0)
//			{
//				prim = bas.addPrimitive(exponent);
//				# Cycle over provided coefficients
//				while (nextVarArg(line,coeff)) prim.addCoefficient(coeff);
//			}
//
//			# Get next line, ignoring blanks
//			do
//			{
//				readLineF("%s %s %i %f %r", e, name, n, exponent, line);
//			} while (e == "");
//			#printf("%s %s %i %f [%s]\n", e, name, n, exponent, line);
//		}
//		# We end up with the 'TOTAL NUMBER OF BASIS SET SHELLS' line (beginning at 'SET...'), so we can do a check
//		readVar(line, discard, discard, discard, n);
//		printf("         Number of basis set shells read in : %i\n", m.nBasisShells());
//		printf("            --> number specified in logfile : %i\n", n);
//		if (n != m.nBasisShells()) error("Failed to read in basis set information.");
//		# Also, check the total number of cartesian basis functions
//		readLineF("%r", line);
//		n = atoi( afterStr(line,"=") );
//		printf("Number of implied cartesian basis functions : %i\n", m.nBasisCartesians());
//		printf("            --> number specified in logfile : %i\n", n);
//		if (n != m.nBasisCartesians()) error("Failed to read in basis set information.");
//	}
//
//	# Determine number of atoms - search for line containing 'TOTAL NUMBER OF ATOMS'
//	if (!find("TOTAL NUMBER OF ATOMS",line)) error("Couldn't determine number of atoms from GAMESS-US output.");
//
//	# Get number of atoms from line
//	readVarF(line, "%48*%i",natoms);
//
//	# Now search for sets of coordinate
//	nstructures = 0;
//	# Lines containing 'COORDINATES OF ALL ATOMS ARE' are the beginning of coordinate sections
//	while (find("COORDINATES OF ALL ATOMS ARE"))
//	{
//		# Found a set of coordinates. Skip 2 lines and then read coordinates
//		nstructures++;
//		skipLine(2);
//		writeVarF(name, "Frame %i", nstructures);
//		m = addFrame(name);
//		for (n=1; n<=natoms; ++n)
//		{
//			readLine(e,discard,rx,ry,rz);
//			newAtom(e,rx,ry,rz);
//		}
//		# Recalculate bonding
//		rebond();
//		finaliseFrame();
//	}
//
//	# Molecular orbitals?
//	if (find("MOLECULAR ORBITALS"))
//	{
//		skipLine(2);
//		# MO information is provided 5 orbs per line, in the format:
//		#                     1          2          3          4
//		#                   -0.5942     0.2657     0.9175     1.5319
//		#                     A          A          A          A
//		#    1  H  1  S   -0.290651  -0.111641  -0.783160   1.077794
//		#    .  .  .  .
//		#    NC .  .  .   
//		# ...where NC is the number of cartesian basis functions.
//		for (n=1; n<m.nBasisShells(); n +=5)
//		{
//			# Read in orbital numbers as a sanity check
//			readLine(discard);
//			# End of orbital specification?
//			if (atoi(discard) == 0) break;
//			else if (n != atoi(discard))
//			{
//				printf("Failed to read in MO information at orbital number %i (read [%s] from file)\n", n, discard);
//				break;
//			}
//			# Read in eigenvalues and symmetry type
//			readLine(orb_nrg[1], orb_nrg[2], orb_nrg[3], orb_nrg[4], orb_nrg[5]);
//			readLine(orb_symm[1], orb_symm[2], orb_symm[3], orb_symm[4], orb_symm[5]);
//			# Create eigenvectors
//			for (l=1; l<6; ++l)
//			{
//				if ((l+n-1) > m.nBasisShells()) break;
//				verbose(" -- Created MO number %i with eigenvalue %f\n", l+n-1, orb_nrg[l]);
//				orbs[l] = m.newEigenvector();
//				orbs[l].eigenvalue = orb_nrg[l];
//				orbs[l].name = orb_symm[l];
//			}
//			# Next lines contain coefficients...
//			for (count=1; count<=m.nBasisCartesians(); ++count)
//			{
//				readLine(discard,e,shell,type,orb_coeff[1], orb_coeff[2], orb_coeff[3], orb_coeff[4], orb_coeff[5]);
//				for (l=1; l<6; ++l)
//				{
//					if ((l+n-1) > m.nBasisShells()) break;
//					orbs[l].vector[count] = orb_coeff[l];
//				}
//			}
//		}
//	}
//
//	# Frequencies?
//	rewind();
//	if (find("FREQUENCIES IN CM**-1"))
//	{
//		# Skip warning of non-stationary point if present...
//		if (find("THIS IS NOT A STATIONARY POINT"))
//		{
//			skipLine(3);
//			printf(" *** The frequencies correspond to a non-stationary point.\n");
//		}
//		else skipLine(2);
//		# Vibration information is provided 5 vibrations per line, in the format:
//		#                       1          2          3          4            5
//		#    FREQUENCY:      39.84 I     19.94       19.03       15.12       11.87
//		#     SYMMETRY:         A          A           A           A           A       (optional)
//		# REDUCED MASS:      2.15985     3.96061     4.04986     3.97821     5.98898
//		# IR INTENSITY:      0.00121     0.00399     0.00426     0.00020     0.00070
//		#  <blank line>
//		#    1  HYDROGEN  X  0.05470495  0.01254668 -0.05069468  0.01778118 -0.01077563
//		#    .  .  .  .
//		#    3N .  .  .   
//		# ...where N is the number of atoms in the model
//		for (n=1; n<3*m.nAtoms(); n +=5)
//		{
//			# Read in vibration numbers as a sanity check
//			readLine(discard);
//			if (n != atoi(discard))
//			{
//				printf("Failed to read in vibration information at vibration number %i (read [%s] from file)\n", n, discard);
//				break;
//			}
//			# Read in frequencies and imaginary flags
//			readLineF("%* %f%2s%f%2s%f%2s%f%2s%f%2s", vib_freq[1], vib_imag[1], vib_freq[2], vib_imag[2], vib_freq[3], vib_imag[3], vib_freq[4], vib_imag[4], vib_freq[5], vib_imag[5]);
//			readLineF("%18s %r", discard, line);
//			if (stripChars(discard," ") == "REDUCEDMASS:") readVar(line, vib_rmass[1], vib_rmass[2], vib_rmass[3], vib_rmass[4], vib_rmass[5]);
//			else
//			{
//				readVar(line, vib_symm[1], vib_symm[2], vib_symm[3], vib_symm[4], vib_symm[5]);
//				readLine(discard, vib_rmass[1], vib_rmass[2], vib_rmass[3], vib_rmass[4], vib_rmass[5]);
//			}
//			readLine(discard, vib_ir[1], vib_ir[2], vib_ir[3], vib_ir[4], vib_ir[5]);
//			# Create vibrations
//			for (l=1; l<6; ++l)
//			{
//				if ((l+n-1) > 3*m.nAtoms()) break;
//				if (contains(vib_imag[l],"I")) vib_freq[l] = -vib_freq[l];
//				verbose("Created vibration number %i with frequency %f\n", l+n-1, vib_freq[l]);
//				vibs[l] = m.newVibration();
//				vibs[l].frequency = vib_freq[l];
//			}
//			skipLine(1);
//			# Next lines contain atomic displacements
//			for (count=1; count<=m.nAtoms(); ++count)
//			{
//				readLine(discard,e,type,vib_dx[1], vib_dx[2], vib_dx[3], vib_dx[4], vib_dx[5]);
//				readLine(type,vib_dy[1], vib_dy[2], vib_dy[3], vib_dy[4], vib_dy[5]);
//				readLine(type,vib_dz[1], vib_dz[2], vib_dz[3], vib_dz[4], vib_dz[5]);
//				for (l=1; l<6; ++l)
//				{
//					if ((l+n-1) > 3*m.nAtoms()) break;
//				#	vibs[l].displacements[count] = { vib_dx[l], vib_dy[l], vib_dz[l] };
//					vibs[l].displacements[count].x = vib_dx[l] ;
//					vibs[l].displacements[count].y = vib_dy[l] ;
//					vibs[l].displacements[count].z = vib_dz[l] ;
//				}
//			}
//			# Skip over Sayvetz information
//			skipLine(11);
//		}
//		printf("Read in data for %i vibrations.\n", m.nVibrations());
//	}
//
//	# Atomic Charges
//	rewind();
//	if (find("ELECTROSTATIC POTENTIAL"))
//	{
//		# Skip to last section
//		do { }
//		while (find("ELECTROSTATIC POTENTIAL"));
//		# Now search for "NET CHARGES"
//		if (find("NET CHARGES"))
//		{
//			skipLine(3);
//			for (i = m.atoms; i; ++i) readLine(discard, i.q);
//			printf("Found and read in ESP charges.\n");
//		}
//	}
//
//	finaliseModel();
//}
//
//filter(type="importmodel", name="GAMESS-US Cartesian Input", nickname="gamusinp", extension="inp", glob="*.inp", zmap="name", id=5)
//{
//	# Variables
//	string symm,symbol,title;
//	int n,z;
//	double rx,ry,rz;
//
//	# Search for $DATA section
//	if (!find("DATA")) error("Couldn't find 'DATA' section in input file.");
//
//	# Read in title card
//	getLine(title);
//	newModel(title);
//
//	# Skip over symmetry specification
//	readLine(symm);
//	if (symm <> "C1")
//	{
//		# Single blank line means the master frame is in use, and only this line to skip
//		readLine(symm);
//		if (symm != "") skipLine();
//	}
//	
//	# Coordinates follow, terminated by $END
//	readLine(symbol,z,rx,ry,rz);
//	while (symbol != "$END")
//	{
//		newAtom(symbol,rx,ry,rz);
//		readLine(symbol,z,rx,ry,rz);
//	}
//
//	# Done
//	rebond();
//	finaliseModel();
//}
//
//filter(type="exportmodel", name="GAMESS-US Input", nickname="gamusinp", extension="inp", glob="*.inp", id=5)
//{
//	# GUI Control Definitions
//	# Main dialog creation function
//	void createDefaultDialog(Dialog ui)
//	{
//		ui.title = "GAMESS-US Export Options";
//		widget group, group2, w, w2, tabs, page;
//
//		# General control options group ($CONTRL)
//		# -- General method options
//		group = ui.addGroup("jobgroup", "Job Control ($CONTRL)", 1, 1);
//		group2 = group.addGroup("rungroup", "Method", 1, 1, 0, 1);
//		group2.addCombo("contrl_runtyp", "Run Type", "ENERGY,GRADIENT,HESSIAN,GAMMA,OPTIMIZE,TRUDGE,SADPOINT,MEX,IRC,VSCF,DRC,MD,GLOBOP,OPTFMO,GRADEXTR,SURFACE,G3MP2,PROP,RAMAN,NACME,NMR,EDA,TRANSITN,FFIELD,TDHF,TDHFX,MAKEFP,FMO0", 5, 1, 1);
//		group2.addCombo("contrl_scftyp", "SCF Type", "RHF,UHF,ROHF,GVB,MCSCF,NONE", 1, 1, 2);
//		group2.addIntegerSpin("contrl_mult", "Multiplicity", 1, 8, 1, 1, 1, 3);
//		group2.addIntegerSpin("contrl_charge", "Charge", -1000, 1000, 1, 0, 1, 4);
//		group2.addCombo("contrl_exetyp", "Exe Type", "RUN,CHECK,DEBUG", 1, 3, 1);
//		group2.addCombo("contrl_relwfn", "Rel. Wfn.", "NONE,DK,RESC,NESC", 1, 3, 2);
//		group2.addIntegerSpin("contrl_maxit", "Max SCF It.", 1, 1000, 10, 30, 3, 3);
//		group2.addCheck("contrl_zmt", "ZMatrix", 0, 3, 4);
//		group2.addCheck("cart_symbols", "Use Symbols", 1, 4, 4);
//		# -- DFT options
//		group2 = group.addGroup("dftgroup", "DFT", 2, 1, 0, 1);
//		string griddfttypes = "NONE,SLATER,BECKE,GILL,OPTX,PW91X,PBEX,VWN,VWN1,PZ81,P86,LYP,PW91C,PBEc,OP,SVWN,BLYP,BOP,BP86,GVWN,GPW91,PBEVWN,PBEOP,OLYP,EDF1,PW91,PBE,revPBE,RPBE,PBEsol,HCTH93,HCTH120,HCTH147,HCTH407,SOGGA,MOHLYP,B97-D,BHHLYP,B3PW91,B3LYP,B3LYP1,B97,B97-1,B97-2,B97-3,B97-K,B98,PBE0,X3LYP,CAMB3LYP,wB97,wB97X,wB97X-D,B2PLYP,wB97X-2,wB97X-2L,VS98,PKZB,tHCTH,tHCTHhyb,BMK,TPSS,TPSSh,TPSSm,revTPSS,M05,M05-2X,M06,M06-L,M06-2X,M06-HF,M08-HX,M08-SO";
//		string gridfreedfttypes = "NONE,XALPHA,SLATER,BECKE,DEPRISTO,CAMA,HALF,VWN,PWLOC,LYP,BVWN,BLYP,BPWLOC,B3LYP,CAMB,XVWN,XPWLOC,SVWN,SPWLOC,WIGNER,WS,WIGEXP";
//		group2.addCombo("contrl_dfttyp", "Type", griddfttypes, 1, 1, 1);
//		w = group2.addCombo("dftgrid", "Method", "GRID,GRIDFREE", 1, 1, 2);
//		w.onInteger(1, 1, "sendstring", "contrl_dfttyp", "items", griddfttypes);
//		w.onInteger(2, 2, "sendstring", "contrl_dfttyp", "items", gridfreedfttypes);
//		w.onInteger(1, 2, "sendinteger", "contrl_dfttyp", "value", 2);
//		group2.addCombo("contrl_tddft", "TDDFT Type", "NONE,EXCITE", 1, 1, 3);
//		group2.addSpacer(TRUE,TRUE,1,4,1);
//		# -- Post-HF Methods
//		group2 = group.addGroup("postgroup", "Post-HF", 3, 1, 0, 0);
//		group2.addCombo("contrl_mplevl", "MP Level", "0,2", 1, 5, 2);
//		group2.addCombo("contrl_cityp", "CI Type", "NONE,CIS,ALDET,ORMAS,FSOCI,GENCI,GUGA", 1, 7, 2);
//		group2.addCombo("contrl_cctyp", "CC Type", "NONE,LCCD,CCD,CCSD,CCSD(T),R-CC,CR-CC,CR-CCL,CCSD(TQ),CR-CC(Q),EOM-CCSD,CR-EOM,CR-EOML,IP-EOM2,EA-EOM2,EA-EOM3A", 1, 7, 3);
//		# -- Other options
//		group2 = group.addGroup("miscgroup", "Misc", 3, 2, 0, 0);
//		group2.addCombo("contrl_pp", "Pseudo", "NONE,READ,SBKJC,HW,MCP", 1, 1, 1);
//		group2.addCheck("contrl_isphere", "ISPHERE", 0, 3, 1);
//
//		# Basis set specification ($BASIS)
//		group = ui.addGroup("basisgroup", "Basis Set ($BASIS)", 1, 3);
//		w = group.addCombo("basis_gbasis", "Basis", "MINI,MIDI,STO,N21,N31,N311,DZV,DH,TZV,MC,CCD,CCT,CCQ,CC5,CC6,ACCD,ACCT,ACCQ,ACC5,ACC6,CCDC,CCTC,CCQC,CC5C,CC6C,ACCDC,ACCTC,ACCQC,ACC5C,ACC6C,PC0,PC1,PC2,PC3,PC4,APC0,APC1,APC2,APC3,APC4,SBKJC,HW,MCP-DZP,MCP-TZP,MCP-QZP,IMCP-SR1,IMCP-SR2,IMCP-NR1,IMCP-NR2,MNDO,AM1,PM3", 5, 1, 1);
//
//		group2 = group.addGroup("suppbasisgroup", "Segmented Basis Options", 1, 2);
//		group2.addIntegerSpin("basis_ngauss", "NGAUSS", 3, 6, 1, 6, 1, 1);
//		group2.addIntegerSpin("basis_ndfunc", "NDFUNC", 0, 3, 1, 0, 3, 1);
//		group2.addIntegerSpin("basis_npfunc", "NPFUNC", 0, 3, 1, 0, 5, 1);
//		group2.addIntegerSpin("basis_nffunc", "NFFUNC", 0, 1, 1, 0, 7, 1);
//		group2.addCheck("basis_diffsp", "DIFFSP", 0, 1, 2);
//		group2.addCheck("basis_diffs", "DIFFS", 0, 3, 2);
//		w.onInteger(1, 10, "sendbool", "suppbasisgroup", "enabled");
//		w.onInteger(3, 6, "sendbool", "basis_ngauss", "enabled");
//		w.onInteger(1, 10, "sendinteger", "contrl_isphere", "value", 0);
//		w.onInteger(11, 40, "sendinteger", "contrl_isphere", "value", 1);
//		w.onInteger(41, 42, "sendinteger", "contrl_isphere", "value", 0);
//		w.onInteger(43, 45, "sendinteger", "contrl_isphere", "value", 1);
//		w.onInteger(46, 52, "sendinteger", "contrl_isphere", "value", 0);
//
//		# Tabs containing various options
//		tabs = ui.addTabs("optiontabs", 1, 4);
//		
//		# System options ($SYSTEM)
//		page = tabs.addPage("systempage", "$SYSTEM");
//		page.addIntegerSpin("system_mwords", "MWORDS", 1, 100000, 10, 100);
//		page.addIntegerSpin("system_memddi", "MEMDDI", 0, 100000, 10, 100);
//		page.addIntegerSpin("system_timlim", "TIMLIM", 1, 100000, 10, 2880);
//
//		# Stationary point location options ($STATPT)
//		page = tabs.addPage("page_statpt", "$STATPT");
//		page.addCombo("statpt_method", "METHOD", "NR,RFO,QA,SCHLEGEL,CONOPT", 3, 1, 1);
//		page.addDoubleSpin("statpt_opttol", "OPTTOL", 0.0, 1.0, 0.0001, 0.0001, 3, 1);
//		page.addIntegerSpin("statpt_nstep", "NSTEP", 1, 1000000, 5, 20, 5, 1);
//		page.addIntegerSpin("statpt_ifolow", "IFOLOW", 1, 100000, 1, 1, 7, 1); 
//		page.addCombo("statpt_hess", "HESS", "GUESS,READ,RDAB,RDALL,CALC", 1, 1, 2);
//		page.addIntegerSpin("statpt_ihrep", "IHREP", 0, 1000, 1, 0, 3, 2);
//		page.addDoubleSpin("statpt_ststep", "STSTEP", 0.0, 1.0, 0.005, 0.01, 5, 2);
//		page.addCheck("statpt_hssend", "HSSEND", 0, 7, 2);
//		page.addCheck("statpt_stpt", "Stationary Point", 0, 8, 2);
//		page.addEdit("statpt_ifreez", "IFREEZ", "", 1, 3, 6);
//
//		# Intrinsic reaction coordinate run ($IRC)
//		page = tabs.addPage("page_irc", "$IRC");
//		page.addCheck("irc_forwrd", "FORWRD", 1, 1, 1);
//		page.addCheck("irc_saddle", "SADDLE", 1, 2, 1);
//		page.addDoubleSpin("irc_stride", "STRIDE", 0.001, 1.0, 0.05, 0.3, 3, 1);
//		page.addIntegerSpin("irc_npoint", "NPOINT", 1, 1000, 5, 1, 5, 1);
//		page.addCombo("irc_pace", "PACE", "GS2,LINEAR,QUAD,AMPC4,RK4", 1, 7, 1);
//
//		# Electron density calculation ($ELDENS)
//		page = tabs.addPage("page_eldens", "$ELDENS");
//		page.addCheck("eldens_ieden", "Calculate?", 0, 1, 1);
//		page.addIntegerSpin("eldens_morb", "MORB", 0, 10000, 1, 0, 3, 1);
//		page.addCombo("eldens_where", "WHERE", "COMASS,NUCLEI,POINTS,GRID", 2, 5, 1);
//		page.addCombo("eldens_output", "OUTPUT", "PUNCH,PAPER,BOTH", 3, 7, 1);
//
//		# Electrostatic potential calculation ($ELPOT)
//		page = tabs.addPage("page_elpot", "$ELPOT");
//		page.addCheck("elpot_iepot", "Calculate?", 0, 1, 1);
//		page.addCombo("elpot_where", "WHERE", "COMASS,NUCLEI,POINTS,GRID,PDC", 2, 3, 1);
//		page.addCombo("elpot_output", "OUTPUT", "PUNCH,PAPER,BOTH,NONE", 3, 5, 1);
//
//		# Point selection ($PDC)
//		page = tabs.addPage("page_pdc", "$PDC");
//		page.addCombo("pdc_ptsel", "PTSEL", "GEODESIC,CONNOLLY,CHELPG", 1, 1, 1);
//		page.addCombo("pdc_constr", "CONSTR", "NONE,CHARGE,DIPOLE,QUPOLE", 2, 3, 1);
//
//		# Grid specification ($GRID), placed in 'opts' tabs
//		page = tabs.addPage("page_grid", "$GRID");
//		w = page.addCheck("grid_modgrd", "3D Grid", 0, 1, 1);
//		page.addDoubleSpin("grid_size", "Grid Increment", 0.00001, 2.0, 0.1, 0.25, 2, 1);
//		group = page.addGroup("ogroup", "ORIGIN", 1, 2);
//		group.addDoubleSpin("grid_originx", "X", -1000.0, 1000.0, 1.0, -10.0, 1, 3);
//		group.addDoubleSpin("grid_originy", "Y", -1000.0, 1000.0, 1.0, -10.0, 1, 4);
//		group.addDoubleSpin("grid_originz", "Z", -1000.0, 1000.0, 1.0, -10.0, 1, 5); 
//		group = page.addGroup("xgroup", "XVEC", 3, 2);
//		group.addDoubleSpin("grid_xvecx", "X", -1000.0, 1000.0, 1.0,  10.0, 3, 3);
//		group.addDoubleSpin("grid_xvecy", "Y", -1000.0, 1000.0, 1.0, -10.0, 3, 4);
//		group.addDoubleSpin("grid_xvecz", "Z", -1000.0, 1000.0, 1.0, -10.0, 3, 5);
//		group = page.addGroup("ygroup", "YVEC", 5, 2);
//		group.addDoubleSpin("grid_yvecx", "X", -1000.0, 1000.0, 1.0, -10.0, 5, 3);
//		group.addDoubleSpin("grid_yvecy", "Y", -1000.0, 1000.0, 1.0,  10.0, 5, 4);
//		group.addDoubleSpin("grid_yvecz", "Z", -1000.0, 1000.0, 1.0, -10.0, 5, 5);
//		group = page.addGroup("zgroup", "ZVEC", 7, 2);
//		group.addDoubleSpin("grid_zvecx", "X", -1000.0, 1000.0, 1.0, -10.0, 7, 3);
//		group.addDoubleSpin("grid_zvecy", "Y", -1000.0, 1000.0, 1.0, -10.0, 7, 4);
//		group.addDoubleSpin("grid_zvecz", "Z", -1000.0, 1000.0, 1.0,  10.0, 7, 5);
//		group.enabled = FALSE;
//		w.onInteger(1, 2, "sendbool", "zgroup", "enabled");
//
//		# PCM options ($PCM), placed in 'opts' tab
//		page = tabs.addPage("page_pcm", "$PCM");
//		w = page.addCombo("pcm_solvnt", "Solvent", "NONE,INPUT,'WATER (H2O)',CH3OH,'CLFORM (CHCl3)','METHYCL (CH2Cl2)','BENZENE (C6H6)','CLBENZ (C6H5Cl)','NEPTANE (C7H16)','ANILINE (C6H5NH2)',C2H5OH,'CTCL (CCl4)','12DCLET (C2H4Cl2)','TOLUENE (C6H5CH3)','NITMET (CH3NO2)','CYCHEX (C6H12)','ACETONE (CH3COCH3)',THF,'DMSO (DMETSOX)'", 1, 1, 1);
//		group = page.addGroup("pcminputgroup", "Custom Solvent Definition", 1, 2, 5, 0);
//		group.enabled = FALSE;
//		ui.widget("pcm_solvnt").onInteger(2, 2, "sendbool", "pcminputgroup", "enabled");
//		group.addDoubleSpin("pcm_rsolv", "Radius", 0.0, 100.0, 0.1, 3.0);
//		group.addDoubleSpin("pcm_eps", "EPS", 0.0, 10000.0, 1.0, 20.0);
//		group.addDoubleSpin("pcm_epsinf", "EPSINF", 0.0, 10000.0, 1.0, 20.0);
//		group.addDoubleSpin("pcm_tce", "TCE", 0.0, 10000.0, 1.0, 20.0);
//		group.addDoubleSpin("pcm_vmol", "VMOL", 0.0, 10000.0, 1.0, 20.0);
//		group.addDoubleSpin("pcm_sten", "STEN", 0.0, 10000.0, 1.0, 20.0);
//		group.addDoubleSpin("pcm_dsten", "DSTEN", 0.0, 10000.0, 1.0, 20.0);
//		group.addDoubleSpin("pcm_cmf", "CMF", 0.0, 10000.0, 1.0, 20.0);
//		w.onInteger(2, 2, "sendbool", "pcminputgroup", "enabled");
//		w = page.addCheck("pcm_icav", "Calculate cavitation energy", 0, 3, 1);
//		w.onInteger(0, 1, "sendbool", "pcm_tce", "enabled");
//		w.onInteger(0, 1, "sendbool", "pcm_vmol", "enabled");
//		w.onInteger(0, 1, "sendbool", "pcm_sten", "enabled");
//		w.onInteger(0, 1, "sendbool", "pcm_dsten", "enabled");
//		w.onInteger(0, 1, "sendbool", "pcm_cmf", "enabled");
//	}
//	# Execute dialog
//	if (!showDefaultDialog()) error("Options dialog canceled.\n");
//	Dialog ui = defaultDialog();
//
//	#
//	# Write Data
//	#
//	string line;
//	# Write $CONTRL line(s)
//	writeLineF(" $CONTRL SCFTYP=%s RUNTYP=%s COORD=%s ICHARG=%i MULT=%i $END\n", ui.asString("contrl_scftyp"), ui.asString("contrl_runtyp"), ui.asInteger("contrl_zmt") ? "ZMAT" : "UNIQUE", ui.asInteger("contrl_charge"), ui.asInteger("contrl_mult"));
//	line = "";
//	if (ui.asString("contrl_dfttyp") != "NONE") line += " DFTTYP=" + ui.asString("contrl_dfttyp");
//	if (ui.asString("contrl_tddft") != "NONE") line += " TDDFT=" + ui.asString("contrl_tddft");
//	if (ui.asString("contrl_mplevl") != "0") line += " MPLEVL=" + ui.asString("contrl_mplevl");
//	if (ui.asString("contrl_cityp") != "NONE") line += " CITYP=" + ui.asString("contrl_cityp");
//	if (ui.asString("contrl_cctyp") != "NONE") line += " CCTYP=" + ui.asString("contrl_cctyp");
//	if (ui.asString("contrl_pp") != "NONE") line += " ECPTYP=" + ui.asString("contrl_pp");
//	if (ui.asInteger("contrl_maxit") != 30) line += " MAXIT=" + ui.asString("contrl_maxit");
//	if (ui.asInteger("contrl_isphere") == 1) line += " ISPHER=1";
//	if (line != "") writeLineF(" $CONTRL%s $END\n", line);
//
//	# Write $SYSTEM line
//	writeLineF(" $SYSTEM MEMDDI=%i TIMLIM=%i MWORDS=%i $END\n", ui.asInteger("system_memddi"), ui.asInteger("system_timlim"), ui.asInteger("system_mwords"));
//
//	# Write $BASIS line(s)
//	if (ui.isRange("basis_gbasis", 3, 6))
//	{
//		sprintf(line, "GBASIS=%s NGAUSS=%i", ui.asString("basis_gbasis"), ui.asInteger("basis_ngauss"));
//		if (ui.asInteger("basis_ndfunc") != 0) line += " NDFUNC=" + ui.asString("basis_ndfunc");
//		if (ui.asInteger("basis_npfunc") != 0) line += " NPFUNC=" + ui.asString("basis_npfunc");
//		if (ui.asInteger("basis_nffunc") != 0) line += " NFFUNC=" + ui.asString("basis_nffunc");
//		writeLineF(" $BASIS %s $END\n", line);
//		if (ui.isString("basis_diffsp", ".TRUE.") || ui.isString("basis_diffs", ".TRUE.")) writeLineF(" $BASIS DIFFSP=%s DIFFS=%s $END\n", ui.asString("basis_diffsp"), ui.asString("basis_diffs"));
//	}
//	else writeLineF(" $BASIS GBASIS=%s $END\n", ui.asString("basis_gbasis"));
//
//	# Write additional groups, depending on job type
//	if (ui.isString("contrl_runtyp", "OPTIMIZE") || ui.isString("contrl_runtyp", "SADPOINT"))
//	{
//		writeVarF(line, " $STATPT METHOD=%s NSTEP=%i OPTTOL=%f HESS=%s", ui.asString("statpt_method"), ui.asInteger("statpt_nstep"), ui.asDouble("statpt_opttol"), ui.asString("statpt_hess"));
//		if (ui.asInteger("statpt_ihrep") != 0) line += " IHREP=" + ui.asString("statpt_ihrep");
//		if (ui.isString("statpt_hssend", ".TRUE.")) line += " HSSEND=.TRUE.";
//		writeLineF("%s $END\n", line);
//		if (ui.isString("contrl_runtyp", "SADPOINT")) writeLineF(" $STATPT IFOLOW=%i STPT=%s STSTEP=%f $END\n", ui.asInteger("statpt_ifolow"), ui.asString("statpt_stpt"), ui.asDouble("statpt_ststep"));
//		if (!ui.isString("statpt_ifreez", "")) writeLineF(" $STATPT IFREEZ(1)=%s $END\n", ui.asString("statpt_ifreez"));
//	}
//	else if (ui.isString("contrl_runtyp", "IRC")) writeLineF(" $IRC PACE=%s FORWRD=%s SADDLE=%s STRIDE=%f NPOINT=%i $END\n", ui.asString("irc_pace"), ui.asString("irc_forwrd"), ui.asString("irc_saddle"), ui.asDouble("irc_stride"), ui.asInteger("irc_npoint"));
//
//	# Write $ELDENS if required
//	if (ui.asInteger("eldens_ieden")) writeLineF(" $ELDEN IEDEN=1 MORB=%i WHERE=%s OUTPUT=%s $END\n", ui.asInteger("eldens_morb"), ui.asString("eldens_where"), ui.asString("eldens_output"));
//
//	# Write $ELPOT if required
//	if (ui.asInteger("elpot_iepot"))
//	{
//		writeLineF(" $ELPOT IEPOT=1 WHERE=%s OUTPUT=%s $END\n", ui.asString("elpot_where"), ui.asString("elpot_output"));
//		if (ui.isString("elpot_where", "PDC")) writeLineF(" $PDC PTSEL=%s CONSTR=%s $END\n", ui.asString("pdc_ptsel"), ui.asString("pdc_constr"));
//	}
//
//	# Write $GRID if required
//	if (ui.isString("eldens_where", "GRID") || ui.isString("elpot_where", "GRID"))
//	{
//		writeLineF(" $GRID MODGRD=%i UNITS=ANGS SIZE=%f $END\n", ui.asInteger("grid_modgrd"), ui.asDouble("grid_size"));
//		writeLineF(" $GRID ORIGIN(1)=%f ORIGIN(2)=%f ORIGIN(3)=%f $END\n", ui.asDouble("grid_originx"), ui.asDouble("grid_originy"), ui.asDouble("grid_originz"));
//		writeLineF(" $GRID XVEC(1)=%f XVEC(2)=%f XVEC(3)=%f $END\n", ui.asDouble("grid_xvecx"), ui.asDouble("grid_xvecy"), ui.asDouble("grid_xvecz"));
//		writeLineF(" $GRID YVEC(1)=%f YVEC(2)=%f YVEC(3)=%f $END\n", ui.asDouble("grid_yvecx"), ui.asDouble("grid_yvecy"), ui.asDouble("grid_yvecz"));
//		if (ui.asInteger("grid_modgrd")) writeLineF(" $GRID ZVEC(1)=%f ZVEC(2)=%f ZVEC(3)=%f $END\n", ui.asDouble("grid_zvecx"), ui.asDouble("grid_zvecy"), ui.asDouble("grid_zvecz"));
//	}
//
//	# Write $PCM if required
//	if (ui.asString("pcm_solvnt") != "NONE")
//	{
//		writeVarF(line, " $PCM SOLVNT=%s", beforeStr(ui.asString("pcm_solvnt"), " "));
//		if (ui.asInteger("pcm_icav")) line += " ICAV=" + ui.asString("pcm_icav");
//		writeLineF("%s $END\n", line);
//		if (ui.isString("pcm_solvnt", "INPUT"))
//		{
//			writeLineF(" $PCM RSOLV=%f EPS=%f EPSINF=%f", ui.asDouble("pcm_rsolv"), ui.asDouble("pcm_eps"), ui.asDouble("pcm_epsinf"));
//			if (ui.asInteger("pcm_icav")) writeLineF(" TCE=%f VMOL=%f STEN=%f DSTEN=%f CMF=%f $END\n", ui.asDouble("pcm_tce"), ui.asDouble("pcm_vmol"), ui.asDouble("pcm_sten"), ui.asDouble("pcm_dsten"), ui.asDouble("pcm_cmf"));
//			else writeLineF(" $END\n");
//		}
//	}
//
//	# Write $GUESS, and $SCF groups
//	writeLine(" $GUESS GUESS=HUCKEL $END");
//	writeLine(" $SCF DIRSCF=.TRUE. $END");
//
//	# Now for the DATA section
//	writeLine(" $DATA");
//	writeLine(aten.frame.name);
//	writeLine("C1");
//	if (ui.asInteger("contrl_zmt") == 0)
//	{
//		if (ui.asInteger("cart_symbols")) for (atom i = aten.frame.atoms; i != 0; ++i) writeLineF("%-15s  %4.1f  %12.6f %12.6f %12.6f\n", i.symbol,i.z*1.0,i.rx,i.ry,i.rz);
//		else for (atom i = aten.frame.atoms; i != 0; ++i) writeLineF("%-15s  %4.1f  %12.6f %12.6f %12.6f\n", i.name,i.z*1.0,i.rx,i.ry,i.rz);
//	}
//	else writeZMatrix(aten.frame);
//	writeLine(" $END");
//}
	return true;
}

// Return whether this plugin can export data
bool GAMESSModelPlugin::canExport()
{
	return false;
}

// Export data to the spegamessied file
bool GAMESSModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool GAMESSModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool GAMESSModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool GAMESSModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool GAMESSModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool GAMESSModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool GAMESSModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

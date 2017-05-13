/*
        *** GAMESS-US Model Plugin Functions
        *** src/plugins/io_gamessus/gamess_funcs.cpp
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

#include "plugins/io_gamessus/gamessus.hui"
#include "plugins/io_gamessus/gamessexportoptions.h"
#include "model/model.h"

// Constructor
GAMESSUSModelPlugin::GAMESSUSModelPlugin()
{
	// Setup plugin options
	// -- General control options group ($CONTRL)
	pluginOptions_.add("contrl_runtyp", "OPTIMIZE");
	pluginOptions_.add("contrl_scftyp", "RHF");
	pluginOptions_.add("contrl_mult", 1);
	pluginOptions_.add("contrl_charge", 0);
	pluginOptions_.add("contrl_exetyp", "RUN");
	pluginOptions_.add("contrl_relwfn", "NONE");
	pluginOptions_.add("contrl_maxit", 30);
	pluginOptions_.add("contrl_zmt", 0);
	pluginOptions_.add("cart_symbols", ".TRUE.");
	// -- DFT options
	pluginOptions_.add("contrl_dfttyp", "NONE");
	pluginOptions_.add("dftgrid", "GRID");
	pluginOptions_.add("contrl_tddft", "NONE");
	// --Post-HF Methods
	pluginOptions_.add("contrl_mplevl", "0");
	pluginOptions_.add("contrl_cityp", "NONE");
	pluginOptions_.add("contrl_cctyp", "NONE");
	// --Other options
	pluginOptions_.add("contrl_pp", "NONE");
	pluginOptions_.add("contrl_isphere", 0);
	// -- Basis set specification ($BASIS)
	pluginOptions_.add("basis_gbasis", "N31");
	pluginOptions_.add("basis_ngauss", 6);
	pluginOptions_.add("basis_ndfunc", 0);
	pluginOptions_.add("basis_npfunc", 0);
	pluginOptions_.add("basis_nffunc", 0);
	pluginOptions_.add("basis_diffsp", 0);
	pluginOptions_.add("basis_diffs", 0);

	// -- System options ($SYSTEM)
	pluginOptions_.add("system_mwords", 100);
	pluginOptions_.add("system_memddi", 100);
	pluginOptions_.add("system_timlim", 2880);

	// -- Stationary point location options ($STATPT)
	pluginOptions_.add("statpt_method", "SCHLEGEL");
	pluginOptions_.add("statpt_opttol", 0.0001);
	pluginOptions_.add("statpt_nstep", 20);
	pluginOptions_.add("statpt_ifolow", 1);
	pluginOptions_.add("statpt_hess", "GUESS");
	pluginOptions_.add("statpt_ihrep", 0);
	pluginOptions_.add("statpt_ststep", 0.01);
	pluginOptions_.add("statpt_hssend", 0);
	pluginOptions_.add("statpt_stpt", "0");
	pluginOptions_.add("statpt_ifreez", "");

	// -- Intrinsic reaction coordinate run ($IRC)
	pluginOptions_.add("irc_forwrd", "1");
	pluginOptions_.add("irc_saddle", "1");
	pluginOptions_.add("irc_stride", "0.3");
	pluginOptions_.add("irc_npoint", "1");
	pluginOptions_.add("irc_pace", "GS2");

	// -- Electron density calculation ($ELDENS)
	pluginOptions_.add("eldens_ieden", 0);
	pluginOptions_.add("eldens_morb", 0);
	pluginOptions_.add("eldens_where", "NUCLEI");
	pluginOptions_.add("eldens_output", "BOTH");

	// -- Electrostatic potential calculation ($ELPOT)
	pluginOptions_.add("elpot_iepot", 0);
	pluginOptions_.add("elpot_where", "NUCLEI");
	pluginOptions_.add("elpot_output", "BOTH");

	// -- Point selection ($PDC)
	pluginOptions_.add("pdc_ptsel", "GEODESIC");
	pluginOptions_.add("pdc_constr", "DIPOLE");

	// -- Grid specification ($GRID), placed in 'opts' tabs
	pluginOptions_.add("grid_modgrd", 0);
	pluginOptions_.add("grid_size", 0.25);
	pluginOptions_.add("grid_originx", -10.0);
	pluginOptions_.add("grid_originy", -10.0);
	pluginOptions_.add("grid_originz", -10.0); 
	pluginOptions_.add("grid_xvecx", 10.0);
	pluginOptions_.add("grid_xvecy", -10.0);
	pluginOptions_.add("grid_xvecz", -10.0);
	pluginOptions_.add("grid_yvecx", -10.0);
	pluginOptions_.add("grid_yvecy", 10.0);
	pluginOptions_.add("grid_yvecz", -10.0);
	pluginOptions_.add("grid_zvecx", -10.0);
	pluginOptions_.add("grid_zvecy", -10.0);
	pluginOptions_.add("grid_zvecz", 10.0);

	// -- PCM options ($PCM), placed in 'opts' tab
	pluginOptions_.add("pcm_solvnt", "NONE");
	pluginOptions_.add("pcm_rsolv", 0.1);
	pluginOptions_.add("pcm_eps", 20.0);
	pluginOptions_.add("pcm_epsinf", 20.0);
	pluginOptions_.add("pcm_tce", 20.0);
	pluginOptions_.add("pcm_vmol", 20.0);
	pluginOptions_.add("pcm_sten", 20.0);
	pluginOptions_.add("pcm_dsten", 20.0);
	pluginOptions_.add("pcm_cmf", 20.0);
	pluginOptions_.add("pcm_icav", 0);
}

// Destructor
GAMESSUSModelPlugin::~GAMESSUSModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* GAMESSUSModelPlugin::makeCopy() const
{
	return new GAMESSUSModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType GAMESSUSModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int GAMESSUSModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString GAMESSUSModelPlugin::name() const
{
	return QString("GAMESS-US input file");
}

// Nickname of plugin
QString GAMESSUSModelPlugin::nickname() const
{
	return QString("gamus");
}

// Return whether the plugin is enabled
bool GAMESSUSModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString GAMESSUSModelPlugin::description() const
{
	return QString("Import/export for GAMESS-US input files");
}

// Related file extensions
QStringList GAMESSUSModelPlugin::extensions() const
{
	return QStringList() << "inp";
}

// Exact names
QStringList GAMESSUSModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool GAMESSUSModelPlugin::canImport() const
{
	return false;
}

// Import data from the specified file
bool GAMESSUSModelPlugin::importData()
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

	return false;
}

// Return whether this plugin can export data
bool GAMESSUSModelPlugin::canExport() const
{
	return true;
}

// Export data to the specified file
bool GAMESSUSModelPlugin::exportData()
{
	Model* m = targetModel();

// 		w = page.addCombo("pcm_solvnt", "Solvent", "NONE,INPUT,'WATER (H2O)',CH3OH,'CLFORM (CHCl3)','METHYCL (CH2Cl2)','BENZENE (C6H6)','CLBENZ (C6H5Cl)','NEPTANE (C7H16)','ANILINE (C6H5NH2)',C2H5OH,'CTCL (CCl4)','12DCLET (C2H4Cl2)','TOLUENE (C6H5CH3)','NITMET (CH3NO2)','CYCHEX (C6H12)','ACETONE (CH3COCH3)',THF,'DMSO (DMETSOX)'", 1, 1, 1);

	// Write $CONTRL line(s)
	QString scftyp = pluginOptions_.value("contrl_scftyp"), runtyp = pluginOptions_.value("contrl_runtyp"), zmt = pluginOptions_.value("contrl_zmt").toInt() ? "ZMAT" : "UNIQUE";
	int icharg = pluginOptions_.value("contrl_charge").toInt(), mult = pluginOptions_.value("contrl_mult").toInt();
	fileParser_.writeLineF(" $CONTRL SCFTYP=%s RUNTYP=%s COORD=%s ICHARG=%i MULT=%i $END", qPrintable(scftyp), qPrintable(runtyp), qPrintable(zmt), icharg, mult);

	QString line;
	if (pluginOptions_.value("contrl_dfttyp") != "NONE") line += " DFTTYP=" + pluginOptions_.value("contrl_dfttyp");
	if (pluginOptions_.value("contrl_tddft") != "NONE") line += " TDDFT=" + pluginOptions_.value("contrl_tddft");
	if (pluginOptions_.value("contrl_mplevl") != "0") line += " MPLEVL=" + pluginOptions_.value("contrl_mplevl");
	if (pluginOptions_.value("contrl_cityp") != "NONE") line += " CITYP=" + pluginOptions_.value("contrl_cityp");
	if (pluginOptions_.value("contrl_cctyp") != "NONE") line += " CCTYP=" + pluginOptions_.value("contrl_cctyp");
	if (pluginOptions_.value("contrl_pp") != "NONE") line += " ECPTYP=" + pluginOptions_.value("contrl_pp");
	if (pluginOptions_.value("contrl_maxit").toInt() != 30) line += " MAXIT=" + pluginOptions_.value("contrl_maxit");
	if (pluginOptions_.value("contrl_isphere").toInt() == 1) line += " ISPHER=1";
	if (!line.isEmpty()) fileParser_.writeLineF(" $CONTRL%s $END", qPrintable(line));

	// Write $SYSTEM line
	fileParser_.writeLineF(" $SYSTEM MEMDDI=%i TIMLIM=%i MWORDS=%i $END", pluginOptions_.value("system_memddi").toInt(), pluginOptions_.value("system_timlim").toInt(), pluginOptions_.value("system_mwords").toInt());

	// Write $BASIS line(s)
	QString gbasis = pluginOptions_.value("basis_gbasis");
	if ((gbasis == "STO") || (gbasis == "N21") || (gbasis == "N31") || (gbasis == "N311"))
	{
		line = QString(" $BASIS GBASIS=%1 NGAUSS=%2").arg(gbasis).arg(pluginOptions_.value("basis_ngauss").toInt());
		if (pluginOptions_.value("basis_ndfunc").toInt() != 0) line += " NDFUNC=" + pluginOptions_.value("basis_ndfunc").toInt();
		if (pluginOptions_.value("basis_npfunc").toInt() != 0) line += " NPFUNC=" + pluginOptions_.value("basis_npfunc").toInt();
		if (pluginOptions_.value("basis_nffunc").toInt() != 0) line += " NFFUNC=" + pluginOptions_.value("basis_nffunc").toInt();
		fileParser_.writeLineF("%s $END", qPrintable(line));

		line = QString();
		
		if (pluginOptions_.value("basis_diffsp").toInt()) line += " DIFFSP=.TRUE.";
		if (pluginOptions_.value("basis_diffs").toInt()) line += " DIFFS=.TRUE.";
		if (!line.isEmpty()) fileParser_.writeLine( QString(" $BASIS %1 $END").arg(line) );
	}
	else fileParser_.writeLine( QString(" $BASIS GBASIS=%1 $END").arg(pluginOptions_.value("basis_gbasis")) );

	// Write additional groups, depending on job type
	if ((pluginOptions_.value("contrl_runtyp") == "OPTIMIZE") || (pluginOptions_.value("contrl_runtyp") == "SADPOINT"))
	{
		line = QString(" $STATPT METHOD=%1 NSTEP=%2 OPTTOL=%3 HESS=%4"). arg(pluginOptions_.value("statpt_method")).arg(pluginOptions_.value("statpt_nstep").toInt()).arg( pluginOptions_.value("statpt_opttol").toDouble()).arg(pluginOptions_.value("statpt_hess"));
		if (pluginOptions_.value("statpt_ihrep").toInt() != 0) line += " IHREP=" + pluginOptions_.value("statpt_ihrep");
		if (pluginOptions_.value("statpt_hssend") == ".TRUE.") line += " HSSEND=.TRUE.";
		fileParser_.writeLineF("%s $END", qPrintable(line));
		if (pluginOptions_.value("contrl_runtyp") == "SADPOINT") fileParser_.writeLineF(" $STATPT IFOLOW=%i STPT=%s STSTEP=%f $END", pluginOptions_.value("statpt_ifolow").toInt(), qPrintable(pluginOptions_.value("statpt_stpt")), pluginOptions_.value("statpt_ststep").toDouble());
		if (pluginOptions_.value("statpt_ifreez") != "") fileParser_.writeLineF(" $STATPT IFREEZ(1)=%s $END", qPrintable(pluginOptions_.value("statpt_ifreez")));
	}
	else if (pluginOptions_.value("contrl_runtyp") == "IRC")
	{
		line = QString(" $IRC PACE=%s FORWRD=%s SADDLE=%s").arg(pluginOptions_.value("irc_pace"), pluginOptions_.value("irc_forwrd"), pluginOptions_.value("irc_saddle"));
		line += QString(" STRIDE=%f").arg(pluginOptions_.value("irc_stride").toDouble());
		fileParser_.writeLine(line);
		line += QString(" NPOINT=%i $END").arg(pluginOptions_.value("irc_npoint").toInt());
	}

	// Write $ELDENS if required
	if (pluginOptions_.value("eldens_ieden").toInt()) fileParser_.writeLineF(" $ELDEN IEDEN=1 MORB=%i WHERE=%s OUTPUT=%s $END", pluginOptions_.value("eldens_morb").toInt(), qPrintable(pluginOptions_.value("eldens_where")), qPrintable(pluginOptions_.value("eldens_output")));

	// Write $ELPOT if required
	if (pluginOptions_.value("elpot_iepot").toInt())
	{
		fileParser_.writeLineF(" $ELPOT IEPOT=1 WHERE=%s OUTPUT=%s $END", qPrintable(pluginOptions_.value("elpot_where")), qPrintable(pluginOptions_.value("elpot_output")));
		if (pluginOptions_.value("elpot_where") == "PDC") fileParser_.writeLineF(" $PDC PTSEL=%s CONSTR=%s $END", qPrintable(pluginOptions_.value("pdc_ptsel")), qPrintable(pluginOptions_.value("pdc_constr")));
	}

	// Write $GRID if required
	if ((pluginOptions_.value("eldens_where") == "GRID") || (pluginOptions_.value("elpot_where") == "GRID"))
	{
		fileParser_.writeLineF(" $GRID MODGRD=%i UNITS=ANGS SIZE=%f $END", pluginOptions_.value("grid_modgrd").toInt(), pluginOptions_.value("grid_size").toDouble());
		fileParser_.writeLineF(" $GRID ORIGIN(1)=%f ORIGIN(2)=%f ORIGIN(3)=%f $END", pluginOptions_.value("grid_originx").toDouble(), pluginOptions_.value("grid_originy").toDouble(), pluginOptions_.value("grid_originz").toDouble());
		fileParser_.writeLineF(" $GRID XVEC(1)=%f XVEC(2)=%f XVEC(3)=%f $END", pluginOptions_.value("grid_xvecx").toDouble(), pluginOptions_.value("grid_xvecy").toDouble(), pluginOptions_.value("grid_xvecz").toDouble());
		fileParser_.writeLineF(" $GRID YVEC(1)=%f YVEC(2)=%f YVEC(3)=%f $END", pluginOptions_.value("grid_yvecx").toDouble(), pluginOptions_.value("grid_yvecy").toDouble(), pluginOptions_.value("grid_yvecz").toDouble());
		if (pluginOptions_.value("grid_modgrd").toInt()) fileParser_.writeLineF(" $GRID ZVEC(1)=%f ZVEC(2)=%f ZVEC(3)=%f $END", pluginOptions_.value("grid_zvecx").toDouble(), pluginOptions_.value("grid_zvecy").toDouble(), pluginOptions_.value("grid_zvecz").toDouble());
	}

	// Write $PCM if required
	if (pluginOptions_.value("pcm_solvnt") != "NONE")
	{
		line = QString(" $PCM SOLVNT=%s").arg(pluginOptions_.value("pcm_solvnt").section(QChar(' '), 0));
		if (pluginOptions_.value("pcm_icav").toInt()) line += QString(" ICAV=%i").arg(pluginOptions_.value("pcm_icav").toInt());
		fileParser_.writeLineF("%s $END", qPrintable(line));
		if (pluginOptions_.value("pcm_solvnt") == "INPUT")
		{
			fileParser_.writeLineF(" $PCM RSOLV=%f EPS=%f EPSINF=%f", pluginOptions_.value("pcm_rsolv").toDouble(), pluginOptions_.value("pcm_eps").toDouble(), pluginOptions_.value("pcm_epsinf").toDouble());
			if (pluginOptions_.value("pcm_icav").toInt()) fileParser_.writeLineF(" TCE=%f VMOL=%f STEN=%f DSTEN=%f CMF=%f $END", pluginOptions_.value("pcm_tce").toDouble(), pluginOptions_.value("pcm_vmol").toDouble(), pluginOptions_.value("pcm_sten").toDouble(), pluginOptions_.value("pcm_dsten").toDouble(), pluginOptions_.value("pcm_cmf").toDouble());
			else fileParser_.writeLineF(" $END");
		}
	}

	// Write $GUESS, and $SCF groups
	fileParser_.writeLine(" $GUESS GUESS=HUCKEL $END");
	fileParser_.writeLine(" $SCF DIRSCF=.TRUE. $END");

	// Now for the DATA section
	fileParser_.writeLine(" $DATA");
	fileParser_.writeLine(m->name());
	fileParser_.writeLine("C1");
	if (pluginOptions_.value("contrl_zmt") == "0")
	{
		if (pluginOptions_.value("cart_symbols") == ".TRUE.") for (Atom* i = m->atoms(); i != NULL; i = i->next) fileParser_.writeLineF("%-15s  %4.1f  %12.6f %12.6f %12.6f", ElementMap::symbol(i->element()), i->element()*1.0, i->r().x, i->r().y, i->r().z);
		else for (Atom* i = m->atoms(); i != NULL; i = i->next) fileParser_.writeLineF("%-15s  %4.1f  %12.6f %12.6f %12.6f", ElementMap::name(i->element()), i->element()*1.0, i->r().x, i->r().y, i->r().z);
	}
	else
	{
		ZMatrix* zmat = m->zMatrix();
		zmat->print();
	}

	fileParser_.writeLine(" $END");

	return true;
}

// Import next partial data chunk
bool GAMESSUSModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool GAMESSUSModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool GAMESSUSModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool GAMESSUSModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool GAMESSUSModelPlugin::hasExportOptions() const
{
	return true;
}

// Show export options dialog
bool GAMESSUSModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	GAMESSUSExportOptionsDialog optionsDialog(targetOptions);
	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}
 

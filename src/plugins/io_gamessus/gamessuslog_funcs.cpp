/*
        *** GAMESS-US Log Plugin Functions
        *** src/plugins/io_gamessus/gamessuslog_funcs.cpp
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

#include "plugins/io_gamessus/gamessuslog.hui"
// #include "plugins/io_gamessus/gamessusexportoptions.h"
#include "model/model.h"
#include "base/eigenvector.h"

// Constructor
GAMESSUSLogModelPlugin::GAMESSUSLogModelPlugin()
{
	// Setup plugin options
// 	pluginOptions_.add("XXX", 0);
}

// Destructor
GAMESSUSLogModelPlugin::~GAMESSUSLogModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* GAMESSUSLogModelPlugin::makeCopy() const
{
	return new GAMESSUSLogModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType GAMESSUSLogModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int GAMESSUSLogModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString GAMESSUSLogModelPlugin::name() const
{
	return QString("GAMESS-US log (output) file");
}

// Nickname of plugin
QString GAMESSUSLogModelPlugin::nickname() const
{
	return QString("gamuslog");
}

// Return whether the plugin is enabled
bool GAMESSUSLogModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString GAMESSUSLogModelPlugin::description() const
{
	return QString("Import for GAMESS-US log (output) files");
}

// Related file extensions
QStringList GAMESSUSLogModelPlugin::extensions() const
{
	return QStringList() << "log";
}

// Exact names
QStringList GAMESSUSLogModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool GAMESSUSLogModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool GAMESSUSLogModelPlugin::importData()
{
	// Variable declaration
// 	int result,nstructures,natoms,n,l,lastshell,shell,count,orb_no[5];
// 	Atom* i;
// 	Vibration vibs[5];
// 	string line,e,name,discard,type, vib_imag[5], vib_symm[5];
// 	double rx, ry, rz, exponent, coeff;
// 	double vib_freq[5], vib_dx[5], vib_dy[5], vib_dz[5], vib_rmass[5], vib_ir[5];

	// Create a new model, and store its pointer for use later
	Model* targetModel = createModel(fileParser_.filename());

	// Find initial coordinates (in Bohr)
	if (fileParser_.find("ATOM      ATOMIC                      COORDINATES (BOHR)"))
	{
		fileParser_.skipLines();
		while (!fileParser_.eofOrBlank())   //e,discard,rx,ry,rz) != 0) newAtom(e,rx,ry,rz);
		{
			// Line with no arguments signals end of coordinates
			if (!fileParser_.parseLine()) return false;
			if (fileParser_.nArgs() == 0) break;

			createAtom(targetModel, fileParser_.argc(0), fileParser_.arg3d(2));
		};
		targetModel->bohrToAngstrom();
	}
	else
	{
		Messenger::error("No coordinates in file?");
		return false;
	}

	// Read in basis set information
	if (fileParser_.find("ATOMIC BASIS SET"))
	{
		fileParser_.skipLines(6);

		// Loop over basis shell definitions, stopping when we find the text 'TOTAL'
		if (!fileParser_.parseLine(Parser::SkipBlanks)) return false;
// 		readLineF("%s %s %i %f %r", e, name, n, exponent, line);

		// Messenger::print("%s %s %i %f [%s]", e, name, n, exponent, line);
		int lastshell = -1, count = 0;
		BasisShell* bas = NULL;
		while (fileParser_.argc(0) != "TOTAL")
		{
			// Start of a new atom?
			if (fileParser_.nArgs() == 1)
			{
				++count;
				lastshell = 0;
				Messenger::print("Found start of atom %i, which is element %s", count, qPrintable(fileParser_.argc(0)));
			}
			else
			{
				// Create new basis shell?
				// if (lastshell != shell) Messenger::print("Lastn (%i) is != shell (%i), so creating a new shell...", lastshell, shell);
				if (lastshell != fileParser_.argi(0))
				{
					BasisShell::BasisShellType bft = BasisShell::basisShellType(fileParser_.argc(1), true);
					if (bft == BasisShell::nBasisShellTypes) return false;
					bas = targetModel->addBasisShell();
					bas->setAtomId(count);
					bas->setType(bft);
				}
				lastshell = fileParser_.argi(0);
			}

			// Store primitive's exponent and coefficient(s)
			if (lastshell != 0)
			{
				BasisPrimitive* prim = bas->addPrimitive();
				prim->setExponent(fileParser_.argd(3));

				// Cycle over provided coefficients
				for (int n = 4; n <fileParser_.nArgs(); ++n) prim->addCoefficient(fileParser_.argd(n));
			}

			// Get next line, ignoring blanks
			if (fileParser_.eofOrBlank()) return false;
			if (!fileParser_.parseLine(Parser::SkipBlanks)) return false;
			// Messenger::print("%s %s %i %f [%s]", e, name, n, exponent, line);
		}

		// We end up with the 'TOTAL NUMBER OF BASIS SET SHELLS' line (beginning at 'SET...'), so we can do a check
		Messenger::print("         Number of basis set shells read in : %i", targetModel->nBasisShells());
		Messenger::print("            --> number specified in logfile : %i", fileParser_.argi(7));
		if (fileParser_.argi(7) != targetModel->nBasisShells())
		{
			Messenger::error("Failed to read in basis set information.");
			return false;
		}

		// Also, check the total number of cartesian basis functions
		if (!fileParser_.parseLine(Parser::SkipBlanks)) return false;
		Messenger::print("Number of implied cartesian basis functions : %i", targetModel->nCartesianBasisFunctions());
		Messenger::print("            --> number specified in logfile : %i", fileParser_.argi(7));
		if (fileParser_.argi(7) != targetModel->nCartesianBasisFunctions())
		{
			Messenger::error("Failed to read in basis set information.");
			return false;
		}
	}

	// Determine number of atoms - search for line containing 'TOTAL NUMBER OF ATOMS'
	if (!fileParser_.find("TOTAL NUMBER OF ATOMS"))
	{
		Messenger::error("Couldn't determine number of atoms from GAMESS-US output.");
		return false;
	}

	// Get number of atoms from line
	int nAtoms = fileParser_.argi(5);

	// Now search for sets of coordinate
	int nStructures = 0;

	// Lines containing 'COORDINATES OF ALL ATOMS ARE' are the beginning of coordinate sections
	while (fileParser_.find("COORDINATES OF ALL ATOMS ARE"))
	{
		// Found a set of coordinates. Skip 2 lines and then read coordinates
		++nStructures;
		fileParser_.skipLines(2);
		Model* frame = createFrame();
		frame->setName(QString("Frame %1").arg(nStructures));
		for (int n=0; n<nAtoms; ++n)
		{
			if (!fileParser_.parseLine()); 		//readLine(e,discard,rx,ry,rz);
			createAtom(frame, fileParser_.argc(0), fileParser_.arg3d(2));
		}
	}

	// Molecular orbitals?
	fileParser_.rewind();
	if (fileParser_.find("MOLECULAR ORBITALS"))
	{
		fileParser_.skipLines(2);
		// MO information is provided 5 orbs per line, in the format:
		//                     1          2          3          4
		//                   -0.5942     0.2657     0.9175     1.5319
		//                     A          A          A          A
		//    1  H  1  S   -0.290651  -0.111641  -0.783160   1.077794
		//    .  .  .  .
		//    NC .  .  .   
		// ...where NC is the number of cartesian basis functions.

		Eigenvector* orbs[5];
		double orb_nrg[5], orb_coeff[5];
		QString orb_symm[5];

		for (int n=1; n<targetModel->nBasisShells(); n +=5)
		{
			// Use orbital numbers as a sanity check
			if (!fileParser_.parseLine()) return false;
			if (fileParser_.nArgs() == 0) break;
			else if (n != fileParser_.argi(0))
			{
				Messenger::print("Failed to read in MO information at orbital number %i", n);
				break;
			}

			// Read in eigenvalues and symmetry type
			if (!fileParser_.parseLine()) return false;
			for (int m = 0; (m < 5) && (m < fileParser_.nArgs()); ++m) orb_nrg[m] = fileParser_.argd(m);
			if (!fileParser_.parseLine()) return false;
			for (int m = 0; (m < 5) && (m < fileParser_.nArgs()); ++m) orb_symm[m] = fileParser_.argc(m);

			// Create eigenvectors
			for (int l=0; l<5; ++l)
			{
				if ((l+n) > targetModel->nBasisShells()) break;
				Messenger::print(Messenger::Verbose, " -- Created MO number %i with eigenvalue %f", l+n, orb_nrg[l]);
				orbs[l] = targetModel->addEigenvector();
				orbs[l]->initialise(targetModel->nCartesianBasisFunctions());
				orbs[l]->setEigenvalue(orb_nrg[l]);
				orbs[l]->setName(orb_symm[l]);
			}

			// Next lines contain coefficients...
			for (int count=0; count<targetModel->nCartesianBasisFunctions(); ++count)
			{
				if (!fileParser_.parseLine()) return false;
// 				readLine(discard,e,shell,type,orb_coeff[1], orb_coeff[2], orb_coeff[3], orb_coeff[4], orb_coeff[5]);
				for (int l=0; l<5; ++l)
				{
					if ((l+n) > targetModel->nBasisShells()) break;
					orbs[l]->setValue(count, orb_coeff[l]);
				}
			}
		}
	}

	// Frequencies?
	fileParser_.rewind();
	if (fileParser_.find("FREQUENCIES IN CM**-1"))
	{
		// Skip warning of non-stationary point if present...
		if (fileParser_.find("THIS IS NOT A STATIONARY POINT"))
		{
			fileParser_.skipLines(3);
			Messenger::print(" *** The frequencies correspond to a non-stationary point.");
		}
		else fileParser_.skipLines(2);

		// Vibration information is provided 5 vibrations per line, in the format:
		//                       1          2          3          4            5
		//    FREQUENCY:      39.84 I     19.94       19.03       15.12       11.87
		//     SYMMETRY:         A          A           A           A           A       (optional)
		// REDUCED MASS:      2.15985     3.96061     4.04986     3.97821     5.98898
		// IR INTENSITY:      0.00121     0.00399     0.00426     0.00020     0.00070
		//  <blank line>
		//    1  HYDROGEN  X  0.05470495  0.01254668 -0.05069468  0.01778118 -0.01077563
		//    .  .  .  .
		//    3N .  .  .   
		// ...where N is the number of atoms in the model
// 		for (int n=1; n<3*nAtoms; n +=5)
// 		{
// 			// Read in vibration numbers as a sanity check
// 			if (!fileParser_.parseLine()) return false;
// 			if (n != fileParser_.argi(0))
// 			{
// 				Messenger::print("Failed to read in vibration information at vibration number %i (read [%s] from file)", n, fileParser_.argi(0));
// 				break;
// 			}
// 
// 			// Read in frequencies and imaginary flags
// 			readLineF("%* %f%2s%f%2s%f%2s%f%2s%f%2s", vib_freq[1], vib_imag[1], vib_freq[2], vib_imag[2], vib_freq[3], vib_imag[3], vib_freq[4], vib_imag[4], vib_freq[5], vib_imag[5]);
// 			readLineF("%18s %r", discard, line);
// 			if (stripChars(discard," ") == "REDUCEDMASS:") readVar(line, vib_rmass[1], vib_rmass[2], vib_rmass[3], vib_rmass[4], vib_rmass[5]);
// 			else
// 			{
// 				readVar(line, vib_symm[1], vib_symm[2], vib_symm[3], vib_symm[4], vib_symm[5]);
// 				readLine(discard, vib_rmass[1], vib_rmass[2], vib_rmass[3], vib_rmass[4], vib_rmass[5]);
// 			}
// 			readLine(discard, vib_ir[1], vib_ir[2], vib_ir[3], vib_ir[4], vib_ir[5]);
// 
// 			// Create vibrations
// 			for (l=1; l<6; ++l)
// 			{
// 				if ((l+n-1) > 3*m.nAtoms()) break;
// 				if (contains(vib_imag[l],"I")) vib_freq[l] = -vib_freq[l];
// 				verbose("Created vibration number %i with frequency %f", l+n-1, vib_freq[l]);
// 				vibs[l] = m.newVibration();
// 				vibs[l].frequency = vib_freq[l];
// 			}
// 			skipLine(1);
// 
// 			// Next lines contain atomic displacements
// 			for (count=1; count<=m.nAtoms(); ++count)
// 			{
// 				readLine(discard,e,type,vib_dx[1], vib_dx[2], vib_dx[3], vib_dx[4], vib_dx[5]);
// 				readLine(type,vib_dy[1], vib_dy[2], vib_dy[3], vib_dy[4], vib_dy[5]);
// 				readLine(type,vib_dz[1], vib_dz[2], vib_dz[3], vib_dz[4], vib_dz[5]);
// 				for (l=1; l<6; ++l)
// 				{
// 					if ((l+n-1) > 3*m.nAtoms()) break;
// 					vibs[l].displacements[count].x = vib_dx[l] ;
// 					vibs[l].displacements[count].y = vib_dy[l] ;
// 					vibs[l].displacements[count].z = vib_dz[l] ;
// 				}
// 			}
// 			// Skip over Sayvetz information
// 			fileParser_.skipLine(11);
// 		}
// 		Messenger::print("Read in data for %i vibrations.", m.nVibrations());
	}

	// Atomic Charges
	fileParser_.rewind();
	if (fileParser_.find("ELECTROSTATIC POTENTIAL"))
	{
		// Skip to last section
		do { }
		while (fileParser_.find("ELECTROSTATIC POTENTIAL"));

		// Now search for "NET CHARGES"
		if (fileParser_.find("NET CHARGES"))
		{
			fileParser_.skipLines(3);
			Atom* i = targetModel->atoms();
			for (int n = 0; n < nAtoms; ++n, i = i->next)
			{
				if (!fileParser_.parseLine()) return false;
				i->setCharge(fileParser_.argd(1));
			}
			Messenger::print("Found and read in ESP charges.");
		}
	}

	return true;
}

// Return whether this plugin can export data
bool GAMESSUSLogModelPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
bool GAMESSUSLogModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool GAMESSUSLogModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool GAMESSUSLogModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool GAMESSUSLogModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool GAMESSUSLogModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool GAMESSUSLogModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool GAMESSUSLogModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}
 

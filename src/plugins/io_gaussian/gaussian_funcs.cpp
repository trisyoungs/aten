/*
        *** Gaussian Model Plugin Functions
        *** src/plugins/io_gaussian/gaussian_funcs.cpp
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

#include "plugins/io_gaussian/gaussian.hui"
#include "model/model.h"

// Constructor
GaussianModelPlugin::GaussianModelPlugin()
{
}

// Destructor
GaussianModelPlugin::~GaussianModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* GaussianModelPlugin::makeCopy()
{
	return new GaussianModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory GaussianModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString GaussianModelPlugin::name() const
{
	return QString("Gaussian (dlputils) 3D probability density");
}

// Nickname of plugin
QString GaussianModelPlugin::nickname() const
{
	return QString("gaussian");
}

// Description (long name) of plugin
QString GaussianModelPlugin::description() const
{
	return QString("Import/export for dlputils Gaussian files");
}

// Related file extensions
QStringList GaussianModelPlugin::extensions() const
{
	return QStringList() << "gaussian";
}

// Exact names
QStringList GaussianModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool GaussianModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool GaussianModelPlugin::importData()
{
//filter(type="exportmodel", name="Gaussian Input", nickname="gaussinp", extension="gjf", glob="*.gjf", id=17)
//{
//	# Main dialog creation function
//	void createDefaultDialog(Dialog ui)
//	{
//		ui.title = "Gaussian09 Export Options";
//		widget w, w2, stack, page, tab, tabs = ui.addTabs("tabs");
//		# Job type
//		tab = tabs.addPage("jobpage", "Job Type");
//		w = tab.addCombo("runtype", "Run Type", "SP,Opt,Freq,IRC,IRCMax,Scan,Polar,ADMP,BOMD,Force,Stable,Volume", 2);
//		tab.addIntegerSpin("mult", "Multiplicity", 1, 8, 1, 1);
//		tab.addIntegerSpin("charge", "Charge", -100, 100, 1, 0);
//		stack = tab.addStack("runstack", 1, 2, 5, 0);
//		w.onInteger(1, 12, "sendinteger", "runstack", "value");
//		# -- SP
//		page = stack.addPage("stack_sp", "");
//		page.addLabel("","No Options");
//		# -- Opt
//		page = stack.addPage("stack_opt", "");
//		page.addCombo("opt_method", "Optimisation Type", "<default>,QST2,QST3,TS,Saddle=1,Saddle=2,Conical", 1);
//		page.addIntegerSpin("opt_maxcycles", "MaxCycles", 0, 1000, 10, 30, 1, 2);
//		page.addIntegerSpin("opt_maxstep", "MaxStep", 0, 1000, 10, 30, 3, 2);
//		page.addCheck("opt_restart", "Restart", 0, 5, 2);
//		# -- Freq
//		page = stack.addPage("stack_freq", "");
//		page.addLabel("","No Options (yet)");
//		# -- IRC
//		page = stack.addPage("stack_irc", "");
//		page.addLabel("","No Options (yet)");
//		# -- IRCMax
//		page = stack.addPage("stack_ircmax", "");
//		page.addLabel("","No Options (yet)");
//		# -- Scan
//		page = stack.addPage("stack_scan", "");
//		page.addLabel("","No Options (yet)");
//		# -- Polar
//		page = stack.addPage("stack_polar", "");
//		page.addLabel("","No Options (yet)");
//		# -- ADMP
//		page = stack.addPage("stack_admp", "");
//		page.addLabel("","No Options (yet)");
//		# -- BOMD
//		page = stack.addPage("stack_bomd", "");
//		page.addLabel("","No Options (yet)");
//		# -- Force
//		page = stack.addPage("stack_force", "");
//		page.addLabel("","No Options (yet)");
//		# -- Stable
//		page = stack.addPage("stack_stable", "");
//		page.addLabel("","No Options (yet)");
//		# -- Volume
//		page = stack.addPage("stack_volume", "");
//		page.addLabel("","No Options (yet)");
//
//		# Model Chemistry
//		tab = tabs.addPage("modchempage", "Model Chemistry");
//		tab.addCombo("modchem", "Method", "AM1,PM3,PM3MM,PM6,PDDG,HF,CASSCF,MP2,MP3,MP4(DQ),MP4(SDQ),MP4(SDTQ),MP5,QCISD,CCD,CCSD,QCISD(T),QCISD(TQ),BD,EPT,CBS,G1,G2,G2MP2,G3,G3MP2,G3B3,G3MP2B3,G4,G4MP2,W1,CIS,TD,EOM,ZINDO,DFTB,CI,GVB", 6, 1, 1, 2);
//		tab.addCombo("modchem_opt", "", "<default>,R,U,RO", 1);
//	
//		# Job Options
//		tab = tabs.addPage("joboptspage", "Job Options");
//		tab.addCombo("symmetry", "Symmetry", "Symmetry,NoSymmetry", 1);
//		tab.addCheck("symmetry_loose", "Loose", 0);
//		tab.addCombo("coords", "Coordinates", "Cartesian, ZMatrix", 1);
//	
//		# Basis Set
//		tab = tabs.addPage("basispage", "Basis Set");
//		w = tab.addCombo("basis", "Basic Basis", "STO-3,3-21,6-21,4-31,6-31,6-311,D95V,D95,SHC,SBK,LanL2MB,LanL2DZ,SDD,SDDAll,Dunning,Alhrichs,MidiX,EPR,UGBS,MTSmall,DGauss,CBSB7", 5);
//		tab.addCombo("subbasis", "Sub Basis", "<none>", 1);
//		w2 = tab.addGroup("elements", "Elements", 1, 2, 3);
//		w2.addLabel("elementlabel", "POOOP");
//		w2 = tab.addGroup("extrabasis", "Additional Basis Functions", 1, 3, 3);
//		w2.addCombo("diffuse", "Diffuse", "<none>", 1, 1, 1);
//		w2.addCombo("polar", "Polarisation (Heavy Atom, General)", "<none>", 1, 1, 2);
//		w2.addCombo("lightpolar", "Polarisation (Light Atom)", "<none>", 1, 1, 3);
//		# -- Link events to show valid options for extra basis functions, and display element text
//		w.onInteger(1, 22, "sendstring", "elementlabel", "text",
//			"???",						# -- STO-3G
//			"H-Xe",						# -- 3-21G
//			"H-Cl",						# -- 6-21G
//			"H-Ne",						# -- 4-31G
//			"H-Kr",						# -- 6-31G
//			"H-Kr",						# -- 6-311G
//			"H-Ne",						# -- D95V
//			"H-Cl, except Na and Mg",			# -- D95
//			"H-Cl",						# -- SHC
//			"H-Rn",						# -- Stevens/Basch/Krauss
//			"H-La, Hf-Bi",					# -- LanL2MB
//			"H, Li-La, Hf-Bi",				# -- LanL2DZ
//			"All, except Fr and Ra",			# -- SDD
//			"All, except Fr and Ra",			# -- SDDAll
//			"H-Ar, Ca-Kr",					# -- Dunning
//			"All, except Fr and Ra",			# -- Alhrichs
//			"H, C-F, S-Cl, I, Br",				# -- MidiX
//			"H, C-F, S-Cl, I, Br",				# -- EPR
//			"H-Lr",						# -- UGBS
//			"H-Ar",						# -- MTSmall
//			"See Basis Options",				# -- DGauss
//			"H-Kr");					# -- CBSB7
//		w.onInteger(1, 22, "sendstring", "subbasis", "items",
//			"<none>",					# -- STO-3G
//			"<none>",					# -- 3-21G
//			"<none>",					# -- 6-21G
//			"<none>",					# -- 4-31G
//			"<none>",					# -- 6-31G
//			"<none>",					# -- 6-311G
//			"<none>",					# -- D95V
//			"<none>",					# -- D95
//			"<none>",					# -- SHC
//			"CEP-4,CEP-31,CEP-121",				# -- Stevens/Basch/Krauss
//			"<none>",					# -- LanL2MB
//			"<none>",					# -- LanL2DZ
//			"<none>",					# -- SDD
//			"<none>",					# -- SDDAll
//			"cc-pVDZ,cc-pVTZ,cc-pVQZ,cc-pV5Z,cc-pV6Z",	# -- Dunning
//			"SV,SVP,TZV,TZVP,QZVP",				# -- Alhrichs
//			"<none>",					# -- MidiX
//			"EPR-II,EPR-III",				# -- EPR
//			"<none>",					# -- UGBS
//			"<none>",					# -- MTSmall
//			"'DGDZVP (H-Xe)','DGDZVP2 (H-F,Al-Ar,Sc-Zn)','DGTZVP (H,C-F,Al-Ar)'", # -- DGauss
//			"<none>");					# -- CBSB7
//		w.onInteger(1, 22, "sendstring", "diffuse", "items",
//			"<none>",					# -- STO-3G
//			"<none>,+",					# -- 3-21G
//			"<none>",					# -- 6-21G
//			"<none>",					# -- 4-31G
//			"<none>,+,++",					# -- 6-31G
//			"<none>,+,++",					# -- 6-311G
//			"<none>,+,++",					# -- D95V
//			"<none>,+,++",					# -- D95
//			"<none>",					# -- SHC
//			"<none>",					# -- Stevens/Basch/Krauss
//			"<none>",					# -- LanL2MB
//			"<none>",					# -- LanL2DZ
//			"<none>",					# -- SDD
//			"<none>",					# -- SDDAll
//			"<none>,AUG",					# -- Dunning
//			"<none>",					# -- Alhrichs
//			"<none>",					# -- MidiX
//			"<none>",					# -- EPR
//			"<none>,+,++,2+,2++",				# -- UGBS
//			"<none>",					# -- MTSmall
//			"<none>",					# -- DGauss
//			"<none>,+,++");					# -- CBSB7
//		w.onInteger(1, 22, "sendstring", "polar", "items",
//			"<none>",					# -- STO-3G
//			"<none>",					# -- 3-21G
//			"<none>,*,**",					# -- 6-21G
//			"<none>,*,**",					# -- 4-31G
//			"<none>,d,2d,3d,df,2df,3df",			# -- 6-31G
//			"<none>,d,2d,3d,df,2df,3df",			# -- 6-311G
//			"<none>,d",					# -- D95V
//			"<none>,d,2d,3d,df,2df,3df",			# -- D95
//			"<none>,*",					# -- SHC
//			"<none>,*",					# -- Stevens/Basch/Krauss
//			"<none>",					# -- LanL2MB
//			"<none>",					# -- LanL2DZ
//			"<none>",					# -- SDD
//			"<none>",					# -- SDDAll
//			"<none>",					# -- Dunning
//			"<none>",					# -- Alhrichs
//			"<none>",					# -- MidiX
//			"<none>",					# -- EPR
//			"<none>,1P,2P,3P,1V,2V,3V,1O,2O,3O",		# -- UGBS
//			"<none>",					# -- MTSmall
//			"<none>",					# -- DGauss
//			"<none>");					# -- CBSB7
//		w.onInteger(1, 22, "sendstring", "lightpolar", "items",
//			"<none>",					# -- STO-3G
//			"<none>",					# -- 3-21G
//			"<none>",					# -- 6-21G
//			"<none>",					# -- 4-31G
//			"<none>,p,2p,3p,pd,2pd,3pd",			# -- 6-31G
//			"<none>,p,2p,3p,pd,2pd,3pd",			# -- 6-311G
//			"<none>,p",					# -- D95V
//			"<none>,p,2p,3p,pd,2pd,3pd",			# -- D95
//			"<none>",					# -- SHC
//			"<none>",					# -- Stevens/Basch/Krauss
//			"<none>",					# -- LanL2MB
//			"<none>",					# -- LanL2DZ
//			"<none>",					# -- SDD
//			"<none>",					# -- SDDAll
//			"<none>",					# -- Dunning
//			"<none>",					# -- Alhrichs
//			"<none>",					# -- MidiX
//			"<none>",					# -- EPR
//			"<none>",					# -- UGBS
//			"<none>",					# -- MTSmall
//			"<none>",					# -- DGauss
//			"<none>");					# -- CBSB7
//	}
//	# Execute dialog
//	if (!showDefaultDialog()) error("Options dialog canceled.\n");
//	Dialog ui = defaultDialog();
//
//	#
//	# Write Link0 Section
//	#
//	# TODO
//
//	# Write Route Section - Run Type
//	string line, keyword, options;
//	switch (ui.asString("runtyp"))
//	{
//		case ("SP"):
//			keyword = "# SP";
//			break;
//		case ("Opt"):
//			options = "";
//			if (ui.asString("opt_method") != "<default>") options += ui.asString("opt_method");
//			if (ui.asInteger("opt_maxcycles") != 0) { if (options != "") options += ","; options += "MaxCycles=" + ui.asString("opt_maxcycles"); }
//			if (ui.asInteger("opt_maxstep") != 0) { if (options != "") options += ","; options += "MaxStep=" + ui.asString("opt_maxstep"); }
//			if (ui.asInteger("opt_restart")) { if (options != "") options += ",Restart"; }
//			if (options == "") keyword = "# Opt";
//			else keyword = toa("# Opt=(%s)", options);
//			break;
//		default:
//			error("Unrecognised runtype encountered while writing route section.");
//	}
//	line = keyword;
//	# -- Add on other general options to this line
//	if (ui.isString("symmetry", "NoSymmetry")) line += " NoSymm";
//	else if (ui.asInteger("symmetry_loose")) line += " Symm=Loose";
//	# -- Write line
//	writeLineF("%s\n", line);
//
//	# Write Route Section - Method/Basis
//	line = "# ";
//	if (ui.asString("modchem_opt") != "<default>") line += ui.asString("modchem_opt");
//	line += ui.asString("modchem") + "/";
//	# -- Construct basis set string
//	if (ui.asString("subbasis") == "<none>") options = ui.asString("basis");
//	else options = ui.asString("subbasis");
//	# -- Add on diffuse symbols fist (mainly for Pople and SBK basis sets)
//	if (ui.asString("diffuse") != "<none>") options += ui.asString("diffuse");
//	# -- Add on 'G' (Pople and SBK basis sets only)
//	if ((ui.asInteger("basis") <= 6) || (ui.asInteger("basis") == 10)) options += "G";
//	# -- Polarisation functions now - some basis sets use '*' notation, so treat these separately
//	if ((ui.asString("polar") != "<none>") && (ui.asString("lightpolar") != "<none>")) options += toa("(%s,%s)",ui.asString("polar"),ui.asString("lightpolar"));
//	else if (ui.asString("polar") != "<none>") options += toa("%s",ui.asString("polar"));
//	else if (ui.asString("lightpolar") != "<none>") options += toa("%s",ui.asString("lightpolar"));
//	# -- Done!
//	line += options;
//	writeLine(line);
//	
//	# Blank line, title card, blank line
//	model m = aten.frame;
//	writeLineF("\n%s\n\n",m.name);
//
//	# Charge and multiplicity
//	writeLineF("%i %i\n", ui.asInteger("charge"), ui.asInteger("mult"));
//
//	# Atom coordinates (or z-matrix)
//	if (ui.asInteger("coords") == 1) for (atom i = aten.frame.atoms; i != 0; ++i) writeLineF("%-5s  %12.6f %12.6f %12.6f\n", i.symbol,i.rx,i.ry,i.rz);
//	else writeZMatrix(aten.frame);
//}
//
//filter(type="importmodel", name="Gaussian03 Log File", nickname="g03log", extension="log", extension="out", glob="*.log|*.out", zmap="name", search="Gaussian(R) 03 program", id=911)
//{
//	# Variable declaration
//	int result,nstructures,natoms,n,el,orientation;
//	string line,name,discard;
//	atom i;
//	double rx,ry,rz;
//
//	# Find input orientation and determine number of atoms
//	orientation = 0;
//	if (find("Input orientation:")) orientation = 1;
//	else if (find("Standard orientation:")) orientation = 2;
//	if (orientation == 0) error("Couldn't find any coordinates!");
//	# Skip four header lines
//	skipLine(4);
//	# Create a new model, and store its pointer for use later
//	model m = newModel(filterFilename());
//	natoms = 0;
//	do
//	{
//		readLine(n, el, discard, rx, ry, rz);
//		if (el != 0)
//		{
//			natoms++;
//			i = newAtom(el,rx,ry,rz);
//		}
//	} while (el != 0);
//	printf("There are %i atoms in the structure.\n", natoms);
//
//	# Now search for sets of coordinate
//	nstructures = 0;
//	while (!eof())
//	{
//		# Lines containing 'COORDINATES OF ALL ATOMS ARE' are the beginning of coordinate sections
//		result = 0;
//		if (orientation == 1) result = find("Input orientation:");
//		else if (orientation == 2) result = find("Standard orientation:");
//		if (result)
//		{
//			# Found a set of coordinates. Skip 4 lines and then read coordinates
//			nstructures++;
//			skipLine(4);
//			writeVarF(name, "Frame %i", nstructures);
//			addFrame(name);
//			for (n=1; n<=natoms; ++n)
//			{
//				readLine(n,el,discard,rx,ry,rz);
//				newAtom(el,rx,ry,rz);
//			}
//			# Recalculate bonding
//			rebond();
//			finaliseFrame();
//		}
//	}
//
//	finaliseModel();
//}

	return true;
}

// Return whether this plugin can export data
bool GaussianModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool GaussianModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool GaussianModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool GaussianModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool GaussianModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool GaussianModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool GaussianModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool GaussianModelPlugin::showExportOptionsDialog()
{
	return false;
}
 

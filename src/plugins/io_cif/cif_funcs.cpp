/*
        *** CIF Model Plugin Functions
        *** src/plugins/io_cif/cif_funcs.cpp
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

#include "plugins/io_cif/cif.hui"
#include "model/model.h"

// Constructor
CIFModelPlugin::CIFModelPlugin()
{
}

// Destructor
CIFModelPlugin::~CIFModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* CIFModelPlugin::makeCopy()
{
	return new CIFModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory CIFModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString CIFModelPlugin::name() const
{
	return QString("Crystallographic Information Format (CIF)");
}

// Nickname of plugin
QString CIFModelPlugin::nickname() const
{
	return QString("cif");
}

// Description (long name) of plugin
QString CIFModelPlugin::description() const
{
	return QString("Import for Crystallographic Information Format models");
}

// Related file extensions
QStringList CIFModelPlugin::extensions() const
{
	return QStringList() << "cif";
}

// Exact names
QStringList CIFModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool CIFModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool CIFModelPlugin::importData()
{
	// Set options for file parser
	int parseOptions = Parser::UseQuotes+Parser::NormalCommas;

	createModel("CifModel");

	// Step through file look for dictionary keywords that we care about.
	// If we find CIFLoop ("loop_") then construct a list of LoopKeywords that we care about, and as soon
	// as the loop_ definition ends (first line that doesn't have a keyword starting with '_') read in loop data until a blank line is encountered.

	bool inLoop = false;
	QList<int> loopItems;
	Vec3<double> cellAngles, cellLengths;
	while (!fileParser_.eofOrBlank())
	{
		if (!fileParser_.parseLine(parseOptions)) return false;

		// If we are currently constructing a loop, need to check whether we should start reading in data based on that loop
		if (inLoop && (fileParser_.argc(0).at(0) != QChar('_')))
		{
			// Before we start, need to work out what sort of loop we have..
			printf("LoopItems = ");for (int n=0; n<loopItems.count(); ++n) printf("%i ", loopItems.at(n)); printf("\n");
			
			do
			{
				// Loop over our stored (enumerated) loop keyword values
				// Parse next line...
				fileParser_.parseLine(parseOptions);
			} while (fileParser_.nArgs() > 0);

			// Continue with the main loop
			inLoop = false;
			loopItems.clear();
			continue;
		}

		// If this is a blank line, continue...
		if (fileParser_.nArgs() == 0) continue;

		// Try to convert first argument to a keyword - if we are already in a loop, convert it to a loopkeyword
		if (inLoop)
		{
			// Store the result, even if its unrecognised 'nLoopKeywords', since we need to know which parser data items to skip
			LoopKeyword keyword = loopKeyword(fileParser_.argc(0));
			printf("LOOPKWD = %s\n", qPrintable(fileParser_.argc(0)));
			loopItems << keyword;
		}
		else
		{
			DictionaryKeyword keyword = dictionaryKeyword(fileParser_.argc(0));
			printf("DICTIONARYKWD = %s\n", qPrintable(fileParser_.argc(0)));
			switch (keyword)
			{
				case (CIFModelPlugin::CIFLoop):
					// Sanity check - are we already in a loop?
					if (inLoop) Messenger::warn("CIF read error - found a loop_ while already parsing another...");
					inLoop = true;
					loopItems.clear();
					break;
				case (CIFModelPlugin::ChemicalNameCommon):
				case (CIFModelPlugin::ChemCompName):
					targetModel()->setName(fileParser_.argc(1));
					break;
				case (CIFModelPlugin::CellLengthA):
				case (CIFModelPlugin::CellLengthB):
				case (CIFModelPlugin::CellLengthC):
					cellLengths[keyword-CIFModelPlugin::CellLengthA] = fileParser_.argd(1);
					break;
				case (CIFModelPlugin::CellAngleAlpha):
				case (CIFModelPlugin::CellAngleBeta):
				case (CIFModelPlugin::CellAngleGamma):
					cellLengths[keyword-CIFModelPlugin::CellAngleAlpha] = fileParser_.argd(1);
					break;
				case (CIFModelPlugin::SymmetrySpacegroupNameHM):
				case (CIFModelPlugin::SymmetrySpacegroupNameHMAlt):
					targetModel()->cell().setSpacegroup(fileParser_.argc(1), standardOptions_.forceRhombohedral());
					break;
			}
		}
	}

//		# Loops
//		else if (keywd == "loop_") { inloop = 1; nloopdata = 0; looptype = "none"; continue; }
//		# End of loop data items (if first char is a '_')
//		else if (inloop > 0)
//		{
//			if ((char == "_") && (inloop == 2))
//			{
//				inloop = 0;
//				# Reset index integers
//				id_lbl = 0;
//				id_ts = 0;
//			}
//			else if ((char != "_") && (inloop == 1)) inloop = 2;
//		}
//
//		# Loop handling
//		if (inloop == 1)		# Loop data specification
//		{
//			keywdstripped = replaceChars(keywd,"_."," ");
//			readVar(keywdstripped, data1, data2, data3);
//			if ((data1 == "atom") && (data2 == "site"))
//			{
//				if (data3 == "aniso") continue;
//				looptype = "atom";
//				nloopdata++;
//				if (keywd == "_atom_site_type_symbol") id_ts = nloopdata;
//				else if (keywd == "_atom_site_label") id_lbl = nloopdata;
//				else if (keywd == "_atom_site_fract_x") id_r[1] = nloopdata;
//				else if (keywd == "_atom_site_fract_y") id_r[2] = nloopdata;
//				else if (keywd == "_atom_site_fract_z") id_r[3] = nloopdata;
//				else if (keywd == "_atom_site_cartn_x") { id_r[1] = nloopdata; fractional = FALSE; }
//				else if (keywd == "_atom_site_cartn_y") { id_r[2] = nloopdata; fractional = FALSE; }
//				else if (keywd == "_atom_site_cartn_z") { id_r[3] = nloopdata; fractional = FALSE; }
//			}
//			else if ((data1 == "symmetry") && (data2 == "equiv"))
//			{
//				if (m.cell.sgId <> 0)
//				{
//					printf("Generator data ignored - spacegroup is already set.\n");
//					inloop = 0;
//					continue;
//				}
//				looptype = "gen";
//				nloopdata++;
//				if (keywd == "_symmetry_equiv_pos_as_xyz") id_gen = nloopdata;
//			}
//			else if (((data1 == "space") && (data2 == "group")) || (data3 == "symop"))
//			{
//				if (m.cell.sgId <> 0)
//				{
//					printf("Generator data ignored - spacegroup is already set.\n");
//									if (keywd == "_atom_site_type_symbol") id_ts = nloopdata;
//				else if (keywd == "_atom_site_label") id_lbl = nloopdata;
//				else if (keywd == "_atom_site_fract_x") id_r[1] = nloopdata;
//				else if (keywd == "_atom_site_fract_y") id_r[2] = nloopdata;
//				else if (keywd == "_atom_site_fract_z") id_r[3] = nloopdata;
//				else if (keywd == "_atom_site_cartn_x") { id_r[1] = nloopdata; fractional = FALSE; }
//				else if (keywd == "_atom_site_cartn_y") { id_r[2] = nloopdata; fractional = FALSE; }
//				else if (keywd == "_atom_site_cartn_z") { id_r[3] = nloopdata; fractional = FALSE; }inloop = 0;
//					continue;
//				}
//				looptype = "gen";
//				nloopdata++;
//				if (keywd == "_space_group_symop_operation_xyz") id_gen = nloopdata;
//			}
//			else if ((data1 == "chem") && (data2 == "comp") && (data3 == "atom"))
//			{
//				# PDB-style atom section (as found in mmCIFs)
//				if (data3 == "aniso") continue;
//				looptype = "atom";
//				fractional = FALSE;
//				nloopdata++;
//				if (keywd == "_chem_comp_atom.type_symbol") id_ts = nloopdata;
//				else if (keywd == "_chem_comp_atom.model_Cartn_x") id_r[1] = nloopdata;
//				else if (keywd == "_chem_comp_atom.model_Cartn_y") id_r[2] = nloopdata;
//				else if (keywd == "_chem_comp_atom.model_Cartn_z") id_r[3] = nloopdata;
//			}
//		}
//		else if (inloop == 2)		# Loop data items
//		{
//			# Read in 'nloopdata' delimited arguments
//			for (n = 0; n < nloopdata; n++)
//			{
//				while (readNext(args[n+1]) == FALSE) getLine(line);
//				#printf("Arg/data %i = '%s'\n", nread, args[nread+1] );
//			}
//
//			# Our next action depends on the loop type
//			if (looptype == "atom")
//			{
//				# Take element name from type symbol, if defined, otherwise atom label
//				if (id_ts != 0) el = args[id_ts];
//				else if (id_lbl != 0) el = args[id_lbl];
//				else
//				{
//					printf("Warning: No element data found for atom.\n");
//					el = "XX";
//				}
//				newAtom(el, atof(args[id_r[1]]), atof(args[id_r[2]]), atof(args[id_r[3]]));
//				# printf("Created atom %s with coords %f %f %f\n", el, atof(args[id_r[1]]), atof(args[id_r[2]]), atof(args[id_r[3]]));
//			}
//			else if (looptype == "gen") addGenerator(args[id_gen]);
//		}
//	}
//
//	# Were we given a full cell spec?
//	if (cellspec == 6)
//	{
//		cell(length[1],length[2],length[3],cellangle[1],cellangle[2],cellangle[3]);
//		if (fractional) fracToReal();
//		# Pack according to spacegroup
//		pack();
//	}
//	else
//	{
//		printf("Warning: Full cell specification was not found in file.\n");
//		if (fractional) printf("Warning: Atom coordinates are still fractional.\n");
//	}
//
//	rebond();
//	finaliseModel();
//}
	return true;
}

// Return whether this plugin can export data
bool CIFModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool CIFModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool CIFModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool CIFModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool CIFModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool CIFModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool CIFModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool CIFModelPlugin::showExportOptionsDialog()
{
	return false;
}

/*
 * Dictionary
 */

// Return CIF dictionary keyword from string
CIFModelPlugin::DictionaryKeyword CIFModelPlugin::dictionaryKeyword(QString s)
{
	static QStringList CIFDictionaryKeywords = QStringList() << "loop_" << "_chemical_name_common" << "_chem_comp.name" << "_cell_length_a" << "_cell_length_b" << "_cell_length_c" << "_cell_angle_alpha" << "_cell_angle_beta" << "_cell_angle_gamma" << "_symmetry_space_group_name_H-M" << "_space_group_name_H-M_alt";

	for (int n=0; n<CIFModelPlugin::nDictionaryKeywords; ++n) if (CIFDictionaryKeywords.at(n) == s) return (CIFModelPlugin::DictionaryKeyword) n;
	return CIFModelPlugin::nDictionaryKeywords;
}

// Return CIF loop keyword from string
CIFModelPlugin::LoopKeyword CIFModelPlugin::loopKeyword(QString s)
{
	static QStringList CIFLoopKeywords = QStringList() << "_atom_site_type_symbol" << "_atom_site_label" << "_atom_site_fract_x" << "_atom_site_fract_y" << "_atom_site_fract_z" << "_atom_site_cartn_x" << "_atom_site_cartn_y" << "_atom_site_cartn_z" << "_symmetry_equiv_pos_as_xyz";

	for (int n=0; n<CIFModelPlugin::nLoopKeywords; ++n) if (CIFLoopKeywords.at(n) == s) return (CIFModelPlugin::LoopKeyword) n;
	return CIFModelPlugin::nLoopKeywords;
}

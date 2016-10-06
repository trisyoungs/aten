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
#include <QRegularExpression>

// Constructor
CIFModelPlugin::CIFModelPlugin()
{
}

// Destructor
CIFModelPlugin::~CIFModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* CIFModelPlugin::makeCopy() const
{
	return new CIFModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType CIFModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int CIFModelPlugin::category() const
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
	return QString("Basic import for Crystallographic Information Format models");
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
bool CIFModelPlugin::canImport() const
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

	QList<int> loopItems;
	Vec3<double> cellAngles, cellLengths;
	enum LoopType { NoLoop, AtomLoop, SymmetryOperatorsLoop, UnknownLoop };
	LoopType loopType = NoLoop;
	const int FractionalBit = 1;
	Atom* i = NULL;

	// Set up a regular expression to remove bracketed errors from numeric strings
	QRegularExpression numberRE("([\\d.\\-eE]+)\\(*.*\\)*");
	QRegularExpressionMatch numberMatch;

	// Parse file
	while (!fileParser_.eofOrBlank())
	{
		if (!fileParser_.parseLine(parseOptions)) return false;

		// If we are currently constructing a loop, need to check whether we should start reading in data based on that loop
		if ((loopType != NoLoop) && (fileParser_.argc(0).at(0) != QChar('_')))
		{
			// Before we start, need to work out what sort of loop we have..
			for (int n=0; n<loopItems.count(); ++n)
			{
				if (n == CIFModelPlugin::nLoopKeywords) continue;
				if ((loopItems.at(n) >= CIFModelPlugin::AtomSiteTypeSymbol) && (loopItems.at(n) <= CIFModelPlugin::ChemCompAtomModelCartnZ)) loopType = AtomLoop;
				else if (loopItems.at(n) == CIFModelPlugin::SymmetryEquivPosAsXyz) loopType = SymmetryOperatorsLoop;
				if (loopType != UnknownLoop) break;
			}

			// Go through data lines associated to loop
			do
			{
				// If in an atom loop, create a new atom before we start
				if (loopType == AtomLoop) i = createAtom(targetModel());
				// Loop over our stored (enumerated) loop keyword values
				for (int n=0; n<loopItems.count(); ++n)
				{
					LoopKeyword keyword = (LoopKeyword) loopItems.at(n);
					switch (keyword)
					{
						case (CIFModelPlugin::AtomSiteTypeSymbol):
						case (CIFModelPlugin::AtomSiteLabel):
						case (CIFModelPlugin::ChemCompAtomTypeSymbol):
							if (i->element() == 0) i->setElement(ElementMap::find(fileParser_.argc(n)));
							break;
						case (CIFModelPlugin::AtomSiteFractX):
						case (CIFModelPlugin::AtomSiteFractY):
						case (CIFModelPlugin::AtomSiteFractZ):
							numberMatch = numberRE.match(fileParser_.argc(n));
							i->r()[keyword-CIFModelPlugin::AtomSiteFractX] = numberMatch.captured(1).toDouble();
							i->addBit(FractionalBit);
							break;
						case (CIFModelPlugin::AtomSiteCartnX):
						case (CIFModelPlugin::AtomSiteCartnY):
						case (CIFModelPlugin::AtomSiteCartnZ):
							numberMatch = numberRE.match(fileParser_.argc(n));
							i->r()[keyword-CIFModelPlugin::AtomSiteCartnX] = numberMatch.captured(1).toDouble();
							break;
						case (CIFModelPlugin::ChemCompAtomModelCartnX):
						case (CIFModelPlugin::ChemCompAtomModelCartnY):
						case (CIFModelPlugin::ChemCompAtomModelCartnZ):
							numberMatch = numberRE.match(fileParser_.argc(n));
							i->r()[keyword-CIFModelPlugin::ChemCompAtomModelCartnX] = numberMatch.captured(1).toDouble();
							break;
						case (CIFModelPlugin::SymmetryEquivPosAsXyz):
							targetModel()->cell().addGenerator()->set(fileParser_.argc(n));
							break;
						case (CIFModelPlugin::nLoopKeywords):
							break;
					}
				}

				// Parse next line...
				// If it is empty (convenient) or we find another keyword (inconvenient) then stop the loop, but break out into the keyword check below
				fileParser_.parseLine(parseOptions);
			} while ((fileParser_.nArgs() > 0) && (!fileParser_.argc(0).contains('_')));

			// Continue with the main loop
			loopType = NoLoop;
			loopItems.clear();
		}

		// If this is a blank line, continue...
		if (fileParser_.nArgs() == 0) continue;

		// Try to convert first argument to a keyword - if we are already in a loop, convert it to a loopkeyword
		if (loopType != NoLoop)
		{
			// Store the result, even if its unrecognised 'nLoopKeywords', since we need to know which parser data items to skip
			LoopKeyword keyword = loopKeyword(fileParser_.argc(0));
// 			printf("LOOPKWD = %s\n", qPrintable(fileParser_.argc(0)));
			loopItems << keyword;
		}
		else
		{
			DictionaryKeyword keyword = dictionaryKeyword(fileParser_.argc(0));
// 			printf("DICTIONARYKWD = %s\n", qPrintable(fileParser_.argc(0)));
			switch (keyword)
			{
				case (CIFModelPlugin::CIFLoop):
					// Sanity check - are we already in a loop?
					if (loopType != NoLoop) Messenger::warn("CIF read error - found a loop_ while already parsing another...");
					loopType = UnknownLoop;
					loopItems.clear();
					break;
				case (CIFModelPlugin::ChemicalNameCommon):
				case (CIFModelPlugin::ChemCompName):
					if (fileParser_.hasArg(1)) targetModel()->setName(fileParser_.argc(1));
					else targetModel()->setName(readSemicolonText());
					break;
				case (CIFModelPlugin::CellLengthA):
				case (CIFModelPlugin::CellLengthB):
				case (CIFModelPlugin::CellLengthC):
					numberMatch = numberRE.match(fileParser_.argc(1));
					cellLengths[keyword-CIFModelPlugin::CellLengthA] = numberMatch.captured(1).toDouble();
					break;
				case (CIFModelPlugin::CellAngleAlpha):
				case (CIFModelPlugin::CellAngleBeta):
				case (CIFModelPlugin::CellAngleGamma):
					numberMatch = numberRE.match(fileParser_.argc(1));
					cellAngles[keyword-CIFModelPlugin::CellAngleAlpha] = numberMatch.captured(1).toDouble();
					break;
				case (CIFModelPlugin::SymmetrySpacegroupNameHM):
				case (CIFModelPlugin::SymmetrySpacegroupNameHMAlt):
					targetModel()->cell().setSpacegroup(fileParser_.argc(1), standardOptions_.isSetAndOn(FilePluginStandardImportOptions::ForceRhombohedralSwitch));
					break;
        default:
          break;  
			}
		}
	}

	// Set unit cell definition
	targetModel()->cell().setLengths(cellLengths);
	targetModel()->cell().setAngles(cellAngles);

	// Convert any atoms with fractional coordinates
	for (Atom* i = targetModel()->atoms(); i != NULL; i = i->next)
	{
		if (i->hasBit(FractionalBit)) i->r() = targetModel()->cell().fracToReal(i->r());
	}

	// Pack model
	if (!standardOptions_.isSetAndOn(FilePluginStandardImportOptions::PreventPackingSwitch)) targetModel()->pack();

	// Fold model
	if (!standardOptions_.isSetAndOn(FilePluginStandardImportOptions::PreventFoldingSwitch)) targetModel()->foldAllAtoms();

	// Rebond model
	if (!standardOptions_.isSetAndOn(FilePluginStandardImportOptions::PreventRebondingSwitch)) targetModel()->calculateBonding(true);

	return true;
}

// Return whether this plugin can export data
bool CIFModelPlugin::canExport() const
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
bool CIFModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool CIFModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool CIFModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool CIFModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

/*
 * CIF Functions
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
	static QStringList CIFLoopKeywords = QStringList() << "_atom_site_type_symbol" << "_atom_site_label" << "_atom_site_fract_x" << "_atom_site_fract_y" << "_atom_site_fract_z" << "_atom_site_cartn_x" << "_atom_site_cartn_y" << "_atom_site_cartn_z" << "_chem_comp_atom.type_symbol" << "_chem_comp_atom.model_Cartn_x" << "_chem_comp_atom.model_Cartn_y" << "_chem_comp_atom.model_Cartn_z" << "_symmetry_equiv_pos_as_xyz";

	for (int n=0; n<CIFModelPlugin::nLoopKeywords; ++n) if (CIFLoopKeywords.at(n) == s) return (CIFModelPlugin::LoopKeyword) n;
	return CIFModelPlugin::nLoopKeywords;
}

// Try to read in semicolon-bounded text immediately following the current file position
QString CIFModelPlugin::readSemicolonText()
{
	// Store current file position in case we need to go back
	std::streampos currentPos = fileParser_.tellg();

	bool success = false;
	// Read the next line
	QString text, line;
	fileParser_.readLine(line);
	if (line.trimmed() == ";")
	{
		// Good, one or more following lines are the text we want
		do
		{
			if (!fileParser_.readLine(line)) break;
			if (line.trimmed() == ";") break;
			text += line;
		} while (!fileParser_.eofOrBlank());
	}

	if (!success) fileParser_.seekg(currentPos);
	return text;
}

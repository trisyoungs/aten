/*
        *** DL_POLY Expression Plugin Functions
        *** src/plugins/io_dlpoly/field_funcs.cpp
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

#include "plugins/io_dlpoly/field.hui"
#include "plugins/io_dlpoly/common.h"
#include "model/model.h"
#include "base/pattern.h"
#include "base/forcefieldbound.h"

// Constructor
DLPExpressionPlugin::DLPExpressionPlugin()
{
	// Setup plugin options
	pluginOptions_.add("assumeUA", "false");
	pluginOptions_.add("massTolerance", "0.01");
	pluginOptions_.add("chargeTolerance", "0.01");
	pluginOptions_.add("reduceTypes", "true");
}

// Destructor
DLPExpressionPlugin::~DLPExpressionPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
BasePluginInterface* DLPExpressionPlugin::makeCopy() const
{
	return new DLPExpressionPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType DLPExpressionPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int DLPExpressionPlugin::category() const
{
	return PluginTypes::ExpressionFilePlugin;
}

// Name of plugin
QString DLPExpressionPlugin::name() const
{
	return QString("DL_POLY FIELD File" );
}

// Nickname of plugin
QString DLPExpressionPlugin::nickname() const
{
	return QString("dlpolyfield");
}

// Description (long name) of plugin
QString DLPExpressionPlugin::description() const
{
	return QString("Import/export for DL_POLY FIELD files");
}

// Related file extensions
QStringList DLPExpressionPlugin::extensions() const
{
	return QStringList() << "FIELD";
}

// Exact names
QStringList DLPExpressionPlugin::exactNames() const
{
	return QStringList() << "FIELD";
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool DLPExpressionPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool DLPExpressionPlugin::importData()
{
// 	# Variable declaration
// 	Forcefield ff;
// 	FFBound ffb;
// 	FFAtom at1, at2;
// 	int i, j, k, l, m, n, natoms, nmols, mol, nrepeat, nbound, nvdw;
// 	double mass, charge, qdiff, data[10];
// 	string discard, s, keyword, form, name, names[100];
// 
// 	# Main dialog creation functionreduceTypes
// 	void createDefaultDialog(Dialog ui)
// 	{
// 		ui.title = "DL_POLY FIELD Import Options";
// 		widget group, w;
// 		ui.verticalFill = TRUE;
// 		ui.addCheck("assumeua", "Assume United Atom", 0);
// 		ui.addCheck("reducetypes", "Reduce Types", 1);
// 		ui.addDoubleSpin("masstol", "Mass Tolerance", 0.0, 1.0, 0.01, 0.01);
// 		ui.addDoubleSpin("qtol", "Charge Tolerance", 0.0, 1.0, 0.01, 0.01);
// 	}
// 	# Execute dialog and grab values
// 	if (!showDefaultDialog()) error("Options dialog canceled.\n");
// 	Dialog ui = defaultDialog();


	// Get options
	bool reduceTypes = FilePluginInterface::toBool(pluginOptions().value("reduceTypes"));
	double chargeTolerance = pluginOptions().value("chargeTolerance").toDouble();
	double massTolerance = pluginOptions().value("massTolerance").toDouble();

	// First line is header information - use as FF name
	QString line;
  Forcefield* newFF;
	if (!fileParser_.readLine(line)) {
	  newFF = createForcefield(QString("unnamed field"));
  }else{
	  newFF = createForcefield(line);
  }
	// Create a new forcefield

	// Next meaningful line is energy units
	if (!fileParser_.parseLine(Parser::StripComments+Parser::SkipBlanks) ) return false;
  QString word = fileParser_.argc(0).toLower(); 
  if (word == "units"){
    QString unitString = fileParser_.argc(1).toLower();
   	Prefs::EnergyUnit unit = Prefs::energyUnit(unitString);
    if (unit == Prefs::nEnergyUnits)
  	{
	  	Messenger::error("FIELD file energy unit (%s) is not compatible with Aten.", qPrintable(unitString));
	  	return false;
    }
  }

  int nMols;
	// Next is number of molecule types described or multipolar data section in the file
	if (!fileParser_.parseLine(Parser::StripComments+Parser::SkipBlanks)) return false;
  word = fileParser_.argc(0).toLower(); 

  if ((word == "mult")||(word=="multipolar")) {
  	if (!fileParser_.parseLine(Parser::StripComments+Parser::SkipBlanks)) return false;
  //more stuff may need to be read from here but last line needs to be a reading
  }
	if (fileParser_.argc(0).toLower() == "molecules"){
	  nMols = fileParser_.argi(1);
    Messenger::print("Number of molecule types specified in FIELD file : %i", nMols);
  } else {
  	Messenger::error("Didn't find 'molecules' directive where expected.");
	  return false;
	}
	// Loop over molecule types
	for (int mol=0; mol<nMols; ++mol)
	{
		// First line is pattern name, next is number of molecules (neither of which we care about)
		if (!fileParser_.readLine(line)) return false;
		Messenger::print("Reading molecule type '%s'...", qPrintable(line));
		if (!fileParser_.skipLines(1)) return false;

		// Read number of atoms per molecule of this type
		if (!fileParser_.parseLine()) return false;
		if (fileParser_.argc(0).toLower() != "atoms")
		{
			Messenger::error("Didn't find 'atoms' directive where expected.");
			return false;
		}
		int nAtoms = fileParser_.argi(1);
		Messenger::print("  -- This molecule type contains %i atoms", nAtoms);

		// Loop over atoms
		QStringList atomNames;
		int n = 0;
		while (n < nAtoms)
		{
			// Read each atom definition:   Name    Mass   Charge   nRepeat
			if (!fileParser_.parseLine()) return false;
			QString typeName = fileParser_.argc(0);
			double typeMass = fileParser_.argd(1);
			double typeCharge = fileParser_.argd(2);
			int nRepeat = fileParser_.argi(3) == 0 ? 1 : fileParser_.argi(3);
			
			// If reduceTypes is enabled, check previous atomtype definitions for ones with same name and charge (within tolerance)
			ForcefieldAtom* ffa = NULL;
			if (reduceTypes)
			{
				// Loop over atom types already defined in the forcefield, and find one with similar name and charge to this one
				for (ffa = newFF->types(); ffa != NULL; ffa = ffa->next)
				{
					if (ffa->name() != typeName) continue;
					if (fabs(typeCharge - ffa->charge()) < chargeTolerance) break; 
				}
			}

			// If ffa is NULL this type already exists and we should move on.
			// Otherwise, create a new type.
			if (ffa == NULL)
			{
				// Need to find element based on the mass we were given....
				int element = ElementMap::z(typeMass, massTolerance);
				if (element == 0) Messenger::warn("Couldn't determine element for atom %i, whose mass is %f", n+1, typeMass);
				ffa = newFF->addType(-1, typeName, typeName, element, "");
				ffa->setCharge(typeCharge);
			}

			// Increase atom counter and store names for upcoming bound definitions
			for (int m=0; m<nRepeat; ++m, ++n) atomNames << typeName;
		}

		// Next sections are optional, and terminated by 'end' keyword
		int ii, jj, kk, ll;
		QString formString;
		ForcefieldBound* ffb;
		while (!fileParser_.eofOrBlank())
		{
			if (!fileParser_.parseLine()) return false;
			QString keyword = fileParser_.argc(0).toLower();
			if (keyword == "bonds")
			{
				Messenger::print("Found 'bonds' block...");
				for (n=0; n<fileParser_.argi(1); ++n)
				{
					if (!fileParser_.parseLine()) return false;
					formString = fileParser_.argc(0).toLower();
					BondFunctions::BondFunction bf = BondFunctions::bondFunction(formString);
					if (bf == BondFunctions::nBondFunctions)
					{
						Messenger::print("Functional form of bond term (%s) is not present in Aten.", qPrintable(formString));
						continue;
					}
					ii = fileParser_.argi(1) - 1; 
					jj = fileParser_.argi(2) - 1;

					// Does a bond definition between atom names i and j already exist?
					if (newFF->findBond(atomNames.at(ii), atomNames.at(jj)))
					{
						Messenger::print("Bond %s-%s has already been created in the forcefield. Skipped...", qPrintable(atomNames.at(ii)), qPrintable(atomNames.at(jj)));
						continue;
					}

					// Create new definition
					ffb = newFF->addBond(bf, atomNames.at(ii), atomNames.at(jj));
					if (bf == BondFunctions::Harmonic)
					{
						ffb->setParameter(BondFunctions::HarmonicK, fileParser_.argd(3));
						ffb->setParameter(BondFunctions::HarmonicEq, fileParser_.argd(4));
					}
					else if (bf == BondFunctions::Morse)
					{
						ffb->setParameter(BondFunctions::MorseD, fileParser_.argd(3));
						ffb->setParameter(BondFunctions::MorseK, fileParser_.argd(5));
						ffb->setParameter(BondFunctions::MorseEq, fileParser_.argd(4));
					}
				}
			}
			else if (keyword == "constraints")
			{
				Messenger::print("Found 'constraints' block...");
				for (n=0; n<fileParser_.argi(1); ++n)
				{
					if (!fileParser_.parseLine()) return false;
					ii = fileParser_.argi(0) - 1; 
					jj = fileParser_.argi(0) - 1;

					// Does a constraint bond definition between atom names i and j already exist?
					if (newFF->findBond(atomNames.at(ii), atomNames.at(jj)))
					{
						Messenger::print("Constraint bond %s-%s has already been created in the forcefield. Skipped...", qPrintable(atomNames.at(ii)), qPrintable(atomNames.at(jj)));
						continue;
					}

					// Create new definition
					newFF->addBond(BondFunctions::Constraint, atomNames.at(ii), atomNames.at(jj));
					ffb->setParameter(BondFunctions::HarmonicK, 1000.0);
					ffb->setParameter(BondFunctions::HarmonicEq, fileParser_.argd(2));
				}
			}
			else if (keyword == "angles")
			{
				Messenger::print("Found 'angles' block...");
				for (n=0; n<fileParser_.argi(1); ++n)
				{
					if (!fileParser_.parseLine()) return false;
					formString = fileParser_.argc(0).toLower();
					AngleFunctions::AngleFunction af = AngleFunctions::angleFunction(formString);
					if (af == AngleFunctions::nAngleFunctions)
					{
						Messenger::print("Functional form of angle term (%s) is not present in Aten.", qPrintable(formString));
						continue;
					}
					ii = fileParser_.argi(1) - 1; 
					jj = fileParser_.argi(2) - 1;
					kk = fileParser_.argi(3) - 1;
					//readLine(form, i, j, k, data[1], data[2], data[3], data[4]);

					// Does an angle definition between atom names i, j, and k already exist?
					if (newFF->findAngle(atomNames.at(ii), atomNames.at(jj), atomNames.at(kk)))
					{
						Messenger::print("Angle %s-%s-%s has already been created in the forcefield. Skipped...", qPrintable(atomNames.at(ii)), qPrintable(atomNames.at(jj)), qPrintable(atomNames.at(kk)));
						continue;
					}

					// Create new definition
					ffb = newFF->addAngle(af, atomNames.at(ii), atomNames.at(jj), atomNames.at(kk));
					if (af == AngleFunctions::Harmonic)
					{
						ffb->setParameter(AngleFunctions::HarmonicK, fileParser_.argd(4));
						ffb->setParameter(AngleFunctions::HarmonicEq, fileParser_.argd(5));
					}
					else if (af == AngleFunctions::Cosine)
					{
						ffb->setParameter(AngleFunctions::CosineK, fileParser_.argd(4));
						ffb->setParameter(AngleFunctions::CosineN, fileParser_.argd(6));
						ffb->setParameter(AngleFunctions::CosineEq, fileParser_.argd(5));
					}
					else if (af == AngleFunctions::HarmonicCosine)
					{
						ffb->setParameter(AngleFunctions::HarmonicCosineK, fileParser_.argd(4));
						ffb->setParameter(AngleFunctions::HarmonicCosineEq, fileParser_.argd(5));
					}
				}
			}
			else if (keyword == "dihedrals")
			{
				Messenger::print("Found 'dihedrals' block...");
				for (n=0; n<fileParser_.argi(1); ++n)
				{
					if (!fileParser_.parseLine()) return false;
					formString = fileParser_.argc(0).toLower();
					TorsionFunctions::TorsionFunction tf = TorsionFunctions::torsionFunction(formString);
					if (formString == "opls") tf = TorsionFunctions::Cos3C;
					if (tf == TorsionFunctions::nTorsionFunctions)
					{
						Messenger::print("Functional form of torsion term (%s) is not present in Aten.", qPrintable(formString));
						continue;
					}
					ii = fileParser_.argi(1) - 1; 
					jj = fileParser_.argi(2) - 1;
					kk = fileParser_.argi(3) - 1;
					ll = fileParser_.argi(4) - 1;
// 					readLine(form, i, j, k, l, data[1], data[2], data[3], data[4], data[5]);

					// Does a torsion definition between atom names i, j, and k already exist?
					if (newFF->findTorsion(atomNames.at(ii), atomNames.at(jj), atomNames.at(kk), atomNames.at(ll)))
					{
						Messenger::print("Torsion  %s-%s-%s-%s has already been created in the forcefield. Skipped...", qPrintable(atomNames.at(ii)), qPrintable(atomNames.at(jj)), qPrintable(atomNames.at(kk)), qPrintable(atomNames.at(ll)));
						continue;
					}

					// Create new definition
					ffb = newFF->addTorsion(tf, atomNames.at(ii), atomNames.at(jj), atomNames.at(kk), atomNames.at(ll));
					if (tf == TorsionFunctions::Cosine)
					{
						ffb->setParameter(TorsionFunctions::CosineK, fileParser_.argd(5));
						ffb->setParameter(TorsionFunctions::CosineN, fileParser_.argd(7));
						ffb->setParameter(TorsionFunctions::CosineEq, fileParser_.argd(6));
					}
					else if (tf == TorsionFunctions::Cos3)
					{
						ffb->setParameter(TorsionFunctions::Cos3K1, fileParser_.argd(5));
						ffb->setParameter(TorsionFunctions::Cos3K2, fileParser_.argd(6));
						ffb->setParameter(TorsionFunctions::Cos3K3, fileParser_.argd(7));
					}
					else if (tf == TorsionFunctions::Cos3C)
					{
						ffb->setParameter(TorsionFunctions::Cos3CK0, fileParser_.argd(5));
						ffb->setParameter(TorsionFunctions::Cos3CK1, fileParser_.argd(6));
						ffb->setParameter(TorsionFunctions::Cos3CK2, fileParser_.argd(7));
						ffb->setParameter(TorsionFunctions::Cos3CK3, fileParser_.argd(7));
					}
					if (tf != TorsionFunctions::nTorsionFunctions)
					{
						ffb->setElecScale(fileParser_.argd(8));
						ffb->setVdwScale(fileParser_.argd(9));
					}
				}
			}
			else if (keyword == "finish") break;
			else Messenger::warn("Unrecognised keyword in FIELD file - '%s'", qPrintable(keyword));
		}
	}

	// VDW Specification
	if (!fileParser_.parseLine()) return false;
	if (fileParser_.argc(0).toLower() != "vdw") Messenger::warn("Didn't find 'vdw' section where expected. VDW information not converted.");
	else
	{
		Messenger::print("Found 'vdw' block...");
		for (int n=0; n<fileParser_.argi(1); ++n)
		{
			// Read in each line of data, searching for those where the first atomtype is equal to the second
			if (!fileParser_.parseLine()) return false;
			QString nameI = fileParser_.argc(0);
// 			readLine(names[1], names[2], form, data[1], data[2], data[3], data[4], data[5]);
			if (nameI != fileParser_.argc(1)) continue;

			// We may have added multiple types of the same name earlier, so search the whole list explicitly
			for (ForcefieldAtom* ffa = newFF->types(); ffa != NULL; ffa = ffa->next)
			{
				if (ffa->name() != nameI) continue;

				// Check functional form
				if (fileParser_.argc(2) == "lj")
				{
					ffa->setVdwForm(VdwFunctions::Lj); 
					ffa->setParameter(VdwFunctions::LjEpsilon, fileParser_.argd(3));
					ffa->setParameter(VdwFunctions::LjSigma, fileParser_.argd(4));
// 					interDef("lj", at1.id, at1.charge, data[1], data[2]);
				}
				else Messenger::warn("Functional form of VDW term (%s) is not present in Aten.", qPrintable(fileParser_.argc(2)));
			}
		}
	}

	return true;
}

// Return whether this plugin can export data
bool DLPExpressionPlugin::canExport() const
{
	return true;
}

// Export data to the specified file
bool DLPExpressionPlugin::exportData()
{
	// First, write header consisting of title, energy units, and number of molecule types
	if (!fileParser_.writeLine(targetModel()->name())) return false;

	Prefs::EnergyUnit energyUnit = Prefs::KiloJoules;
	if (!fileParser_.writeLineF("units %s", Prefs::energyUnit(energyUnit))) return false;

	// Write number of molecule types (number of patterns)
	if (!fileParser_.writeLineF("molecules %i", targetModel()->nPatterns())) return false;

	// Loop over patterns (molecule types)
	for (Pattern* p = targetModel()->patterns(); p != NULL; p = p->next)
	{
		int nConstraints = 0;

		// Write pattern name and number of molecules
		if (!fileParser_.writeLine(p->name())) return false;
		if (!fileParser_.writeLineF("nummols %i", p->nMolecules())) return false;

		// Write atoms - just loop over those in the first molecule of the pattern
		if (!fileParser_.writeLineF("atoms %i", p->nAtoms())) return false;
		Atom* i = p->firstAtom();
		for (int n=0; n<p->nAtoms(); ++n)
		{
			ForcefieldAtom* ffi = i->type();
			if (!i->type())
			{
				Messenger::warn("Atom %i in pattern %i does not have an atom type associated to it...", i->id()+1, qPrintable(p->name()));
				if (!fileParser_.writeLineF("%-8s   %10.5f  %9.6f   1   %i   1", ElementMap::symbol(i), ElementMap::atomicMass(i), i->charge(), i->isPositionFixed())) return false;
			}
			else if (!fileParser_.writeLineF("%-8s   %10.5f  %9.6f   1   %i   1", qPrintable(i->type()->name()), i->type()->elementMass(), i->charge(), i->isPositionFixed())) return false;
			i = i->next;
		}

		// Bonds in pattern
		// Note: DL_POLY constraint terms are written in their own section, so we must 
		// determine the number of constraints and subtract this from the number of actual bonds
		int nc = 0;
		for (PatternBound* pb = p->bonds(); pb != NULL; pb = pb->next) if (pb->data()->bondForm() == BondFunctions::Constraint) ++nc;
		if (!fileParser_.writeLineF("bonds %i", p->nBonds() - nc)) return false;
		for (PatternBound* pb = p->bonds(); pb != NULL; pb = pb->next)
		{
			// Convert functional form to be recognised by DL_POLY
			switch (pb->data()->bondForm())
			{
				case (BondFunctions::Harmonic):
					if (!fileParser_.writeLineF("harm     %5i %5i %10.4f %10.4f", pb->atomId(0)+1, pb->atomId(1)+1, pb->data()->convertedParameter(BondFunctions::HarmonicK, energyUnit), pb->data()->parameter(BondFunctions::HarmonicEq))) return false;
					break;
				case (BondFunctions::Morse):
					if (!fileParser_.writeLineF("morse    %5i %5i %10.4f %10.4f %10.4f", pb->atomId(0)+1, pb->atomId(1)+1, pb->data()->convertedParameter(BondFunctions::MorseD, energyUnit), pb->data()->parameter(BondFunctions::MorseEq), pb->data()->parameter(BondFunctions::MorseK))) return false;
					break;
				case (BondFunctions::Constraint):
					++nConstraints;
					break;
				default:
					Messenger::warn("Functional form of bond term (%s) not convertible to DL_POLY.", BondFunctions::functionData[pb->data()->bondForm()].name);
					if (!fileParser_.writeLineF("X%s  %5i %5i %10.4f %10.4f %10.4f %10.4f", BondFunctions::functionData[pb->data()->bondForm()].name, pb->atomId(0)+1, pb->atomId(1)+1, pb->data()->convertedParameter(0, energyUnit), pb->data()->convertedParameter(1, energyUnit), pb->data()->convertedParameter(2, energyUnit), pb->data()->convertedParameter(3, energyUnit))) return false;
			}
		}

		// Angles in pattern
		nc = 0;
		for (PatternBound* pb = p->angles(); pb != NULL; pb = pb->next) if (pb->data()->angleForm() == AngleFunctions::BondConstraint) ++nc;
		if (!fileParser_.writeLineF("angles %i", p->nAngles() - nc)) return false;
		for (PatternBound* pb = p->angles(); pb != NULL; pb = pb->next)
		{
			// Convert functional form to be recognised by DL_POLY
			switch (pb->data()->angleForm())
			{
				case (AngleFunctions::Harmonic):
					if (!fileParser_.writeLineF("harm     %5i %5i %5i %10.4f %10.4f", pb->atomId(0)+1, pb->atomId(1)+1, pb->atomId(2)+1, pb->data()->convertedParameter(AngleFunctions::HarmonicK, energyUnit), pb->data()->parameter(AngleFunctions::HarmonicEq))) return false;
					break;
				case (AngleFunctions::Cosine):
					if (!fileParser_.writeLineF("cos      %5i %5i %5i %10.4f %10.4f %10.4f", pb->atomId(0)+1, pb->atomId(1)+1, pb->atomId(2)+1, pb->data()->convertedParameter(AngleFunctions::CosineK, energyUnit), pb->data()->parameter(AngleFunctions::CosineEq), pb->data()->parameter(AngleFunctions::CosineN))) return false;
					break;
				case (AngleFunctions::HarmonicCosine):
					if (!fileParser_.writeLineF("hcos     %5i %5i %5i %10.4f %10.4f", pb->atomId(0)+1, pb->atomId(1)+1, pb->atomId(2)+1, pb->data()->convertedParameter(AngleFunctions::HarmonicCosineK, energyUnit), pb->data()->parameter(AngleFunctions::HarmonicCosineEq))) return false;
					break;
				case (AngleFunctions::BondConstraint):
					++nConstraints;
					break;
				default:
					Messenger::warn("Functional form of angle term (%s) not convertible to DL_POLY.", AngleFunctions::functionData[pb->data()->angleForm()].name);
					if (!fileParser_.writeLineF("X%s  %5i %5i %5i %10.4f %10.4f %10.4f %10.4f", AngleFunctions::functionData[pb->data()->angleForm()].name, pb->atomId(0)+1, pb->atomId(1)+1, pb->atomId(2)+1, pb->data()->convertedParameter(0, energyUnit), pb->data()->convertedParameter(1, energyUnit), pb->data()->convertedParameter(2, energyUnit), pb->data()->convertedParameter(3, energyUnit))) return false;
			}
		}

		// Torsions in pattern
		if (!fileParser_.writeLineF("dihedrals %i", p->nTorsions())) return false;
		for (PatternBound* pb = p->torsions(); pb != NULL; pb = pb->next)
		{
			// Convert functional form to be recognised by DL_POLY
			switch (pb->data()->torsionForm())
			{
				case (TorsionFunctions::Cosine):
					if (!fileParser_.writeLineF("cos      %5i %5i %5i %5i %10.4f %10.4f %10.4f %10.4f %10.4f", pb->atomId(0)+1, pb->atomId(1)+1, pb->atomId(2)+1, pb->atomId(3)+1, pb->data()->convertedParameter(TorsionFunctions::CosineK, energyUnit), pb->data()->parameter(TorsionFunctions::CosineEq), pb->data()->parameter(TorsionFunctions::CosineN), pb->data()->elecScale(), pb->data()->vdwScale())) return false;
					break;
				case (TorsionFunctions::Cos3):
					if (!fileParser_.writeLineF("cos3     %5i %5i %5i %5i %10.4f %10.4f %10.4f %10.4f %10.4f", pb->atomId(0)+1, pb->atomId(1)+1, pb->atomId(2)+1, pb->atomId(3)+1, pb->data()->convertedParameter(TorsionFunctions::Cos3K1, energyUnit), pb->data()->convertedParameter(TorsionFunctions::Cos3K2, energyUnit), pb->data()->convertedParameter(TorsionFunctions::Cos3K3, energyUnit), pb->data()->elecScale(), pb->data()->vdwScale())) return false;
					break;
				case (TorsionFunctions::Cos3C):
					if (!fileParser_.writeLineF("opls     %5i %5i %5i %5i %10.4f %10.4f %10.4f %10.4f %10.4f", pb->atomId(0)+1, pb->atomId(1)+1, pb->atomId(2)+1, pb->atomId(3)+1, pb->data()->convertedParameter(TorsionFunctions::Cos3CK0, energyUnit), pb->data()->convertedParameter(TorsionFunctions::Cos3CK1, energyUnit), pb->data()->convertedParameter(TorsionFunctions::Cos3CK2, energyUnit), pb->data()->convertedParameter(TorsionFunctions::Cos3CK3, energyUnit), pb->data()->elecScale())) return false;
					break;
				default:
					Messenger::warn("Functional form of torsion term (%s) not convertible to DL_POLY.", TorsionFunctions::functionData[pb->data()->torsionForm()].name);
					if (!fileParser_.writeLineF("X%s  %5i %5i %5i %5i %10.4f %10.4f %10.4f %10.4f", TorsionFunctions::functionData[pb->data()->torsionForm()].name, pb->atomId(0)+1, pb->atomId(1)+1, pb->atomId(2)+1, pb->atomId(2)+1, pb->data()->convertedParameter(0, energyUnit), pb->data()->convertedParameter(1, energyUnit), pb->data()->convertedParameter(2, energyUnit), pb->data()->elecScale(), pb->data()->vdwScale())) return false;
			}
		}

		// Constraint terms
		if (nConstraints > 0)
		{
			if (!fileParser_.writeLineF("constraints %i", nConstraints)) return false;

			// ...proper bond constraints first
			for (PatternBound* pb = p->bonds(); pb != NULL; pb = pb->next)
			{
				if (pb->data()->bondForm() == BondFunctions::Constraint) if (!fileParser_.writeLineF("    %5i %5i  %10.4f", pb->atomId(0)+1, pb->atomId(1)+1, pb->data()->parameter(BondFunctions::ConstraintEq))) return false;
			}

			// ...and now angles that are of type 'bondconstraint'
			for (PatternBound* pb = p->angles(); pb != NULL; pb = pb->next)
			{
				if (pb->data()->angleForm() == AngleFunctions::BondConstraint) if (!fileParser_.writeLineF("    %5i %5i  %10.4f", pb->atomId(0)+1, pb->atomId(2)+1, pb->data()->parameter(AngleFunctions::BondConstraintEq))) return false;
			}
		}

		// Terminating line
		if (!fileParser_.writeLine("finish")) return false;
	}

	// VDW Specification
	// -- Get total number of pair terms to write
	int nVdw = 0;
	for (int n=1; n<=targetModel()->nUniqueForcefieldTypes(); ++n) nVdw += n;
	if (!fileParser_.writeLineF("vdw %i", nVdw)) return false;

	// -- Now write pair information
	for (RefListItem<ForcefieldAtom,int>* ri = targetModel()->uniqueForcefieldTypes(); ri != NULL; ri = ri->next)
	{
		ForcefieldAtom* ffi = ri->item;

		for (RefListItem<ForcefieldAtom,int>* rj = ri; rj != NULL; rj = rj->next)
		{
			ForcefieldAtom* ffj = rj->item;

			// Check functional forms of each atomtype
			if (ffi->vdwForm() != ffj->vdwForm())
			{
				Messenger::warn("Functional forms of ffTypes '%s' (%s) and '%s' (%s) differ - raw data written to FIELD file...", qPrintable(ffi->name()), VdwFunctions::functionData[ffi->vdwForm()].name, qPrintable(ffj->name()), VdwFunctions::functionData[ffj->vdwForm()].name); 
				if (!fileParser_.writeLineF("%-8s  %-8s   %12.6f  %12.6f  %12.6f  %12.6f  %12.6f  %12.6f", qPrintable(ffi->name()), qPrintable(ffj->name()),  ffi->convertedParameter(0, energyUnit), ffi->convertedParameter(1, energyUnit), ffi->convertedParameter(2, energyUnit), ffj->convertedParameter(0, energyUnit), ffj->convertedParameter(1, energyUnit), ffj->convertedParameter(2, energyUnit))) return false;

				continue;
			}

			switch (ffi->vdwForm())
			{
				case (VdwFunctions::Lj):
				case (VdwFunctions::LjGeometric):
					if (!fileParser_.writeLineF("%-8s %-8s  lj  %12.6f  %12.6f", qPrintable(ffi->name()), qPrintable(ffj->name()), ffi->combinedAndConvertedParameter(VdwFunctions::LjEpsilon, ffj, energyUnit), ffi->combinedAndConvertedParameter(VdwFunctions::LjSigma, ffj, energyUnit))) return false;
					break;
				case (VdwFunctions::Buckingham):
					Messenger::warn("Buckingham potential not included in FIELD file export yet. Raw data written.");
					break;
				default:
					Messenger::error("Functional form of VDW term (%s) not accounted for in FIELD plugin.", VdwFunctions::functionData[ffi->vdwForm()].name);
			}
		}
	}

	// Final line
	if (!fileParser_.writeLine("close")) return false;

	return true;
}

// Import next partial data chunk
bool DLPExpressionPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool DLPExpressionPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool DLPExpressionPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool DLPExpressionPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool DLPExpressionPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool DLPExpressionPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

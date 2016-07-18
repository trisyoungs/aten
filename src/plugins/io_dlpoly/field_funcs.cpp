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
	/* none */
}

// Destructor
DLPExpressionPlugin::~DLPExpressionPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* DLPExpressionPlugin::makeCopy()
{
    return new DLPExpressionPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory DLPExpressionPlugin::category() const
{
    return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString DLPExpressionPlugin::name() const
{
    return QString("DL_POLY FIELD File" );
}

// Nickname of plugin
QString DLPExpressionPlugin::nickname() const
{
    return QString("field");
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
bool DLPExpressionPlugin::canImport()
{
	return false;
}

// Import data from the specified file
bool DLPExpressionPlugin::importData()
{
	return false;
}

// Return whether this plugin can export data
bool DLPExpressionPlugin::canExport()
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
bool DLPExpressionPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool DLPExpressionPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool DLPExpressionPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool DLPExpressionPlugin::showExportOptionsDialog()
{
	return false;
}

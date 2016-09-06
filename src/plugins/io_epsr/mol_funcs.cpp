/*
        *** EPSRMol Model Plugin Functions
        *** src/plugins/io_epsr/mol_funcs.cpp
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

#include "plugins/io_epsr/mol.hui"
#include "plugins/io_epsr/molexportoptions.h"
#include "model/model.h"
#include "base/pattern.h"
#include "ff/forcefield.h"
#include "base/forcefieldbound.h"
#include "templates/datapair.h"

// Constructor
EPSRMolModelPlugin::EPSRMolModelPlugin()
{
	// Plugin options
	pluginOptions_.add("temp", "300.0");
	pluginOptions_.add("vibTemp", "65.0");
	pluginOptions_.add("angTemp", "3.0");
	pluginOptions_.add("dihTemp", "10.0");
	pluginOptions_.add("eCore", "0.0");
	pluginOptions_.add("dCore", "0.0");
	pluginOptions_.add("modelGeometry", "false");
	pluginOptions_.add("writeDihedrals", "false");
}

// Destructor
EPSRMolModelPlugin::~EPSRMolModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* EPSRMolModelPlugin::makeCopy()
{
	return new EPSRMolModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory EPSRMolModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString EPSRMolModelPlugin::name() const
{
	return QString("EPSR Molfiles");
}

// Nickname of plugin
QString EPSRMolModelPlugin::nickname() const
{
	return QString("epsrmol");
}

// Description (long name) of plugin
QString EPSRMolModelPlugin::description() const
{
	return QString("Export for Empirical Potential Structure Refinement (EPSR) molfiles");
}

// Related file extensions
QStringList EPSRMolModelPlugin::extensions() const
{
	return QStringList() << "mol";
}

// Exact names
QStringList EPSRMolModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool EPSRMolModelPlugin::canImport()
{
	return false;
}

// Import data from the specified file
bool EPSRMolModelPlugin::importData()
{
	return false;
}

// Return whether this plugin can export data
bool EPSRMolModelPlugin::canExport()
{
	return true;
}

// Export data to the specified file
bool EPSRMolModelPlugin::exportData()
{
	// Create a temporary forcefield to store some terms in
	Forcefield dummyFF;

	// Write distinguishing header line - we will use no atom ID offset
	if (!fileParser_.writeLineF(" .gmol           0")) return false;

	// Write atom coordinates and bonds
	for (Atom* i = targetModel()->atoms(); i != NULL; i = i->next)
	{
		// Grab type name or, if there isn't one, the element symbol
		QString typeName = i->type() ? i->type()->name() : ElementMap::symbol(i);

		// Write atom line as far as the number of bonds
		if (!fileParser_.writeF(" atom %i %s %f %f %f %i", i->id()+1, qPrintable(typeName), i->r().x, i->r().y, i->r().z, i->nBonds())) return false;

		// Append bond connectivity to line
		for (RefListItem<Bond,int>* ri = i->bonds(); ri != NULL; ri = ri->next)
		{
			Bond* b = ri->item;
			if (!fileParser_.writeF(" %i", b->partner(i)->id()+1)) return false;
		}
		if (!fileParser_.writeLine()) return false;

		// Add a new type to our temporary forcefield (if we haven't already for this type)
		if (!dummyFF.findType(typeName))
		{
			ForcefieldAtom* ffa = dummyFF.addType(-1, typeName, typeName, i->element(), "", "");

			// Setup van der Waals for type as best we can
			ffa->setVdwForm(VdwFunctions::Lj);
			ffa->setCharge(i->charge());
			if (i->type())
			{
				if (i->type()->vdwForm() == VdwFunctions::Lj) for (int n=0; n<VdwFunctions::functionData[VdwFunctions::Lj].nParameters; ++n) ffa->setParameter(n, i->type()->parameter(n));
				else if (i->type()->vdwForm() == VdwFunctions::LjGeometric) for (int n=0; n<VdwFunctions::functionData[VdwFunctions::LjGeometric].nParameters; ++n) ffa->setParameter(n, i->type()->parameter(n));
				else Messenger::warn("Can't use currently-assigned atom type to get interaction parameters for atom %i since it is not of the correct form (%s).", i->id()+1, VdwFunctions::functionData[i->type()->vdwForm()].name);
			}
			Messenger::print("Potential data for atom %i : %s %8.4f %8.4f %8.4f", i->id()+1, qPrintable(ffa->name()), ffa->parameter(0), ffa->parameter(1), ffa->charge());
		}
	}

	// Loop over bonds and angles in the system, creating terms for them in the forcefield
	// We will sum the distances involved in each distinct bond so we can write the average (unless requested not to)
	RefList<ForcefieldBound, DataPair<double,int> > bondData;
	for (Bond* b = targetModel()->bonds(); b != NULL; b = b->next)
	{
		Atom* i = b->atomI();
		Atom* j = b->atomJ();
		ForcefieldAtom* ffi = i->type();
		ForcefieldAtom* ffj = j->type();
		QString typeA = ffi ? ffi->name() : ElementMap::symbol(i);
		QString typeB = ffj ? ffj->name() : ElementMap::symbol(j);

		// Search the temporary forcefield for this particular bond, creating it if it doesn't exist
		ForcefieldBound* ffb = dummyFF.findBond(typeA, typeB);
		if (!ffb) ffb = dummyFF.addBond(BondFunctions::None, typeA, typeB);

		// If types are assigned and we find a suitable term in the associated forcefield, set these parameters in our ForcefieldBound
		if (ffi && ffj && ffi->parent())
		{
			// Set the form of the bond to 'harmonic' - doesn't really matter what we set it to, as long as it isn't 'none' for the purposes of a later check
			ffb->setBondForm(BondFunctions::Harmonic);

			ForcefieldBound* existing = ffi->parent()->findBond(ffi, ffj);
			if (existing)
			{
				if (existing->bondForm() == BondFunctions::Constraint) ffb->setParameter(BondFunctions::HarmonicEq, existing->parameter(BondFunctions::ConstraintEq));
				else if (existing->bondForm() == BondFunctions::Harmonic) ffb->setParameter(BondFunctions::HarmonicEq, existing->parameter(BondFunctions::HarmonicEq));
				else if (existing->bondForm() == BondFunctions::Morse) ffb->setParameter(BondFunctions::HarmonicEq, existing->parameter(BondFunctions::MorseEq));
				else ffb->setBondForm(BondFunctions::None);
			}
			
			if (ffb->bondForm() == BondFunctions::None) Messenger::warn("Bond between types %s-%s is not defined in the forcefield - will use geometry from model.", qPrintable(ffi->equivalent()), qPrintable(ffj->equivalent()));
		}

		// Find corresponding item in bondData, or create it if it doesn't exist
		RefListItem<ForcefieldBound, DataPair<double,int> >* ri = bondData.contains(ffb);
		if (!ri) ri = bondData.add(ffb);

		// Sum bond distance and increase count
		ri->data.data1() += targetModel()->distance(i, j);
		ri->data.data2()++;
	}

	// Write out accumulated bond data
	for (RefListItem<ForcefieldBound, DataPair<double,int> >* ri = bondData.first(); ri != NULL; ri = ri->next)
	{
		ForcefieldBound* ffb = ri->item;
		if (ffb->bondForm() == BondFunctions::None)
		{
			if (!fileParser_.writeLineF("bond %s %s %f", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), ri->data.data1() / double(ri->data.data2()))) return false;
		}
		else if (!fileParser_.writeLineF("bond %s %s %f", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), ffb->parameter(BondFunctions::HarmonicEq))) return false;
	}

	// Same for angles...
	RefList<ForcefieldBound, DataPair<double,int> > angleData;
	for (Atom* j = targetModel()->atoms(); j != NULL; j = j->next)
	{
		ForcefieldAtom* ffj = j->type();
		QString typeJ = ffj ? ffj->name() : ElementMap::symbol(j->element());

		for (RefListItem<Bond,int>* rij = j->bonds(); rij != NULL; rij = rij->next)
		{
			for (RefListItem<Bond,int>* rjk = rij->next; rjk != NULL; rjk = rjk->next)
			{
				Atom* i = rij->item->partner(j);
				Atom* k = rjk->item->partner(j);
				ForcefieldAtom* ffi = i->type();
				ForcefieldAtom* ffk = k->type();

				QString typeI = ffi ? ffi->name() : ElementMap::symbol(i->element());
				QString typeK = ffk ? ffk->name() : ElementMap::symbol(k->element());

				// Search the temporary forcefield for this particular bond, creating it if it doesn't exist
				ForcefieldBound* ffb = dummyFF.findAngle(typeI, typeJ, typeK);
				if (!ffb) ffb = dummyFF.addAngle(AngleFunctions::None, typeI, typeJ, typeK);

				// If types are assigned and we find a suitable term in the associated forcefield, set these parameters in our ForcefieldBound
				if (ffi && ffj && ffk && ffi->parent())
				{
					// Set the form of the angle to 'harmonic' - doesn't really matter what we set it to, as long as it isn't 'none' for the purposes of a later check
					ffb->setAngleForm(AngleFunctions::Harmonic);

					ForcefieldBound* existing = ffi->parent()->findAngle(ffi, ffj, ffk);
					if (existing)
					{
						if (existing->angleForm() == AngleFunctions::Harmonic) ffb->setParameter(AngleFunctions::HarmonicEq, existing->parameter(AngleFunctions::HarmonicEq));
						else if (existing->angleForm() == AngleFunctions::Cosine) ffb->setParameter(AngleFunctions::HarmonicEq, existing->parameter(AngleFunctions::CosineEq));
						else if (existing->angleForm() == AngleFunctions::HarmonicCosine) ffb->setParameter(AngleFunctions::HarmonicEq, existing->parameter(AngleFunctions::HarmonicCosineEq));
						else ffb->setAngleForm(AngleFunctions::None);
					}
					
					if (ffb->angleForm() == AngleFunctions::None) Messenger::warn("Angle between types %s-%s-%s is not defined in the forcefield, or does not specify an equilibrium angle - will use geometry from model.", qPrintable(ffi->equivalent()), qPrintable(ffj->equivalent()), qPrintable(ffj->equivalent()));
				}

				// Find corresponding item in angleData, or create it if it doesn't exist
				RefListItem<ForcefieldBound, DataPair<double,int> >* ri = angleData.contains(ffb);
				if (!ri) ri = angleData.add(ffb);

				// Sum bond distance and increase count
				ri->data.data1() += targetModel()->angle(i, j, k);
				ri->data.data2()++;
			}
		}
	}

	// Write out accumulated angle data
	for (RefListItem<ForcefieldBound, DataPair<double,int> >* ri = angleData.first(); ri != NULL; ri = ri->next)
	{
		ForcefieldBound* ffb = ri->item;
		if (ffb->angleForm() == AngleFunctions::None)
		{
			if (!fileParser_.writeLineF("angle %s %s %s %f", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), qPrintable(ffb->typeName(2)), ri->data.data1() / double(ri->data.data2()))) return false;
		}
		else if (!fileParser_.writeLineF("angle %s %s %s %f", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), qPrintable(ffb->typeName(2)), ffb->parameter(AngleFunctions::HarmonicEq))) return false;
	}

	// Write dihedral information
	if (pluginOptions_.value("writeDihedrals") == "true")
	{
		for (Bond* bjk = targetModel()->bonds(); bjk != NULL; bjk = bjk->next)
		{
			// Loop over other bonds at first terminus, excluding the central one 'bjk'
			for (RefListItem<Bond,int>* rij = bjk->atomI()->bonds(); rij != NULL; rij = rij->next)
			{
				Bond* bij = rij->item;
				if (bij == bjk) continue;

				// Loop over other bonds at second terminus, excluding the central one 'bjk'
				for (RefListItem<Bond,int>* rkl = bjk->atomJ()->bonds(); rkl != NULL; rkl = rkl->next)
				{
					Bond* bkl = rkl->item;
					if (bkl == bjk) continue;
					Atom* i = bij->partner(bjk->atomI());
					Atom* l = bkl->partner(bjk->atomJ());
					if (!fileParser_.writeLineF("dihedral %3i %3i %3i %3i %f", i->id()+1, bjk->atomI()->id()+1, bjk->atomJ()->id()+1, l->id()+1, targetModel()->torsion(i,bjk->atomI(),bjk->atomJ(),l))) return false;
				}
			}
		}
	}

	// Write rotational headgroup specifications
	// Need to know which atoms are in rings, so create a temporary pattern definition of the system
	Pattern dummyPattern;
	dummyPattern.setParent(targetModel());
	dummyPattern.initialise(0, 0, 1, targetModel()->nAtoms());
	dummyPattern.findRings();
	for (Bond* b = targetModel()->bonds(); b != NULL; b = b->next)
	{
		Atom* i = b->atomI();
		Atom* j = b->atomJ();

		// Are these atoms involved in the same ring?
		if (dummyPattern.atomsInRing(i,j)) continue;

		// Tree select using this bond as a reference, and find the least number of atoms to rotate
		targetModel()->selectNone(true);
		targetModel()->selectTree(i, true, false, b);
		RefList<Atom,int> fromI = targetModel()->selectedAtoms(true);
		targetModel()->selectNone(true);
		targetModel()->selectTree(j, true, false, b);
		RefList<Atom,int> fromJ = targetModel()->selectedAtoms(true);
		RefList<Atom,int>& rotationAtoms = (fromI.nItems() > fromJ.nItems() ? fromJ : fromI);
		if (rotationAtoms.nItems() <= 1) continue;
		
		if (!fileParser_.writeLineF("rot  %i  %i", i->id()+1, j->id()+1)) return false;
	}

	// Potential parameters (in kJ/mol)
	for (ForcefieldAtom* ffa = dummyFF.types()->next; ffa != NULL; ffa = ffa->next)
	{
		if (!fileParser_.writeLineF("potential %s %f %f %f %f %s", qPrintable(ffa->name()), prefs.convertEnergyTo(ffa->parameter(VdwFunctions::LjEpsilon), Prefs::KiloJoules), ffa->parameter(VdwFunctions::LjSigma), ElementMap::atomicMass(ffa->element()), ffa->charge(), ElementMap::symbol(ffa->element()))) return false;
	}

	// Control parameters
	if (!fileParser_.writeLineF("temperature %f", pluginOptions_.value("temp").toDouble())) return false;
	if (!fileParser_.writeLineF("vibtemp %f", pluginOptions_.value("vibTemp").toDouble())) return false;
	if (!fileParser_.writeLineF("angtemp %f", pluginOptions_.value("angTemp").toDouble())) return false;
	if (!fileParser_.writeLineF("dihtemp %f", pluginOptions_.value("dihTemp").toDouble())) return false;
	if (!fileParser_.writeLineF("density %f", 0.1)) return false;
	if (!fileParser_.writeLineF("ecoredcore %f %f", pluginOptions_.value("eCore").toDouble(), pluginOptions_.value("dCore").toDouble())) return false;

	return true;
}

// Import next partial data chunk
bool EPSRMolModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool EPSRMolModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool EPSRMolModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool EPSRMolModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool EPSRMolModelPlugin::hasExportOptions()
{
	return true;
}

// Show export options dialog
bool EPSRMolModelPlugin::showExportOptionsDialog()
{
	EPSRMolExportOptionsDialog optionsDialog(pluginOptions_);

	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}
 

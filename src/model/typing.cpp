/*
	*** Atom typing routines
	*** src/energy/typing.cpp
	Copyright T. Youngs 2007-2011

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

#include "base/elements.h"
#include "main/aten.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "base/pattern.h"
#include "classes/forcefieldatom.h"
#include "classes/forcefieldbound.h"

// Return number of unique (by name) forcefield atom types used over all patterns in the model
int Model::nUniqueForcefieldTypes() const
{
	return uniqueForcefieldTypes_.nItems();
}

// Return the list of forcefield types in the model
Refitem <ForcefieldAtom,int> *Model::uniqueForcefieldTypes()
{
	return uniqueForcefieldTypes_.first();
}

// Return the nth forcefield type interaction in the model
Refitem <ForcefieldAtom,int> *Model::uniqueForcefieldType(int i)
{
	return uniqueForcefieldTypes_[i];
}

// Return number of forcefield bond interactions in the model
int Model::nForcefieldBonds() const
{
	return forcefieldBonds_.nItems();
}

// Return the first in the list of forcefield bond interactions in the model
Refitem <ForcefieldBound,int> *Model::forcefieldBonds()
{
	return forcefieldBonds_.first();
}

// Return the nth forcefield bond interaction in the model
Refitem <ForcefieldBound,int> *Model::forcefieldBond(int i)
{
	return forcefieldBonds_[i];
}

// Return number of forcefield angle interactions in the model
int Model::nForcefieldAngles() const
{
	return forcefieldAngles_.nItems();
}

// Return the first in the list of forcefield angle interactions in the model
Refitem <ForcefieldBound,int> *Model::forcefieldAngles()
{
	return forcefieldAngles_.first();
}

// Return the nth forcefield angle interaction in the model
Refitem <ForcefieldBound,int> *Model::forcefieldAngle(int i)
{
	return forcefieldAngles_[i];
}

// Return number of forcefield torsion interactions in the model
int Model::nForcefieldTorsions() const
{
	return forcefieldTorsions_.nItems();
}

// Return the first in the list of forcefield torsion interactions in the model
Refitem <ForcefieldBound,int> *Model::forcefieldTorsions()
{
	return forcefieldTorsions_.first();
}

// Return the nth forcefield torsion interaction in the model
Refitem <ForcefieldBound,int> *Model::forcefieldTorsion(int i)
{
	return forcefieldTorsions_[i];
}

/*
Atom typing is performed in several steps.

0)	Current bonding pattern in the model / pattern is augmented
1) +-	Ring structures are located and stored
2) |	Atom hybridisations are assigned based only on bond types on individual atoms
3) |	Aromatic atoms are flagged as Atomtype::AromaticEnvironment, based on analysis of ring structures
4) +-	Typing rules from the forcefield are then applied to each atom in turn

*/

// Set type of specified atom
void Model::setAtomType(Atom *i, ForcefieldAtom *ffa, bool fixed)
{
	i->setType(ffa);
	i->setTypeFixed(fixed);
}

// Describe atoms in model
void Model::describeAtoms()
{
	// Locate ring structure and assign atom hybridisations in all patterns.
	msg.enter("Model::describeAtoms");
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next) p->describeAtoms();
	msg.exit("Model::describeAtoms");
}

// Type all atoms
bool Model::typeAll()
{
	// Perform forcefield typing on all patterns in the model.
	// Most routines here only use the first molecule in the pattern, so we must propagate the type info
	// to other molecules at the end.
	msg.enter("Model::typeAll");
	// Must have a valid pattern...
	createPatterns();
	if (!arePatternsValid())
	{
		msg.print("Atom typing cannot be performed without valid patterns.\nCheck pattern definition, atom ordering, and bond consistency between atoms, or add the default (1*N) pattern for a quick fix.\n");
		msg.exit("Model::typeAll");
		return FALSE;
	}

	// If no forcefield is set in this model, grab it from the current default forcefield
	if (forcefield_ == NULL)
	{
		if (aten.currentForcefield() != NULL) setForcefield(aten.currentForcefield());
		else
		{
			msg.print("Error: No forcefield set in model, and no default forcefield is available.\n");
			msg.exit("Model::typeAll");
			return FALSE;
		}
	}
	msg.print("Typing all patterns in model '%s' (associated forcefield is '%s')...\n", name_.get(), forcefield_->name());
	
	// Assign forcefield types to atoms
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		if (!p->typeAtoms())
		{
			msg.exit("Model::typeAll");
			return FALSE;
		}
		// Finally, propagate the data now contained in the initial molecule in each pattern to all other molecules
		p->propagateAtomtypes();
		p->propagateBondTypes();
		msg.print("Done.\n");
	}
	// Log change in the model
	changeLog.add(Log::Coordinates);
	msg.exit("Model::typeAll");
	return TRUE;
}

// Set atomtypes of selected atoms
void Model::selectionSetType(ForcefieldAtom *ffa, bool fixed)
{
	msg.enter("Pattern::selectionSetType");
	for (Refitem<Atom,int> *ri = selection_.first(); ri != NULL; ri = ri->next) setAtomType(ri->item, ffa, fixed);
	changeLog.add(Log::Coordinates);
	msg.exit("Pattern::selectionSetType");
}

// Remove typing from the model
void Model::removeTyping()
{
	// Remove all atom typing from the current model
	msg.enter("Model::removeTyping");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) setAtomType(i, NULL, FALSE);
	msg.exit("Model::removeTyping");
}

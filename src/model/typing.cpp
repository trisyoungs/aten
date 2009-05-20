/*
	*** Atom typing routines
	*** src/energy/typing.cpp
	Copyright T. Youngs 2007-2009

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

// Prevent Windows macro idiocy
#define NOMINMAX

#include "base/elements.h"
#include "main/aten.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "base/pattern.h"
#include "classes/forcefieldatom.h"
#include "classes/forcefieldbound.h"

// Return number of unique atom types in model
int Model::nUniqueTypes()
{
	return uniqueTypes_.nItems();
}

// Return the list of unique types in the model
ForcefieldAtom *Model::uniqueTypes()
{
	return uniqueTypes_.first();
}

// Return the nth unique type interaction in the model
ForcefieldAtom *Model::uniqueType(int i)
{
	return uniqueTypes_[i];
}

// Return number of unique bond interactions in the model
int Model::nUniqueBondTerms()
{
	return uniqueBondTerms_.nItems();
}

// Return the first in the list of unique bond interactions in the model
ForcefieldBound *Model::uniqueBondTerms()
{
	return uniqueBondTerms_.first();
}

// Return the nth unique bond interaction in the model
ForcefieldBound *Model::uniqueBondTerm(int i)
{
	return uniqueBondTerms_[i];
}

// Return number of unique angle interactions in the model
int Model::nUniqueAngleTerms()
{
	return uniqueAngleTerms_.nItems();
}

// Return the first in the list of unique angle interactions in the model
ForcefieldBound *Model::uniqueAngleTerms()
{
	return uniqueAngleTerms_.first();
}

// Return the nth unique angle interaction in the model
ForcefieldBound *Model::uniqueAngleTerm(int i)
{
	return uniqueAngleTerms_[i];
}

// Return number of unique torsion interactions in the model
int Model::nUniqueTorsionTerms()
{
	return uniqueTorsionTerms_.nItems();
}

// Return the first in the list of unique torsion interactions in the model
ForcefieldBound *Model::uniqueTorsionTerms()
{
	return uniqueTorsionTerms_.first();
}

// Return the nth unique torsion interaction in the model
ForcefieldBound *Model::uniqueTorsionTerm(int i)
{
	return uniqueTorsionTerms_[i];
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
void Model::setAtomtype(Atom *i, ForcefieldAtom *ffa, bool fixed)
{
	//static ForcefieldAtom *oldtype;
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
	autocreatePatterns();
	if (!arePatternsValid())
	{
		msg.print("Atom typing cannot be performed without valid patterns.\n Check pattern definition.\n");
		msg.exit("Model::typeAll");
		return FALSE;
	}
	// Describe the atoms / rings in the patterns
	describeAtoms();
	if (forcefield_ == NULL) msg.print("Typing all patterns in model '%s' (no forcefield associated -- using default)...\n", name_.get());
	else msg.print("Typing all patterns in model '%s' (associated forcefield is '%s')...\n", name_.get(), forcefield_->name());
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
	for (Atom *i = firstSelected(); i != NULL; i = i->nextSelected()) setAtomtype(i, ffa, fixed);
	changeLog.add(Log::Coordinates);
	msg.exit("Pattern::selectionSetType");
}

// Remove typing from the model
void Model::removeTyping()
{
	// Remove all atom typing from the current model
	msg.enter("Model::removeTyping");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) setAtomtype(i, NULL, FALSE);
	msg.exit("Model::removeTyping");
}

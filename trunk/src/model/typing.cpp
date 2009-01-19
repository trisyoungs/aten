/*
	*** Atom typing routines
	*** src/energy/typing.cpp
	Copyright T. Youngs 2007,2008

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

// Clear hybridisation data
void Pattern::clearHybrids()
{
	// Set all environment flags of the atoms in pattern to Atomtype::NoEnvironment
	msg.enter("Pattern::clearHybrids");
	Atom *i = firstAtom_;
	for (int n=0; n<nAtoms_; n++)
	{
		i->setEnvironment(Atom::NoEnvironment);
		i = i->next;
	}
	msg.exit("Pattern::clearHybrids");
}

// Assign hybridisation data
void Pattern::assignHybrids()
{
	// Assign hybridisation types to the atoms in this pattern.
	msg.enter("Pattern::assignHybrids");
	Atom *i = firstAtom_;
	for (int n=0; n<nAtoms_; n++)
	{
		// Set to no environment to begin with
		i->setEnvironment(Atom::NoEnvironment);
		// Work out the hybridisation based on the bond types connected to the atom.
		// We can increase the hybridisation at any point, but never decrease it.
		for (Refitem<Bond,int> *bref = i->bonds(); bref != NULL; bref = bref->next)
		{
			switch (bref->item->type())
			{
				case (Bond::Single):
					if (i->environment() < Atom::Sp3Environment) i->setEnvironment(Atom::Sp3Environment);
					break;
				case (Bond::Double):
					if (i->environment() < Atom::Sp2Environment) i->setEnvironment(Atom::Sp2Environment);
					break;
				case (Bond::Triple):
					if (i->environment() < Atom::SpEnvironment) i->setEnvironment(Atom::SpEnvironment);
					break;
			}
		}
		i = i->next;
	}
	msg.exit("Pattern::assignHybrids");
}

// Type atoms in pattern
bool Pattern::typeAtoms()
{
	// Assign atom types from the forcefield based on the typing rules supplied.
	// Since there may be more than one match for a given atom (when relaxed rules are used, e.g.
	// UFF) we find the best of the types available. If any one criterion doesn't match in the atom 
	// type description, we reject it. Otherwise, store the number of criteria that matched and only
	// accept a different atom type if we manage to match a complete set containing more rules.
	// Return FALSE if one or more atoms could not be typed
	msg.enter("Pattern::typeAtoms");
	int a, newmatch, bestmatch, nfailed;
	Atomtype *at;
	Atom *i;
	Forcefield *ff;
	ForcefieldAtom *ffa;
	bool result = TRUE;
	// Select the forcefield we're typing with. First, if this pattern doesn't have a specific ff, take the model's ff
	ff = forcefield_;
	if (ff == NULL)
	{
		// No forcefield associated to pattern - grab parent Model's
		ff = parent_->forcefield();
		if (ff == NULL)
		{
			msg.print("Typing pattern %s (using default forcefield)...", name());
			ff = aten.defaultForcefield();
		}
		else msg.print("Typing pattern %s (inheriting Model's forcefield '%s')...", name(), ff->name());
	}
	else msg.print("Typing pattern %s (using associated forcefield '%s')...", name(), ff->name());	
	if (ff == NULL)
	{	
		msg.print("Can't type pattern '%s' - no forcefield associated to pattern or model, and no default set.\n", name_.get());
		msg.exit("Pattern::typeAtoms");
		return FALSE;
	}
	// Loop over atoms in the pattern's molecule
	i = firstAtom_;
	nfailed = 0;
	for (a=0; a<nAtoms_; a++)
	{
		// Check to see if this atom type has been manually set
		if (i->hasFixedType())
		{
			i = i->next;
			continue;
		}
		msg.print(Messenger::Typing,"Pattern::typeAtoms : FFTyping atom number %i, element %s\n", a, elements().symbol(i->element()));
		bestmatch = 0;
		parent_->setAtomtype(i, NULL, FALSE);
		// Check for element 'XX' first
		if (i->element() == 0)
		{
			msg.print("Failed to type atom %i since it has no element type.\n",i->id()+1);
			nfailed ++;
			result = FALSE;
		}
		// Loop over forcefield atom types
		for (ffa = ff->types(); ffa != NULL; ffa = ffa->next)
		{
			// Grab next atomtype and reset tempi variables
			at = ffa->atomtype();
			// First, check element is the same, otherwise skip
			if (i->element() != at->characterElement()) continue;
			// See how well this ff description matches the environment of our atom 'i'
			msg.print(Messenger::Typing,"Pattern::typeAtoms : Matching type id %i\n",ffa->typeId());
			newmatch = at->matchAtom(i,&rings_,parent_,i);
			msg.print(Messenger::Typing,"Pattern::typeAtoms : ...Total match score for type %i = %i\n", ffa->typeId(), newmatch);
			if (newmatch > bestmatch)
			{
				// Better match found...
				bestmatch = newmatch;
				i->setType(ffa);
			}
		}
		if (i->type() == NULL)
		{
			msg.print("Failed to type atom - %s, id = %i, nbonds = %i.\n", elements().name(i), i->id()+1, i->nBonds());
			nfailed ++;
			result = FALSE;
		}
		else msg.print(Messenger::Typing,"Assigned forcefield type for atom is : %i (%s)\n", i->type(), i->type()->name());
		i = i->next;
	}
	// Print warning if we failed...
	if (nfailed != 0) msg.print("Failed to type %i atoms in pattern '%s'.\n", nfailed, name_.get());
	msg.exit("Pattern::typeAtoms");
	return result;
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

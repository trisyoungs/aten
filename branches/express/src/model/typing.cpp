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

#include "base/elements.h"
#include "base/master.h"
#include "model/model.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"
#include "classes/bond.h"
#include "classes/ring.h"

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

// Return number of unique bond interactions in the model
int Model::nUniqueBondTerms()
{
	return uniqueBondTerms_.nItems();
}

// Return the list of unique bond interactions in the model
ForcefieldBound *Model::uniqueBondTerms()
{
	return uniqueBondTerms_.first();
}

// Return number of unique angle interactions in the model
int Model::nUniqueAngleTerms()
{
	return uniqueAngleTerms_.nItems();
}

// Return the list of unique angle interactions in the model
ForcefieldBound *Model::uniqueAngleTerms()
{
	return uniqueAngleTerms_.first();
}

// Return number of unique torsion interactions in the model
int Model::nUniqueTorsionTerms()
{
	return uniqueTorsionTerms_.nItems();
}

// Return the list of unique torsion interactions in the model
ForcefieldBound *Model::uniqueTorsionTerms()
{
	return uniqueTorsionTerms_.first();
}

/*
Atom typing is performed in several steps.

0)	Current bonding pattern in the model / pattern is augmented
1) +-	Ring structures are located and stored
2) |	Atom hybridisations are assigned based only on bond types on individual atoms
3) |	Aromatic atoms are flagged as Atomtype::AromaticEnvironment, based on analysis of ring structures
4) +-	Typing rules from the forcefield are then applied to each atom in turn

*/

void printstuff(Pattern *p)
{
	Atom *i = p->firstAtom();
	for (int n=0; n<p->nAtoms(); n++)
	{
		msg(Debug::Verbose,"Atom %i, %s[%i], nbonds=%i, valency=%i, type=%s\n",n,elements.symbol(i),
			i->id(),i->nBonds(),elements.valency(i),Atomtype::atomEnvironment(i->environment()));
		i = i->next;
	}
}

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
	// Locate ring structures, augment bonding, and assign atom hybridisations in all patterns.
	dbgBegin(Debug::Calls,"Model::describeAtoms");
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		// 1) Locate ring structures
		p->findRings();
		// Augment bonding in model
		//p->augment();   TODO
		// 2) Reset atom environments
		p->clearHybrids();
		printstuff(p);
		// 3) Assign hybridisation types
		p->assignHybrids();
		printstuff(p);
		// 4) Go through the ring list and see if any are aromatic
		for (Ring *r = p->rings(); r != NULL; r = r->next) if (r->isAromatic()) r->setAromatic();
	}
	dbgEnd(Debug::Calls,"Model::describeAtoms");
}

// Type all atoms
bool Model::typeAll()
{
	// Perform forcefield typing on all patterns in the model.
	// Most routines here only use the first molecule in the pattern, so we must propagate the type info
	// to other molecules at the end.
	dbgBegin(Debug::Calls,"Model::typeAll");
	// Must have a valid pattern...
	autocreatePatterns();
	if (!arePatternsValid())
	{
		msg(Debug::None,"Atom typing cannot be performed without valid patterns.\n Check pattern definition.\n");
		dbgEnd(Debug::Calls,"Model::typeAll");
		return FALSE;
	}
	// Describe the atoms / rings in the patterns
	describeAtoms();
	if (forcefield_ == NULL) msg(Debug::None,"Typing all patterns in model '%s' (no forcefield associated -- using default)...\n", name_.get());
	else msg(Debug::None,"Typing all patterns in model '%s' (associated forcefield is '%s')...\n", name_.get(), forcefield_->name());
	// Assign forcefield types to atoms
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		if (p->forcefield() == NULL) msg(Debug::None,"Typing pattern %s (inheriting parent's forcefield)...", p->name());
		else msg(Debug::None,"Typing pattern %s (associated forcefield is '%s')...", p->name(), p->forcefield()->name());
		if (!p->typeAtoms())
		{
			dbgEnd(Debug::Calls,"Model::typeAll");
			return FALSE;
		}
		// Finally, propagate the data now contained in the initial molecule in each pattern to all other molecules
		p->propagateAtomtypes();
		p->propagateBondTypes();
		msg(Debug::None,"Done.\n");
	}
	// Log change in the model
	logChange(Change::CoordinateLog);
	dbgEnd(Debug::Calls,"Model::typeAll");
	return TRUE;
}

// Clear hybridisation data
void Pattern::clearHybrids()
{
	// Set all environment flags of the atoms in pattern to Atomtype::NoEnvironment
	dbgBegin(Debug::Calls,"Pattern::clearHybrids");
	Atom *i = firstAtom_;
	for (int n=0; n<nAtoms_; n++)
	{
		i->setEnvironment(Atomtype::NoEnvironment);
		i = i->next;
	}
	dbgEnd(Debug::Calls,"Pattern::clearHybrids");
}

// Assign hybridisation data
void Pattern::assignHybrids()
{
	// Assign hybridisation types to the atoms in this pattern.
	dbgBegin(Debug::Calls,"Pattern::assignHybrids");
	Atom *i = firstAtom_;
	for (int n=0; n<nAtoms_; n++)
	{
		// Set to AE_UNBOUND to begin with
		i->setEnvironment(Atomtype::NoEnvironment);
		// Work out the hybridisation based on the bond types connected to the atom.
		// We can increase the hybridisation at any point, but never decrease it.
		for (Refitem<Bond,int> *bref = i->bonds(); bref != NULL; bref = bref->next)
		{
			switch (bref->item->order())
			{
				case (Bond::Single):
					if (i->environment() < Atomtype::Sp3Environment) i->setEnvironment(Atomtype::Sp3Environment);
					break;
				case (Bond::Double):
					if (i->environment() < Atomtype::Sp3Environment) i->setEnvironment(Atomtype::Sp2Environment);
					break;
				case (Bond::Triple):
					if (i->environment() < Atomtype::SpEnvironment) i->setEnvironment(Atomtype::SpEnvironment);
					break;
			}
		}
		i = i->next;
	}
	dbgEnd(Debug::Calls,"Pattern::assignHybrids");
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
	dbgBegin(Debug::Calls,"Pattern::typeAtoms");
	int a, n, newmatch, bestmatch, nfailed;
	Atomtype *at;
	Atom *i;
	ForcefieldAtom *ffa;
	bool result = TRUE;
	// Select the forcefield we're typing with. First, if this pattern doesn't have a specific ff, take the model's ff
	if (forcefield_ == NULL) forcefield_ = parent_->forcefield();
	// If there is still no forcefield, set the defaultff
	if (forcefield_ == NULL) forcefield_ = master.defaultForcefield();
	if (forcefield_ == NULL)
	{	
		msg(Debug::None,"Can't type pattern '%s' - no FF associated to pattern or model, and no default set.\n",name_.get());
		dbgEnd(Debug::Calls,"Pattern::typeAtoms");
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
		msg(Debug::Typing,"Pattern::typeAtoms : FFTyping atom number %i, element %s\n",a,elements.symbol(i->element()));
		bestmatch = 0;
		parent_->setAtomtype(i, NULL, FALSE);
		// Check for element 'XX' first
		if (i->element() == 0)
		{
			msg(Debug::None,"Failed to type atom %i since it has no element type.\n",i->id()+1);
			nfailed ++;
			result = FALSE;
		}
		// Loop over forcefield atom types
		for (ffa = forcefield_->types(); ffa != NULL; ffa = ffa->next)
		{
			// Grab next atomtype and reset tempi variables
			at = ffa->atomtype();
			// First, check element is the same, otherwise skip
			if (i->element() != at->characterElement()) continue;
			// See how well this ff description matches the environment of our atom 'i'
			msg(Debug::Typing,"Pattern::typeAtoms : Matching type id %i\n",ffa->typeId());
			newmatch = at->matchAtom(i,&rings_,parent_,i);
			msg(Debug::Typing,"Pattern::typeAtoms : ...Total match score for type %i = %i\n",ffa->typeId(),newmatch);
			if (newmatch > bestmatch)
			{
				// Better match found...
				bestmatch = newmatch;
				i->setType(ffa);
			}
		}
		msg(Debug::Typing,"Pattern::typeAtoms : FFType for atom is : %i\n",i->type());
		if (i->type() == NULL)
		{
			msg(Debug::None,"Failed to type atom - %s, id = %i, nbonds = %i.\n",elements.name(i),i->id()+1,i->nBonds());
			nfailed ++;
			result = FALSE;
		}
		i = i->next;
	}
	// Print warning if we failed...
	if (nfailed != 0) msg(Debug::None,"Failed to type %i atoms in pattern '%s'.\n",nfailed,name_.get());
	dbgEnd(Debug::Calls,"Pattern::typeAtoms");
	return result;
}

// Set atomtypes of selected atoms
void Model::selectionSetType(ForcefieldAtom *ffa, bool fixed)
{
	dbgBegin(Debug::Calls,"Pattern::selectionSetType");
	for (Atom *i = firstSelected(); i != NULL; i = i->nextSelected()) setAtomtype(i, ffa, fixed);
	logChange(Change::CoordinateLog);
	dbgEnd(Debug::Calls,"Pattern::selectionSetType");
}

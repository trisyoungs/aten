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

/*
Atom typing is performed in several steps.

0)	Current bonding pattern in the model / pattern is augmented
1) +-	Ring structures are located and stored
2) |	Atom hybridisations are assigned based only on bond types on individual atoms
3) |	Aromatic atoms are flagged as AE_AROMATIC, based on analysis of ring structures
4) +-	Typing rules from the forcefield are then applied to each atom in turn

*/

void printstuff(Pattern *p)
{
	Atom *i = p->firstAtom();
	for (int n=0; n<p->nAtoms(); n++)
	{
		msg(DM_VERBOSE,"Atom %i, %s[%i], nbonds=%i, valency=%i, type=%s\n",n,elements.symbol(i),
			i->id(),i->nBonds(),elements.valency(i),text_from_AE(i->env()));
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
	dbgBegin(DM_CALLS,"Model::describeAtoms");
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		// 1) Locate ring structures
		p->findRings();
		// Augment bonding in model
		//p->augment();   TODO TGAY
		// 2) Reset atom environments
		p->clearHybrids();
		printstuff(p);
		// 3) Assign hybridisation types
		p->assignHybrids();
		printstuff(p);
		// 4) Go through the ring list and see if any are aromatic
		for (Ring *r = p->rings(); r != NULL; r = r->next) if (r->isAromatic()) r->setAromatic();
	}
	dbgEnd(DM_CALLS,"Model::describeAtoms");
}

// Type all atoms
bool Model::typeAll()
{
	// Perform forcefield typing on all patterns in the model.
	// Most routines here only use the first molecule in the pattern, so we must propagate the type info
	// to other molecules at the end.
	dbgBegin(DM_CALLS,"Model::typeAll");
	// Must have a valid pattern...
	autocreatePatterns();
	if (!arePatternsValid())
	{
		msg(DM_NONE,"Atom typing cannot be performed without a valid pattern.\n Check pattern definition.\n");
		dbgEnd(DM_CALLS,"Model::typeAll");
		return FALSE;
	}
	// Describe the atoms / rings in the patterns
	describeAtoms();
	// Assign forcefield types to atoms
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		msg(DM_NONE,"Typing pattern %s...",p->name());
		if (!p->typeAtoms())
		{
			dbgEnd(DM_CALLS,"Model::typeAll");
			return FALSE;
		}
		// Finally, propagate the data now contained in the initial molecule in each pattern to all other molecules
		p->propagateAtomtypes();
		p->propagateBondTypes();
		msg(DM_NONE,"Done.\n");
	}
	dbgEnd(DM_CALLS,"Model::typeAll");
	return TRUE;
}

// Clear hybridisation data
void Pattern::clearHybrids()
{
	// Set all environment flags of the atoms in pattern to AE_UNSPECIFIED
	dbgBegin(DM_CALLS,"Pattern::clearHybrids");
	Atom *i = firstAtom_;
	for (int n=0; n<nAtoms_; n++)
	{
		i->setEnv(AE_UNSPECIFIED);
		i = i->next;
	}
	dbgEnd(DM_CALLS,"Pattern::clearHybrids");
}

// Assign hybridisation data
void Pattern::assignHybrids()
{
	// Assign hybridisation types to the atoms in this pattern.
	dbgBegin(DM_CALLS,"Pattern::assignHybrids");
	Atom *i = firstAtom_;
	for (int n=0; n<nAtoms_; n++)
	{
		// Set to AE_UNBOUND to begin with
		i->setEnv(AE_UNSPECIFIED);
		// Work out the hybridisation based on the bond types connected to the atom.
		// We can increase the hybridisation at any point, but never decrease it.
		for (Refitem<Bond,int> *bref = i->bonds(); bref != NULL; bref = bref->next)
		{
			switch (bref->item->order())
			{
				case (BT_SINGLE):
					if (i->env() < AE_SP3) i->setEnv(AE_SP3);
					break;
				case (BT_DOUBLE):
					if (i->env() < AE_SP2) i->setEnv(AE_SP2);
					break;
				case (BT_TRIPLE):
					if (i->env() < AE_SP) i->setEnv(AE_SP);
					break;
			}
		}
		i = i->next;
	}
	dbgEnd(DM_CALLS,"Pattern::assignHybrids");
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
	dbgBegin(DM_CALLS,"Pattern::typeAtoms");
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
		msg(DM_NONE,"Can't type pattern '%s' - no FF associated to pattern or model, and no default set.\n",name_.get());
		dbgEnd(DM_CALLS,"Pattern::typeAtoms");
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
		msg(DM_TYPING,"Pattern::typeAtoms : FFTyping atom number %i, element %s\n",a,elements.symbol(i->element()));
		bestmatch = 0;
		parent_->setAtomtype(i, NULL, FALSE);
		// Check for element 'XX' first
		if (i->element() == 0)
		{
			msg(DM_NONE,"Failed to type atom %i since it has no element type.\n",i->id()+1);
			nfailed ++;
			result = FALSE;
		}
		// Loop over forcefield atom types
		for (ffa = forcefield_->types(); ffa != NULL; ffa = ffa->next)
		{
			// Grab next atomtype and reset tempi variables
			at = ffa->atomType();
			// First, check element is the same, otherwise skip
			if (i->element() != at->characterElement()) continue;
			// See how well this ff description matches the environment of our atom 'i'
			msg(DM_TYPING,"Pattern::typeAtoms : Matching type id %i\n",ffa->typeId());
			newmatch = at->matchAtom(i,&rings_,parent_,i);
			msg(DM_TYPING,"Pattern::typeAtoms : ...Total match score for type %i = %i\n",ffa->typeId(),newmatch);
			if (newmatch > bestmatch)
			{
				// Better match found...
				bestmatch = newmatch;
				i->setType(ffa);
			}
		}
		msg(DM_TYPING,"Pattern::typeAtoms : FFType for atom is : %i\n",i->type());
		if (i->type() == NULL)
		{
			msg(DM_NONE,"Failed to type atom - %s, id = %i, nbonds = %i.\n",elements.name(i),i->id()+1,i->nBonds());
			nfailed ++;
			result = FALSE;
		}
		i = i->next;
	}
	// Print warning if we failed...
	if (nfailed != 0) msg(DM_NONE,"Failed to type %i atoms in pattern '%s'.\n",nfailed,name_.get());
	dbgEnd(DM_CALLS,"Pattern::typeAtoms");
	return result;
}

// Set atomtypes of selected atoms
void Model::selectionSetType(ForcefieldAtom *ffa, bool fixed)
{
	dbgBegin(DM_CALLS,"Pattern::selectionSetType");
	for (Atom *i = firstSelected(); i != NULL; i = i->nextSelected()) setAtomtype(i, ffa, fixed);
	dbgEnd(DM_CALLS,"Pattern::selectionSetType");
}

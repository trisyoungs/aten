/*
	*** Atom ring
	*** src/classes/ring.cpp
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

#include "model/model.h"
#include "classes/ring.h"
#include "classes/bond.h"
#include "base/elements.h"

// Constructor
Ring::Ring()
{
	// Private variables
	requestedSize_ = -1;
	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructors
Ring::~Ring()
{
	atoms_.clear();
}

// Circular list browsing
Refitem<Atom,int> *Ring::getNext(Refitem<Atom,int> *ri) {
	return (ri->next == NULL ? atoms_.first() : ri->next);
}

Refitem<Atom,int> *Ring::getPrev(Refitem<Atom,int> *ri) {
	return (ri->prev == NULL ? atoms_.last() : ri->prev);
}

// Return atom reflist first item
Refitem<Atom,int> *Ring::firstAtom()
{
	return atoms_.first();
}

// Return atom reflist last item
Refitem<Atom,int> *Ring::lastAtom()
{
	return atoms_.last();
}

// Return size of atom reflist
int Ring::nAtoms()
{
	return atoms_.nItems();
}

// Set requested size
void Ring::setRequestedSize(int size)
{
	requestedSize_ = size;
}

// Return requested size
int Ring::requestedSize()
{
	return requestedSize_;
}

// Search ring list for specified atom
bool Ring::containsAtom(Atom *i)
{
	return atoms_.search(i);
}

// Add atom to ring
bool Ring::addAtom(Atom *i)
{
	dbgBegin(DM_CALLS,"Ring::addAtom");
	// Size check
	if (atoms_.nItems() == requestedSize_)
	{
		dbgEnd(DM_CALLS,"Ring::addAtom");
		return FALSE;
	}
	// Duplicate check
	if (atoms_.search(i) != NULL)
	{
		dbgEnd(DM_CALLS,"Ring::addAtom");
		return FALSE;
	}
	// Append a ringatomx to the list, pointing to atom i
	// Store atom ID in the Refitem's data variable
	atoms_.add(i,i->id());
	dbgEnd(DM_CALLS,"Ring::addAtom");
	return TRUE;
}

// Remove the specified refitem from the find
void Ring::removeAtom(Refitem<Atom,int> *ri)
{
	atoms_.remove(ri);
}

// Is ring aromatic
bool Ring::isAromatic()
{
	// Determine whether the ring is aromatic.
	dbgBegin(DM_CALLS,"Ring::isAromatic");
	// SP2 atom types should have already been defined, so use these to determine aromaticity.
	// Use a set of exceptions for heteroatoms_.such as N and O...
	int okatoms= 0;
	bool exitearly = FALSE, result = FALSE;
	Refitem<Atom,int> *ra = atoms_.first();
	while (ra != NULL)
	{
		switch (ra->item->element())
		{
			case (6):	// Carbon
				// Accept nothing less than an AE_SP2. If its not AE_SP" we can break early.
				if (ra->item->isEnv(AE_SP2)) okatoms++;
				else exitearly = TRUE;
				break;
			case (7):	// Nitrogen
				// Add nitrogens anyway, but if they're not flagged as AE_SP2 we must change it's
				// state afterwards, provided the ring checks out to be aromatic. (set_aromatic())
				okatoms++;
				break;
			case (8):	// Oxygen
				// Same as for nitrogen
				okatoms++;
				break;
			case (16):	// Sulfur
				// Again, see O and N
				okatoms++;
				break;
		}
		ra = ra->next;
		if (exitearly) break;
	}
	// Now we just check 'okatoms if it equals the number of atoms_.in the ring, then it should be aromatic!
	if (okatoms== atoms_.nItems()) result = TRUE;
	dbgEnd(DM_CALLS,"Ring::isAromatic");
	return result;
}

// Set aromatic
void Ring::setAromatic()
{
	// Set the environment flags of the constituent atoms_.of the ring to AE_AROMATIC.
	dbgBegin(DM_CALLS,"Ring::setAromatic");
	for (Refitem<Atom,int> *ra = atoms_.first(); ra != NULL; ra = ra->next)
		ra->item->setEnv(AE_AROMATIC);
	dbgEnd(DM_CALLS,"Ring::setAromatic");
}

// Finalise ring
void Ring::finish()
{
	// Perform some finishing tasks on the list
	dbgBegin(DM_CALLS,"Ring::finish");
	Refitem<Atom,int> *ra, *temp, *lowid;
	// Make the list head point to the atom with the lowest id
	if (atoms_.nItems() == 0)
	{	
		printf("No atoms_.in ring - can't finalise!\n");
		dbgEnd(DM_CALLS,"Ring::finish");
		return;
	}
	// First, find the lowest atomid
	ra = atoms_.first();
	lowid = ra;
	while (ra != NULL)
	{
		if (ra->data < lowid->data) lowid = ra;
		ra = ra->next;
	}
	// Make this atom (with lowest id) the head of the list
	ra = atoms_.first();
	while (ra != lowid)
	{
		atoms_.moveHeadToTail();
		ra = atoms_.first();
	}
	// Set the bond type flags in each ringatomx
	ra = atoms_.first();
	while (ra != NULL)
	{
		// Search the bond list of this atom for the next atom in the list
		Refitem<Bond,int> *bref = ra->item->bonds();
		while (bref != NULL)
		{
			if (bref->item->partner(ra->item) == getNext(ra)->item) ra->data = bref->item->order();
			bref = bref->next;
		}
		ra = ra->next;
	}
	dbgEnd(DM_CALLS,"Ring::finish");
}

// Copy ring
void Ring::copy(Ring *source)
{
	// Copy the data in source to the current ring.
	atoms_.clear();
	Refitem<Atom,int> *ra = source->atoms_.first();
	while (ra != NULL)
	{
		bool success = addAtom(ra->item);
		ra = ra->next;
	}
}

// Print
void Ring::print()
{
	// Print out the data of the ring.
	// Beware, since if it has been 'finished' it will be a circular list
	msg(DM_VERBOSE,"Ring has %i atoms_.: ",atoms_.nItems());
	Refitem<Atom,int> *ra = atoms_.first();
	while (ra != NULL)
	{
		msg(DM_VERBOSE,"%s(%i),",elements.symbol(ra->item),ra->data);
		//printf("%s(%i),",elements.el[ra->i->el].symbol.c_str(),ra->i->tempi);
		ra = ra->next;
	}
	msg(DM_VERBOSE,"\n");
}

// Clear atoms_.in reflist
void Ring::clear()
{
	atoms_.clear();
}

// Add atoms_.to Reflist
void Ring::addAtomsToReflist(Reflist<Atom,int> *rlist, Atom *i)
{
	// Add all atoms_.in the ring 'r' to the list, excluding the atom 'i'
	for (Refitem<Atom,int> *ra = atoms_.first(); ra != NULL; ra = ra->next) if (ra->item != i) rlist->add(ra->item);
}

// Augment ring atom
void Ring::augmentAtom(Refitem<Atom,int> *refatom, Model *parent)
{
	dbgBegin(DM_CALLS,"Ring::augmentAtom");
	// Assumes current bond order differences are in i->tempi
	Atom *i, *j;
	i = refatom->item;
	if (i->tempi < 0)
	{
		// Atom has fewer bonds than expected, so try to augment within ring.
		j = getNext(refatom)->item;
		if (j->tempi < 0) parent->augmentBond(i,j,+1);
		if (i->tempi != 0)
		{
			j = getPrev(refatom)->item;
			if (j->tempi < 0) parent->augmentBond(i,j,+1);
		}
	}
	else if (i->tempi > 0)
	{
		// Atom has more bonds than expected, so try to de-augment within ring.
		j = getNext(refatom)->item;
		if (j->tempi > 0) parent->augmentBond(i,j,-1);
		if (i->tempi != 0)
		{
			j = getPrev(refatom)->item;
			if (j->tempi > 0) parent->augmentBond(i,j,-1);
		}
	}
	dbgEnd(DM_CALLS,"Ring::augmentAtom");
}

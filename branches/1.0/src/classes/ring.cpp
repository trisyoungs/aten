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

#include "classes/ring.h"
#include "base/atom.h"
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

// Destructor
Ring::~Ring()
{
	atoms_.clear();
}

/*
// Referenced atoms / bonds
*/

// Circular list browsing
Refitem<Atom,int> *Ring::getNext(Refitem<Atom,int> *ri)
{
	return (ri->next == NULL ? atoms_.first() : ri->next);
}

Refitem<Atom,int> *Ring::getPrev(Refitem<Atom,int> *ri)
{
	return (ri->prev == NULL ? atoms_.last() : ri->prev);
}

// Return first referenced atom
Refitem<Atom,int> *Ring::atoms()
{
	return atoms_.first();
}

// Return last referenced atom
Refitem<Atom,int> *Ring::lastAtom()
{
	return atoms_.last();
}

// Return number of referenced atoms
int Ring::nAtoms()
{
	return atoms_.nItems();
}

// Return first referenced bond
Refitem<Bond,Bond::BondType> *Ring::bonds()
{
	return bonds_.first();
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
	msg.enter("Ring::addAtom");
	// Size check
	if (atoms_.nItems() == requestedSize_)
	{
		msg.exit("Ring::addAtom");
		return FALSE;
	}
	// Duplicate check
	if (atoms_.search(i) != NULL)
	{
		msg.exit("Ring::addAtom");
		return FALSE;
	}
	// Append a ringatom to the list, pointing to atom i
	// Store atom ID in the Refitem's data variable
	atoms_.add(i,i->id());
	msg.exit("Ring::addAtom");
	return TRUE;
}

// Remove the specified refitem from the find
void Ring::removeAtom(Refitem<Atom,int> *ri)
{
	atoms_.remove(ri);
}

// Return the total bond order penalty of atoms in the ring
int Ring::totalBondOrderPenalty()
{
	int result = 0;
	for (Refitem<Atom,int> *ri = atoms_.first(); ri != NULL; ri = ri->next) result += elements().bondOrderPenalty(ri->item, ri->item->totalBondOrder()/2);
	return result;
}

// Store current bond types for referenced bonds
void Ring::storeBondTypes()
{
	for (Refitem<Bond,Bond::BondType> *rb = bonds_.first(); rb != NULL; rb = rb->next) rb->data = rb->item->type();
}

// Recall stored bond orders for referenced bonds
void Ring::recallBondTypes()
{
	for (Refitem<Bond,Bond::BondType> *rb = bonds_.first(); rb != NULL; rb = rb->next) rb->item->setType(rb->data);
}

// Set aromatic
void Ring::setAromatic()
{
	msg.enter("Ring::setAromatic");
	// Set atom environments to be Atomtype::AromaticEnvironment
	for (Refitem<Atom,int> *ra = atoms_.first(); ra != NULL; ra = ra->next)
		ra->item->setEnvironment(Atom::AromaticEnvironment);
	// Set bonds to be Bond::Aromatic
	for (Refitem<Bond,Bond::BondType> *rb = bonds_.first(); rb != NULL; rb = rb->next)
		rb->item->setType(Bond::Aromatic);
	printf("Oooh - an aromatic ring\n");
	msg.exit("Ring::setAromatic");
}

/*
// Methods
*/

// Finalise ring
void Ring::finalise()
{
	// Perform some finishing tasks on the list
	msg.enter("Ring::finalise");
	Refitem<Atom,int> *ra, *lowid;
	// Make the list head point to the atom with the lowest id
	if (atoms_.nItems() == 0)
	{	
		printf("No atoms in ring - can't finalise!\n");
		msg.exit("Ring::finalise");
		return;
	}
	// First, find the lowest atomid in the ring
	lowid = atoms_.first();
	for (ra = atoms_.first(); ra != NULL; ra = ra->next)
		if (ra->item->id() < lowid->item->id()) lowid = ra;
	// Make this atom (with lowest id) the head of the list
	ra = atoms_.first();
	while (ra != lowid)
	{
		atoms_.moveHeadToTail();
		ra = atoms_.first();
	}
	// Construct reference list of bonds
	bonds_.clear();
	for (ra = atoms_.first(); ra != NULL; ra = ra->next)
	{
		Bond *b = ra->item->findBond(getNext(ra)->item);
		if (b == NULL) printf("Odd internal error - couldn't find bond between atoms in ring.\n");
		else bonds_.add(b);
		// Search the bond list of this atom for the next atom in the list
		//for (Refitem<Bond,int> *bref = ra->item->bonds(); bref != NULL; bref = bref->next)
		//	if (bref->item->partner(ra->item) == getNext(ra)->item) ra->data = bref->item->order();
	}
	msg.exit("Ring::finalise");
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
	msg.print(Messenger::Verbose,"Ring has %i atoms: ",atoms_.nItems());
	Refitem<Atom,int> *ra = atoms_.first();
	while (ra != NULL)
	{
		msg.print(Messenger::Verbose,"%s(%i),", elements().symbol(ra->item),ra->data);
		//printf("%s(%i),",elements.el[ra->i->el].symbol.c_str(),ra->i->tempi);
		ra = ra->next;
	}
	msg.print(Messenger::Verbose,"\n");
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

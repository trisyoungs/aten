/*
	*** Atom ring
	*** src/base/ring.cpp
	Copyright T. Youngs 2007-2018

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

#include "base/ring.h"
#include "base/pattern.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Ring types
const char* RingTypes[Ring::nRingTypes] = { "any", "aliphatic", "non-aromatic", "aromatic" };
const char* Ring::ringType(Ring::RingType rt)
{
	return RingTypes[rt];
}

// Constructor
Ring::Ring() : ListItem<Ring>()
{
	// Private variables
	parent_ = NULL;
	requestedSize_ = -1;
}

// Destructor
Ring::~Ring()
{
	atoms_.clear();
}

// Ring equality
bool Ring::operator==(Ring &r) const
{
	// Check ring sizes first
	if (atoms_.nItems() != r.atoms_.nItems()) return false;
	// Search for first atom of ring 'r' in this ring's atom list
	RefListItem<Atom,int>* commonatom, *ri, *rj;
	for (commonatom = atoms_.first(); commonatom != NULL; commonatom = commonatom->next) if (commonatom->item == r.atoms_.first()->item) break;
	if (commonatom == NULL) return false;
	// The atom exists in both rings, so check all atoms....
	ri = r.atoms_.first();
	rj = ri;
	for (int i=0; i<atoms_.nItems(); ++i)
	{
		// Clockwise list traversal of supplied ring
		if (ri != NULL)
		{
			if (ri->item == commonatom->item) ri = r.getNext(ri);
			else ri = NULL;
		}
		// Clockwise list traversal of supplied ring
		if (rj != NULL)
		{
			if (rj->item == commonatom->item) rj = r.getPrev(rj);
			else rj = NULL;
		}
		// Step our local ring pointer on one...
		commonatom = getNext(commonatom);
	}
	if ((ri == NULL) && (rj == NULL)) return false;
	return true;
}

// Set pattern parent
void Ring::setParent(Pattern* p)
{
	parent_ = p;
}

// Return parent pattern
Pattern* Ring::parent()
{
	return parent_;
}

/*
// Referenced atoms / bonds
*/

// Circular list browsing
RefListItem<Atom,int>* Ring::getNext(RefListItem<Atom,int>* ri) const
{
	return (ri->next == NULL ? atoms_.first() : ri->next);
}

RefListItem<Atom,int>* Ring::getPrev(RefListItem<Atom,int>* ri) const
{
	return (ri->prev == NULL ? atoms_.last() : ri->prev);
}

// Return first referenced atom
RefListItem<Atom,int>* Ring::atoms() const
{
	return atoms_.first();
}

// Return array of referenced atoms
RefListItem<Atom,int>** Ring::atomArray()
{
	return atoms_.array();
}

// Return last referenced atom
RefListItem<Atom,int>* Ring::lastAtom() const
{
	return atoms_.last();
}

// Return number of referenced atoms
int Ring::nAtoms() const
{
	return atoms_.nItems();
}

// Return first referenced bond
RefListItem<Bond,Bond::BondType> *Ring::bonds() const
{
	return bonds_.first();
}

// Set requested size
void Ring::setRequestedSize(int size)
{
	requestedSize_ = size;
}

// Return requested size
int Ring::requestedSize() const
{
	return requestedSize_;
}

// Search ring list for specified atom
bool Ring::containsAtom(Atom* i)
{
	return atoms_.contains(i);
}


// Add atom to ring
bool Ring::addAtom(Atom* i)
{
	Messenger::enter("Ring::addAtom");
	// Size check
	if (atoms_.nItems() == requestedSize_)
	{
		Messenger::exit("Ring::addAtom");
		return false;
	}
	// Duplicate check
	if (atoms_.contains(i) != NULL)
	{
		Messenger::exit("Ring::addAtom");
		return false;
	}
	// Append a ringatom to the list, pointing to atom i, storing the atom ID in the RefListItem's data variable
	atoms_.add(i,i->id());
	Messenger::exit("Ring::addAtom");
	return true;
}

// Remove the specified refitem from the find
void Ring::removeAtom(RefListItem<Atom,int>* ri)
{
	atoms_.remove(ri);
}

// Return the total bond order penalty of atoms in the ring
int Ring::totalBondOrderPenalty()
{
	int result = 0;
	for (RefListItem<Atom,int>* ri = atoms_.first(); ri != NULL; ri = ri->next) result += ElementMap::bondOrderPenalty(ri->item, ri->item->totalBondOrder()/2);
	return result;
}

// Store current bond types for referenced bonds
void Ring::storeBondTypes()
{
	for (RefListItem<Bond,Bond::BondType> *rb = bonds_.first(); rb != NULL; rb = rb->next) rb->data = rb->item->type();
}

// Recall stored bond orders for referenced bonds
void Ring::recallBondTypes()
{
	for (RefListItem<Bond,Bond::BondType> *rb = bonds_.first(); rb != NULL; rb = rb->next) rb->item->setType(rb->data);
}

// Detect ring type based on atom hybridicities and bonds
void Ring::detectType()
{
	Messenger::enter("Ring::detectType");
	int nsingle = 0, ndouble = 0, nother = 0, naromatic = 0;
	Bond::BondType lasttype, thistype;
	bool alternating = true;
	// Get numbers of single/double bonds, and whether they alternate around the ring
	for (RefListItem<Bond,Bond::BondType> *rb = bonds_.first(); rb != NULL; rb = rb->next)
	{
		thistype = rb->item->type();
		if (thistype == Bond::Single) nsingle ++;
		else if (thistype == Bond::Aromatic) naromatic ++;
		else if (thistype == Bond::Double) ndouble ++;
		else
		{
			nother ++;
			alternating = false;
			continue;
		}
		// Check previous bond for 'alternateness'
		lasttype = (rb == bonds_.first() ? bonds_.last()->item->type() : rb->prev->item->type());
		if ((lasttype == Bond::Single) && (thistype != Bond::Double)) alternating = false;
		else if ((lasttype == Bond::Double) && (thistype != Bond::Single)) alternating = false;
	}

	// Set type
	if (nsingle == bonds_.nItems()) type_ = Ring::AliphaticRing;
	else if (naromatic == bonds_.nItems()) type_ = Ring::AromaticRing;
	else if ((bonds_.nItems()%2) == 0)
	{
		// For rings with an even number of atoms, the bonds *must* alternate in type
		if (nsingle == ndouble)
		{
			type_ = (alternating ? Ring::AromaticRing : Ring::NonAromaticRing);
		}
		else
		{
			// Inequality between number of singla and double bonds - any aromatic bonds hanging around?
			if (abs(nsingle-ndouble) <= (naromatic*2)) type_ = Ring::AromaticRing;
			else type_ = Ring::NonAromaticRing;
		}
	}
	else
	{
		int nhetero = 0, group;
		bool failed = false;
		Bond::BondType bt1, bt2;
		// For rings with an odd number of atoms, adjacent single bonds may use a medial heteroatom to grant aromaticity
		for (RefListItem<Atom,int>* ra = atoms_.first(); ra != NULL; ra = ra->next)
		{
			group = ElementMap::group(ra->item);
			// If its a heteroatom there's a chance. If not, we're done
			if ((group == 15) || (group == 16))
			{
				// Check for single bonds either side
				bt1 = ra->item->findBond(getPrev(ra)->item)->type();
				bt2 = ra->item->findBond(getNext(ra)->item)->type();
				// TODO Need to check against the bondorder penalty
			//bondOrderPenalty(Atom* i, int bo);
				if ((bt1 == Bond::Single) && (bt2 == Bond::Single)) nhetero ++;
				else failed = true;
			}
			else failed = true;
			if (failed) break;
		}
		if (failed) type_ = Ring::NonAromaticRing;
		else
		{
			// Get total number of pi electrons now...
			int npi = (nhetero + ndouble) * 2;	//TODO
			type_ = Ring::AromaticRing;	
		}
	}
	// Set aromatic bonds if so detected
	if (type_ == Ring::AromaticRing)
	{
		for (RefListItem<Atom,int>* ra = atoms_.first(); ra != NULL; ra = ra->next) ra->item->setEnvironment(Atom::AromaticEnvironment);
		for (RefListItem<Bond,Bond::BondType> *rb = bonds_.first(); rb != NULL; rb = rb->next) 
		{
			if (rb->item->type() != Bond::Aromatic) parent_->parent()->changeBond(rb->item, Bond::Aromatic);
// 			rb->data = Bond::Aromatic;
		}
	}
	Messenger::exit("Ring::detectType");
}

// Return type of ring
Ring::RingType Ring::type() const
{
	return type_;
}

/*
 * Methods
 */

// Finalise ring
void Ring::finalise()
{
	// Perform some finishing tasks on the list
	Messenger::enter("Ring::finalise");
	RefListItem<Atom,int>* ra, *lowid;
	// Make the list head point to the atom with the lowest id
	if (atoms_.nItems() == 0)
	{	
		printf("No atoms in ring - can't finalise!\n");
		Messenger::exit("Ring::finalise");
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
		Bond* b = ra->item->findBond(getNext(ra)->item);
		if (b == NULL) printf("Odd internal error - couldn't find bond between atoms in ring.\n");
		else bonds_.add(b);
		// Search the bond list of this atom for the next atom in the list
		//for (RefListItem<Bond,int>* bref = ra->item->bonds(); bref != NULL; bref = bref->next)
		//	if (bref->item->partner(ra->item) == getNext(ra)->item) ra->data = bref->item->order();
	}
	Messenger::exit("Ring::finalise");
}

// Copy ring
void Ring::copy(Ring* source)
{
	// Copy the data in source to the current ring.
	atoms_.clear();
	for (RefListItem<Atom,int>* ra = source->atoms_.first(); ra != NULL; ra= ra->next) addAtom(ra->item);
}

// Print
void Ring::print() const
{
	// Print out the data of the ring.
	Messenger::print(Messenger::Verbose, "Ring has %i atoms: ",atoms_.nItems());
	for (RefListItem<Atom,int>* ra = atoms_.first(); ra != NULL; ra = ra->next)
		Messenger::print(Messenger::Verbose, "%s(%i),", ElementMap::symbol(ra->item),ra->data);
	Messenger::print(Messenger::Verbose, "");
}

// Clear atoms_.in reflist
void Ring::clear()
{
	atoms_.clear();
}

// Add atoms_.to RefList
void Ring::addAtomsToRefList(RefList<Atom,int>* rlist, Atom* i)
{
	// Add all atoms_.in the ring 'r' to the list, excluding the atom 'i'
	for (RefListItem<Atom,int>* ra = atoms_.first(); ra != NULL; ra = ra->next) if (ra->item != i) rlist->add(ra->item);
}

/*
	*** Atom ring
	*** src/classes/ring.h
	Copyright T. Youngs 2007-2010

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

#ifndef ATEN_RING_H
#define ATEN_RING_H

#include "base/atom.h"
#include "base/bond.h"

// Ring
class Ring
{
	public:
	// Constructor / Destructor
	Ring();
	~Ring();
	// List pointers
	Ring *prev, *next;
	// Circular list browsing
	Refitem<Atom,int> *getNext(Refitem<Atom,int> *ri) const;
	Refitem<Atom,int> *getPrev(Refitem<Atom,int> *ri) const;
	// Ring type
	enum RingType { AnyRing, AliphaticRing, NonAromaticRing, AromaticRing, nRingTypes };
	static const char *ringType(Ring::RingType);
	// Ring equality
	bool operator==(Ring &r) const;

	/*
	// Constituent Atoms / Bonds
	*/
	private:
	// List of referenced atoms
	Reflist<Atom,int> atoms_;
	// List of referenced bonds (and their types)
	Reflist<Bond,Bond::BondType> bonds_;
	// Requested size of ring when ring searching
	int requestedSize_;
	// Type of ring
	RingType type_;

	public:
	// Return first referenced atom
	Refitem<Atom,int> *atoms() const;
	// Return last referenced atom
	Refitem<Atom,int> *lastAtom() const;
	// Return first referenced bond
	Refitem<Bond,Bond::BondType> *bonds() const;
	// Return size of atom reflist
	int nAtoms() const;
	// Search ring list for specified atom
	bool containsAtom(Atom *i);
	// Set requested size
	void setRequestedSize(int size);
	// Return requested size
	int requestedSize() const;
	// Append the atom 'i' to the end of the list. Returns FALSE is this exceeds MAXRINGSIZE
	bool addAtom(Atom*);
	// Remove the specified refitem from the find
	void removeAtom(Refitem<Atom,int>*);
	// Store current bond types for referenced bonds
	void storeBondTypes();
	// Recall stored bond orders for referenced bonds
	void recallBondTypes();
	// Return the total bond order penalty of atoms in the ring
	int totalBondOrderPenalty();
	// Detect ring type based on atom hybridicities and bonds
	void detectType();
	// Return type of ring
	RingType type();


	/*
	// Methods
	*/
	public:
	// Duplicate the data (and list) in the specified ring
	void copy(Ring*);
	// Prepare the structure ready for use after atoms have been added
	void finalise();
	// Print out the data contained in the structure
	void print() const;
	// Add atoms in ring to supplied reflist
	void addAtomsToReflist(Reflist<Atom,int>*, Atom*);
	// Clear atoms in reflist
	void clear();
};

#endif

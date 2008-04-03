/*
	*** Atom ring
	*** src/classes/ring.h
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

#ifndef ATEN_RING
#define ATEN_RING

#include "classes/atom.h"

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
	Refitem<Atom,int> *getNext(Refitem<Atom,int> *ri);
	Refitem<Atom,int> *getPrev(Refitem<Atom,int> *ri);

	private:
	// List of referenced atoms
	Reflist<Atom,int> atoms_;
	// Requested size of ring when ring searching
	int requestedSize_;

	/*
	// Methods
	*/
	public:
	// Return atom reflist first item
	Refitem<Atom,int> *firstAtom();
	// Return atom reflist last item
	Refitem<Atom,int> *lastAtom();
	// Return size of atom reflist
	int nAtoms();
	// Search ring list for specified atom
	bool containsAtom(Atom *i);
	// Set requested size
	void setRequestedSize(int size);
	// Return requested size
	int requestedSize();
	// Append the atom 'i' to the end of the list. Returns FALSE is this exceeds MAXRINGSIZE
	bool addAtom(Atom*);
	// Remove the specified refitem from the find
	void removeAtom(Refitem<Atom,int>*);
	// Duplicate the data (and list) in the specified ring
	void copy(Ring*);
	// 'Finalize' the list ready for use
	void finish();
	// Returns TRUE if the ring is aromatic, FALSE if otherwise
	bool isAromatic();
	// Sets the atom environments of the atoms in the ring to AtomEnvironment::AromaticEnvironment
	void setAromatic();
	// Augments the specified atom within the ring
	void augmentAtom(Refitem<Atom,int>*, Model *parent);
	// Comparison operator between two rings
	//bool same_as(ring*);
	// Print out the data contained in the structure
	void print();
	// Add atoms in ring to supplied reflist
	void addAtomsToReflist(Reflist<Atom,int>*, Atom*);
	// Clear atoms in reflist
	void clear();
};

#endif

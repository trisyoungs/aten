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

#ifndef H_RING
#define H_RING

#include "classes/atom.h"

// Ring
class ring
{
	public:
	// Constructor / Destructor
	ring();
	~ring();
	// List pointers
	ring *prev, *next;

	public:
	// List of referenced atoms
	reflist<atom,int> atoms;
	// Requested size of ring when ring searching
	int requested_size;
	// Circular list browsing
	refitem<atom,int> *get_next(refitem<atom,int> *ri) { return (ri->next == NULL ? atoms.first() : ri->next); }
	refitem<atom,int> *get_prev(refitem<atom,int> *ri) { return (ri->prev == NULL ? atoms.last() : ri->prev); }

	/*
	// Methods
	*/
	public:
	// Append the atom 'i' to the end of the list. Returns FALSE is this exceeds MAXRINGSIZE
	bool add_atom(atom*);
	// Duplicate the data (and list) in the specified ring
	void copy(ring*);
	// 'Finalize' the list ready for use
	void finish();
	// Returns TRUE if the ring is aromatic, FALSE if otherwise
	bool is_aromatic();
	// Sets the atom environments of the atoms in the ring to AE_AROMATIC
	void set_aromatic();
	// Augments the specified atom within the ring
	void augment_atom(refitem<atom,int>*, model *parent);
	// Comparison operator between two rings
	//bool same_as(ring*);
	// Print out the data contained in the structure
	void print();
	void add_atoms_to_reflist(reflist<atom,int>*, atom*);
};

#endif

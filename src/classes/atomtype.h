/*
	*** Forcefield atom type
	*** src/classes/atomtype.h

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

#ifndef H_ATOMTYPE_H
#define H_ATOMTYPE_H

#include "classes/bond.h"
#include "templates/list.h"
#include "templates/reflist.h"

// Atom environment
enum atom_env { AE_UNSPECIFIED=-1, AE_UNBOUND, AE_SP3, AE_SP2, AE_SP, AE_AROMATIC, AE_NITEMS };
const char *text_from_AE(atom_env);

// Geometries about atomic centres
enum atom_geom { AG_NONE, AG_FREEORSINGLE, AG_LINEAR, AG_TSHAPE, AG_TRIGPLANAR, AG_TETRAHEDRAL, AG_SQPLANAR, AG_TRIGBIPYRAMID, AG_OCTAHEDRAL, AG_NITEMS };
atom_geom AG_from_text(const char *);
const char *text_from_AG(atom_geom);

// Forward declarations
class atomtype;
class ffatom;
class atom;
class model;
class ring;
class forcefield;

// Ring type
class ringtype
{
	// Substructure definition of a separate list of atoms in a ring
	public:
	// Constructor / Destructor
	ringtype();
	~ringtype();
	// List pointers
	ringtype *prev, *next;

	private:
	// Number of atoms in ring
	int ringsize;
	// Optional specification of atoms in ring
	list<atomtype> ringatoms;
	// Add data to the structure from the supplied string
	void expand(const char*, forcefield*);
	// Print the information contained in the structure
	void print();
	// Friend classes
	friend class atomtype;
};

// Atom type
class atomtype
{
	public:
	// Constructor / Destructor
	atomtype();
	~atomtype();
	// List pointers, used in bound atom list and list of atoms in rings
	atomtype *prev, *next;
	// Element (only used in head of atomtype tree, specifies the absolute element that the type describes)
	int el; 				
	// Add data to the structure from the supplied string
	void expand(const char*, forcefield*);
	// See if this type matches any atoms in the list provided
	int match_in_list(reflist<atom>*, list<ring>*, model*);
	// See if this type matches the atom (+ ring data of pattern)
	int match_atom(atom*, list<ring>*, model*);
	// Print the information contained in the structure
	void print();

	/*
	// High-level descriptors
	*/
	private:
	// Environment of atom 
	atom_env env;
	// Geometry of bonding about atom
	atom_geom geom;
	// Required oxidation state (99 for don't mind)
	int os;
	// List of atoms to which this atom must be bound
	list<atomtype> boundlist;
	// List of rings that this atom must be a member of
	list<ringtype> ringlist;
	// Number of bond connections the atom should have
	int nbonds;
	// Specifies atom must *not* be in a cycle of any type
	bool acyclic;
	
	/*
	// Bound atoms descriptors
	*/
	private:
	// List of elements that the bound atom may be (can be empty for 'unspecified' [*])
	int *allowed_el;
	// List of ffatom types that the bound atom may be
	reflist<ffatom> allowed_types;
	// Number of elements specified in bound_el[]
	int nallowedel;
	// Number of times this match is required
	int nrepeat;
	// Type of bond to bound (parent) atom
	bond_type bound_bond;

	public:
	// Expand the allowed_el array with the element string provided
	void set_elements(const char*, forcefield*);
};

#endif

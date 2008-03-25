/*
	*** Forcefield atom type
	*** src/classes/atomtype.h
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

#ifndef ATEN_ATOMTYPE_H
#define ATEN_ATOMTYPE_H

#include "classes/bond.h"
#include "templates/list.h"
#include "templates/reflist.h"

// Atom typing commands
enum AtomtypeCommand { ATC_SP, ATC_SP2, ATC_SP3, ATC_AROMATIC, ATC_RING, ATC_NORING, ATC_NBONDS, ATC_BOND, ATC_REPEAT, ATC_OS, ATC_NHYDROGENS, ATC_NITEMS };
AtomtypeCommand ATC_from_text(const char*);

// Ring typing commands
enum RingTypeCommand { RTC_SIZE, RTC_REPEAT, RTC_NOTSELF, RTC_NITEMS };
RingTypeCommand RTC_from_text(const char*);

// Atom environment
enum AtomEnv { AE_UNSPECIFIED, AE_NOBONDS, AE_SP3, AE_SP2, AE_SP, AE_AROMATIC, AE_NITEMS };
const char *text_from_AE(AtomEnv);

// Geometries about atomic centres
enum AtomGeometry { AG_UNSPECIFIED, AG_UNBOUND, AG_ONEBOND, AG_LINEAR, AG_TSHAPE, AG_TRIGPLANAR, AG_TETRAHEDRAL, AG_SQPLANAR, AG_TRIGBIPYRAMID, AG_OCTAHEDRAL, AG_NITEMS };
AtomGeometry AG_from_text(const char*);
const char *text_from_AG(AtomGeometry);

// Forward declarations
class Atomtype;
class ForcefieldAtom;
class Atom;
class Model;
class Ring;
class Forcefield;

// Ring type
class RingType
{
	// Substructure definition of a separate list of atoms in a ring
	public:
	// Constructor
	RingType();
	// List pointers
	RingType *prev, *next;

	private:
	// Number of atoms in ring
	int nAtoms_;
	// Optional specification of atoms in ring
	List<Atomtype> ringAtoms_;
	// Add data to the structure from the supplied string
	void expand(const char *commands, Forcefield *parentff, ForcefieldAtom *parent);
	// Number of times this match is required
	int nRepeat_;
	// Print the information contained in the structure
	void print();
	// Flag that the owner bound atom (if any) should not itself appear in the ring
	bool selfAbsent_;
	// Friend classes
	friend class Atomtype;
};

// Atom type
class Atomtype
{
	public:
	// Constructor / Destructor
	Atomtype();
	~Atomtype();
	// List pointers, used in bound atom list and list of atoms in rings
	Atomtype *prev, *next;

	/*
	// Character
	*/
	private:
	// Character element (i.e. the element that the matching atom must be)
	int characterElement_;

	public:
	// Set character element
	void setCharacterElement(int el);
	// Return character element
	int characterElement();
	// Add data to the structure from the supplied string
	void expand(const char *commands, Forcefield *parentff, ForcefieldAtom *parent);
	// See if this type matches any atoms in the list provided
	int matchInList(Reflist<Atom,int>*, List<Ring>*, Model*, Atom*);
	// See if this type matches the atom (+ ring data of pattern)
	int matchAtom(Atom*, List<Ring>*, Model*, Atom*);
	// Print the information contained in the structure
	void print();

	/*
	// High-level descriptors
	*/
	private:
	// Environment of atom
	AtomEnv env_;
	// Geometry of bonding about atom
	AtomGeometry geometry_;
	// Required oxidation state (99 for don't mind)
	int os_;
	// List of atoms to which this atom must be bound
	List<Atomtype> boundList_;
	// List of rings that this atom must be a member of
	List<RingType> ringList_;
	// Number of bond connections the atom should have
	int nBonds_;
	// Specifies atom must *not* be in a cycle of any type
	bool acyclic_;
	
	/*
	// Bound atoms descriptors
	*/
	private:
	// Specifies number of attached hydrogens
	int nHydrogen_;
	// List of elements that the bound atom may be (can be empty for 'unspecified' [*])
	int *allowedElements_;
	// List of ForcefieldAtom types that the bound atom may be
	Reflist<ForcefieldAtom,int> allowedTypes_;
	// Number of elements specified in bound_el[]
	int nAllowedElements_;
	// Number of times this match is required
	int nRepeat_;
	// Type of bond to bound (parent) atom
	Bond::BondType boundBond_;
	
	public:
	// Expand the allowedElements_ array with the element string provided
	void setElements(const char*, Forcefield*);
};

#endif

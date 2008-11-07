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

#include "base/atom.h"
#include "base/bond.h"
#include "templates/list.h"
#include "templates/reflist.h"

// Forward declarations
class Atomtype;
class ForcefieldAtom;
class Atom;
class Model;
class Ring;
class Forcefield;

// Ring type
class Ringtype
{
	// Substructure definition of a separate list of atoms in a ring
	public:
	// Constructor
	Ringtype();
	// List pointers
	Ringtype *prev, *next;
	// Ring typing commands
	enum RingtypeCommand { SizeCommand, RepeatCommand, NotSelfCommand, nRingtypeCommands };
	RingtypeCommand ringtypeCommand(const char*);

	private:
	// Number of atoms in ring
	int nAtoms_;
	// Optional specification of atoms in ring
	List<Atomtype> ringAtoms_;
	// Add data to the structure from the supplied string
	bool expand(const char *commands, Forcefield *parentff, ForcefieldAtom *parent);
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
	// Atom typing commands
	enum AtomtypeCommand { SpCommand, Sp2Command, Sp3Command, AromaticCommand, RingCommand, NoRingCommand, NBondsCommand, BondCommand, RepeatCommand, OxidationStateCommand, NHydrogensCommand, nAtomtypeCommands };
	static AtomtypeCommand atomtypeCommand(const char*);

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
	bool expand(const char *commands, Forcefield *parentff, ForcefieldAtom *parent);
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
	Atom::AtomEnvironment environment_;
	// Geometry of bonding about atom
	Atom::AtomGeometry geometry_;
	// Required oxidation state (99 for don't mind)
	short int os_;
	// List of atoms to which this atom must be bound
	List<Atomtype> boundList_;
	// List of rings that this atom must be a member of
	List<Ringtype> ringList_;
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
	bool setElements(const char*, Forcefield*);
	// Set the bound bond type
	void setBoundBond(Bond::BondType bt);
};

#endif

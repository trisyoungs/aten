/*
	*** Clipboard
	*** src/classes/clipboard.h
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

#ifndef ATEN_CLIPBOARD_H
#define ATEN_CLIPBOARD_H

#include "base/atom.h"
#include "base/bond.h"
#include "templates/list.h"

// Forward declarations
class Pattern;
class Model;
class Clipatom;

// Clipboard Atom
class Clipatom : public Atom
{
	public:
	// Constructor
	Clipatom();
	// Get the next Clipatom in the list
	Clipatom* getNext();

	/*
	// Atom Pointer
	*/
	private:
	// Atom pointer to original atom (before any paste operation is called on the clipboard) or newly-pasted Atom thereafter
	Atom *atomPointer_;

	public:
	// Set old atom pointer
	void setAtomPointer(Atom *i);
	// Returns the atom pointer of the cloned atom
	Atom *atomPointer();
};

// Clipboard Bond
class Clipbond
{
	public:
	// Constructor
	Clipbond();
	// List pointer
	Clipbond *next, *prev;

	private:
	// Pointers to both atoms involved in the bond
	Clipatom *atomI_, *atomJ_;
	// Bond type
	Bond::BondType type_;

	public:
	// Set clipatoms for bond
	void setAtoms(Clipatom *i, Clipatom *j);
	// Return first clipatom in bond
	Clipatom *atomI();
	// Return second clipatom in bond
	Clipatom *atomJ();
	// Set bond type
	void setType(Bond::BondType bt);
	// Return bond of bond
	Bond::BondType type();
	// Returns the partner of the specified atom in the bond structure
	//Clipatom *partner(Clipatom *i);
};

// Clipboard
class Clipboard
{
	/*
	// Atoms
	*/
	private:
	// Delete an atom from the Clipboards atom list
	void deleteAtom(Clipatom*);
	// List of copied atoms
	List<Clipatom> atoms_;

	public:
	// Clear the contents of the Clipboard
	void clear();
	// After copying, fix the internal IDs to describe bonding
	void fixBondIds();
	// Copy specified atom to Clipboard
	void copyAtom(Atom*);
	// Return number of atoms in Clipboard
	int nAtoms();
	// Return list of copied atoms
	Clipatom *atoms();

	/*
	// Bonds
	*/
	private:
	// List of bonds copied
	List<Clipbond> bonds_;
	// Copy bonds for atoms in the atomlist
	void copyBonds();
	// For bonds bound to Clipatom* (atomi|j) set bondi|j to atom*
	//void setNewBondPointers(Clipatom*, Atom*);

	public:
	// Check for presence of bond in list
	bool hasBond(int ii, int jj); 

	/*
	// Model
	*/
	private:
	// Paste bonds for newly pasted atoms
	void pasteBonds(Model*);

	public:
	// Copy all atoms in the specified model to the Clipboard
	void copySelection(Model *m);
	// Copy all marked atoms in the specified model
	void copyMarked(Model *m);
	// Copy all atoms in the specified model to the Clipboard
	void copyAll(Model*);
	// Cut atom selection from specified model
	void cutSelection(Model*);
	// Paste Clipboard contents into the model
	void pasteToModel(Model*, bool selectpasted = TRUE);
	// Paste Clipboard contents into specified pattern
	void pasteToPattern(Model*, Pattern*);
	// Paste Clipboard contents into specified config / pattern
	void pasteToModel(Model*, Pattern*, int);
	// Paste Clipboard contents to model at a translated position
	void pasteToModel(Model*, Vec3<double>);

	/*
	// Clipboard Transformations
	*/
	public:
	// Translate clipped atoms by supplied vector
	void translate(const Vec3<double>&);
};

#endif

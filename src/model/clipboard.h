/*
	*** Clipboard
	*** src/base/clipboard.h
	Copyright T. Youngs 2007-2017

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
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Pattern;
class Model;
class ClipAtom;

// Clipboard Atom
class ClipAtom : public ListItem<ClipAtom>
{
	public:
	// Constructor
	ClipAtom();

	/*
	 * Atom Data
	 */
	private:
	// Atom information
	Atom atom_; 
	// Atom pointer to original atom (before any paste operation is called on the clipboard) or newly-pasted Atom thereafter
	Atom* atomPointer_;

	public:
	// Copy atom information and store original pointer
	void set(Atom* i);
	// Set atom pointer
	void setAtomPointer(Atom* i);
	// Return the original/pasted atom pointer of the cloned atom
	Atom* atomPointer();
	// Return the new atom data structure
	Atom& atom();
};

// Clipboard Bond
class ClipBond : public ListItem<ClipBond>
{
	public:
	// Constructor
	ClipBond();

	private:
	// Pointers to both atoms involved in the bond
	ClipAtom* atomI_, *atomJ_;
	// Bond type
	Bond::BondType type_;

	public:
	// Set ClipAtoms for bond
	void setAtoms(ClipAtom* i, ClipAtom* j);
	// Return first ClipAtom in bond
	ClipAtom* atomI();
	// Return second ClipAtom in bond
	ClipAtom* atomJ();
	// Set bond type
	void setType(Bond::BondType bt);
	// Return bond of bond
	Bond::BondType type();
	// Returns the partner of the specified atom in the bond structure
	//ClipAtom* partner(ClipAtom* i);
};

// Clipboard
class Clipboard
{
	/*
	 * Atoms
	 */
	private:
	// Delete an atom from the Clipboards atom list
	void deleteAtom(ClipAtom*);
	// List of copied atoms
	List<ClipAtom> atoms_;

	public:
	// Clear the contents of the Clipboard
	void clear();
	// After copying, fix the internal IDs to describe bonding
	void fixBondIds();
	// Copy atom to clipboard, inserting it into the current list in the correct (original) atom ID position
	void copyAtom(Atom* i);
	// Renumber copied atoms
	void renumberAtoms();
	// Return number of atoms in Clipboard
	int nAtoms();
	// Return list of copied atoms
	ClipAtom* atoms();

	
	/*
	 * Bonds
	 */
	private:
	// List of bonds copied
	List<ClipBond> bonds_;
	// Copy bonds for atoms in the atomlist
	void copyBonds();
	// For bonds bound to ClipAtom* (atomi|j) set bondi|j to atom*
	//void setNewBondPointers(ClipAtom*, Atom*);

	public:
	// Check for presence of bond in list
	bool hasBond(int ii, int jj);
	// Return number of bonds stored in clipboard
	int nBonds();


	/*
	 * Model
	 */
	private:
	// Paste bonds for newly pasted atoms
	void pasteBonds(Model* target);

	public:
	// Copy all selected atoms in the specified model to the Clipboard
	void copySelection(Model* m, bool quiet = false);
	// Copy all marked atoms in the specified model
	void copyMarked(Model* m);
	// Copy all atoms in the specified model to the Clipboard
	void copyAll(Model* m, bool quiet = false);
	// Cut atom selection from specified model
	void cutSelection(Model* m);
	// Paste Clipboard contents into the model
	void pasteToModel(Model* targetModel, bool selectPasted = true);
	// Paste Clipboard contents into specified pattern
	void pasteToPattern(Model* targetModel, Pattern* targetPattern);
	// Paste Clipboard contents into specified config / pattern
	void pasteToModel(Model* targetModel, Pattern* targetPattern, int mol);
	// Paste Clipboard contents to model at a translated position
	void pasteToModel(Model* targetModel, Vec3<double> translation);

	
	/*
	 * Clipboard Transformations
	 */
	public:
	// Translate clipped atoms by supplied vector
	void translate(const Vec3< double >& translation);
};

ATEN_END_NAMESPACE

#endif

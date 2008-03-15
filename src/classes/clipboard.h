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

#include "classes/atom.h"
#include "templates/list.h"

// Forward declarations
class pattern;
class model;
class clipatom;

// Clipboard Bond
class clipbond : public linkbond
{
	private:
	// Pointers to atoms in clipatom list which make up the bond
	clipatom *clipi, *clipj;
	public:
	// Constructor
	clipbond();
	// Destructor
	~clipbond();
	// Sets the pointer clipi
	void set_clipi(clipatom *i) { clipi = i; }
	// Returns the pointer clipi
	clipatom *get_clipi() { return clipi; }
	// Sets the pointer clipj
	void set_clipj(clipatom *j) { clipj = j; }
	// Returns the pointer clipj
	clipatom *get_clipj() { return clipj; }
	// Return next clipbond in list
	clipbond *get_next() { return (next == NULL ? NULL : (clipbond*) next); }
};

// Clipboard Atom
class clipatom : public atom
{
	public:
	// Constructor / Destructor
	clipatom();
	~clipatom();
	// Get the next clipatom in the list
	clipatom* get_next() { return (next == NULL ? NULL : (clipatom*) next); }

	/*
	// New / Old Pointers
	*/
	private:
	// Atom pointer (newly pastedp atom)
	atom *oldptr;

	public:
	// Set old atom pointer
	void set_oldptr(atom *i) { oldptr = i; }
	// Returns the atom pointer of the cloned atom
	atom *get_oldptr() { return oldptr; }
};

// Clipboard
class clipboard
{
	public:
	// Constructor / Destructor
	clipboard();
	~clipboard();

	/*
	// Atoms
	*/
	private:
	// Delete an atom from the clipboards atom list
	void delete_atom(clipatom*);
	// List of copied atoms
	list<clipatom> atoms;

	public:
	// Clear the contents of the clipboard
	void clear();
	// After copying, fix the internal IDs to describe bonding
	void fix_bond_ids();
	// Copy specified atom to clipboard
	void copy_atom(atom*);
	// Return number of atoms in clipboard
	int get_natoms() { return atoms.size(); }
	// Return list of copied atoms
	clipatom *get_atoms() { return atoms.first(); }

	/*
	// Bonds
	*/
	private:
	// List of bonds copied
	list<clipbond> bonds;
	// Copy bonds for atoms in the atomlist
	void copy_bonds_for_atoms();
	// For bonds bound to clipatom* (clipi|j) set bondi|j to atom*
	void bonds_set_newptr(clipatom*, atom*);

	public:
	// Check for presence of bond in list
	bool has_bond(int ii, int jj); 

	/*
	// Model
	*/
	private:
	// Paste bonds for newly pasted atoms
	void paste_bonds(model*);

	public:
	// Copy all atoms in the specified model to the clipboard
	void copy_selection(model*);
	// Copy all atoms in the specified model to the clipboard
	void copy_all(model*);
	// Cut atom selection from specified model
	void cut_selection(model*);
	// Paste clipboard contents into the model
	void paste_to_model(model*, bool selectpasted = TRUE);
	// Paste clipboard contents into specified pattern
	void paste_to_pattern(model*, pattern*);
	// Paste clipboard contents into specified config / pattern
	void paste_to_model(model*, pattern*, int);
	// Paste clipboard contents to model at a translated position
	void paste_to_model(model*, vec3<double>);

	/*
	// Clipboard Transformations
	*/
	public:
	// Translate clipped atoms by supplied vector
	void translate(const vec3<double>&);
};

#endif

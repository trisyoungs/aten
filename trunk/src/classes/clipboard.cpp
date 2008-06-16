/*
	*** Clipboard
	*** src/classes/clipboard.cpp
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

#include "classes/clipboard.h"
#include "classes/pattern.h"
#include "model/model.h"
#include "base/debug.h"

// Constructors
Clipatom::Clipatom()
{
	// Private variables
	atomPointer_ = NULL;
}

/*
// Clipatom functions
*/

// Get the next Clipatom in the list
Clipatom* Clipatom::getNext()
{
	return (next == NULL ? NULL : (Clipatom*) next);
}

// Set old atom pointer
void Clipatom::setAtomPointer(Atom *i)
{
	atomPointer_ = i;
}

// Returns the atom pointer of the cloned atom
Atom *Clipatom::atomPointer()
{
	return atomPointer_;
}

/*
// Clipbond functions
*/

Clipbond::Clipbond()
{
	// Private variables
	atomI_ = NULL;
	atomJ_ = NULL;
	// Public variables
	next = NULL;
	prev = NULL;
}

// Set atoms for bond
void Clipbond::setAtoms(Clipatom *i, Clipatom *j)
{
	atomI_ = i;
	atomJ_ = j;
}

// Set bond order
void Clipbond::setOrder(Bond::BondType bt)
{
	order_ = bt;
}

// Return order of bond
Bond::BondType Clipbond::order()
{
	return order_;
}

// Return first atom in bond
Clipatom *Clipbond::atomI()
{
	return atomI_;
}

// Return second atom in bond
Clipatom *Clipbond::atomJ()
{
	return atomJ_;
}

/*
// Clipboard functions
*/

// Return number of atoms in Clipboard
int Clipboard::nAtoms()
{
	return atoms_.nItems();
}

// Return list of copied atoms
Clipatom *Clipboard::atoms()
{
	return atoms_.first();
}

// Copy atom to clipboard
void Clipboard::copyAtom(Atom *i)
{
	dbgBegin(Debug::Calls,"Clipboard::copyAtom");
        // Initialise the new clipatom
	Clipatom *newatom = atoms_.add();
	newatom->copy(i);
	newatom->setAtomPointer(i);
	newatom->setId(atoms_.nItems()-1);
	dbgEnd(Debug::Calls,"Clipboard::copyAtom");
}

// Empty clipboard
void Clipboard::clear()
{
	// Clear the list of atoms on the clipboard
	dbgBegin(Debug::Calls,"Clipboard::clear");
	atoms_.clear();
	bonds_.clear();
	dbgEnd(Debug::Calls,"Clipboard::clear");
}

/* Replace pointers in bond list
void Clipboard::setNewBondPointers(Clipatom *clipptr, Atom *newptr)
{
	dbgBegin(Debug::Calls,"Clipboard::setNewBondPointers");
	// Go through bond list searching for clipatom in clipi or clipj. Set associated bondi/bondj to newptr.
	for (Linkbond *b = bonds_.first(); b != NULL; b = b->next)
	{
		if (b->atomI() == clipptr) b->setAtomI(newptr);
		else if (b->atomJ() == clipptr) b->setAtomJ(newptr);
	}
	printf("Linkbonds List after setNewBondPointers:\n");
	for (Linkbond *b = bonds_.first(); b != NULL; b = b->next)
		printf("   Bond %li  = original atom IDs %i and %i\n",b,b->atomI()->id(),b->atomJ()->id());
	dbgEnd(Debug::Calls,"Clipboard::setNewBondPointers");
} */

// Copy bonds for atoms
void Clipboard::copyBonds()
{
	dbgBegin(Debug::Calls,"Clipboard::copyBonds");
	// Go through pairs of oldptrs in the atoms list and check for bonds, adding to our list as we go.
	// The bonds we generate will point to pairs of Clipatoms.
	Bond *oldbond;
	for (Clipatom *ii = atoms_.first(); ii != NULL; ii = ii->getNext())
	{
		for (Clipatom *jj = ii->getNext(); jj != NULL; jj = jj->getNext())
		{
			
			oldbond = ii->atomPointer()->findBond(jj->atomPointer());
			if (oldbond != NULL)
			{
				Clipbond *b = bonds_.add();
				b->setAtoms(ii, jj);
				b->setOrder(oldbond->order());
			}
		}
	}
	dbgEnd(Debug::Calls,"Clipboard::copyBonds");
}

// Copy selection
void Clipboard::copySelection(Model *m)
{
	dbgBegin(Debug::Calls,"Clipboard::copySelection");
	if (m->nSelected() == 0)
	{
		msg(Debug::None,"Nothing selected to copy.\n");
		dbgEnd(Debug::Calls,"Clipboard::copyAll");
	}
	// Clear the clipboard first and make sure atom ids are valid
	clear();
	// Copy atoms
	for (Atom *i = m->atoms(); i != NULL; i = i->next) if (i->isSelected()) copyAtom(i);
	// Copy bonds
	copyBonds();
	dbgEnd(Debug::Calls,"Clipboard::copySelection");
}

// Copy model
void Clipboard::copyAll(Model *m)
{
	dbgBegin(Debug::Calls,"Clipboard::copyAll");
	// Clear the clipboard first and make sure atom ids are valid
	clear();
	for (Atom *i = m->atoms(); i != NULL; i = i->next) copyAtom(i);
	msg(Debug::Verbose, "Copied %i atoms from model %s\n", atoms_.nItems(), m->name());
	// Copy bonds
	copyBonds();
	dbgEnd(Debug::Calls,"Clipboard::copyAll");
}

// Cut selection
void Clipboard::cutSelection(Model *m)
{
	// Cut the selection from the specified model into the clipboard
	dbgBegin(Debug::Calls,"Clipboard::cutSelection");
	// Copy selection...
	copySelection(m);
	// ..and then we can use delete_selection to rid ourselves of the selection
	m->selectionDelete();
	dbgEnd(Debug::Calls,"Clipboard::cutSelection");
}

// Paste to model
void Clipboard::pasteToModel(Model *m, bool selectpasted)
{
	// Paste the contents of the clipboard into the model specified.
	// Deselect all atoms of the model, and select the pasted atoms_.
	dbgBegin(Debug::Calls,"Clipboard::pasteToModel");
	Atom *pastedi;
	if (selectpasted) m->selectNone();
	for (Clipatom *i = atoms_.first(); i != NULL; i = i->getNext())
	{
		// Create a new atom in the target model
		pastedi = m->addCopy(i);
		//printf("Pasted atom has id %i\n",pastedi->id());
		if (selectpasted) m->selectAtom(pastedi);
		// Store reference to the newly-pasted atom
		i->setAtomPointer(pastedi);
		m->projectAtom(pastedi);
	}	
	// Add in bonds to pasted atoms
	pasteBonds(m);
	dbgEnd(Debug::Calls,"Clipboard::pasteToModel");
}

// Paste to pattern
void Clipboard::pasteToPattern(Model *m, Pattern *p)
{
	// Paste the contents of the clipboard into the model specified.
	// An optional pattern is supplied, indicating atoms should be pasted into its local list. Otherwise, use the model.
	// Deselect all atoms of the model, and select the pasted atoms_.
	dbgBegin(Debug::Calls,"Clipboard::pasteToPattern");
	Atom *pastedi;
	m->selectNone();
	for (Clipatom *i = atoms_.first(); i != NULL; i = i->getNext())
	{
		// Create a new atom in the target model
		pastedi = p->appendCopy(pastedi);
		m->selectAtom(pastedi);
		// Store reference to the newly-pasted atom
		i->setAtomPointer(pastedi);
	}	
	// Add in bonds to pasted atoms
	pasteBonds(m);
	// Project the newly-pasted (and currently selected) atoms
	m->projectSelection();
	dbgEnd(Debug::Calls,"Clipboard::pasteToPattern");
}

// Paste to model in specified pattern / position
void Clipboard::pasteToModel(Model *destmodel, Pattern *p, int mol)
{
	// Paste the contents of the clipboard into the configuration supplied, and in the pattern / molecule position given.
	dbgBegin(Debug::Calls,"Clipboard::pasteToModel");
	// Check pattern spec against number of atoms in clipboard
	if (p->nAtoms() != atoms_.nItems())
	{
		printf("Number of atoms in clipboard (%i) does not match number in one molecule of pattern (%i).\n", atoms_.nItems(), p->nAtoms());
		dbgEnd(Debug::Calls,"Clipboard::pasteToModel");
		return;
	}
	// Paste the atoms
	Clipatom *i = atoms_.first();
	Atom **modelatoms = destmodel->atomArray();
	int cfgi = p->startAtom() + mol*p->nAtoms();
	while (i != NULL)
	{
		// Just put position data into the config (overwriting anything that might be there already)
		modelatoms[cfgi]->r() = i->r();
		modelatoms[cfgi]->setCharge(i->charge());
		cfgi ++;
		i = i->getNext();
	}	
	// No need to paste bonds or re-project.
	dbgEnd(Debug::Calls,"Clipboard::pasteToModel");
}

// Paste to model translated
void Clipboard::pasteToModel(Model *m, Vec3<double> t)
{
	dbgBegin(Debug::Calls,"Clipboard::pasteToModel[translated]");
	Atom *pastedi;
	// Deselect all atoms of the model, and select the pasted atoms_.
	m->selectNone();
	for (Clipatom *i = atoms_.first(); i != NULL; i = i->getNext())
	{
		// Create a new atom in the target model
		pastedi = m->addCopy(i);
		// Translate the new atom
		pastedi->r() += t;
		m->selectAtom(pastedi);
		// Store reference to the newly-pasted atom
		i->setAtomPointer(pastedi);
	}	
	// Add in bonds to pasted atoms
	pasteBonds(m);
	// Project the newly-pasted (and currently selected) atoms
	m->projectSelection();
	dbgEnd(Debug::Calls,"Clipboard::pasteToModel[translated]");
}

// Paste bonds
void Clipboard::pasteBonds(Model *m)
{
	dbgBegin(Debug::Calls,"Clipboard::pasteBonds");
	// By this point, bondi and bondj pointers in the bondlist will refer to clipatom* pointers
	for (Clipbond *b = bonds_.first(); b != NULL; b = b->next)
		m->bondAtoms(b->atomI()->atomPointer(), b->atomJ()->atomPointer(), b->order());
	dbgEnd(Debug::Calls,"Clipboard::pasteBonds");
}

// Translate Clipped Atoms
void Clipboard::translate(const Vec3<double> &v)
{
	for (Clipatom *i = atoms_.first(); i != NULL; i = i->getNext()) i->r() += v;
}

// Look for bond in list
bool Clipboard::hasBond(int ii, int jj)
{
	// Given the two atom ids (which should correspond to those of the clipboard's atoms list) see if we copied a bond between them
	static int idi, idj;
	for (Clipbond *b = bonds_.first(); b != NULL; b = b->next)
	{
		idi = b->atomI()->id();
		idj = b->atomJ()->id();
		if (((ii == idi) && (jj == idj)) || ((ii == idj) && (jj == idi))) return TRUE;
	}
	return FALSE;
}

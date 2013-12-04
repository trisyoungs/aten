/*
	*** Clipboard
	*** src/classes/clipboard.cpp
	Copyright T. Youngs 2007-2013

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

#include "model/clipboard.h"
#include "base/bond.h"
#include "base/pattern.h"
#include "model/model.h"
#include "base/progress.h"

/*
// Clipatom functions
*/

// Constructors
Clipatom::Clipatom()
{
	// Private variables
	atomPointer_ = NULL;
	
	// Public variables
	prev = NULL;
	next = NULL;
}

// Copy atom information and store original pointer
void Clipatom::set(Atom *i)
{
	atom_.copy(i);
	atomPointer_ = i;
}

// Set atom pointer
void Clipatom::setAtomPointer(Atom *i)
{
	atomPointer_ = i;
}

// Return the original atom pointer of the cloned atom
Atom *Clipatom::atomPointer()
{
	return atomPointer_;
}

// Return the new atom data structure
Atom &Clipatom::atom()
{
	return atom_;
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

// Set bond type
void Clipbond::setType(Bond::BondType bt)
{
	type_ = bt;
}

// Return type of bond
Bond::BondType Clipbond::type()
{
	return type_;
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

// Copy atom to clipboard, inserting it into the current list in the correct (original) atom ID position
void Clipboard::copyAtom(Atom *i)
{
	msg.enter("Clipboard::copyAtom");
        // Initialise the new clipatom
	Clipatom *newatom, *j;
	if (atoms_.nItems() == 0) newatom = atoms_.add();
	else
	{
		// Find first atom in current list with original ID *higher* than the current one
		for (j = atoms_.first(); j != NULL; j = j->next) if (j->atomPointer()->id() > i->id()) break;
		// Now, insert new item in list *before* the current 'j' atom (in other words, after 'j->prev')
		newatom = atoms_.insert(j == NULL ? atoms_.last() : j->prev);
	}
	newatom->set(i);
	msg.exit("Clipboard::copyAtom");
}

// Renumber copied atoms
void Clipboard::renumberAtoms()
{
	int count = 0;
	for (Clipatom *i = atoms_.first(); i != NULL; i = i->next) i->atom().setId(count++);
}

// Empty clipboard
void Clipboard::clear()
{
	// Clear the list of atoms on the clipboard
	msg.enter("Clipboard::clear");
	atoms_.clear();
	bonds_.clear();
	msg.exit("Clipboard::clear");
}

// Copy bonds for atoms
void Clipboard::copyBonds()
{
	msg.enter("Clipboard::copyBonds");
	// Go through pairs of oldptrs in the atoms list and check for bonds, adding to our list as we go.
	// The bonds we generate will point to pairs of Clipatoms.
	Bond *oldbond;
	for (Clipatom *ii = atoms_.first(); ii != NULL; ii = ii->next)
	{
		for (Clipatom *jj = ii->next; jj != NULL; jj = jj->next)
		{
			
			oldbond = ii->atomPointer()->findBond(jj->atomPointer());
			if (oldbond != NULL)
			{
				Clipbond *b = bonds_.add();
				b->setAtoms(ii, jj);
				b->setType(oldbond->type());
			}
		}
	}
	msg.exit("Clipboard::copyBonds");
}

// Copy selection
void Clipboard::copySelection(Model* m, bool quiet)
{
	msg.enter("Clipboard::copySelection");
	if (m->nSelected() == 0)
	{
		msg.print("Nothing selected to copy.\n");
		msg.exit("Clipboard::copySelection");
		return;
	}
	
	// Clear the clipboard first and make sure atom ids are valid
	clear();
	
	// Copy atoms
	if (!quiet) msg.print("Copying %i atoms from model '%s'...", m->nSelected(), m->name());
	for (Refitem<Atom,int> *ri = m->selection(); ri != NULL; ri = ri->next) copyAtom(ri->item);
	renumberAtoms();
	
	// Copy bonds
	if (!quiet) msg.print("bonds...");
	copyBonds();
	if (!quiet) msg.print(" Done.\n");
	
	msg.exit("Clipboard::copySelection");
}

// Copy selection
void Clipboard::copyMarked(Model *m)
{
	msg.enter("Clipboard::copyMarked");
	if (m->nMarked() == 0)
	{
		msg.print("Nothing marked to copy.\n");
		msg.exit("Clipboard::copyMarked");
		return;
	}
	
	// Clear the clipboard first and make sure atom ids are valid
	clear();
	
	// Copy atoms
	for (Refitem<Atom,int> *ri = m->selection(TRUE); ri != NULL; ri = ri->next) copyAtom(ri->item);
	renumberAtoms();
	
	// Copy bonds
	copyBonds();
	
	msg.exit("Clipboard::copyMarked");
}

// Copy model
void Clipboard::copyAll(Model *m, bool quiet)
{
	msg.enter("Clipboard::copyAll");
	
	// Clear the clipboard first and make sure atom ids are valid
	clear();
	
	// Copy atoms
	if (!quiet) msg.print("Copying all atoms from model '%s'...", m->name());
	for (Atom *i = m->atoms(); i != NULL; i = i->next) copyAtom(i);
	renumberAtoms();
	if (!quiet) msg.print("bonds...");
	
	// Copy bonds
	copyBonds();
	if (!quiet) msg.print(" Done.\n");
	
	msg.exit("Clipboard::copyAll");
}

// Cut selection
void Clipboard::cutSelection(Model *m)
{
	// Cut the selection from the specified model into the clipboard
	msg.enter("Clipboard::cutSelection");
	// Copy selection...
	copySelection(m);
	// ..and then we can use delete_selection to rid ourselves of the selection
	m->selectionDelete();
	msg.exit("Clipboard::cutSelection");
}

// Paste to model
void Clipboard::pasteToModel(Model *m, bool selectpasted)
{
	// Paste the contents of the clipboard into the model specified.
	// Deselect all atoms of the model, and select the pasted atoms_.
	msg.enter("Clipboard::pasteToModel");
	Atom *pastedi;
	if (selectpasted) m->selectNone();
	int count = 0;
	for (Clipatom *i = atoms_.first(); i != NULL; i = i->next)
	{
		// Create a new atom in the target model
		pastedi = m->addCopy(&i->atom());
		//printf("Pasted atom has id %i\n",pastedi->id());
		if (selectpasted) m->selectAtom(pastedi);
		// Store reference to the newly-pasted atom
		i->setAtomPointer(pastedi);
	}
	// Add in bonds to pasted atoms
	pasteBonds(m);
	msg.exit("Clipboard::pasteToModel");
}

// Paste to pattern
void Clipboard::pasteToPattern(Model *m, Pattern *p)
{
	// Paste the contents of the clipboard into the model specified.
	// An optional pattern is supplied, indicating atoms should be pasted into its local list. Otherwise, use the model.
	// Deselect all atoms of the model, and select the pasted atoms_.
	msg.enter("Clipboard::pasteToPattern");
	Atom *pastedi = NULL;
	m->selectNone();
	for (Clipatom *i = atoms_.first(); i != NULL; i = i->next)
	{
		// Create a new atom in the target model
		pastedi = p->appendCopy(pastedi);
		m->selectAtom(pastedi);
		// Store reference to the newly-pasted atom
		i->setAtomPointer(pastedi);
	}	
	// Add in bonds to pasted atoms
	pasteBonds(m);
	msg.exit("Clipboard::pasteToPattern");
}

// Paste to model in specified pattern / position
void Clipboard::pasteToModel(Model *destmodel, Pattern *p, int mol)
{
	// Paste the contents of the clipboard into the configuration supplied, and in the pattern / molecule position given.
	msg.enter("Clipboard::pasteToModel");
	// Check pattern spec against number of atoms in clipboard
	if (p->nAtoms() != atoms_.nItems())
	{
		printf("Number of atoms in clipboard (%i) does not match number in one molecule of pattern (%i).\n", atoms_.nItems(), p->nAtoms());
		msg.exit("Clipboard::pasteToModel");
		return;
	}
	// Paste the atoms
	Clipatom *i = atoms_.first();
	Atom **modelatoms = destmodel->atomArray();
	int cfgi = p->startAtom() + mol*p->nAtoms();
	while (i != NULL)
	{
		// Just put position data into the config (overwriting anything that might be there already)
		modelatoms[cfgi]->r() = i->atom().r();
		modelatoms[cfgi]->setCharge(i->atom().charge());
		cfgi ++;
		i = i->next;
	}	
	// No need to paste bonds or re-project.
	msg.exit("Clipboard::pasteToModel");
}

// Paste to model translated
void Clipboard::pasteToModel(Model *m, Vec3<double> t)
{
	msg.enter("Clipboard::pasteToModel[translated]");
	Atom *pastedi;
	// Deselect all atoms of the model, and select the pasted atoms_.
	m->selectNone();
	for (Clipatom *i = atoms_.first(); i != NULL; i = i->next)
	{
		// Create a new atom in the target model
		pastedi = m->addCopy(&i->atom());
		// Translate the new atom
		m->translateAtom(pastedi, t);
		m->selectAtom(pastedi);
		// Store reference to the newly-pasted atom
		i->setAtomPointer(pastedi);
	}	
	// Add in bonds to pasted atoms
	pasteBonds(m);
	msg.exit("Clipboard::pasteToModel[translated]");
}

// Paste bonds
void Clipboard::pasteBonds(Model *m)
{
	msg.enter("Clipboard::pasteBonds");
	// By this point, bondi and bondj pointers in the bondlist will refer to clipatom* pointers
	for (Clipbond *b = bonds_.first(); b != NULL; b = b->next) m->bondAtoms(b->atomI()->atomPointer(), b->atomJ()->atomPointer(), b->type());
	msg.exit("Clipboard::pasteBonds");
}

// Translate Clipped Atoms
void Clipboard::translate(const Vec3<double> &v)
{
	for (Clipatom *i = atoms_.first(); i != NULL; i = i->next) i->atom().r() += v;
}

// Look for bond in list
bool Clipboard::hasBond(int ii, int jj)
{
	// Given the two atom ids (which should correspond to those of the clipboard's atoms list) see if we copied a bond between them
	static int idi, idj;
	for (Clipbond *b = bonds_.first(); b != NULL; b = b->next)
	{
		idi = b->atomI()->atom().id();
		idj = b->atomJ()->atom().id();
		if (((ii == idi) && (jj == idj)) || ((ii == idj) && (jj == idi))) return TRUE;
	}
	return FALSE;
}

// Return number of bonds stored in clipboard
int Clipboard::nBonds()
{
	return bonds_.nItems();
}

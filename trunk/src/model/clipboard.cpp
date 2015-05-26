/*
	*** Clipboard
	*** src/base/clipboard.cpp
	Copyright T. Youngs 2007-2015

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
#include "base/pattern.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

/*
 * ClipAtom functions
 */

// Constructors
ClipAtom::ClipAtom() : ListItem<ClipAtom>()
{
	// Private variables
	atomPointer_ = NULL;
}

// Copy atom information and store original pointer
void ClipAtom::set(Atom* i)
{
	atom_.copy(i);
	atomPointer_ = i;
}

// Set atom pointer
void ClipAtom::setAtomPointer(Atom* i)
{
	atomPointer_ = i;
}

// Return the original atom pointer of the cloned atom
Atom* ClipAtom::atomPointer()
{
	return atomPointer_;
}

// Return the new atom data structure
Atom &ClipAtom::atom()
{
	return atom_;
}

/*
 * ClipBond functions
 */

ClipBond::ClipBond() : ListItem<ClipBond>()
{
	// Private variables
	atomI_ = NULL;
	atomJ_ = NULL;
}

// Set atoms for bond
void ClipBond::setAtoms(ClipAtom* i, ClipAtom* j)
{
	atomI_ = i;
	atomJ_ = j;
}

// Set bond type
void ClipBond::setType(Bond::BondType bt)
{
	type_ = bt;
}

// Return type of bond
Bond::BondType ClipBond::type()
{
	return type_;
}

// Return first atom in bond
ClipAtom* ClipBond::atomI()
{
	return atomI_;
}

// Return second atom in bond
ClipAtom* ClipBond::atomJ()
{
	return atomJ_;
}

/*
 * Clipboard functions
 */

// Return number of atoms in Clipboard
int Clipboard::nAtoms()
{
	return atoms_.nItems();
}

// Return list of copied atoms
ClipAtom* Clipboard::atoms()
{
	return atoms_.first();
}

// Copy atom to clipboard, inserting it into the current list in the correct (original) atom ID position
void Clipboard::copyAtom(Atom* i)
{
	Messenger::enter("Clipboard::copyAtom");
        // Initialise the new ClipAtom
	ClipAtom* newatom, *j;
	if (atoms_.nItems() == 0) newatom = atoms_.add();
	else
	{
		// Find first atom in current list with original ID *higher* than the current one
		for (j = atoms_.first(); j != NULL; j = j->next) if (j->atomPointer()->id() > i->id()) break;
		// Now, insert new item in list *before* the current 'j' atom (in other words, after 'j->prev')
		newatom = j == NULL ? atoms_.add() : atoms_.insertAfter(j->prev);
	}
	newatom->set(i);
	Messenger::exit("Clipboard::copyAtom");
}

// Renumber copied atoms
void Clipboard::renumberAtoms()
{
	int count = 0;
	for (ClipAtom* i = atoms_.first(); i != NULL; i = i->next) i->atom().setId(count++);
}

// Empty clipboard
void Clipboard::clear()
{
	// Clear the list of atoms on the clipboard
	Messenger::enter("Clipboard::clear");
	atoms_.clear();
	bonds_.clear();
	Messenger::exit("Clipboard::clear");
}

// Copy bonds for atoms
void Clipboard::copyBonds()
{
	Messenger::enter("Clipboard::copyBonds");
	// Go through pairs of oldptrs in the atoms list and check for bonds, adding to our list as we go.
	// The bonds we generate will point to pairs of ClipAtoms.
	Bond* oldbond;
	for (ClipAtom* ii = atoms_.first(); ii != NULL; ii = ii->next)
	{
		for (ClipAtom* jj = ii->next; jj != NULL; jj = jj->next)
		{
			
			oldbond = ii->atomPointer()->findBond(jj->atomPointer());
			if (oldbond != NULL)
			{
				ClipBond* b = bonds_.add();
				b->setAtoms(ii, jj);
				b->setType(oldbond->type());
			}
		}
	}
	Messenger::exit("Clipboard::copyBonds");
}

// Copy selection
void Clipboard::copySelection(Model* m, bool quiet)
{
	Messenger::enter("Clipboard::copySelection");
	if (m->nSelected() == 0)
	{
		Messenger::print("Nothing selected to copy.");
		Messenger::exit("Clipboard::copySelection");
		return;
	}
	
	// Clear the clipboard first and make sure atom ids are valid
	clear();
	
	// Copy atoms
	if (!quiet) Messenger::print("Copying %i atoms from model '%s'...", m->nSelected(), qPrintable(m->name()));
	for (Refitem<Atom,int>* ri = m->selection(); ri != NULL; ri = ri->next) copyAtom(ri->item);
	renumberAtoms();
	
	// Copy bonds
	if (!quiet) Messenger::print("bonds...");
	copyBonds();
	if (!quiet) Messenger::print(" Done.");
	
	Messenger::exit("Clipboard::copySelection");
}

// Copy selection
void Clipboard::copyMarked(Model* m)
{
	Messenger::enter("Clipboard::copyMarked");
	if (m->nMarked() == 0)
	{
		Messenger::print("Nothing marked to copy.");
		Messenger::exit("Clipboard::copyMarked");
		return;
	}
	
	// Clear the clipboard first and make sure atom ids are valid
	clear();
	
	// Copy atoms
	for (Refitem<Atom,int>* ri = m->selection(true); ri != NULL; ri = ri->next) copyAtom(ri->item);
	renumberAtoms();
	
	// Copy bonds
	copyBonds();
	
	Messenger::exit("Clipboard::copyMarked");
}

// Copy model
void Clipboard::copyAll(Model* m, bool quiet)
{
	Messenger::enter("Clipboard::copyAll");
	
	// Clear the clipboard first and make sure atom ids are valid
	clear();
	
	// Copy atoms
	if (!quiet) Messenger::print("Copying all atoms from model '%s'...", qPrintable(m->name()));
	for (Atom* i = m->atoms(); i != NULL; i = i->next) copyAtom(i);
	renumberAtoms();
	if (!quiet) Messenger::print("bonds...");
	
	// Copy bonds
	copyBonds();
	if (!quiet) Messenger::print(" Done.");
	
	Messenger::exit("Clipboard::copyAll");
}

// Cut selection
void Clipboard::cutSelection(Model* m)
{
	// Cut the selection from the specified model into the clipboard
	Messenger::enter("Clipboard::cutSelection");
	// Copy selection...
	copySelection(m);
	// ..and then we can use selectionDelete() to rid ourselves of the selected atoms
	m->selectionDelete();
	Messenger::exit("Clipboard::cutSelection");
}

// Paste to model
void Clipboard::pasteToModel(Model* targetModel, bool selectPasted)
{
	// Paste the contents of the clipboard into the model specified.
	// Deselect all atoms of the model, and select the pasted atoms_.
	Messenger::enter("Clipboard::pasteToModel");
	Atom* pastedi;

	if (selectPasted)
	{
		targetModel->selectNone();
		for (ClipAtom* i = atoms_.first(); i != NULL; i = i->next)
		{
			// Create a new atom in the target model
			pastedi = targetModel->addCopy(&i->atom());

			// Select atom
			targetModel->selectAtom(pastedi);

			// Store reference to the newly-pasted atom
			i->setAtomPointer(pastedi);
		}
	}
	else
	{
		for (ClipAtom* i = atoms_.first(); i != NULL; i = i->next)
		{
			// Create a new atom in the target model
			pastedi = targetModel->addCopy(&i->atom());

			// Store reference to the newly-pasted atom
			i->setAtomPointer(pastedi);
		}
	}

	// Add in bonds to pasted atoms
	pasteBonds(targetModel);
	Messenger::exit("Clipboard::pasteToModel");
}

// Paste to pattern
void Clipboard::pasteToPattern(Model* targetModel, Pattern* targetPattern)
{
	// Paste the contents of the clipboard into the model specified.
	// An optional pattern is supplied, indicating atoms should be pasted into its local list. Otherwise, use the model.
	Messenger::enter("Clipboard::pasteToPattern");
	Atom* pastedi = NULL;

	// Deselect all atoms of the model, and select the pasted atoms_.
	targetModel->selectNone();
	for (ClipAtom* i = atoms_.first(); i != NULL; i = i->next)
	{
		// Create a new atom in the target model
		pastedi = targetPattern->appendCopy(pastedi);
		targetModel->selectAtom(pastedi);

		// Store reference to the newly-pasted atom
		i->setAtomPointer(pastedi);
	}
	
	// Add in bonds to pasted atoms
	pasteBonds(targetModel);
	Messenger::exit("Clipboard::pasteToPattern");
}

// Paste to model in specified pattern / position
void Clipboard::pasteToModel(Model* targetModel, Pattern* targetPattern, int mol)
{
	// Paste the contents of the clipboard into the configuration supplied, and in the pattern / molecule position given.
	Messenger::enter("Clipboard::pasteToModel");

	// Check pattern spec against number of atoms in clipboard
	if (targetPattern->nAtoms() != atoms_.nItems())
	{
		printf("Number of atoms in clipboard (%i) does not match number in one molecule of pattern (%i).\n", atoms_.nItems(), targetPattern->nAtoms());
		Messenger::exit("Clipboard::pasteToModel");
		return;
	}

	// Paste the atoms
	ClipAtom* i = atoms_.first();
	Atom** modelatoms = targetModel->atomArray();
	int cfgi = targetPattern->startAtom() + mol*targetPattern->nAtoms();
	while (i != NULL)
	{
		// Just put position data into the config (overwriting anything that might be there already)
		modelatoms[cfgi]->r() = i->atom().r();
		modelatoms[cfgi]->setCharge(i->atom().charge());
		++cfgi;
		i = i->next;
	}

	Messenger::exit("Clipboard::pasteToModel");
}

// Paste to model translated
void Clipboard::pasteToModel(Model* m, Vec3<double> translation)
{
	Messenger::enter("Clipboard::pasteToModel[translated]");
	Atom* pastedi;
	// Deselect all atoms of the model, and select the pasted atoms_.
	m->selectNone();
	for (ClipAtom* i = atoms_.first(); i != NULL; i = i->next)
	{
		// Create a new atom in the target model
		pastedi = m->addCopy(&i->atom());
		// Translate the new atom
		m->translateAtom(pastedi, translation);
		m->selectAtom(pastedi);
		// Store reference to the newly-pasted atom
		i->setAtomPointer(pastedi);
	}	
	// Add in bonds to pasted atoms
	pasteBonds(m);
	Messenger::exit("Clipboard::pasteToModel[translated]");
}

// Paste bonds
void Clipboard::pasteBonds(Model* target)
{
	Messenger::enter("Clipboard::pasteBonds");
	// By this point, bondi and bondj pointers in the bondlist will refer to ClipAtom* pointers
	for (ClipBond* b = bonds_.first(); b != NULL; b = b->next) target->bondAtoms(b->atomI()->atomPointer(), b->atomJ()->atomPointer(), b->type());
	Messenger::exit("Clipboard::pasteBonds");
}

// Translate Clipped Atoms
void Clipboard::translate(const Vec3<double>& translation)
{
	for (ClipAtom* i = atoms_.first(); i != NULL; i = i->next) i->atom().r() += translation;
}

// Look for bond in list
bool Clipboard::hasBond(int ii, int jj)
{
	// Given the two atom ids (which should correspond to those of the clipboard's atoms list) see if we copied a bond between them
	static int idi, idj;
	for (ClipBond* b = bonds_.first(); b != NULL; b = b->next)
	{
		idi = b->atomI()->atom().id();
		idj = b->atomJ()->atom().id();
		if (((ii == idi) && (jj == idj)) || ((ii == idj) && (jj == idi))) return true;
	}
	return false;
}

// Return number of bonds stored in clipboard
int Clipboard::nBonds()
{
	return bonds_.nItems();
}

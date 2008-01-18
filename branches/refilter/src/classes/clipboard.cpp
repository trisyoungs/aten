/*
	*** Clipboard
	*** src/classes/clipboard.cpp
	Copyright T. Youngs 2007

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
clipboard::clipboard()
{
	#ifdef MEMDEBUG
		memdbg.create[MD_CLIPBOARD] ++;
	#endif
}

clipatom::clipatom()
{
	oldptr = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_CLIPATOM] ++;
	#endif
}

clipbond::clipbond()
{
	clipi = NULL;
	clipj = NULL;
	type = BT_UNSPECIFIED;
	#ifdef MEMDEBUG
		memdbg.create[MD_CLIPBOND] ++;
	#endif
}

// Destructors
clipboard::~clipboard()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_CLIPBOARD] ++;
	#endif
}

clipatom::~clipatom()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_CLIPATOM] ++;
	#endif
}

clipbond::~clipbond()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_CLIPBOND] ++;
	#endif
}

// Copy atom to clipboard
void clipboard::copy_atom(atom *i)
{
	dbg_begin(DM_CALLS,"clipboard::copy_atom");
        // Initialise the new clipatom
	clipatom *newatom = atoms.add();
	newatom->copy(i);
	newatom->set_oldptr(i);
	newatom->set_id(atoms.size()-1);
	dbg_end(DM_CALLS,"clipboard::copy_atom");
}

// Empty clipboard
void clipboard::clear()
{
	// Clear the list of atoms on the clipboard
	dbg_begin(DM_CALLS,"clipboard::clear");
	atoms.clear();
	bonds.clear();
	dbg_end(DM_CALLS,"clipboard::clear");
}

// Replace pointers in bond list
void clipboard::bonds_set_newptr(clipatom *clipptr, atom *newptr)
{
	dbg_begin(DM_CALLS,"clipboard::bonds_set_newptr");
	// Go through bond list searching for clipatom in clipi or clipj. Set associated bondi/bondj to newptr.
	clipbond *b = bonds.first();
	while (b != NULL)
	{
		if (b->get_clipi() == clipptr) b->bondi = newptr;
		else if (b->get_clipj() == clipptr) b->bondj = newptr;
		b = b->get_next();
	}
	dbg_end(DM_CALLS,"clipboard::bonds_set_newptr");
}

// Copy bonds for atoms
void clipboard::copy_bonds_for_atoms()
{
	dbg_begin(DM_CALLS,"clipboard::copy_bonds_for_atoms");
	// Go through pairs of oldptrs in the atoms list and check for bonds, adding to our list as we go.
	// The list we generate will consist of bonds between *new* pointers.
	bond *oldb;
	clipatom *ii = atoms.first();
	while (ii != NULL)
	{
		clipatom *jj = ii->get_next();
		while (jj != NULL)
		{
			
			oldb = ii->get_oldptr()->find_bond(jj->get_oldptr());
			if (oldb != NULL)
			{
				clipbond *b = bonds.add();
				b->set_clipi(ii);
				b->set_clipj(jj);
				b->type = oldb->type;
			}
			jj = jj->get_next();
		}
		ii = ii->get_next();
	}
	dbg_end(DM_CALLS,"clipboard::copy_bonds_for_atoms");
}

// Copy selection
void clipboard::copy_selection(model *m)
{
	// Copy the selection in the specified model to the clipboard
	dbg_begin(DM_CALLS,"clipboard::copy_selection");
	// Clear the clipboard first and make sure atom ids are valid
	clear();
	// Copy atoms
	atom *i = m->get_atoms();
	while (i != NULL)
	{
		if (i->is_selected()) copy_atom(i);
		i = i->next;
	}
	// Copy bonds
	copy_bonds_for_atoms();
	dbg_end(DM_CALLS,"clipboard::copy_selection");
}

// Copy model
void clipboard::copy_all(model *m)
{
	// Copy all atoms in the specified model to the clipboard
	dbg_begin(DM_CALLS,"clipboard::copy_all");
	// Clear the clipboard first and make sure atom ids are valid
	clear();
	atom *i = m->get_atoms();
	while (i != NULL)
	{
		copy_atom(i);
		i = i->next;
	}
	msg(DM_VERBOSE, "Copied %i atoms from model %s\n",atoms.size(), m->get_name());
	// Copy bonds
	copy_bonds_for_atoms();
	dbg_end(DM_CALLS,"clipboard::copy_all");
}

// Cut selection
void clipboard::cut_selection(model *m)
{
	// Cut the selection from the specified model into the clipboard
	dbg_begin(DM_CALLS,"clipboard::cut_selection");
	// Copy selection...
	copy_selection(m);
	// ..and then we can use delete_selection to rid ourselves of the selection
	model *m2 = (model*) m;
	m2->selection_delete();
	dbg_end(DM_CALLS,"clipboard::cut_selection");
}

// Paste to model
void clipboard::paste_to_model(model *m)
{
	// Paste the contents of the clipboard into the model specified.
	// An optional pattern is supplied, indicating atoms should be pasted into its local list. Otherwise, use the model.
	// Deselect all atoms of the model, and select the pasted atoms.
	dbg_begin(DM_CALLS,"clipboard::paste_to_model");
	atom *pastedi, *ii, *jj;
	m->select_none();
	clipatom *i = atoms.first();
	while (i != NULL)
	{
		// Create a new atom in the target model
		pastedi = m->add_copy(i);
		//printf("Pasted atom has id %i\n",pastedi->get_id());
		m->select_atom(pastedi);
		// Now we have the new pointer for this pasted atom, replace references to the old clipatom with the newly pasted 'real' atom
		bonds_set_newptr(i,pastedi);
		i = i->get_next();
	}	
	// Add in bonds to pasted atoms
	paste_bonds(m);
	// Project the newly-pasted (and currently selected) atoms
	m->project_selection();
	dbg_end(DM_CALLS,"clipboard::paste_to_model");
}

// Paste to pattern
void clipboard::paste_to_pattern(model *m, pattern *p)
{
	// Paste the contents of the clipboard into the model specified.
	// An optional pattern is supplied, indicating atoms should be pasted into its local list. Otherwise, use the model.
	// Deselect all atoms of the model, and select the pasted atoms.
	dbg_begin(DM_CALLS,"clipboard::paste_to_pattern");
	atom *pastedi;
	m->select_none();
	for (clipatom *i = atoms.first(); i != NULL; i = i->get_next())
	{
		// Create a new atom in the target model
		pastedi = p->append_copy(pastedi);
		m->select_atom(pastedi);
		// Now we have the new pointer for this pasted atom, replace references to the old clipatom with the newly pasted 'real' atom
		bonds_set_newptr(i,pastedi);
	}	
	// Add in bonds to pasted atoms
	paste_bonds(m);
	// Project the newly-pasted (and currently selected) atoms
	m->project_selection();
	dbg_end(DM_CALLS,"clipboard::paste_to_pattern");
}

// Paste to model in specified pattern / position
void clipboard::paste_to_model(model *destmodel, pattern *p, int mol)
{
	// Paste the contents of the clipboard into the configuration supplied, and in the pattern / molecule position given.
	dbg_begin(DM_CALLS,"clipboard::paste_to_model");
	// Check pattern spec against number of atoms in clipboard
	if (p->get_natoms() != atoms.size())
	{
		printf("Number of atoms in clipboard (%i) does not match number in one molecule of pattern (%i).\n", atoms.size(), p->get_natoms());
		dbg_end(DM_CALLS,"clipboard::paste_to_model");
		return;
	}
	// Paste the atoms
	clipatom *i = atoms.first();
	atom **modelatoms = destmodel->get_atomarray();
	int cfgi = p->get_startatom() + mol*p->get_natoms();
	while (i != NULL)
	{
		// Just put position data into the config (overwriting anything that might be there already)
		modelatoms[cfgi]->r() = i->r();
		modelatoms[cfgi]->set_charge(i->get_charge());
		cfgi ++;
		i = i->get_next();
	}	
	// No need to paste bonds or re-project.
	dbg_end(DM_CALLS,"clipboard::paste_to_model");
}

// Paste bonds
void clipboard::paste_bonds(model *m)
{
	dbg_begin(DM_CALLS,"clipboard::paste_bonds");
	for (clipbond *b = bonds.first(); b != NULL; b = b->get_next())
	{
		// By this point, bondi and bondj pointers in the bondlist will refer to clipatom* pointers
		m->bond_atoms(b->bondi,b->bondj,b->type);
	}
	dbg_end(DM_CALLS,"clipboard::paste_bonds");
}

// Paste to model translated
void clipboard::paste_to_model(model *m, vec3<double> t)
{
	// Paste the contents of the clipboard into the model specified.
	// An optional pattern is supplied, indicating atoms should be pasted into its local list. Otherwise, use the model.
	// Deselect all atoms of the model, and select the pasted atoms.
	dbg_begin(DM_CALLS,"clipboard::paste_to_model[translated]");
	atom *pastedi, *ii, *jj;
	m->select_none();
	for (clipatom *i = atoms.first(); i != NULL; i = i->get_next())
	{
		// Create a new atom in the target model
		pastedi = m->add_copy(i);
		// Translate the new atom
		pastedi->r() += t;
		m->select_atom(pastedi);
		// Now we have the new pointer for this pasted atom, replace references to the old clipatom with the newly pasted 'real' atom
		bonds_set_newptr(i,pastedi);
	}	
	// Add in bonds to pasted atoms
	paste_bonds(m);
	// Project the newly-pasted (and currently selected) atoms
	m->project_selection();
	dbg_end(DM_CALLS,"clipboard::paste_to_model[translated]");
}

// Translate Clipped Atoms
void clipboard::translate(const vec3<double> &v)
{
	for (clipatom *i = atoms.first(); i != NULL; i = i->get_next()) i->r() += v;
}

// Look for bond in list
bool clipboard::has_bond(int ii, int jj)
{
	// Given the two atom ids (which should correspond to those of the clipboard's atoms list) see if we copied a bond between them
	static int idi, idj;
	for (clipbond *b = bonds.first(); b != NULL; b = b->get_next())
	{
		idi = b->get_clipi()->get_id();
		idj = b->get_clipj()->get_id();
		if (((ii == idi) && (jj == idj)) || ((ii == idj) && (jj == idi))) return TRUE;
	}
	return FALSE;
}

/*
	*** Model atom functions
	*** src/model/atom.cpp
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

#include "model/model.h"
#include "classes/atom.h"
#include "classes/undostate.h"
#include "base/elements.h"
#include "base/master.h"

// Add atom
atom *model::add_atom(int newel, vec3<double> pos)
{
	dbg_begin(DM_CALLS,"model::add_atom");
	atom *newatom = atoms.add();
	newatom->set_element(newel);
	newatom->set_id(atoms.size() - 1);
	newatom->r() = pos;
	mass += elements.mass(newel);
	calculate_density();
	log_change(LOG_STRUCTURE);
	// Add the change to the undo state (if there is one)
	if (recordingstate != NULL)
	{
		change *newchange = recordingstate->changes.add();
		newchange->set(UE_ATOM,newatom);
	}
	dbg_end(DM_CALLS,"model::add_atom");
	return newatom;
}

// Add atom copy
atom *model::add_copy(atom *source)
{
	dbg_begin(DM_CALLS,"model::add_copy");
	atom *newatom = atoms.add();
	newatom->copy(source);
	newatom->set_id(atoms.size() - 1);
	log_change(LOG_STRUCTURE);
	mass += elements.mass(newatom->get_element());
	calculate_density();
	// Add the change to the undo state (if there is one)
	if (recordingstate != NULL)
	{
		change *newchange = recordingstate->changes.add();
		newchange->set(UE_ATOM,newatom);
	}
	dbg_end(DM_CALLS,"model::add_copy");
	return newatom;
}

// Add atom copy at specified position in list
atom *model::add_copy(atom *afterthis, atom *source)
{
	dbg_begin(DM_CALLS,"model::add_copy");
	atom *newatom = atoms.insert(afterthis);
	//printf("Adding copy after... %li %li\n",afterthis,source);
	newatom->copy(source);
	renumber_atoms( (afterthis != NULL ? afterthis->prev : NULL) );
	log_change(LOG_STRUCTURE);
	mass += elements.mass(newatom->get_element());
	calculate_density();
	// Add the change to the undo state (if there is one)
	if (recordingstate != NULL)
	{
		change *newchange = recordingstate->changes.add();
		newchange->set(UE_ATOM,newatom);
	}
	dbg_end(DM_CALLS,"model::add_copy");
	return newatom;
}

// Remove atom
void model::remove_atom(atom *xatom)
{
	dbg_begin(DM_CALLS,"model::remove_atom");
	// Delete a specific atom (passed as xatom)
	mass -= elements.mass(xatom->get_element());
	if (mass < 0.0) mass = 0.0;
	calculate_density();
	// Renumber the ids of all atoms in the list after this one
	for (atom *i = xatom->next; i != NULL; i = i->next) i->decrease_id();
	if (xatom->is_selected()) deselect_atom(xatom);
	log_change(LOG_STRUCTURE);
	// Add the change to the undo state (if there is one)
	if (recordingstate != NULL)
	{
		change *newchange = recordingstate->changes.add();
		newchange->set(-UE_ATOM,xatom);
	}
	atoms.remove(xatom);
	dbg_end(DM_CALLS,"model::remove_atom");
}

// Delete Atom
void model::delete_atom(atom *xatom)
{
	dbg_begin(DM_CALLS,"model::delete_atom");
	// The atom may be present in other, unassociated lists (e.g. measurements), so we must
	// also check those lists for this atom and remove it.
	if (xatom == NULL) msg(DM_NONE,"No atom to delete.\n");
	else
	{
		// Remove measurements
		remove_measurements(xatom);
		// Delete All Bonds To Specific Atom
		refitem<bond> *bref = xatom->get_bonds();
		while (bref != NULL)
		{
			// Need to detach the bond from both atoms involved
			bond *b = bref->item;
			atom *j = b->get_partner(xatom);
			unbond_atoms(xatom,j,b);
			bref = xatom->get_bonds();
		}
		// Finally, delete the atom
		remove_atom(xatom);
	}
	dbg_end(DM_CALLS,"model::delete_atom");
}

// Transmute atom
void model::transmute_atom(atom *i, int el)
{
	dbg_begin(DM_CALLS,"model::transmute_atom");
	if (i == NULL) msg(DM_NONE,"No atom to transmute.\n");
	else
	{
		int oldel = i->get_element();
		if (oldel != el)
		{
			mass -= elements.mass(i);
			i->set_element(el);
			mass += elements.mass(i);
			calculate_density();
			log_change(LOG_STRUCTURE);
			// Add the change to the undo state (if there is one)
			if (recordingstate != NULL)
			{
				change *newchange = recordingstate->changes.add();
				newchange->set(UE_TRANSMUTE,i->get_id(),oldel,el);
			}
		}
	}
	dbg_end(DM_CALLS,"model::transmute_atom");
}

// Clear atoms
void model::clear_atoms()
{
	dbg_begin(DM_CALLS,"model::clear_atoms");
	atom *i = atoms.first();
	while (i != NULL)
	{
		delete_atom(i);
		i = atoms.first();
	}
	dbg_end(DM_CALLS,"model::clear_atoms");
}

// Find atom
atom *model::find_atom(int id)
{
	// Find an atom according to its internal id (useful when atom ids may have been set differently by import filters)
	atom *i;
	for (i = atoms.first(); i != NULL; i = i->next) if (id == i->get_id()) break;
	return i;
}

// Find atom by tempi
atom *model::find_atom_by_tempi(int tempi)
{
	// Find an atom according to its tempi value
	for (atom *i = atoms.first(); i != NULL; i = i->next) if (i->tempi == tempi) return i;
	return NULL;
}

// Renumber Atoms
void model::renumber_atoms(atom *from)
{
	dbg_begin(DM_CALLS,"model::renumber_atoms");
	static int count;
	static atom *i;
	if (from == NULL)
	{
		count = 0;
		i = atoms.first();
	}
	else
	{
		count = from->get_id();
		i = from->next;
	}
	for (i = i; i != NULL; i = i->next)
	{
		i->set_id(count);
		count ++;
	}
	dbg_end(DM_CALLS,"model::renumber_atoms");
}

// Return atom 'n' in the model
atom *model::get_atom(int n)
{
	dbg_begin(DM_CALLS,"model::get_atom");
	// Check range first
	if ((n < 0) || (n >= atoms.size()))
	{
		printf("model::get_atom <<<< '%i' is out of range for model >>>>\n",n);
		dbg_end(DM_CALLS,"model::get_atom");
		return NULL;
	}
	dbg_end(DM_CALLS,"model::get_atom");
	return atoms.array()[n];
}

// Reset forces on all atoms
void model::zero_forces()
{
	dbg_begin(DM_CALLS,"model::zero_forces");
	for (atom *i = atoms.first(); i != NULL; i = i->next) i->f().zero();
	dbg_end(DM_CALLS,"model::zero_forces");
}

// Reset forces on all fixed atoms
void model::zero_forces_fixed()
{
	dbg_begin(DM_CALLS,"model::zero_forces");
	for (atom *i = atoms.first(); i != NULL; i = i->next) if (i->fixed) i->f().zero();
	dbg_end(DM_CALLS,"model::zero_forces");
}

// Set visibility of specified atom
void model::set_hidden(atom *i, bool hidden)
{
	i->set_hidden(hidden);
	log_change(LOG_VISUAL);
}

// Normalise forces
void model::normalise_forces(double norm)
{
	// 'Normalise' the forces in linecfg such that the largest force is equal to the maximum cartesian step size
	dbg_begin(DM_CALLS,"model::normalise_forces");
	double maxfrc;
	static vec3<double> f;
	atom **modelatoms = get_atomarray();
	int i;
	// Find the largest force
	maxfrc = 0.0;
	for (i=0; i<atoms.size(); i++)
	{
		f = modelatoms[i]->f();
		if (fabs(f.x) > maxfrc) maxfrc = fabs(f.x);
		if (fabs(f.y) > maxfrc) maxfrc = fabs(f.y);
		if (fabs(f.z) > maxfrc) maxfrc = fabs(f.z);
	}
	// Normalise with respect to this force
	maxfrc *= norm;
	for (i=0; i<atoms.size(); i++) modelatoms[i]->f() /= maxfrc;
	dbg_end(DM_CALLS,"model::normalise_forces");
}

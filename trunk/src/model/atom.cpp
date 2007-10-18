/*
	*** Model atom functions
	*** src/model/atom.cpp

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
#include "base/elements.h"
#include "base/master.h"

// Add atom
atom *model::add_atom(int newel)
{
	// Private function to create a new atom in the model.
	// No list linking is performed here
	dbg_begin(DM_CALLS,"model::add_atom");
	atom *newatom = atoms.add();
	newatom->set_element(newel);
	set_atom_colours(newatom);
	newatom->set_id(atoms.size() - 1);
	// Should be safe to take the flag from the first atom in the model
	if (atoms.size() != 0) newatom->set_drawn(atoms.first()->get_drawn());
	mass += elements.mass(newel);
	calculate_density();
	lastatomdrawn = newatom;
	//project_atom(i);
	log_change(LOG_STRUCTURE);
	dbg_end(DM_CALLS,"model::add_atom");
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
	atom *i = xatom->next;
	while (i != NULL)
	{
		i->decrease_id();
		i = i->next;
	}
	if (xatom->is_selected()) deselect_atom(xatom);
	atoms.remove(xatom);
	log_change(LOG_STRUCTURE);
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
		remove_atom(xatom);
		remove_measurements(xatom);
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
		mass -= elements.mass(i);
		i->set_element(el);
		mass += elements.mass(i);
		set_atom_colours(i);
		calculate_density();
		log_change(LOG_STRUCTURE);
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
	// Find an atom according to its internal id
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->get_id() == id) return i;
		i = i->next;
	}
	msg(DM_NONE,"Atom id %i is out of range for model '%s'\n",id,name.get());
	return NULL;
}

// Find atom by tempi
atom *model::find_atom_by_tempi(int tempi)
{
	// Find an atom according to its tempi value
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->tempi == tempi) return i;
		i = i->next;
	}
	return NULL;
}

// Set colours of all atoms
void model::set_atom_colours(atom *onlythis)
{
	dbg_begin(DM_CALLS,"model::set_atom_colours");
	GLint *col;
	static vec3<float> lodelta, hidelta, locol, midcol, hicol, newcolour;
	double lopoint, hipoint, midpoint, quantity, lorange, hirange, largest, smallest;
	atom *i;
	atom_colour scheme = master.get_colour_scheme();
	i = (onlythis == NULL ? atoms.first() : onlythis);
	if (scheme == AC_ELEMENT)
	{
		while (i != NULL)
		{
			i->set_colour(elements.colour(i));
			if (i == onlythis) break;
			i = i->next;
		}
	}
	else
	{
		// We need a colour scale in order to define the atoms colour
		col = prefs.get_colour(COL_ACSCHEMELO);
		locol.set(col[0], col[1], col[2]);
		col = prefs.get_colour(COL_ACSCHEMEMID);
		midcol.set(col[0], col[1], col[2]);
		col = prefs.get_colour(COL_ACSCHEMEHI);
		hicol.set(col[0], col[1], col[2]);
		lodelta = midcol - locol;
		hidelta = midcol - hicol;
		lopoint = prefs.get_colour_scheme_lo(scheme);
		hipoint = prefs.get_colour_scheme_hi(scheme);
		midpoint = (hipoint + lopoint) * 0.5;
		lorange = midpoint - lopoint;
		hirange = midpoint - hipoint;
		largest = -1.0e9;
		smallest = 1.0e9;
		// Now loop over atoms
		while (i != NULL)
		{
			switch (scheme)
			{
				case (AC_CHARGE): quantity = i->get_charge(); break;
				case (AC_VELOCITY): quantity = i->v.magnitude(); break;
				case (AC_FORCE): quantity = i->f.magnitude(); break;
			}
			// Pick and set colour
			if (quantity < midpoint) quantity < lopoint ? newcolour = locol : newcolour = locol + lodelta * ((quantity-lopoint) / lorange);
			else quantity > hipoint ? newcolour = hicol : newcolour = midcol + hidelta * ((quantity-midpoint) / hirange);
	//printf("lo mid hi points = %8.4f %8.4f %8.4f\n",lopoint,midpoint,hipoint);
	//printf("lo hi ranges     = %8.4f %8.4f\n",lorange,hirange);
	//printf("quantity = %8.4f, q/lh = %8.4f\n",quantity, quantity / (quantity < midpoint ? lorange : hirange));
			i->set_colour(int(newcolour.x), int(newcolour.y), int(newcolour.z));
			// Store extremes
			if (quantity > largest) largest = quantity;
			if (quantity < smallest) smallest = quantity;
			if (i == onlythis) break;
			i = i->next;
			if (i == NULL) msg(DM_NONE,"For atom colouring, quantity range is %s to %s.\n",ftoa(smallest),ftoa(largest));
		}
	}
	dbg_end(DM_CALLS,"model::set_atom_colours");
}

// Renumber Atoms
void model::renumber_atoms()
{
	dbg_begin(DM_CALLS,"model::renumber_atoms");
	int count = 0;
	atom *i = atoms.first();
	while (i != NULL)
	{
		i->set_id(count);
		count ++;
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::renumber_atoms");
}

// Create (or just return) the static atom list
atom **model::get_staticatoms()
{
	dbg_begin(DM_CALLS,"model::get_staticatoms");
	if (staticatoms_point == logs[LOG_STRUCTURE])
	{
		dbg_end(DM_CALLS,"model::get_staticatoms");
		return staticatoms;
	}
	// Delete old atom list (if there is one)
	if (staticatoms != NULL) delete[] staticatoms;
	// Create new list
	staticatoms = new atom*[atoms.size()];
	// Fill in atom pointers
	int count = 0;
	for (atom *i = atoms.first(); i != NULL; i = i->next)
	{
	//printf("N=%i\n",count);
		staticatoms[count] = i;
		count ++;
	}
	staticatoms_point = logs[LOG_STRUCTURE];
	dbg_end(DM_CALLS,"model::get_staticatoms");
	return staticatoms;
}

// Reset forces on all atoms
void model::zero_forces()
{
	dbg_begin(DM_CALLS,"model::zero_forces");
	for (atom *i = atoms.first(); i != NULL; i = i->next) i->f.zero();
	dbg_end(DM_CALLS,"model::zero_forces");
}

// Reset forces on all fixed atoms
void model::zero_forces_fixed()
{
	dbg_begin(DM_CALLS,"model::zero_forces");
	for (atom *i = atoms.first(); i != NULL; i = i->next) if (i->fixed) i->f.zero();
	dbg_end(DM_CALLS,"model::zero_forces");
}

// Set visibility of specified atom
void model::set_hidden(atom *i, bool hidden)
{
	i->set_hidden(hidden);
	log_change(LOG_VISUAL);
}

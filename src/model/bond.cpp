/*
	*** Model bond functions
	*** src/model/bond.cpp
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
#include "classes/bond.h"
#include "classes/pattern.h"
#include "base/prefs.h"
#include "base/elements.h"

// Add Bond (pointers)
void model::bond_atoms(atom *i, atom *j, bond_type bt)
{
        // Create a new bond ach atom and add them to the atom's own lists.
	dbg_begin(DM_CALLS,"model::bond_atoms");
	if (i == j) msg(DM_NONE,"Cannot bond an atom to itself!\n");
	else
	{
		bond *newbond = new bond;
		newbond->type = bt;
		newbond->bondi = i;
		i->accept_bond(newbond);
		newbond->bondj = j;
		j->accept_bond(newbond);
		log_change(LOG_STRUCTURE);
	}
	dbg_end(DM_CALLS,"model::bond_atoms");
}

// Add Bond (id's)
void model::bond_atoms(int ii, int jj, bond_type bt)
{
        // Create a new bond for each atom and add them to the atom's own lists.
	dbg_begin(DM_CALLS,"model::bond_atoms[int]");
	if (ii == jj) msg(DM_NONE,"Cannot bond an atom to itself!\n");
	else
	{
		// First, locate the two atoms with the specified id's
		atom *i = find_atom(ii);
		atom *j = find_atom(jj);
		if (i == NULL || j == NULL)
		{
			printf("Couldn't locate one or both atoms in bond with specified ids %i and %i\n",ii,jj);
			dbg_end(DM_CALLS,"model::bond_atoms[int]");
		}
		bond_atoms(i,j,bt);
	}
	dbg_end(DM_CALLS,"model::bond_atoms[int]");
}

// Delete Bond
void model::unbond_atoms(atom *i, atom *j)
{
        // Delete info from bond lists for atoms i and j.
	dbg_begin(DM_CALLS,"model::unbond_atoms");
	// Find bond between atoms
	bond *b = i->find_bond(j);
	if (b == NULL)
	{
		printf("Couldn't locate bond to unbond!\n");
		dbg_end(DM_CALLS,"model::unbond_atoms");
		return;
	}
	b->bondi->detach_bond(b);
	b->bondj->detach_bond(b);
	log_change(LOG_STRUCTURE);
	dbg_end(DM_CALLS,"model::unbond_atoms");
}

// Delete All Bonding
void model::clear_bonding()
{
	dbg_begin(DM_CALLS,"model::clear_bonding");
        // Clear the bond list.
	atom* i = atoms.first();
	while (i != NULL)
	{
		i->clear_bonds();
		i = i->next;
	}
	log_change(LOG_STRUCTURE);
	dbg_end(DM_CALLS,"model::clear_bonding");
}

// Calculate Bonding
void model::calculate_bonding()
{
        // Given the atoms alone, calculate bonding between them using common VDW radii.
	dbg_begin(DM_CALLS,"model::calculate_bonding");
        atom *i,*j;
	int el;
	double dist;
	double tolerance = prefs.get_bond_tolerance();
        double radius_i, radsum;
        clear_bonding();
	msg(DM_NONE,"Calculating bonds in model (tolerance = %5.2f)...",tolerance);
        i = atoms.first();
        while (i != NULL)
        {
		// Check for excluded elements
		el = i->get_element();
		if (el == 0)
		{
			i = i->next;
			continue;
		}
		radius_i = elements.radius(el);
                j = i->next;
                while (j != NULL)
                {
			el = j->get_element();
			if (el == 0)
			{
				j = j->next;
				continue;
			}
			dist = cell.distance(i,j);
                        radsum = radius_i + elements.radius(el);
			if (dist < radsum*tolerance) bond_atoms(i,j,BT_SINGLE);
                        j = j->next;
                }
                i = i->next;
        }
	msg(DM_NONE," Done.\n");
	dbg_end(DM_CALLS,"model::calculate_bonding");
}

// Calculate Bonding within Patterns
void model::pattern_calculate_bonding()
{
	dbg_begin(DM_CALLS,"model::pattern_calculate_bonding");
        atom *i,*j;
	pattern *p;
	int ii,jj,el,m;
	double dist;
	double tolerance = prefs.get_bond_tolerance();
        double radius_i, radsum;
        clear_bonding();
	msg(DM_NONE,"Calculating bonds within patterns (tolerance = %5.2f)...",tolerance);
	// For all the pattern nodes currently defined, bond within molecules
	for (p = patterns.first(); p != NULL; p = p->next)
	{
		// Loop over molecules
		i = p->get_firstatom();
		for (m=0; m<p->get_nmols(); m++)
		{
			for (ii = 0; ii < p->get_natoms()-1; ii ++)
			{
				// Check for excluded elements
				el = i->get_element();
				if (el == 0)
				{
					i = i->next;
					continue;
				}
				radius_i = elements.radius(el);
				// Start loop over second atom in molecule
				j = i->next;
				for (jj = ii+1; jj < p->get_natoms(); jj ++)
				{
					el = j->get_element();
					if (el == 0)
					{
						j = j->next;
						continue;
					}
					dist = cell.distance(i,j);
				//printf("i %i j %i dist %8.3f\n",i->get_id(),j->get_id(),dist);
					radsum = radius_i + elements.radius(el);
					if (dist < radsum*tolerance) bond_atoms(i,j,BT_SINGLE);
					j = j->next;
				}
				i = i->next;
			}
			// Skip on one more atom, since the i loop ran from 0 to natoms-1
			i = i->next;
		}
	}
	msg(DM_NONE," Done.\n");
	dbg_end(DM_CALLS,"model::pattern_calculate_bonding");
}

// Calculate Bonding in current selection
void model::selection_calculate_bonding()
{
	dbg_begin(DM_CALLS,"model::selection_calculate_bonding");
	double tolerance = prefs.get_bond_tolerance();
	double radsum, dist;
	// Calculate all bonds between currently selected atoms
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected())
		{
			atom *j = i->next;
			while (j != NULL)
			{
				if (j->is_selected())
				{
					dist = cell.distance(i,j);
		                        radsum = (elements.radius(i) + elements.radius(j));
					if (dist < radsum*tolerance)
							if (i->find_bond(j) == NULL) bond_atoms(i,j,BT_SINGLE);
				}
				j = j->next;
			}
		}
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::selection_calculate_bonding");
}

// Bond all atoms in current selection
void model::selection_bond_all()
{
	// Add bonds between all atoms in current selection
	dbg_begin(DM_CALLS,"model::selection_bond_all");
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected())
		{
			atom *j = i->next;
			while (j != NULL)
			{
				if (j->is_selected())
					if (i->find_bond(j) == NULL) bond_atoms(i,j,BT_SINGLE);
				j = j->next;
			}
		}
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::selection_bond_all");
}

// Clear Bonding in current selection
void model::selection_clear_bonding()
{
	// Clear all bonds between currently selected atoms
	dbg_begin(DM_CALLS,"model::selection_clear_bonding");
	float radsum;
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected())
		{
			atom *j = i->next;
			while (j != NULL)
			{
				if (j->is_selected())
					if (i->find_bond(j) != NULL) unbond_atoms(i,j);
				j = j->next;
			}
		}
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::selection_clear_bonding");
}

// Augment bonding for all patterns
void model::augment_bonding()
{
	dbg_begin(DM_CALLS,"model::augment_bonding");
	// Perform pattern-wide augmenting
	if (autocreate_patterns())
	{
		pattern *p = patterns.first();
		while (p != NULL)
		{
			p->augment_bonding();
			p->propagate_bondtypes();
			p = p->next;
		}
	}
	dbg_end(DM_CALLS,"model::augment_bonding");
}

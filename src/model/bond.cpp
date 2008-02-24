/*
	*** Model bond functions
	*** src/model/bond.cpp
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

#include "model/model.h"
#include "classes/atom.h"
#include "classes/bond.h"
#include "classes/pattern.h"
#include "classes/undostate.h"
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
		//printf("Bonding atoms %i and %i\n",i->get_id(),j->get_id());
		bond *newbond = new bond;
		newbond->type = bt;
		newbond->bondi = i;
		i->accept_bond(newbond);
		newbond->bondj = j;
		j->accept_bond(newbond);
		log_change(LOG_STRUCTURE);
		// Add the change to the undo state (if there is one)
		if (recordingstate != NULL)
		{
			change *newchange = recordingstate->changes.add();
			newchange->set(UE_BOND,i->get_id(),j->get_id(),bt);
		}
	}
	dbg_end(DM_CALLS,"model::bond_atoms");
}

// Add Bond (id's)
void model::bond_atoms(int ii, int jj, bond_type bt)
{
        // Create a new bond for each atom and add them to the atom's own lists.
	dbg_begin(DM_CALLS,"model::bond_atoms[int]");
	//printf("Atom ids given to model::bond_atoms() are %i and %i (natoms=%i)\n",ii,jj,atoms.size());
	if (ii == jj) msg(DM_NONE,"Cannot bond an atom to itself!\n");
	else
	{
		// First, locate the two atoms with the specified id's
		atom *i = get_atom(ii);
		atom *j = get_atom(jj);
		if (i == NULL || j == NULL)
		{
			printf("Couldn't locate one or both atoms in bond with specified ids %i and %i\n",ii,jj);
			dbg_end(DM_CALLS,"model::bond_atoms[int]");
			return;
		}
		bond_atoms(i,j,bt);
	}
	dbg_end(DM_CALLS,"model::bond_atoms[int]");
}

// Delete Bond
void model::unbond_atoms(atom *i, atom *j, bond *bij)
{
        // Delete info from bond lists for atoms i and j.
	dbg_begin(DM_CALLS,"model::unbond_atoms");
	// Find bond between atoms (unless already supplied)
	bond *b;
	if (bij != NULL) b = bij;
	else
	{
		b = i->find_bond(j);
		if (b == NULL)
		{
			printf("Couldn't locate bond to unbond!\n");
			dbg_end(DM_CALLS,"model::unbond_atoms");
			return;
		}
	}
	// Store type for use later
	bond_type bt = b->type;
	b->bondi->detach_bond(b);
	b->bondj->detach_bond(b);
	log_change(LOG_STRUCTURE);
	// Add the change to the undo state (if there is one)
	if (recordingstate != NULL)
	{
		change *newchange = recordingstate->changes.add();
		newchange->set(-UE_BOND,i->get_id(),j->get_id(),bt);
	}
	dbg_end(DM_CALLS,"model::unbond_atoms");
}

// Delete All Bonding
void model::clear_bonding()
{
	dbg_begin(DM_CALLS,"model::clear_bonding");
        // Clear the bond list.
	for (atom* i = atoms.first(); i != NULL; i = i->next)
	{
		refitem<bond,int> *bref = i->get_bonds();
		while (bref != NULL)
		{
			// Need to detach the bond from both atoms involved
			bond *b = bref->item;
			atom *j = b->get_partner(i);
			unbond_atoms(i,j,b);
			bref = i->get_bonds();
		}
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
	for (i = atoms.first(); i != NULL; i = i->next)
	{
		// Check for excluded elements
		el = i->get_element();
		if (el == 0) continue;
		radius_i = elements.radius(el);
		for (j = i->next; j != NULL; j = j->next)
		{
			el = j->get_element();
			if (el == 0) continue;
			dist = cell.distance(i,j);
			radsum = radius_i + elements.radius(el);
			if (dist < radsum*tolerance) bond_atoms(i,j,BT_SINGLE);
		}
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
	atom *i, *j;
	// Calculate all bonds between currently selected atoms
	for (i = atoms.first(); i != NULL; i = i->next)
	{
		if (i->is_selected())
		{
			for (j = i->next; j != NULL; j = j->next)
			{
				if (j->is_selected())
				{
					dist = cell.distance(i,j);
		                        radsum = (elements.radius(i) + elements.radius(j));
					if (dist < radsum*tolerance)
							if (i->find_bond(j) == NULL) bond_atoms(i,j,BT_SINGLE);
				}
			}
		}
	}
	dbg_end(DM_CALLS,"model::selection_calculate_bonding");
}

// Bond all atoms in current selection
void model::selection_bond_all()
{
	// Add bonds between all atoms in current selection
	dbg_begin(DM_CALLS,"model::selection_bond_all");
	atom *i, *j;
	for (i = atoms.first(); i != NULL; i = i->next)
	{
		if (i->is_selected())
		{
			for (j = i->next; j != NULL; j = j->next)
			{
				if (j->is_selected())
					if (i->find_bond(j) == NULL) bond_atoms(i,j,BT_SINGLE);
			}
		}
	}
	dbg_end(DM_CALLS,"model::selection_bond_all");
}

// Clear Bonding in current selection
void model::selection_clear_bonding()
{
	// Clear all bonds between currently selected atoms
	dbg_begin(DM_CALLS,"model::selection_clear_bonding");
	float radsum;
	atom *i, *j;
	for (i = atoms.first(); i != NULL; i = i->next)
	{
		if (i->is_selected())
		{
			for (j = i->next; j != NULL; j = j->next)
			{
				if (j->is_selected())
					if (i->find_bond(j) != NULL) unbond_atoms(i,j);
			}
		}
	}
	dbg_end(DM_CALLS,"model::selection_clear_bonding");
}

// Alter type of bond
void model::change_bond(bond *b, bond_type bt)
{
	bond_type oldorder = b->type;
	b->type = bt;
	log_change(LOG_STRUCTURE);
	// Add the change to the undo state (if there is one)
	if (recordingstate != NULL)
	{
		change *newchange = recordingstate->changes.add();
		newchange->set(UE_BONDORDER,b->bondi->get_id(),b->bondj->get_id(),oldorder,bt);
	}
}

// Optimise bond order between specified atoms
void model::augment_bond(atom *i, atom *j, int change)
{
	bond *b = i->find_bond(j);
	if (b != NULL) augment_bond(b, change);
	else printf("model::augment_bond <<<< Couldn't find bond between *i and *j >>>>\n");
}

// Optimise bond order of selected bond
void model::augment_bond(bond *b, int change)
{
	// Increase the type of the bond between this atom and 'j' by as much as both atoms will allow.
	// Assumes current bond order differences are held in i->tempi.
	dbg_begin(DM_CALLS,"model::augment_bond");
	int maxchg, n;
	atom *i = b->bondi;
	atom *j = b->bondj;
	// Calc max difference that we can (must) change the bond by...
	maxchg = (abs(i->tempi) < abs(j->tempi) ? i->tempi : j->tempi);
	maxchg /= 2;
	// Sanity check
	if ((change == +1) && (maxchg >= 0))
	{
		dbg_end(DM_CALLS,"model::augment_bond");
		return;
	}
	if ((change == -1) && (maxchg <= 0))
	{
		dbg_end(DM_CALLS,"model::augment_bond");
		return;
	}
	// Store current bond order
	int oldorder = b->type;
	for (n=0; n<abs(maxchg); n++)
	{
		change == +1 ? oldorder ++ : oldorder --;
		j->tempi -= (2*maxchg);
		i->tempi -= (2*maxchg);
		//change == +1 ? oldorder ++ : oldorder --;
	}
	// Set the new bond order
	change_bond(b,(bond_type) oldorder);
	dbg_end(DM_CALLS,"model::augment_bond");
}

// Augment bonding for all model patterns
void model::augment_bonding()
{
	dbg_begin(DM_CALLS,"model::augment_bonding");
	/*
	Assign bond types to the pattern, i.e. automatically determine double, triple, resonant bonds etc.
	We do this by assuming that the structure is chemically 'correct' - i.e. each element is bound to a likely
	number of other elements. If hydrogens are missing then the results will be unpredictable.
	For ions, we do the best we can and force correct bond orders on carbon atoms at the expense of 
	incorrect bond orders on heteroatoms (if possible).
	*/
	if (!autocreate_patterns())
	{
		msg(DM_NONE,"Can't augment bonding without a valid pattern.\n");
		dbg_begin(DM_CALLS,"model::augment_bonding");
		return;
	}
	for (pattern *p = patterns.first(); p != NULL; p = p->next) p->augment();
	dbg_end(DM_CALLS,"model::augment_bonding");
}

void pattern::augment()
{
	dbg_begin(DM_CALLS,"pattern::augment");
	atom *i;
	refitem<bond,int> *bref;
	int n, nheavy;
	msg(DM_NONE,"Augmenting bonds in pattern %s...\n",name.get());
	/*
	We do not reset the present bonding assignments, only check if they're correct. If we find an atom whose
	bond order is too high, we only decrease it if we can find a bound atom in a similar situation.
	So, for the atom 'i':
	-- If its total bond order is equal to its natural valency, do nothing and move on.
	-- If its total bond order is less, get the bound atom with the highest unoccupied valency and increase
		the bond as much as possible. If 'i' is still not satisfied, repeat until all bound atoms have been
		tried.
	-- If its total bond order is higher, search for an atom that also has a too-high bond order. If one is found,
		decrease the bond enough to re-balance. If we can't find one, stop and throw an error.

	Perform this task in three stages to make the whole process more robust. First, do it for rings where we
	only bond within the cycle. Then, do it for terminal atoms or heavy atoms bound to only one other heavy
	atom. Then, do it for the rest.
	*/
	// Calculate current bond orders for atoms in the pattern.
	i = firstatom;
	for (n=0; n<natoms; n++)
	{
		i->tempi = i->total_bond_order() - 2*elements.valency(i->get_element());
		i = i->next;
	}
	// Stage 1 - Augment heavy atoms with only one heavy atom bond
	i = firstatom;
	for (n=0; n<natoms; n++)
	{
		// Calculate number of heavy atoms attached
		nheavy = 0;
		bref = i->get_bonds();
		while (bref != NULL)
		{
			if (bref->item->get_partner(i)->get_element() != 1) nheavy ++;
			bref = bref->next;
		}
		if (nheavy == 1 && i->tempi != 0)
		{
			for (bref = i->get_bonds(); bref != NULL; bref = bref->next)
			{
				if (i->tempi == 0) break;
				if (i->tempi < 0) parent->augment_bond(bref->item,+1);
				else if (i->tempi > 0) parent->augment_bond(bref->item,-1);
			}
		}
		i = i->next;
	}
	// Stage 2 - Augment within cycles
	for (ring *r = rings.first(); r != NULL; r = r->next)
	{
		// Check atoms bond order difference
		for (refitem<atom,int> *ra = r->atoms.first(); ra != NULL; ra = ra->next)
			if (ra->item->tempi != 0) r->augment_atom(ra, parent);
	}
	// Stage 3 - Second pass, augmenting all atoms
	i = firstatom;
	for (n=0; n<natoms; n++)
	{
		printf("%li  i->tempi = %i\n",i,i->tempi);
		if (i->tempi != 0)
		{
			for (bref = i->get_bonds(); bref != NULL; bref = bref->next)
			{
				printf("%li    bond   i->tempi = %i\n",i,i->tempi);
				if (i->tempi == 0) break;
				if (i->tempi < 0) parent->augment_bond(bref->item,+1);
				else if (i->tempi > 0) parent->augment_bond(bref->item,-1);
			}
		}
		i = i->next;
	}
	propagate_bondtypes();
	dbg_end(DM_CALLS,"pattern::augment");
}

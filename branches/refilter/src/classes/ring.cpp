/*
	*** Atom ring
	*** src/classes/ring.cpp
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
#include "classes/ring.h"
#include "classes/bond.h"
#include "base/elements.h"

// Constructor
ring::ring()
{
	requested_size = -1;
	prev = NULL;
	next = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_RING] ++;
	#endif
}

// Destructors
ring::~ring()
{
	atoms.clear();
	#ifdef MEMDEBUG
		memdbg.destroy[MD_RING] ++;
	#endif
}

// Add atom to ring
bool ring::add_atom(atom *i)
{
	dbg_begin(DM_CALLS,"ring::add_atom");
	// Size check
	if (atoms.size() == requested_size)
	{
		dbg_end(DM_CALLS,"ring::add_atom");
		return FALSE;
	}
	// Duplicate check
	if (atoms.search(i) != NULL)
	{
		dbg_end(DM_CALLS,"ring::add_atom");
		return FALSE;
	}
	// Append a ringatomx to the list, pointing to atom i
	// Store atom ID in the refitem's data1 variable
	atoms.add(i,i->get_id(),0);
	dbg_end(DM_CALLS,"ring::add_atom");
	return TRUE;
}

// Is ring aromatic
bool ring::is_aromatic()
{
	// Determine whether the ring is aromatic.
	dbg_begin(DM_CALLS,"ring::is_aromatic");
	// SP2 atom types should have already been defined, so use these to determine aromaticity.
	// Use a set of exceptions for heteroatoms such as N and O...
	int okatoms = 0;
	bool exitearly = FALSE, result = FALSE;
	refitem<atom> *ra = atoms.first();
	while (ra != NULL)
	{
		switch (ra->item->get_element())
		{
			case (6):	// Carbon
				// Accept nothing less than an AE_SP2. If its not AE_SP" we can break early.
				if (ra->item->is_env(AE_SP2)) okatoms ++;
				else exitearly = TRUE;
				break;
			case (7):	// Nitrogen
				// Add nitrogens anyway, but if they're not flagged as AE_SP2 we must change it's
				// state afterwards, provided the ring checks out to be aromatic. (set_aromatic())
				okatoms ++;
				break;
			case (8):	// Oxygen
				// Same as for nitrogen
				okatoms ++;
				break;
			case (16):	// Sulfur
				// Again, see O and N
				okatoms ++;
				break;
		}
		ra = ra->next;
		if (exitearly) break;
	}
	// Now we just check 'okatoms' if it equals the number of atoms in the ring, then it should be aromatic!
	if (okatoms == atoms.size()) result = TRUE;
	dbg_end(DM_CALLS,"ring::is_aromatic");
	return result;
}

// Set aromatic
void ring::set_aromatic()
{
	// Set the environment flags of the constituent atoms of the ring to AE_AROMATIC.
	dbg_begin(DM_CALLS,"ring::set_aromatic");
	refitem<atom> *ra = atoms.first();
	while (ra != NULL)
	{
		ra->item->set_env(AE_AROMATIC);
		ra = ra->next;
	}
	dbg_end(DM_CALLS,"ring::set_aromatic");
}

// Finalise ring
void ring::finish()
{
	// Perform some finishing tasks on the list
	dbg_begin(DM_CALLS,"ring::finish");
	refitem<atom> *ra, *temp, *lowid;
	// Make the list head point to the atom with the lowest id
	if (atoms.size() == 0)
	{	
		printf("No atoms in ring - can't finalise!\n");
		dbg_end(DM_CALLS,"ring::finish");
		return;
	}
	// First, find the lowest atomid
	ra = atoms.first();
	lowid = ra;
	while (ra != NULL)
	{
		if (ra->data1 < lowid->data1) lowid = ra;
		ra = ra->next;
	}
	// Make this atom (with lowest id) the head of the list
	ra = atoms.first();
	while (ra != lowid)
	{
		atoms.move_head_to_tail();
		ra = atoms.first();
	}
	// Set the bond type flags in each ringatomx
	ra = atoms.first();
	while (ra != NULL)
	{
		// Search the bond list of this atom for the next atom in the list
		refitem<bond> *bref = ra->item->get_bonds();
		while (bref != NULL)
		{
			if (bref->item->get_partner(ra->item) == get_next(ra)->item) ra->data1 = bref->item->type;
			bref = bref->next;
		}
		ra = ra->next;
	}
	dbg_end(DM_CALLS,"ring::finish");
}

// Copy ring
void ring::copy(ring *source)
{
	// Copy the data in source to the current ring.
	atoms.clear();
	refitem<atom> *ra = source->atoms.first();
	while (ra != NULL)
	{
		bool succes = add_atom(ra->item);
		ra = ra->next;
	}
}

// Print
void ring::print()
{
	// Print out the data of the ring.
	// Beware, since if it has been 'finished' it will be a circular list
	msg(DM_VERBOSE,"Ring has %i atoms : ",atoms.size());
	refitem<atom> *ra = atoms.first();
	while (ra != NULL)
	{
		msg(DM_VERBOSE,"%s(%i),",elements.symbol(ra->item),ra->data1);
		//printf("%s(%i),",elements.el[ra->i->el].symbol.c_str(),ra->i->tempi);
		ra = ra->next;
	}
	msg(DM_VERBOSE,"\n");
}

// Add atoms to reflist
void ring::add_atoms_to_reflist(reflist<atom> *rlist, atom *i)
{
	// Add all atoms in the ring 'r' to the list, excluding the atom 'i'
	refitem<atom> *ra = atoms.first();
	while (ra != NULL)
	{
		if (ra->item != i) rlist->add(ra->item,0,0);
		ra = ra->next;
	}
}

// Augment ring atom
void ring::augment_atom(refitem<atom> *refatom, model *parent)
{
	dbg_begin(DM_CALLS,"ring::augment_atom");
	// Assumes current bond order differences are in i->tempi
	atom *i, *j;
	i = refatom->item;
	if (i->tempi < 0)
	{
		// Atom has fewer bonds than expected, so try to augment within ring.
		j = get_next(refatom)->item;
		if (j->tempi < 0) parent->augment_bond(i,j,+1);
		if (i->tempi != 0)
		{
			j = get_prev(refatom)->item;
			if (j->tempi < 0) parent->augment_bond(i,j,+1);
		}
	}
	else if (i->tempi > 0)
	{
		// Atom has more bonds than expected, so try to de-augment within ring.
		j = get_next(refatom)->item;
		if (j->tempi > 0) parent->augment_bond(i,j,-1);
		if (i->tempi != 0)
		{
			j = get_prev(refatom)->item;
			if (j->tempi > 0) parent->augment_bond(i,j,-1);
		}
	}
	dbg_end(DM_CALLS,"ring::augment_atom");
}

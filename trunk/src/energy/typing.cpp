/*
	*** Atom typing routines
	*** src/energy/typing.cpp
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

#include "base/elements.h"
#include "model/model.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"
#include "classes/bond.h"

/*
Atom typing is performed in several steps.

0)	Current bonding pattern in the model / pattern is augmented
1) +-	Ring structures are located and stored
2) |	Atom hybridisations are assigned based only on bond types on individual atoms
3) |	Aromatic atoms are flagged as AE_AROMATIC, based on analysis of ring structures
4) +-	Typing rules from the forcefield are then applied to each atom in turn

*/

void printstuff(pattern *p)
{
	atom *i = p->get_firstatom();
	for (int n=0; n<p->get_natoms(); n++)
	{
		msg(DM_VERBOSE,"Atom %i, %s[%i], nbonds=%i, valency=%i, type=%s\n",n,elements.symbol(i),
			i->get_id(),i->get_nbonds(),elements.valency(i),text_from_AE(i->get_env()));
		i = i->next;
	}
}

// Describe atoms in model
void model::describe_atoms()
{
	// Locate ring structures, augment bonding, and assign atom hybridisations in all patterns.
	dbg_begin(DM_CALLS,"model::describe_atoms");
	pattern *p = patterns.first();
	while (p != NULL)
	{
		// 1) Locate ring structures
		p->find_rings();
		// Augment bonding in model
		p->augment_bonding();
		// 2) Reset atom environments
		p->clear_hybrids();
		printstuff(p);
		// 3) Assign hybridisation types
		p->assign_hybrids();
		printstuff(p);
		// 4) Go through the ring list and see if any are aromatic
		ring *r = p->get_rings();
		while (r != NULL)
		{
			if (r->is_aromatic()) r->set_aromatic();
			r = r->next;
		}
		p = p->next;
	}
	dbg_end(DM_CALLS,"model::describe_atoms");
}

// Type all atoms
bool model::type_all()
{
	// Perform forcefield typing on all patterns in the model.
	// Most routines here only use the first molecule in the pattern, so we must propagate the type info
	// to other molecules at the end.
	dbg_begin(DM_CALLS,"model::type_all");
	// Must have a valid pattern...
	autocreate_patterns();
	if (!patterns_are_valid())
	{
		msg(DM_NONE,"Atom typing cannot be performed without a valid pattern.\n Check pattern definition.\n");
		dbg_end(DM_CALLS,"model::type_all");
		return FALSE;
	}
	// Describe the atoms / rings in the patterns
	describe_atoms();
	// Assign forcefield types to atoms
	pattern *p = patterns.first();
	while (p != NULL)
	{
		msg(DM_NONE,"Typing pattern %s...",p->get_name());
		if (!p->type_atoms(ff))
		{
			dbg_end(DM_CALLS,"model::type_all");
			return FALSE;
		}
		// Finally, propagate the data now contained in the initial molecule in each pattern to all other molecules
		p->propagate_atomtypes();
		p->propagate_bondtypes();
		msg(DM_NONE,"Done.\n");
		p = p->next;
	}
	dbg_end(DM_CALLS,"model::type_all");
	return TRUE;
}

// Clear hybridisation data
void pattern::clear_hybrids()
{
	// Set all environment flags of the atoms in pattern to AE_UNSPECIFIED
	dbg_begin(DM_CALLS,"pattern::reset_atomenv");
	atom *i = firstatom;
	for (int n=0; n<natoms; n++)
	{
		i->set_env(AE_UNSPECIFIED);
		i = i->next;
	}
	dbg_end(DM_CALLS,"pattern::reset_atomenv");
}

// Assign hybridisation data
void pattern::assign_hybrids()
{
	// Assign hybridisation types to the atoms in this pattern.
	dbg_begin(DM_CALLS,"pattern::assign_hybrids");
	atom *i = firstatom;
	for (int n=0; n<natoms; n++)
	{
		// Set to AE_UNBOUND to begin with
		i->set_env(AE_UNBOUND);
		// Work out the hybridisation based on the bond types connected to the atom.
		// We can increase the hybridisation at any point, but never decrease it.
		refitem<bond> *bref = i->get_bonds();
		while (bref != NULL)
		{
			switch (bref->item->type)
			{
				case (BT_SINGLE):
					if (i->get_env() < AE_SP3) i->set_env(AE_SP3); break;
				case (BT_DOUBLE):
					if (i->get_env() < AE_SP2) i->set_env(AE_SP2); break;
				case (BT_TRIPLE):
					if (i->get_env() < AE_SP) i->set_env(AE_SP); break;
			}
			bref = bref->next;
		}
		i = i->next;
	}
	dbg_end(DM_CALLS,"pattern::assign_hybrids");
}

// Type atoms in pattern
bool pattern::type_atoms(forcefield *xff)
{
	// Assign atom types from the forcefield based on the typing rules supplied.
	// Since there may be more than one match for a given atom (when relaxed rules are used, e.g.
	// UFF) we find the best of the types available. If any one criterion doesn't match in the atom 
	// type description, we reject it. Otherwise, store the number of criteria that matched and only
	// accept a different atom type if we manage to match a complete set containing more rules.
	// Return FALSE if one or more atoms could not be typed
	dbg_begin(DM_CALLS,"pattern::type_atoms");
	int a,n,newmatch,bestmatch;
	atomtype *at;
	atom *i;
	ffatom *ffa;
	bool result = TRUE;
	// Select the forcefield we're typing with
	if (ff != NULL) xff = ff;
	if (xff == NULL)
	{	
		msg(DM_NONE,"pattern::type_atoms : Can't type - no FF associated to pattern %s.\n",name.get());
		dbg_end(DM_CALLS,"pattern::type_atoms");
		return FALSE;
	}
	// Loop over atoms in the pattern's molecule
	i = firstatom;
	for (a=0; a<natoms; a++)
	{
		msg(DM_TYPING,"pattern::type_atoms : FFTyping atom number %i, element %s\n",a,elements.symbol(i->get_element()));
		bestmatch = 0;
		i->set_fftype(NULL);
		// Loop over forcefield atom types
		for (ffa = xff->get_atomtypes(); ffa != NULL; ffa = ffa->next)
		{
			// Grab next atomtype and reset tempi variables
			at = ffa->get_atomtype();
			// First, check element is the same, otherwise skip
			if (i->get_element() != at->el) continue;
			reset_tempi(0);
			// See how well this ff description matches the environment of our atom 'i'
			msg(DM_TYPING,"pattern::type_atoms : Matching type id %i\n",ffa->get_ffid());
			newmatch = at->match_atom(i,&rings,ownermodel);
			msg(DM_TYPING,"pattern::type_atoms : ...Total match score for type %i = %i\n",ffa->get_ffid(),newmatch);
			if (newmatch > bestmatch)
			{
				// Better match found...
				bestmatch = newmatch;
				i->set_fftype(ffa);
			}
		}
		msg(DM_TYPING,"pattern::type_atoms : FFType for atom is : %i\n",i->get_fftype());
		if (i->get_fftype() == 0)
		{
			msg(DM_NONE,"Failed to type atom - %s, id = %i, nbonds = %i.\n",elements.name(i),i->get_id(),i->get_nbonds());
			result = FALSE;
		}
		i = i->next;
	}
	dbg_end(DM_CALLS,"pattern::type_atoms");
	return result;
}

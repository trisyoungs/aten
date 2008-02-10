/*
	*** Molecular restraint
	*** src/classes/restraint.cpp
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

#include "classes/restraint.h"
#include "templates/reflist.h"
#include "model/model.h"
#include "base/debug.h"

// Constructors
restraints::restraints()
{
	#ifdef MEMDEBUG
	memdbg.create[MD_RESTRAINTS] ++;
	#endif
}

restraint_ij::restraint_ij()
{
	rij = 0.0;
	next = NULL;
	prev = NULL;
	#ifdef MEMDEBUG
	memdbg.create[MD_RESTRAINT_IJ] ++;
	#endif
}

// Destructors
restraints::~restraints()
{
	ijs.clear();
	#ifdef MEMDEBUG
	memdbg.destroy[MD_RESTRAINTS] ++;
	#endif
}

restraint_ij::~restraint_ij()
{
	#ifdef MEMDEBUG
	memdbg.destroy[MD_RESTRAINT_IJ] ++;
	#endif
}

// Add Distance
void restraints::add_ij(reflist<atom,int> &rl)
{
	dbg_begin(DM_CALLS,"restraints::add_ij");
	if (rl.size() != 2)
	{
		printf("Not enough atoms in supplied list to add distance restraint.\n");
		dbg_end(DM_CALLS,"restraints::add_ij");
		return;
	}
	restraint_ij *newdist = ijs.add();
	// Initialise some variables
	refitem<atom,int> *ri = rl.first();
	newdist->i = ri->item;
	newdist->j = ri->next->item;
	newdist->rij = ownermodel->distance(newdist->i, newdist->j);
	dbg_end(DM_CALLS,"restraints::add_ij");
}

// Find Distance
restraint_ij *restraints::does_ij_exist(reflist<atom,int> &rl)
{
	// Go through the list of restraints and check if the current distance is already measured
	dbg_begin(DM_CALLS,"restraints::does_lj_exist");
	restraint_ij *xdist, *result;
	refitem<atom,int> *ri = rl.first();
	result = NULL;
	xdist = ijs.first();
	while (xdist != NULL)
	{
	        if ((xdist->i == ri->item) && (xdist->j == ri->next->item)) result = xdist;
	        if ((xdist->i == ri->next->item) && (xdist->j == ri->item)) result = xdist;
	        if (result != NULL) break;
	        xdist = xdist->next;
	}
	dbg_end(DM_CALLS,"restraints::does_lj_exist");
	return result;
}

// Clear all restraints
void restraints::clear_all()
{
	dbg_begin(DM_CALLS,"restraints::clear_all");
	ijs.clear();
	dbg_end(DM_CALLS,"restraints::clear_all");
}

// Prune restraints for deleted atoms
void restraints::prune_atom(atom *xatom)
{
	// Search the lists of restraints for the supplied atom, and remove any that use it
	dbg_begin(DM_CALLS,"restraints::prune_atom");
	restraint_ij *ij = ijs.first();
	restraint_ij *tempij;
	while (ij != NULL)
	{
		if ((ij->i == xatom) || (ij->j == xatom))
		{
			tempij = ij->next;
			ijs.remove(ij);
			ij = tempij;
		}
		else ij = ij->next;
	}
	/*
	restraint_ijk *ijk = ijks.first();
	restraint_ijk *tempijk;
	while (ijk != NULL)
	{
		if (ijk->i == xatom || ijk->j == xatom || ijk->k == xatom)
		{
			tempijk = ijk->next;
			ijks.remove(ijk);
			ijk = tempijk;
		}
		else ijk = ijk->next;
	}
	restraint_ijkl *ijkl = ijkls.first();
	restraint_ijkl *tempijkl;
	while (ijkl != NULL)
	{
		if (ijkl->i == xatom || ijkl->j == xatom || ijkl->k == xatom || ijkl->l == xatom)
		{
			tempijkl = ijkl->next;
			ijkls.remove(ijkl);
			ijkl = tempijkl;
		}
		else ijkl = ijkl->next;
	}
	*/
	dbg_end(DM_CALLS,"restraints::prune_atom");
}

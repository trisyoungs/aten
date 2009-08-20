/*
	*** Molecular restraint
	*** src/classes/restraint.cpp
	Copyright T. Youngs 2007-2009

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
#include "base/messenger.h"

// Constructors
restraints::restraints()
{
}

restraint_ij::restraint_ij()
{
	rij = 0.0;
	next = NULL;
	prev = NULL;
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
void restraints::add_ij(Reflist<Atom,int> &rl)
{
	msg.enter("restraints::add_ij");
	if (rl.nItems() != 2)
	{
		printf("Not enough atoms in supplied list to add distance restraint.\n");
		msg.exit("restraints::add_ij");
		return;
	}
	restraint_ij *newdist = ijs.add();
	// Initialise some variables
	Refitem<Atom,int> *ri = rl.first();
	newdist->i = ri->item;
	newdist->j = ri->next->item;
	newdist->rij = ownermodel->distance(newdist->i, newdist->j);
	msg.exit("restraints::add_ij");
}

// Find Distance
restraint_ij *restraints::does_ij_exist(Reflist<Atom,int> &rl)
{
	// Go through the list of restraints and check if the current distance is already measured
	msg.enter("restraints::does_lj_exist");
	restraint_ij *xdist, *result;
	Refitem<Atom,int> *ri = rl.first();
	result = NULL;
	xdist = ijs.first();
	while (xdist != NULL)
	{
	        if ((xdist->i == ri->item) && (xdist->j == ri->next->item)) result = xdist;
	        if ((xdist->i == ri->next->item) && (xdist->j == ri->item)) result = xdist;
	        if (result != NULL) break;
	        xdist = xdist->next;
	}
	msg.exit("restraints::does_lj_exist");
	return result;
}

// Clear all restraints
void restraints::clear_all()
{
	msg.enter("restraints::clear_all");
	ijs.clear();
	msg.exit("restraints::clear_all");
}

// Prune restraints for deleted atoms
void restraints::prune_atom(Atom *xatom)
{
	// Search the lists of restraints for the supplied atom, and remove any that use it
	msg.enter("restraints::prune_atom");
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
	msg.exit("restraints::prune_atom");
}

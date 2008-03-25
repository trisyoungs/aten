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
void Model::bondAtoms(Atom *i, Atom *j, Bond::BondType bt)
{
        // Create a new bond each atom and add them to the atom's own lists.
	dbgBegin(DM_CALLS,"Model::bondAtoms");
	if (i == j) msg(DM_NONE,"Cannot bond an atom to itself!\n");
	else
	{
		// Search for old bond between atoms
		Bond *b = i->findBond(j);
		// If we found one, just set the new bond order
		if (b != NULL)
		{
			// Check order of existing bond
			if (b->order() != bt)
			{
				b->setOrder(bt);
				logChange(LOG_STRUCTURE);
				// Add the change to the undo state (if there is one)
				if (recordingState_ != NULL)
				{
					Change *newchange = recordingState_->addChange();
					newchange->set(UE_BOND,i->id(),j->id(),bt);
				}
			}
		}
		else
		{
			b = new Bond;
			b->setOrder(bt);
			b->setAtoms(i,j);
			i->acceptBond(b);
			j->acceptBond(b);
			logChange(LOG_STRUCTURE);
			// Add the change to the undo state (if there is one)
			if (recordingState_ != NULL)
			{
				Change *newchange = recordingState_->addChange();
				newchange->set(UE_BOND,i->id(),j->id(),bt);
			}
		}
	}
	dbgEnd(DM_CALLS,"Model::bondAtoms");
}

// Add Bond (id's)
void Model::bondAtoms(int ii, int jj, Bond::BondType bt)
{
        // Create a new bond for each atom and add them to the atom's own lists.
	dbgBegin(DM_CALLS,"Model::bondAtoms[int]");
	//printf("Atom ids given to Model::bondAtoms() are %i and %i (natoms=%i)\n",ii,jj,atoms_.nItems());
	if (ii == jj) msg(DM_NONE,"Cannot bond an atom to itself!\n");
	else
	{
		// First, locate the two atoms with the specified id's
		Atom *i = atom(ii);
		Atom *j = atom(jj);
		if (i == NULL || j == NULL)
		{
			printf("Couldn't locate one or both atoms in bond with specified ids %i and %i\n",ii,jj);
			dbgEnd(DM_CALLS,"Model::bondAtoms[int]");
			return;
		}
		bondAtoms(i,j,bt);
	}
	dbgEnd(DM_CALLS,"Model::bondAtoms[int]");
}

// Delete Bond
void Model::unbondAtoms(Atom *i, Atom *j, Bond *bij)
{
        // Delete info from bond lists for atoms i and j.
	dbgBegin(DM_CALLS,"Model::unbondAtoms");
	// Find bond between atoms (unless already supplied)
	Bond *b;
	if (bij != NULL) b = bij;
	else
	{
		b = i->findBond(j);
		if (b == NULL)
		{
			printf("Couldn't locate bond to unbond!\n");
			dbgEnd(DM_CALLS,"Model::unbondAtoms");
			return;
		}
	}
	// Store type for use later
	Bond::BondType bt = b->order();
	b->atomI()->detachBond(b);
	b->atomJ()->detachBond(b);
	logChange(LOG_STRUCTURE);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		Change *newchange = recordingState_->addChange();
		newchange->set(-UE_BOND,i->id(),j->id(),bt);
	}
	dbgEnd(DM_CALLS,"Model::unbondAtoms");
}

// Delete All Bonding
void Model::clearBonding()
{
	dbgBegin(DM_CALLS,"Model::clearBonding");
        // Clear the bond list.
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		Refitem<Bond,int> *bref = i->bonds();
		while (bref != NULL)
		{
			// Need to detach the bond from both atoms involved
			Bond *b = bref->item;
			Atom *j = b->partner(i);
			unbondAtoms(i,j,b);
			bref = i->bonds();
		}
	}
	logChange(LOG_STRUCTURE);
	dbgEnd(DM_CALLS,"Model::clearBonding");
}

// Calculate Bonding
void Model::calculateBonding()
{
        // Given the atoms alone, calculate bonding between them using common VDW radii.
	dbgBegin(DM_CALLS,"Model::calculateBonding");
	Atom *i, *j;
	int el;
	double dist;
	double tolerance = prefs.bondTolerance();
	double radius_i, radsum;
	clearBonding();
	msg(DM_NONE,"Calculating bonds in model (tolerance = %5.2f)...",tolerance);
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		// Check for excluded elements
		el = i->element();
		if (el == 0) continue;
		radius_i = elements.atomicRadius(el);
		for (j = i->next; j != NULL; j = j->next)
		{
			el = j->element();
			if (el == 0) continue;
			dist = cell_.distance(i,j);
			radsum = radius_i + elements.atomicRadius(el);
			if (dist < radsum*tolerance) bondAtoms(i,j,Bond::Single);
		}
	}
	msg(DM_NONE," Done.\n");
	dbgEnd(DM_CALLS,"Model::calculateBonding");
}

// Calculate Bonding within Patterns
void Model::patternCalculateBonding()
{
	dbgBegin(DM_CALLS,"Model::patternCalculateBonding");
	Atom *i,*j;
	int ii, jj, el, m;
	double dist;
	double tolerance = prefs.bondTolerance();
	double radius_i, radsum;
	clearBonding();
	msg(DM_NONE,"Calculating bonds within patterns (tolerance = %5.2f)...",tolerance);
	// For all the pattern nodes currently defined, bond within molecules
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		// Loop over molecules
		i = p->firstAtom();
		for (m=0; m<p->nMols(); m++)
		{
			for (ii = 0; ii < p->nAtoms()-1; ii ++)
			{
				// Check for excluded elements
				el = i->element();
				if (el == 0)
				{
					i = i->next;
					continue;
				}
				radius_i = elements.atomicRadius(el);
				// Start loop over second atom in molecule
				j = i->next;
				for (jj = ii+1; jj < p->nAtoms(); jj ++)
				{
					el = j->element();
					if (el == 0)
					{
						j = j->next;
						continue;
					}
					dist = cell_.distance(i,j);
				//printf("i %i j %i dist %8.3f\n",i->id(),j->id(),dist);
					radsum = radius_i + elements.atomicRadius(el);
					if (dist < radsum*tolerance) bondAtoms(i,j,Bond::Single);
					j = j->next;
				}
				i = i->next;
			}
			// Skip on one more atom, since the i loop ran from 0 to natoms-1
			i = i->next;
		}
	}
	msg(DM_NONE," Done.\n");
	dbgEnd(DM_CALLS,"Model::patternCalculateBonding");
}

// Calculate Bonding in current selection
void Model::selectionCalculateBonding()
{
	dbgBegin(DM_CALLS,"Model::selectionCalculateBonding");
	double tolerance = prefs.bondTolerance();
	double radsum, dist;
	Atom *i, *j;
	// Calculate all bonds between currently selected atoms
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->isSelected())
		{
			for (j = i->next; j != NULL; j = j->next)
			{
				if (j->isSelected())
				{
					dist = cell_.distance(i,j);
		                        radsum = (elements.atomicRadius(i) + elements.atomicRadius(j));
					if (dist < radsum*tolerance)
							if (i->findBond(j) == NULL) bondAtoms(i,j,Bond::Single);
				}
			}
		}
	}
	dbgEnd(DM_CALLS,"Model::selectionCalculateBonding");
}

// Bond all atoms in current selection
void Model::selectionBondAll()
{
	// Add bonds between all atoms in current selection
	dbgBegin(DM_CALLS,"Model::selectionBondAll");
	Atom *i, *j;
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->isSelected())
		{
			for (j = i->next; j != NULL; j = j->next)
			{
				if (j->isSelected())
					if (i->findBond(j) == NULL) bondAtoms(i,j,Bond::Single);
			}
		}
	}
	dbgEnd(DM_CALLS,"Model::selectionBondAll");
}

// Clear Bonding in current selection
void Model::selectionClearBonding()
{
	// Clear all bonds between currently selected atoms
	dbgBegin(DM_CALLS,"Model::selectionClearBonding");
	Atom *i, *j;
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->isSelected())
		{
			for (j = i->next; j != NULL; j = j->next)
			{
				if (j->isSelected())
					if (i->findBond(j) != NULL) unbondAtoms(i,j);
			}
		}
	}
	dbgEnd(DM_CALLS,"Model::selectionClearBonding");
}

// Alter type of bond
void Model::changeBond(Bond *b, Bond::BondType bt)
{
	Bond::BondType oldorder = b->order();
	b->setOrder(bt);
	logChange(LOG_STRUCTURE);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		Change *newchange = recordingState_->addChange();
		newchange->set(UE_BONDORDER,b->atomI()->id(),b->atomJ()->id(),oldorder,bt);
	}
}

// Optimise bond order between specified atoms
void Model::augmentBond(Atom *i, Atom *j, int change)
{
	Bond *b = i->findBond(j);
	if (b != NULL) augmentBond(b, change);
	else printf("Model::augmentBond <<<< Couldn't find bond between *i and *j >>>>\n");
}

// Optimise bond order of selected bond
void Model::augmentBond(Bond *b, int change)
{
	// Increase the type of the bond between this atom and 'j' by as much as both atoms will allow.
	// Assumes current bond order differences are held in i->tempi.
	dbgBegin(DM_CALLS,"Model::augmentBond");
	int maxchg, n;
	Atom *i = b->atomI();
	Atom *j = b->atomJ();
	// Calc max difference that we can (must) change the bond by...
	maxchg = (abs(i->tempi) < abs(j->tempi) ? i->tempi : j->tempi);
	maxchg /= 2;
	// Sanity check
	if ((change == +1) && (maxchg >= 0))
	{
		dbgEnd(DM_CALLS,"Model::augmentBond");
		return;
	}
	if ((change == -1) && (maxchg <= 0))
	{
		dbgEnd(DM_CALLS,"Model::augmentBond");
		return;
	}
	// Store current bond order
	int oldorder = b->order();
	for (n=0; n<abs(maxchg); n++)
	{
		change == +1 ? oldorder ++ : oldorder --;
		j->tempi -= (2*maxchg);
		i->tempi -= (2*maxchg);
		//change == +1 ? oldorder ++ : oldorder --;
	}
	// Set the new bond order
	changeBond(b,(Bond::BondType) oldorder);
	dbgEnd(DM_CALLS,"Model::augmentBond");
}

// Augment bonding for all model patterns
void Model::augmentBonding()
{
	dbgBegin(DM_CALLS,"Model::augmentBonding");
	/*
	Assign bond types to the pattern, i.e. automatically determine double, triple, resonant bonds etc.
	We do this by assuming that the structure is chemically 'correct' - i.e. each element is bound to a likely
	number of other elements. If hydrogens are missing then the results will be unpredictable.
	For ions, we do the best we can and force correct bond orders on carbon atoms at the expense of 
	incorrect bond orders on heteroatoms (if possible).
	*/
	if (!autocreatePatterns())
	{
		msg(DM_NONE,"Can't augment bonding without a valid pattern.\n");
		dbgBegin(DM_CALLS,"Model::augmentBonding");
		return;
	}
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next) p->augment();
	dbgEnd(DM_CALLS,"Model::augmentBonding");
}

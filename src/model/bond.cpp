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
#include "model/undostate.h"
#include "model/undoevent.h"
#include "base/bond.h"
#include "base/pattern.h"
#include "classes/prefs.h"

// Return first bond in the model
Bond *Model::bonds()
{
	return bonds_.first();
}

// Return number of bonds in the model
int Model::nBonds()
{
	return bonds_.nItems();
}

// Return the nth bond in the model
Bond *Model::bond(int n)
{
	return bonds_[n];
}

// Add Bond (pointers)
void Model::bondAtoms(Atom *i, Atom *j, Bond::BondType bt)
{
        // Create a new bond each atom and add them to the atom's own lists.
	msg.enter("Model::bondAtoms");
	if (i == j) msg.print("Cannot bond an atom to itself!\n");
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
				Bond::BondType oldtype = b->type();
				b->setType(bt);
				changeLog.add(Log::Structure);
				// Add the change to the undo state (if there is one)
				if (recordingState_ != NULL)
				{
					BondTypeEvent *newchange = new BondTypeEvent;
					newchange->set(i->id(), j->id(), oldtype, bt);
					recordingState_->addEvent(newchange);
				}
			}
		}
		else
		{
			b = bonds_.add();
			b->setType(bt);
			b->setAtoms(i,j);
			i->acceptBond(b);
			j->acceptBond(b);
			changeLog.add(Log::Structure);
			// Add the change to the undo state (if there is one)
			if (recordingState_ != NULL)
			{
				BondEvent *newchange = new BondEvent;
				newchange->set(TRUE, i->id(), j->id(), bt);
				recordingState_->addEvent(newchange);
			}
		}
	}
	msg.exit("Model::bondAtoms");
}

// Add Bond (id's)
void Model::bondAtoms(int ii, int jj, Bond::BondType bt)
{
        // Create a new bond for each atom and add them to the atom's own lists.
	msg.enter("Model::bondAtoms[int]");
	//printf("Atom ids given to Model::bondAtoms() are %i and %i (natoms=%i)\n",ii,jj,atoms_.nItems());
	if (ii == jj) msg.print("Cannot bond an atom to itself!\n");
	else
	{
		// First, locate the two atoms with the specified id's
		Atom *i = atom(ii);
		Atom *j = atom(jj);
		if (i == NULL || j == NULL)
		{
			printf("Couldn't locate one or both atoms in bond with specified ids %i and %i\n",ii,jj);
			msg.exit("Model::bondAtoms[int]");
			return;
		}
		bondAtoms(i,j,bt);
	}
	msg.exit("Model::bondAtoms[int]");
}

// Delete Bond
void Model::unbondAtoms(Atom *i, Atom *j, Bond *bij)
{
        // Delete info from bond lists for atoms i and j.
	msg.enter("Model::unbondAtoms");
	// Find bond between atoms (unless already supplied)
	Bond *b;
	if (bij != NULL) b = bij;
	else
	{
		b = i->findBond(j);
		if (b == NULL)
		{
			printf("Couldn't locate bond to unbond!\n");
			msg.exit("Model::unbondAtoms");
			return;
		}
	}
	// Store type for use later
	Bond::BondType bt = b->type();
	i->detachBond(b);
	j->detachBond(b);
	bonds_.remove(b);
	changeLog.add(Log::Structure);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		BondEvent *newchange = new BondEvent;
		newchange->set(FALSE, i->id(), j->id(), bt);
		recordingState_->addEvent(newchange);
	}
	msg.exit("Model::unbondAtoms");
}

// Delete All Bonding
void Model::clearBonding()
{
	msg.enter("Model::clearBonding");
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
	changeLog.add(Log::Structure);
	msg.exit("Model::clearBonding");
}

// Calculate Bonding
void Model::calculateBonding()
{
        // Given the atoms alone, calculate bonding between them using common VDW radii.
	msg.enter("Model::calculateBonding");
	Atom *i, *j;
	int el;
	double dist;
	double tolerance = prefs.bondTolerance();
	double radius_i, radsum;
	clearBonding();
	msg.print(Messenger::Verbose, "Calculating bonds in model (tolerance = %5.2f)...",tolerance);
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
	msg.print(Messenger::Verbose, "Done.\n");
	msg.exit("Model::calculateBonding");
}

// Calculate Bonding within Patterns
void Model::patternCalculateBonding()
{
	msg.enter("Model::patternCalculateBonding");
	Atom *i,*j;
	int ii, jj, el, m;
	double dist;
	double tolerance = prefs.bondTolerance();
	double radius_i, radsum;
	clearBonding();
	msg.print("Calculating bonds within patterns (tolerance = %5.2f)...",tolerance);
	// For all the pattern nodes currently defined, bond within molecules
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		// Loop over molecules
		i = p->firstAtom();
		for (m=0; m<p->nMolecules(); m++)
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
	msg.print(" Done.\n");
	msg.exit("Model::patternCalculateBonding");
}

// Calculate Bonding in current selection
void Model::selectionCalculateBonding()
{
	msg.enter("Model::selectionCalculateBonding");
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
	msg.exit("Model::selectionCalculateBonding");
}

// Bond all atoms in current selection
void Model::selectionBondAll()
{
	// Add bonds between all atoms in current selection
	msg.enter("Model::selectionBondAll");
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
	msg.exit("Model::selectionBondAll");
}

// Clear Bonding in current selection
void Model::selectionClearBonding()
{
	// Clear all bonds between currently selected atoms
	msg.enter("Model::selectionClearBonding");
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
	msg.exit("Model::selectionClearBonding");
}

// Alter type of bond
void Model::changeBond(Bond *b, Bond::BondType bt)
{
	Bond::BondType oldorder = b->type();
	b->setType(bt);
	changeLog.add(Log::Structure);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		BondTypeEvent *newchange = new BondTypeEvent;
		newchange->set(b->atomI()->id(), b->atomJ()->id(), oldorder, bt);
		recordingState_->addEvent(newchange);
	}
}

// Augment bonding for all model patterns
void Model::augmentBonding()
{
	msg.enter("Model::augmentBonding");
	if (!autocreatePatterns())
	{
		msg.print("Can't augment bonding without a valid pattern.\n");
		msg.exit("Model::augmentBonding");
		return;
	}
	describeAtoms();
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next) p->augment();
	msg.exit("Model::augmentBonding");
}

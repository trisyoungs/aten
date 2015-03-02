/*
	*** Model selection functions
	*** src/model/selection.cpp
	Copyright T. Youngs 2007-2015

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
#include "model/undoevent.h"
#include "model/undostate.h"
#include "base/pattern.h"
#include "base/sysfunc.h"
#include "base/neta.h"

ATEN_USING_NAMESPACE

/*
// Private Functions
*/

// Move specified atom up in the list (to lower ID)
void Model::shiftAtomUp(Atom* i)
{
	Messenger::enter("Model::shiftAtomUp");
	if (i == NULL)
	{
		printf("Internal Error: NULL Atom pointer passed to shiftAtomUp.");
		Messenger::exit("Model::shiftAtomUp");
		return;
	}

	// Ignore request if this is the head of the list
	if (i->id() == 0)
	{
		Messenger::exit("Model::shiftAtomUp");
		return;
	}

	int tempid, oldid;
	oldid = i->id();
	// Shift atom up
	atoms_.shiftUp(i);
	// Swap atomids with the new 'next' atom
	tempid = i->next->id();
	i->next->setId(oldid);
	i->setId(tempid);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		IdShiftEvent *newchange = new IdShiftEvent;
		newchange->set(oldid, -1);
		recordingState_->addEvent(newchange);
	}
	changeLog.add(Log::Structure);
	Messenger::exit("Model::shiftAtomUp");
}

// Move specified atom up in the list (to higher ID)
void Model::shiftAtomDown(Atom* i)
{
	Messenger::enter("Model::shiftAtomDown");
	if (i == NULL)
	{
		printf("Internal Error: NULL Atom pointer passed to shiftAtomDown.");
		Messenger::exit("Model::shiftAtomDown");
		return;
	}

	// Ignore request if this is the head of the list
	if (i->id() == (atoms_.nItems()-1))
	{
		Messenger::exit("Model::shiftAtomDown");
		return;
	}

	int tempid, oldid;
	oldid = i->id();
	// Shift atom down
	atoms_.shiftDown(i);
	// Swap atomids with the new 'next' atom
	tempid = i->prev->id();
	i->prev->setId(oldid);
	i->setId(tempid);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		IdShiftEvent *newchange = new IdShiftEvent;
		newchange->set(oldid, 1);
		recordingState_->addEvent(newchange);
	}
	changeLog.add(Log::Structure);
	Messenger::exit("Model::shiftAtomDown");
}

// Move specified atom so it sits after the reference atom (or head of the list if NULL)
void Model::moveAtomAfter(Atom* i, Atom* reference)
{
	Messenger::enter("Model::moveAtom");
	if (i == NULL)
	{
		printf("Internal Error: NULL Atom pointer passed to Model::moveAtom.");
		Messenger::exit("Model::moveAtom");
		return;
	}
	int oldid = i->id();
	int shift = (reference == NULL ? -i->id() : reference->id() - i->id());

	// Move atom
	atoms_.moveAfter(i, reference);
	renumberAtoms();
	
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		IdShiftEvent *newchange = new IdShiftEvent;
		newchange->set(oldid, shift);
		recordingState_->addEvent(newchange);
	}
	changeLog.add(Log::Structure);
	Messenger::exit("Model::shiftAtomDown");
}

/*
// Public Functions
*/

// Get selection cog
Vec3<double> Model::selectionCentreOfGeometry() const
{
	Vec3<double> result;
	if (selection_.nItems() != 0)
	{
		for (Refitem<Atom,int>* ri = selection_.first(); ri != NULL; ri = ri->next) result += cell_.mim(ri->item, selection_.first()->item);
		result /= selection_.nItems();
	}
	return result;
}

// Get selection com
Vec3<double> Model::selectionCentreOfMass() const
{
	Vec3<double> result;
	Atom* i;
	double massnorm = 0.0;
	if (selection_.nItems() != 0)
	{
		for (Refitem<Atom,int>* ri = selection_.first(); ri != NULL; ri = ri->next)
		{
			i = ri->item;
			if (i->element() == 0)
			{
				Messenger::print("Warning - selection contains an unknown element - mass assumed to be 1.0\n");
				massnorm += 1.0;
				result += cell_.mim(i, selection_.first()->item);
			}
			else
			{
				massnorm += Elements().atomicMass(i);
				result += cell_.mim(i, selection_.first()->item) * Elements().atomicMass(i);
			}
		}
		result /= massnorm;
	}
	return result;
}

// Set selection visibility
void Model::selectionSetHidden(bool hidden)
{
	for (Refitem<Atom,int>* ri = selection(); ri != NULL; ri = ri->next) atomSetHidden(ri->item, hidden);
	changeLog.add(Log::Selection);
}

// Fix selected atom positions
void Model::selectionSetFixed(bool fixed)
{
	// Sets 'fixed' values to TRUE
	for (Refitem<Atom,int>* ri = selection(); ri != NULL; ri = ri->next) atomSetFixed(ri->item, fixed);
}

// Set custom colour of atoms in selection
void Model::selectionSetColour(double r, double g, double b, double a)
{
	for (Refitem<Atom,int>* ri = selection(); ri != NULL; ri = ri->next) atomSetColour(ri->item, r, g, b, a);
	changeLog.add(Log::Style);
}

// Reset custom colour of atoms in selection back to element defaults
void Model::selectionResetColour()
{
	for (Refitem<Atom,int>* ri = selection(); ri != NULL; ri = ri->next) atomResetColour(ri->item);
	changeLog.add(Log::Style);
}

// Set selection style
void Model::selectionSetStyle(Prefs::DrawStyle ds)
{
	// Sets all atoms currently selected to have the drawing style specified
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) atomSetStyle(i, ds);
}

// Select bound and selected atoms from the current atom
void Model::fragmentFromSelectionSelector(Atom* i, Reflist<Atom,int>& list)
{
	Atom* j;
	for (Refitem<Bond,int>* bref = i->bonds(); bref != NULL; bref = bref->next)
	{
		j = bref->item->partner(i);
		if (j->isSelected())
		{
			deselectAtom(j);
			list.add(j);
			fragmentFromSelectionSelector(j, list);
		}
	}
}

// Get atoms of a bound fragment with the current selection
void Model::fragmentFromSelection(Atom* start, Reflist<Atom,int>& list)
{
	Messenger::enter("Model::fragmentFromSelection");
	if ((start == NULL) || (!start->isSelected()))
	{
		Messenger::print("No atom provided, or atom is not selected.");
		Messenger::exit("Model::fragmentFromSelection");
		return;
	}
	// Clear the provided list and add the start atom
	list.clear();
	list.add(start);
	// From the atom provided, put all bound and selected atoms in the reflist provided
	deselectAtom(start);
	fragmentFromSelectionSelector(start, list);
	Messenger::exit("Model::fragmentFromSelection");
}

// Reorder bound atoms/fragments within the selection so that they are consecutive
void Model::reorderSelectedAtoms()
{
	Messenger::enter("Model::reorderSelectedAtoms");

	// Is there a selection?
	if (selection_.nItems() == 0)
	{
		Messenger::print("No atoms selected - no reordering to be performed.\n");
		Messenger::exit("Model::reorderSelectedAtoms");
		return;
	}

	// First, create a copy of the list of selected atoms
	Reflist<Atom,int> targetAtoms = selection_;
	Atom* i, *j;
	Refitem<Atom,int>* ri;
	Refitem<Atom, List<Neta> >* rj, *rk;
	Refitem<Bond,int>* rb;
	int diff, n;

	// First we will make sure that all molecular fragments in the current selection contain consecutive atoms
	Messenger::print("Enforcing consecutive atom numbering in all molecular fragments...\n");
	Reflist<Atom,int> selectedAtoms = targetAtoms;
	while (selectedAtoms.first())
	{
		// Deselect everything, and then treeselect from the first atom remaining in our list
		selectNone(TRUE);
		selectTree(selectedAtoms.first()->item, TRUE);
		for (ri = selection(TRUE); ri != NULL; ri = ri->next)
		{
			// Check - is this atom in our target list? If not, we've treeSelected an extra one, so must exit...
			if (selectedAtoms.contains(ri->item)) selectedAtoms.remove(ri->item);
			else
			{
				Messenger::print("Error: Found an extra atom, bound to one in the current selection, which was not selected itself.\nAborting reorder.");
				return;
			}
		}

		// Move these atoms to the end of the atom list, and remove them from our list
		moveSelectionToEnd(TRUE);
	}

	// Select first fragment, add it to a separate list, and remove its atoms from selectedAtoms
	selectedAtoms = targetAtoms;
	Reflist<Atom, List<Neta> > referenceFragment;
	selectNone(TRUE);
	selectTree(targetAtoms.first()->item,TRUE);
	Neta* neta;
	for (ri = selection(TRUE); ri != NULL; ri = ri->next)
	{
		rj = referenceFragment.add(ri->item);
		selectedAtoms.remove(ri->item);

		// Create a basic NETA description for this atom, with a 10 degree tolerance on the torsions
		neta = rj->data.add();
		neta->createBasic(rj->item, FALSE, 10.0);
	}

	// Test the atom types...
	List<Ring> dummyRingList;
	int referenceId = 0, netaLevel = 0;
	for (rk = referenceFragment.first(); rk != NULL; rk = rk->next, ++referenceId)
	{
		int nMatched = 0;
		for (rj = referenceFragment.first(); rj != NULL; rj = rj->next)
		{
			if (rk->data.first()->matchAtom(rj->item, &dummyRingList, this) != -1) nMatched++;
		}
		Messenger::print(Messenger::Verbose, "Testing constructed NETA for atom index %i : nMatched = %i\n", rk->item->id(), nMatched);
		if (nMatched == 0)
		{
			Messenger::print("Internal Error: Atom type for reference fragment atom %i failed to detect it.\n", referenceId);
			return;
		}
		else if (nMatched == 1) Messenger::print("Typing for reference fragment atom %i tested successfully.\n");
		else Messenger::print("Typing for reference fragment atom %i is not unique - reordering of symmetric subgroups may not be exact.\n");
	}

	// We will create a pattern here to allow us to get a connectivity matrix easily
	Pattern referencePattern;
	referencePattern.setParent(this);
	referencePattern.initialise(0, 0, 1, referenceFragment.nItems());
	referencePattern.createMatrices();

	// We now select fragments sequentially, and reorder the atoms in each one...
	Messenger::print("Reordering atoms in individual fragments...\n");
	while (selectedAtoms.first())
	{
		// Tree select this fragment, and do some basic checking...
		selectNone(TRUE);
		selectTree(selectedAtoms.first()->item, TRUE);
		if (marked_.nItems() != referenceFragment.nItems())
		{
			Messenger::print("Warning: Skipping fragment with atom ids %i to %i since it has a different number of atoms to the reference (first) fragment.\n", marked_.first()->item->id()+1, marked_.last()->item->id()+1);
			for (ri = marked_.first(); ri != NULL; ri = ri->next) selectedAtoms.remove(ri->item);
			continue;
		}

		// Get root id of the fragment here....
		int rootId = marked_.first()->item->id();

		// Loop over referenceAtoms, searching for a NETA match with an atom in the current marked_ selection
		referenceId = 0;
		for (rj = referenceFragment.first(); rj != NULL; rj = rj->next, ++referenceId)
		{
			// Loop over NETA defined for the reference atom, creating new (looser) ones if necessary
			neta = rj->data.first();
			netaLevel = 1;
			ri = NULL;
			while (ri == NULL)
			{
				// Loop over remaining marked_ atoms and see if we can find a match using the current neta
				for (ri = marked_.first(); ri != NULL; ri = ri->next) if (neta->matchAtom(ri->item, &dummyRingList, this) != -1) break;

				if (ri == NULL)
				{
					// Move on to next NETA definition (or create it)
					neta = neta->next;
					if (neta == NULL)
					{
						// Check that we haven't reached the maximum (sensible) number of definitions (where the tolerance is 90 degrees
						if (rj->data.nItems() == 9)
						{
							Messenger::print("Error: Reached maximum torsion tolerance of 90 degrees, and no atoms were matched.\n");
							break;
						}
						else
						{
							neta = rj->data.add();
							neta->createBasic(rj->item, FALSE, rj->data.nItems() * 10.0);
							Messenger::print("Created neta for reference atom %i with torsion tolerance of %f\n", referenceId, rj->data.nItems()*10.0);
						}
					}
				}
				++netaLevel;
			}

			// Did we find a match?
			if (ri == NULL)
			{
				Messenger::print("Error: Failed to find a match for reference atom %i in fragment with atom ids %i to %i.\n", referenceId, marked_.first()->item->id()+1, marked_.last()->item->id()+1);
				Messenger::exit("Model::reorderSelectedAtoms");
				return;
			}
			if (netaLevel >= 4) Messenger::print("Warning: Matched atom to reference index %i with torsion tolerance of %f...\n", referenceId, netaLevel*10.0);

			// Yes we did! Now, check it's position - is it in the correct place?
			if (((ri->item->id() - rootId) - referenceId) != 0) swapAtoms(atoms_[rootId+referenceId], ri->item);

			// Finally, remove this atom from the selectedAtoms and marked_ lists - we are done with it
			deselectAtom(ri->item, TRUE);
			selectedAtoms.remove(ri->item);
		}
	}
	Messenger::exit("Model::reorderSelectedAtoms");
}

// Get empirical formula of selection
void Model::selectionEmpirical(Dnchar &target, bool markonly, bool addspaces) const
{
	Messenger::enter("Model::selectionEmpirical");
	int n, count, elcount[MAXELEMENTS];
	target.clear();
	// Reset element counters
	for (n=0; n<MAXELEMENTS; n++) elcount[n] = 0;
	Atom* i = atoms_.first();
	while (i != NULL)
	{
		if (i->isSelected(markonly)) elcount[i->element()] ++;
		i = i->next;
	}
	// Construct element string
	count = 0;
	for (n=MAXELEMENTS-1; n>0; n--)
		if (elcount[n] != 0)
		{
			if ((count>0) && addspaces) target.strcat(" ");
			target.strcat(Elements().symbol(n));
			if (elcount[n] > 1) target.strcat(itoa(elcount[n]));
			count++;
		}
	Messenger::exit("Model::selectionEmpirical");
}

// Get atom fingerprint of current selection
void Model::selectionAtomFingerprint(Dnchar &target)
{
	Messenger::enter("Model::selectionAtomFingerprint");
	target.clear();
	if (selection_.first() == NULL)
	{
		Messenger::exit("Model::selectionAtomFingerprint");
		return;
	}
	Refitem<Atom,int>* ri = selection_.first();
	int lastel = ri->item->element(), newel;
	int count = 1;
	Atom* i;
	for (ri = ri->next; ri != NULL; ri = ri->next)
	{
		// Check this element against the last. If the last element is the same, increase the counter. If different, append to the string
		i = ri->item;
		newel = i->element();
		if (newel == lastel) count ++;
		else
		{
			target.strcat(Elements().symbol(i));
			target.strcat(itoa(count));
			lastel = newel;
			count = 0;
		}
	}
	// Check for last element chunk
	if (count != 0)
	{
		target.strcat(Elements().symbol(lastel));
		target.strcat(itoa(count));
	}
	Messenger::exit("Model::selectionAtomFingerprint");
}

// Get bond fingerprint of current selection
void Model::selectionBondFingerprint(Dnchar &target)
{
	Messenger::enter("Model::selectionBondFingerprint");
	target.clear();
	int count = 0, diff;
	Refitem<Bond,int>* ri;
	Atom* i = atoms_.first();
	Atom* j;
	while (i != NULL)
	{
		if (i->isSelected()) 
		{
			ri = i->bonds();
			while (ri != NULL)
			{
				j = ri->item->partner(i);
				diff = j->id() - i->id();
				if (diff > 0)
				{
					target.strcat(itoa(count));
					target += '-';
					target.strcat(itoa(count + diff));
					target += ' ';
				}
				ri = ri->next;
			}
			count ++;
		}
		i = i->next;
	}
	Messenger::exit("Model::selectionBondFingerprint");
}

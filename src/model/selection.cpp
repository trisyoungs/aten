/*
	*** Model selection functions
	*** src/model/selection.cpp
	Copyright T. Youngs 2007-2018

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
#include "undo/atom_shift.h"
#include "undo/undostate.h"
#include "base/pattern.h"
#include "base/sysfunc.h"
#include "base/neta.h"

ATEN_USING_NAMESPACE

/*
 * Private Functions
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
		AtomShiftEvent* newchange = new AtomShiftEvent;
		newchange->set(oldid, -1);
		recordingState_->addEvent(newchange);
	}
	logChange(Log::Structure);
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
		AtomShiftEvent* newchange = new AtomShiftEvent;
		newchange->set(oldid, 1);
		recordingState_->addEvent(newchange);
	}
	logChange(Log::Structure);
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
		AtomShiftEvent* newchange = new AtomShiftEvent;
		newchange->set(oldid, shift);
		recordingState_->addEvent(newchange);
	}
	logChange(Log::Structure);
	Messenger::exit("Model::shiftAtomDown");
}

/*
 * Public Functions
 */

// Get selection cog
Vec3<double> Model::selectionCentreOfGeometry(bool markOnly) const
{
	Vec3<double> result;
	if (markOnly)
	{
		if (marked_.nItems() != 0)
		{
			for (RefListItem<Atom,int>* ri = marked_.first(); ri != NULL; ri = ri->next) result += cell_.mim(ri->item, marked_.first()->item);
			result /= marked_.nItems();
		}
	}
	else if (selection_.nItems() != 0)
	{
		for (RefListItem<Atom,int>* ri = selection_.first(); ri != NULL; ri = ri->next) result += cell_.mim(ri->item, selection_.first()->item);
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
		for (RefListItem<Atom,int>* ri = selection_.first(); ri != NULL; ri = ri->next)
		{
			i = ri->item;
			if (i->element() == 0)
			{
				Messenger::print("Warning - selection contains an unknown element - mass assumed to be 1.0");
				massnorm += 1.0;
				result += cell_.mim(i, selection_.first()->item);
			}
			else
			{
				massnorm += ElementMap::atomicMass(i);
				result += cell_.mim(i, selection_.first()->item) * ElementMap::atomicMass(i);
			}
		}
		result /= massnorm;
	}
	return result;
}

// Set selection visibility
void Model::selectionSetHidden(bool hidden)
{
	for (RefListItem<Atom,int>* ri = selection(); ri != NULL; ri = ri->next) atomSetHidden(ri->item, hidden);
	logChange(Log::Selection);
}

// Fix selected atom positions
void Model::selectionSetFixed(bool fixed)
{
	// Sets 'fixed' values to true
	for (RefListItem<Atom,int>* ri = selection(); ri != NULL; ri = ri->next) atomSetFixed(ri->item, fixed);
}

// Set custom colour of atoms in selection
void Model::selectionSetColour(double r, double g, double b, double a)
{
	for (RefListItem<Atom,int>* ri = selection(); ri != NULL; ri = ri->next) atomSetColour(ri->item, r, g, b, a);

	// Make sure current colourScheme is 'own'
	setColourScheme(Prefs::OwnScheme);
	
	logChange(Log::Style);
}

// Reset custom colour of atoms in selection back to element defaults
void Model::selectionResetColour()
{
	for (RefListItem<Atom,int>* ri = selection(); ri != NULL; ri = ri->next) atomResetColour(ri->item);
	logChange(Log::Style);
}

// Set selection style
void Model::selectionSetStyle(Prefs::DrawStyle ds)
{
	for (RefListItem<Atom,int>* ri = selection_.first(); ri != NULL; ri = ri->next) if (ri->item->isSelected()) atomSetStyle(ri->item, ds);

	// Make sure current drawStyle is 'own'
	setDrawStyle(Prefs::OwnStyle);
}

// Select bound and selected atoms from the current atom
void Model::fragmentFromSelectionSelector(Atom* i, RefList<Atom,int>& list)
{
	Atom* j;
	for (RefListItem<Bond,int>* bref = i->bonds(); bref != NULL; bref = bref->next)
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
void Model::fragmentFromSelection(Atom* start, RefList<Atom,int>& list)
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
		Messenger::print("No atoms selected - no reordering to be performed.");
		Messenger::exit("Model::reorderSelectedAtoms");
		return;
	}

	// First, create a copy of the list of selected atoms
	RefList<Atom,int> targetAtoms = selection_;
	Atom* i, *j;
	RefListItem<Atom,int>* ri;
	RefListItem<Atom, List<Neta> >* rj, *rk;
	RefListItem<Bond,int>* rb;
	int diff, n;

	// First we will make sure that all molecular fragments in the current selection contain consecutive atoms
	Messenger::print("Enforcing consecutive atom numbering in all molecular fragments...");
	RefList<Atom,int> selectedAtoms = targetAtoms;
	while (selectedAtoms.first())
	{
		// Deselect everything, and then treeselect from the first atom remaining in our list
		selectNone(true);
		selectTree(selectedAtoms.first()->item, true);
		for (ri = selection(true); ri != NULL; ri = ri->next)
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
		moveSelectionToEnd(true);
	}

	// Select first fragment, add it to a separate list, and remove its atoms from selectedAtoms
	selectedAtoms = targetAtoms;
	RefList<Atom, List<Neta> > referenceFragment;
	selectNone(true);
	selectTree(targetAtoms.first()->item,true);
	Neta* neta;
	for (ri = selection(true); ri != NULL; ri = ri->next)
	{
		rj = referenceFragment.add(ri->item);
		selectedAtoms.remove(ri->item);

		// Create a basic NETA description for this atom, with a 10 degree tolerance on the torsions
		neta = rj->data.add();
		neta->createBasic(rj->item, true, 10.0);
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
		Messenger::print(Messenger::Verbose, "Testing constructed NETA for atom index %i : nMatched = %i", rk->item->id(), nMatched);
		if (nMatched == 0)
		{
			Messenger::print("Internal Error: Atom type for reference fragment atom %i failed to detect it.", referenceId+1);
			return;
		}
		else if (nMatched == 1) Messenger::print("Typing for reference fragment atom %i tested successfully.", rk->item->id()+1);
		else Messenger::print("Typing for reference fragment atom %i is not unique - reordering of symmetric subgroups may not be exact.", rk->item->id()+1);
	}

	// We will create a pattern here to allow us to get a connectivity matrix easily
	Pattern referencePattern;
	referencePattern.setParent(this);
	referencePattern.initialise(0, 0, 1, referenceFragment.nItems());
	referencePattern.createMatrices();

	// We now select fragments sequentially, and reorder the atoms in each one...
	Messenger::print("Reordering atoms in individual fragments...");
	while (selectedAtoms.first())
	{
		// Tree select this fragment, and do some basic checking...
		selectNone(true);
		selectTree(selectedAtoms.first()->item, true);
		if (marked_.nItems() != referenceFragment.nItems())
		{
			Messenger::print("Warning: Skipping fragment with atom ids %i to %i since it has a different number of atoms to the reference (first) fragment.", marked_.first()->item->id()+1, marked_.last()->item->id()+1);
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
							Messenger::print("Error: Reached maximum torsion tolerance of 90 degrees, and no atoms were matched.");
							break;
						}
						else
						{
							neta = rj->data.add();
							neta->createBasic(rj->item, true, rj->data.nItems() * 10.0);
							Messenger::print("Created neta for reference atom %i with torsion tolerance of %f", referenceId, rj->data.nItems()*10.0);
						}
					}
				}
				++netaLevel;
			}

			// Did we find a match?
			if (ri == NULL)
			{
				Messenger::print("Error: Failed to find a match for reference atom %i in fragment with atom ids %i to %i.", referenceId, marked_.first()->item->id()+1, marked_.last()->item->id()+1);
				Messenger::exit("Model::reorderSelectedAtoms");
				return;
			}
			if (netaLevel >= 4) Messenger::print("Warning: Matched atom to reference index %i with torsion tolerance of %f...", referenceId, netaLevel*10.0);

			// Yes we did! Now, check it's position - is it in the correct place?
			if (((ri->item->id() - rootId) - referenceId) != 0) swapAtoms(atoms_[rootId+referenceId], ri->item);

			// Finally, remove this atom from the selectedAtoms and marked_ lists - we are done with it
			deselectAtom(ri->item, true);
			selectedAtoms.remove(ri->item);
		}
	}
	Messenger::exit("Model::reorderSelectedAtoms");
}

// Get empirical formula of selection
QString Model::selectionEmpirical(bool markOnly, bool addSpaces, bool useSubScripts) const
{
	Messenger::enter("Model::selectionEmpirical");

	Array<int> elcount(MAXELEMENTS);
	RefListItem<Atom,int>* firstAtom = (markOnly ? marked_.first() : selection_.first());

	// Loop over atoms in list
	Atom* i;
	for (RefListItem<Atom,int>* ri = firstAtom; ri != NULL; ri = ri->next)
	{
		i = ri->item;
		if (i != NULL) ++elcount[i->element()];
		else printf("Internal Error: RefList had a NULL atom pointer in Model::selectionEmpirical().\n");
	}

	// Construct element string
	QString result;
	for (int n=MAXELEMENTS-1; n>0; --n)
	{
		if (elcount[n] != 0)
		{
			if ((!result.isEmpty()) & addSpaces) result += QString(" ") + ElementMap::symbol(n);
			else result += ElementMap::symbol(n);
			if (elcount[n] > 1)
			{
				if (useSubScripts) result += "<sub>" + QString::number(elcount[n]) + "</sub>";
				else result += QString::number(elcount[n]);
			}
		}
	}

	Messenger::exit("Model::selectionEmpirical");
	return result;
}

// Get atom fingerprint of current selection
QString Model::selectionAtomFingerprint()
{
	Messenger::enter("Model::selectionAtomFingerprint");

	if (selection_.first() == NULL)
	{
		Messenger::exit("Model::selectionAtomFingerprint");
		return QString();
	}
	RefListItem<Atom,int>* ri = selection_.first();
	int lastel = ri->item->element(), newel;
	int count = 1;
	QString result;
	Atom* i;
	for (ri = ri->next; ri != NULL; ri = ri->next)
	{
		// Check this element against the last. If the last element is the same, increase the counter. If different, append to the string
		i = ri->item;
		newel = i->element();
		if (newel == lastel) count ++;
		else
		{
			result += ElementMap::symbol(i);
			result += QString::number(count);
			lastel = newel;
			count = 0;
		}
	}

	// Check for last element chunk
	if (count != 0)
	{
		result += ElementMap::symbol(lastel);
		result += QString::number(count);
	}

	Messenger::exit("Model::selectionAtomFingerprint");
	return result;
}

// Get bond fingerprint of current selection
QString Model::selectionBondFingerprint()
{
	Messenger::enter("Model::selectionBondFingerprint");

	int count = 0, diff;
	RefListItem<Bond,int>* ri;
	QString result;
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
				if (diff > 0) result += QString::number(count) + "-" + QString::number(count+diff) + " ";
				ri = ri->next;
			}
			count ++;
		}
		i = i->next;
	}
	Messenger::exit("Model::selectionBondFingerprint");
	return result;
}

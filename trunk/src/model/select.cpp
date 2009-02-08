/*
	*** Model select functions
	*** src/model/select.cpp
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

#include "main/aten.h"
#include "model/model.h"
#include "model/undoevent.h"
#include "model/undostate.h"
#include "classes/atomtype.h"
#include "base/pattern.h"
#include "gui/gui.h"

// Select Atom
void Model::selectAtom(Atom *i, bool markonly)
{
	if (markonly)
	{
		if (!i->isSelected(TRUE))
		{
			i->setSelected(TRUE, TRUE);
			nMarked_ ++;
		}
	}
	else
	{
		if (!i->isSelected())
		{
			i->setSelected(TRUE);
			nSelected_ ++;
			changeLog.add(Log::Selection);
			// Add the change to the undo state (if there is one)
			if (recordingState_ != NULL)
			{
				SelectEvent *newchange = new SelectEvent;
				newchange->set(TRUE, i->id());
				recordingState_->addEvent(newchange);
			}
		}
	}
}

// Select Atom by ID
void Model::selectAtom(int id, bool markonly)
{
	Atom *i = atom(id);
	if (i != NULL) selectAtom(i, markonly);
}

// Deselect Atom
void Model::deselectAtom(Atom *i, bool markonly)
{
	if (markonly)
	{
		if (i->isSelected(TRUE))
		{
			i->setSelected(FALSE, TRUE);
			nMarked_ --;
		}
	}
	else
	{
		if (i->isSelected())
		{
			i->setSelected(FALSE);
			nSelected_ --;
			changeLog.add(Log::Selection);
			// Add the change to the undo state (if there is one)
			if (recordingState_ != NULL)
			{
				SelectEvent *newchange = new SelectEvent;
				newchange->set(FALSE, i->id());
				recordingState_->addEvent(newchange);
			}
		}
	}
}

// Deselect Atom by ID
void Model::deselectAtom(int id, bool markonly)
{
	Atom *i = atom(id);
	if (i != NULL) deselectAtom(i, markonly);
}

// Toggle Selection State
void Model::selectionToggle(Atom *i, bool markonly)
{
	i->isSelected(markonly) ? deselectAtom(i, markonly) : selectAtom(i, markonly);
}

// Invert Current Selection
void Model::selectionInvert(bool markonly)
{
	msg.enter("Model::selectionInvert");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		i->isSelected(markonly) ? deselectAtom(i, markonly) : selectAtom(i, markonly);
	msg.exit("Model::selectionInvert");
}

// Delete Selected Atoms
void Model::selectionDelete(bool markonly)
{
	msg.enter("Model::selectionDelete");
	Atom *i, *tempi;
	int count = 0;
	bool cancelled = FALSE;
	aten.initialiseProgress("Deleting atoms...", atoms_.nItems()*2);
	// Attempt to be clever here for the sake of undo/redo, while avoiding renumbering at every step.
	// 1) First, delete all measurements and bonds to the selected atoms
	Refitem<Bond,int> *bref;
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		if (!i->isSelected(markonly)) continue;
		// Remove measurements
		removeMeasurements(i);
		// Delete All Bonds To Specific Atom
		bref = i->bonds();
		while (bref != NULL)
		{
			// Need to detach the bond from both atoms involved
			Bond *b = bref->item;
			Atom *j = b->partner(i);
			unbondAtoms(i,j,b);
			bref = i->bonds();
		}
		if (!aten.updateProgress(++count)) cancelled = TRUE;
		if (cancelled) break;
	}
	// 2) Delete the actual atoms
	if (!cancelled)
	{
		i = atoms_.last();
		while (i != NULL)
		{
			if (i->isSelected(markonly))
			{
				tempi = i->prev;
				removeAtom(i, TRUE);
				i = tempi;
			}
			else i = i->prev;
			if (!aten.updateProgress(++count)) break;
		}
	}
	aten.cancelProgress();
	// Renumber atoms and recalculate density here, since we request deletion with no updates
	renumberAtoms();
	calculateDensity();
	msg.exit("Model::selectionDelete");
}

// Expand Current Selection
void Model::selectionExpand(bool markonly)
{
	msg.enter("Model::selectionExpand");
	Atom *i;
	Refitem<Bond,int> *bref;
	// Store the current selection state in i->tempi
	for (i = atoms_.first(); i != NULL; i = i->next) i->tempi = i->isSelected(markonly);
	// Now use the temporary state to find atoms where we select atomic neighbours
	for (i = atoms_.first(); i != NULL; i = i->next)
		if (i->tempi) for (bref = i->bonds(); bref != NULL; bref = bref->next) selectAtom(bref->item->partner(i), markonly);
	msg.exit("Model::selectionExpand");
}

// Select All Atoms
void Model::selectAll(bool markonly)
{
	msg.enter("Model::selectAll");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (!i->isSelected(markonly)) selectAtom(i, markonly);
	msg.exit("Model::selectAll");
}

// Deselect All Atoms
void Model::selectNone(bool markonly)
{
	msg.enter("Model::selectNone");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected(markonly)) deselectAtom(i, markonly);
	if (markonly) nMarked_ = 0;
	else nSelected_ = 0;
	msg.exit("Model::selectNone");
}

// Atom at Screen Coordinates
Atom *Model::atomOnScreen(double x1, double y1)
{
	// See if an atom exists under the coordinates x1,y1
	// Ignore 'hidden' atoms_.
	msg.enter("Model::atomOnScreen");
	// Make sure we have a valid projection
	projectAll();
	Atom *closest = NULL;
	static Vec3<double> wr, sr;
	static double closestz, dist, nclip;
	closestz = 10000.0;
	nclip = prefs.clipNear();
	y1 = gui.mainView.height() - y1;
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->isHidden()) continue;
		wr = -i->rWorld();
		sr = i->rScreen();
	//	printf("Screen coords:");
	//	sr.print();
		if (wr.z > nclip)
		{
			dist = sqrt((sr.x - x1)*(sr.x - x1) + (sr.y - y1)*(sr.y - y1));
			if (dist < i->screenRadius())	// Mouse is inside bounding sphere
			{
				if ((closest == NULL) || (wr.z < closestz))
				{
					closest = i;
					closestz = wr.z;
				}
			}
		}
	}
	msg.exit("Model::atomOnScreen");
	return closest;
}

// Select atoms within bounding box
void Model::selectBox(double x1, double y1, double x2, double y2, bool deselect)
{
	// Box selection - choose all the atoms within the selection area
	msg.enter("Model::selectBox");
	double t;
	Atom *i;
	Vec3<double> sr;
	y1 = gui.mainView.height() - y1;
	y2 = gui.mainView.height() - y2;
	// Handle 'reverse ranges' - make sure x1 < x2 and y1 < y2
	if (x1 > x2)
	{
		t=x1;
		x1=x2;
		x2=t;
	}
	if (y1 > y2)
	{
		t=y1;
		y1=y2;
		y2=t;
	}
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->isHidden()) continue;
		sr = i->rScreen();
		if ((sr.x >= x1) && (sr.x <= x2) && (sr.y >= y1) && (sr.y <= y2)) (deselect ? deselectAtom(i) : selectAtom(i));
	}
	msg.exit("Model::selectBox");
}

// Tree Select
void Model::selectTree(Atom *i, bool markonly, bool deselect)
{
	// The passed atom node is the starting point for the algorithm.
	// From here, select all atoms that are bound - if they are already
	// selected then ignore them. If they are not already selected, then
	// recursively call the routine on that atom.
	msg.enter("Model::selectTree");
	bool status;
	Atom *j;
	Refitem<Bond,int> *bref = i->bonds();
	deselect ? deselectAtom(i, markonly) : selectAtom(i, markonly);
	while (bref != NULL)
	{
		j = bref->item->partner(i);
		status = j->isSelected(markonly);
		if (deselect) status = !status;
		if (!status)
		{
			if (deselect) deselectAtom(j, markonly);
			else selectAtom(j, markonly);
			this->selectTree(j, markonly, deselect);
		}
		bref = bref->next;
	}
	msg.exit("Model::selectTree");
}

// Select by Element
void Model::selectElement(Atom *target, bool markonly, bool deselect)
{
	// Select all atoms which are the same element as the atom i
	selectElement(target->element(), markonly, deselect);
}

// Select by element (from element)
void Model::selectElement(int el, bool markonly, bool deselect)
{
	// Select all atoms which are the same element as the atom with id 'target'
	msg.enter("Model::selectElement");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		if (i->element() == el) (deselect ? deselectAtom(i, markonly) : selectAtom(i, markonly));
	msg.exit("Model::selectElement");
}

// Deselect by Element
void Model::deselectElement(int el, bool markonly)
{
	// Select all atoms which are the same element as the atom i
	msg.enter("Model::deselectElement");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		if (i->element() == el) deselectAtom(i, markonly);
	msg.exit("Model::deselectElement");
}

// Select all atoms which match the provided type
void Model::selectType(int element, const char *typedesc, bool markonly, bool deselect)
{
	msg.enter("Model::selectType");
	Atomtype testat;
	testat.setCharacterElement(element);
	if (!testat.expand(typedesc,NULL,NULL))
	{
		msg.print("Failed to create type description.\n");
		msg.exit("Model::selectType");
		return;
	}
	int count = 0, matchscore = 0, atomscore, n;
	// Prepare for typing
	describeAtoms();
	// Loop over patterns and select atoms
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		Atom *i = p->firstAtom();
		for (n=0; n<p->nAtoms(); n++)
		{
			p->resetTempI(0);
			i->tempi = 1;
			atomscore = testat.matchAtom(i,p->ringList(),this,i);
			if (atomscore > 0)
			{
				// Select this atom in all pattern molecules
				p->selectAtom(n, markonly, deselect);
				count += p->nMolecules();
				matchscore = atomscore;
			}
			i = i->next;
		}
	}
	// Update model
	changeLog.add(Log::Selection);
	// Write results
	msg.print("Type description score = %i. Matched %i atoms.\n", matchscore, count);
	msg.exit("Model::selectType");
}

// Select with bounding Sphere
void Model::selectRadial(Atom *target, double radius)
{
	// Select all atoms which are within the distance 'radius' from atom 'target'
	msg.enter("Model::selectRadial");
	Atom *i = atoms_.first();
	printf("Selection radius is %8.4f Angstroms\n",radius);
	while (i != NULL)
	{
		if (i == target) selectAtom(i);
		else if (distance(target,i) < radius) selectAtom(i);
		i = i->next;
	}
	msg.exit("Model::selectRadial");
}

// Select all atoms in specified pattern
void Model::selectPattern(Pattern *p, bool markonly, bool deselect)
{
	// Select all atoms covered by the specified pattern.
	msg.enter("Model::selectPattern");
	// Check that this pattern is valid and belongs to this model...
	bool found = FALSE;
	for (Pattern *modelp = patterns_.first(); modelp != NULL; modelp = modelp->next) if (p == modelp) found = TRUE;
	if (!found)
	{
		msg.print("Pattern does not belong to this model, or is out of date.\n");
		msg.exit("Model::selectPattern");
		return;
	}
	Atom *i = p->firstAtom();
	for (int n=0; n<p->totalAtoms(); n++)
	{
		deselect ? deselectAtom(i, markonly) : selectAtom(i, markonly);
		i = i->next;
	}
	msg.exit("Model::selectPattern");
}

// Get first selected
Atom *Model::firstSelected(bool markonly)
{
	msg.enter("Model::firstSelected");
	Atom *result = NULL;
	Atom *i = atoms_.first();
	while (i != NULL)
	{
		if (i->isSelected(markonly))
		{
			result = i;
			break;
		}
		i = i->next;
	}
	msg.exit("Model::firstSelected");
	return result;
}

// Select overlapping atoms
void Model::selectOverlaps(double tolerance, bool markonly)
{
	msg.enter("Model::selectOverlaps");
	Atom *i, *j;
	double deltar;
	selectNone(markonly);
	for (i = atoms_.first(); i != atoms_.last(); i = i->next)
	{
		if (i->isSelected(markonly)) continue;
		for (j = i->next; j != NULL; j = j->next)
		{
			deltar = cell_.distance(i, j);
			if (deltar < tolerance)
			{
				msg.print("Atom %i (%s) is %f from atom %i (%s).\n", j->id()+1, elements().symbol(j), deltar, i->id()+1, elements().symbol(i));
				selectAtom(j, markonly);
			}
		}
	}
	msg.exit("Model::selectOverlaps");
}

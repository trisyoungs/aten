/*
	*** Model select functions
	*** src/model/select.cpp
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
#include "classes/pattern.h"
#include "base/master.h"
#include "base/elements.h"
#include "gui/gui.h"

// Select Atom
void Model::selectAtom(Atom *i)
{
	if (!i->isSelected())
	{
		i->setSelected(TRUE);
		nSelected_ ++;
		logChange(Change::SelectionLog);
		// Add the change to the undo state (if there is one)
		if (recordingState_ != NULL)
		{
			Change *newchange = recordingState_->addChange();
			newchange->set(Change::SelectEvent,i->id());
		}
	}
}

// Select Atom by ID
void Model::selectAtom(int id)
{
	Atom *i = atom(id);
	if (i != NULL) selectAtom(i);
}

// Deselect Atom
void Model::deselectAtom(Atom *i)
{
	if (i->isSelected())
	{
		i->setSelected(FALSE);
		nSelected_ --;
		logChange(Change::SelectionLog);
		// Add the change to the undo state (if there is one)
		if (recordingState_ != NULL)
		{
			Change *newchange = recordingState_->addChange();
			newchange->set(-Change::SelectEvent,i->id());
		}
	}
}

// Deelect Atom by ID
void Model::deselectAtom(int id)
{
	Atom *i = atom(id);
	if (i != NULL) deselectAtom(i);
}

// Toggle Selection State
void Model::selectionToggle(Atom *i)
{
	i->isSelected() ? deselectAtom(i) : selectAtom(i);
}

// Invert Current Selection
void Model::selectionInvert()
{
	msg.enter("Model::selectionInvert");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		i->isSelected() ? deselectAtom(i) : selectAtom(i);
	msg.exit("Model::selectionInvert");
}

// Delete Selected Atoms
void Model::selectionDelete()
{
	msg.enter("Model::selectionDelete");
	Atom *i, *tempi;
	int count = 0;
	master.initialiseProgress("Deleting atoms...", atoms_.nItems());
	i = atoms_.first();
	while (i != NULL)
	{
		if (i->isSelected())
		{
			tempi = i->next;
			deleteAtom(i);
			i = tempi;
		}
		else i = i->next;
		if (!master.updateProgress(++count)) break;
	}
	master.cancelProgress();
	msg.exit("Model::selectionDelete");
}

// Expand Current Selection
void Model::selectionExpand()
{
	msg.enter("Model::selectionExpand");
	Atom *i;
	Refitem<Bond,int> *bref;
	// Store the current selection state in i->tempi
	for (i = atoms_.first(); i != NULL; i = i->next) i->tempi = i->isSelected();
	// Now use the temporary state to find atoms where we select atomic neighbours
	for (i = atoms_.first(); i != NULL; i = i->next)
		if (i->tempi) for (bref = i->bonds(); bref != NULL; bref = bref->next) selectAtom(bref->item->partner(i));
	msg.exit("Model::selectionExpand");
}

// Select All Atoms
void Model::selectAll()
{
	msg.enter("Model::selectAll");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (!i->isSelected()) selectAtom(i);
	msg.exit("Model::selectAll");
}

// Deselect All Atoms
void Model::selectNone()
{
	msg.enter("Model::selectNone");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) deselectAtom(i);
	nSelected_ = 0;
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
void Model::selectBox(double x1, double y1, double x2, double y2)
{
	// Box selection - choose all the atoms within the selection area
	msg.enter("Model::selectBox");
	double t;
	Atom *i;
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
		Vec3<double> sr = i->rScreen();
		if ((sr.x >= x1) && (sr.x <= x2))
			if ((sr.y >= y1) && (sr.y <= y2)) selectAtom(i);
	}
	msg.exit("Model::selectBox");
}

// Tree Select
void Model::selectTree(Atom *i)
{
	// The passed atom node is the starting point for the algorithm.
	// From here, select all atoms that are bound - if they are already
	// selected then ignore them. If they are not already selected, then
	// recursively call the routine on that atom.
	msg.enter("Model::selectTree");
	selectAtom(i);
	Refitem<Bond,int> *bref = i->bonds();
	while (bref != NULL)
	{
		Atom *j = bref->item->partner(i);
		if (!j->isSelected())
		{
			selectAtom(j);
			this->selectTree(j);
		}
		bref = bref->next;
	}
	msg.exit("Model::selectTree");
}

// Select by Element
void Model::selectElement(Atom *target)
{
	// Select all atoms which are the same element as the atom i
	selectElement(target->element());
}

// Select by element (from element)
void Model::selectElement(int el)
{
	// Select all atoms which are the same element as the atom with id 'target'

	msg.enter("Model::selectElement");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		if (i->element() == el) selectAtom(i);
	msg.exit("Model::selectElement");
}

// Deelect by Element
void Model::deselectElement(int el)
{
	// Select all atoms which are the same element as the atom i
	msg.enter("Model::deselectElement");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		if (i->element() == el) deselectAtom(i);
	msg.exit("Model::deselectElement");
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

// Select Pattern
void Model::selectPattern(Pattern *p)
{
	// Select all atoms covered by the specified pattern.
	msg.enter("Model::selectPattern");
	Atom *i = p->firstAtom();
	for (int n=0; n<p->totalAtoms(); n++)
	{
		selectAtom(i);
		i = i->next;
	}
	msg.exit("Model::selectPattern");
}

// Get first selected
Atom *Model::firstSelected()
{
	msg.enter("Model::firstSelected");
	Atom *result = NULL;
	Atom *i = atoms_.first();
	while (i != NULL)
	{
		if (i->isSelected())
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
void Model::selectOverlaps(double tolerance)
{
	msg.enter("Model::selectOverlaps");
	Atom *i, *j;
	double deltar;
	selectNone();
	for (i = atoms_.first(); i != atoms_.last(); i = i->next)
	{
		if (i->isSelected()) continue;
		for (j = i->next; j != NULL; j = j->next)
		{
			deltar = cell_.distance(i, j);
			if (deltar < tolerance)
			{
				msg.print("Atom %i (%s) is %f from atom %i (%s).\n", j->id()+1, elements.symbol(j), deltar, i->id()+1, elements.symbol(i));
				selectAtom(j);
			}
		}
	}
	msg.exit("Model::selectOverlaps");
}

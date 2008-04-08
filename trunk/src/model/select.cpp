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
	dbgBegin(Debug::MoreCalls,"Model::selectAtom (%li)",i);
	if (!i->isSelected())
	{
		i->setSelected(TRUE);
		nSelected_ ++;
		logChange(LOG_SELECTION);
		// Add the change to the undo state (if there is one)
		if (recordingState_ != NULL)
		{
			Change *newchange = recordingState_->addChange();
			newchange->set(UE_SELECT,i->id());
		}
	}
	dbgEnd(Debug::MoreCalls,"Model::selectAtom (%li)",i);
}

// Deselect Atom
void Model::deselectAtom(Atom *i)
{
	dbgBegin(Debug::MoreCalls,"Model::deselectAtom (%li)",i);
	if (i->isSelected())
	{
		i->setSelected(FALSE);
		nSelected_ --;
		logChange(LOG_SELECTION);
		// Add the change to the undo state (if there is one)
		if (recordingState_ != NULL)
		{
			Change *newchange = recordingState_->addChange();
			newchange->set(-UE_SELECT,i->id());
		}
	}
	dbgEnd(Debug::MoreCalls,"Model::deselectAtom (%li)",i);
}

// Toggle Selection State
void Model::selectionToggle(Atom *i)
{
	dbgBegin(Debug::MoreCalls,"Model::selectionToggle (%li)",i);
	i->isSelected() ? deselectAtom(i) : selectAtom(i);
	dbgEnd(Debug::MoreCalls,"Model::selectionToggle (%li)",i);
}

// Invert Current Selection
void Model::selectionInvert()
{
	dbgBegin(Debug::Calls,"Model::selectionInvert");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		i->isSelected() ? deselectAtom(i) : selectAtom(i);
	dbgEnd(Debug::Calls,"Model::selectionInvert");
}

// Delete Selected Atoms
void Model::selectionDelete()
{
	dbgBegin(Debug::Calls,"Model::selectionDelete");
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
	dbgEnd(Debug::Calls,"Model::selectionDelete");
}

// Expand Current Selection
void Model::selectionExpand()
{
	dbgBegin(Debug::Calls,"Model::selectionExpand");
	Atom *i;
	Refitem<Bond,int> *bref;
	// Store the current selection state in i->tempi
	for (i = atoms_.first(); i != NULL; i = i->next) i->tempi = i->isSelected();
	// Now use the temporary state to find atoms where we select atomic neighbours
	for (i = atoms_.first(); i != NULL; i = i->next)
		if (i->tempi) for (bref = i->bonds(); bref != NULL; bref = bref->next) selectAtom(bref->item->partner(i));
	dbgEnd(Debug::Calls,"Model::selectionExpand");
}

// Select All Atoms
void Model::selectAll()
{
	dbgBegin(Debug::Calls,"Model::selectAll");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (!i->isSelected()) selectAtom(i);
	dbgEnd(Debug::Calls,"Model::selectAll");
}

// Deselect All Atoms
void Model::selectNone()
{
	dbgBegin(Debug::Calls,"Model::selectNone");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) deselectAtom(i);
	nSelected_ = 0;
	dbgEnd(Debug::Calls,"Model::selectNone");
}

// Atom at Screen Coordinates
Atom *Model::atomOnScreen(double x1, double y1)
{
	// See if an atom exists under the coordinates x1,y1
	// Ignore 'hidden' atoms_.
	dbgBegin(Debug::Calls,"Model::atomOnScreen");
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
	dbgEnd(Debug::Calls,"Model::atomOnScreen");
	return closest;
}

// Select atoms within bounding box
void Model::selectBox(double x1, double y1, double x2, double y2)
{
	// Box selection - choose all the atoms within the selection area
	dbgBegin(Debug::Calls,"Model::selectBox");
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
	dbgEnd(Debug::Calls,"Model::selectBox");
}

// Tree Select
void Model::selectTree(Atom *i)
{
	// The passed atom node is the starting point for the algorithm.
	// From here, select all atoms that are bound - if they are already
	// selected then ignore them. If they are not already selected, then
	// recursively call the routine on that atom.
	dbgBegin(Debug::Calls,"Model::selectTree");
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
	dbgEnd(Debug::Calls,"Model::selectTree");
}

// Select by Element
void Model::selectElement(Atom *target)
{
	// Select all atoms which are the same element as the atom i
	dbgBegin(Debug::Calls,"Model::selectElement");
	Atom *i = atoms_.first();
	while (i != NULL)
	{
		if (i->element() == target->element()) selectAtom(i);
		i = i->next;
	}
	dbgEnd(Debug::Calls,"Model::selectElement");
}

// Select by element (from ID)
void Model::selectElement(int id)
{
	// Select all atoms which are the same element as the atom with id 'target'
	Atom *i = atom(id);
	if (i != NULL) selectElement(i);
}

// Select with bounding Sphere
void Model::selectRadial(Atom *target, double radius)
{
	// Select all atoms which are within the distance 'radius' from atom 'target'
	dbgBegin(Debug::Calls,"Model::selectRadial");
	Atom *i = atoms_.first();
	printf("Selection radius is %8.4f Angstroms\n",radius);
	while (i != NULL)
	{
		if (i == target) selectAtom(i);
		else if (distance(target,i) < radius) selectAtom(i);
		i = i->next;
	}
	dbgEnd(Debug::Calls,"Model::selectRadial");
}

// Select Pattern
void Model::selectPattern(Pattern *p)
{
	// Select all atoms covered by the specified pattern.
	dbgBegin(Debug::Calls,"Model::selectPattern");
	Atom *i = p->firstAtom();
	for (int n=0; n<p->totalAtoms(); n++)
	{
		selectAtom(i);
		i = i->next;
	}
	dbgEnd(Debug::Calls,"Model::selectPattern");
}

// Get first selected
Atom *Model::firstSelected()
{
	dbgBegin(Debug::Calls,"Model::firstSelected");
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
	dbgEnd(Debug::Calls,"Model::firstSelected");
	return result;
}

// Select overlapping atoms
void Model::selectOverlaps(double tolerance)
{
	dbgBegin(Debug::Calls,"Model::selectOverlaps");
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
				msg(Debug::None,"Atom %i (%s) is %f from atom %i (%s).\n", j->id()+1, elements.symbol(j), deltar, i->id()+1, elements.symbol(i));
				selectAtom(j);
			}
		}
	}
	dbgEnd(Debug::Calls,"Model::selectOverlaps");
}

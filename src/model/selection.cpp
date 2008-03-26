/*
	*** Model selection functions
	*** src/model/selection.cpp
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

// Return the number of selected atoms
int Model::nSelected()
{
	return nSelected_;
}

// Move atoms 'up'
void Model::shiftSelectionUp()
{
	dbgBegin(Debug::Calls,"Model::shiftSelectionUp");
	if (nSelected_ == 0)
	{
		msg(Debug::None,"No atoms selected.");
		dbgEnd(Debug::Calls,"Model::shiftSelectionUp");
	}
	int tempid, oldid;
	Atom *i, *next;
	// For each selected atom in the model, shift it one place 'up' the atom list
	i = atoms_.first()->next;
	while (i != NULL)
	{
		next = i->next;
		if (i->isSelected() && (i != atoms_.first()))
		{
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
				Change *newchange = recordingState_->addChange();
				newchange->set(UE_SHIFT,oldid,-1);
			}
		}
		i = next;
	}
	logChange(LOG_STRUCTURE);
	dbgEnd(Debug::Calls,"Model::shiftSelectionUp");
}

// Move atoms 'down'
void Model::shiftSelectionDown()
{
	dbgBegin(Debug::Calls,"Model::shiftSelectionDown");
	if (nSelected_ == 0)
	{
		msg(Debug::None,"No atoms selected.");
		dbgEnd(Debug::Calls,"Model::shiftSelectionDown");
	}
	int tempid, oldid;
	Atom *i, *next;
	//for (n=0; n<atoms.nItems(); n++)
	// For each selected atom in the model, shift it one place 'down' the atom list
	i = atoms_.last()->prev;
	while (i != NULL)
	{
		next = i->prev;
		if (i->isSelected())
		{
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
				Change *newchange = recordingState_->addChange();
				newchange->set(UE_SHIFT,oldid,1);
			}
		}
		i = next;
	}
	logChange(LOG_STRUCTURE);
	dbgEnd(Debug::Calls,"Model::shiftSelectionDown");
}

// Move atoms to start
void Model::moveSelectionToStart()
{
	dbgBegin(Debug::Calls,"Model::moveSelectionToStart");
	int n;
	Atom *next, *i;
	// For each selected atom in the model, shift it to the end of the list
	i = atoms_.last();
	for (n=0; n<atoms_.nItems(); n++)
	{
		next = i->prev;
		if (i->isSelected()) atoms_.moveToStart(i);
		i = next;
	}
	// Renumber atoms
	renumberAtoms();
	logChange(LOG_STRUCTURE);
	dbgEnd(Debug::Calls,"Model::moveSelectionToStart");
}

// Move atoms to end
void Model::moveSelectionToEnd()
{
	dbgBegin(Debug::Calls,"Model::moveSelectionToEnd");
	int n;
	Atom *next, *i;
	// For each selected atom in the model, shift it to the end of the list
	i = atoms_.first();
	for (n=0; n<atoms_.nItems(); n++)
	{
		next = i->next;
		if (i->isSelected()) atoms_.moveToEnd(i);
		i = next;
	}
	// Renumber atoms
	renumberAtoms();
	logChange(LOG_STRUCTURE);
	dbgEnd(Debug::Calls,"Model::moveSelectionToEnd");
}

// Get selection cog
Vec3<double> Model::selectionCog()
{
        Vec3<double> result;
	Atom *first = firstSelected();
        if (first != NULL)
	{
		for (Atom *i = first; i != NULL; i = i->nextSelected()) result += cell_.mim(i,first);
		result /= nSelected_;
	}
        return result;
}

// Set selection visibility
void Model::selectionSetHidden(bool hidden)
{
	for (Atom *i = firstSelected(); i != NULL; i = i->nextSelected()) setHidden(i, hidden);
	logChange(LOG_VISUAL);
}

// Fix selected atom positions
void Model::selectionSetFixed()
{
	// Sets 'fixed' values to TRUE
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) i->setPositionFixed(TRUE);
}

// Free selected atom positions
void Model::selectionSetFree()
{
	// Sets 'fixed' values to TRUE
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) i->setPositionFixed(FALSE);
}

// Set selection style
void Model::selectionSetStyle(Atom::DrawStyle ds)
{
	// Sets all atoms currently selected to have the drawing style specified
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) i->setStyle(ds);
	logChange(LOG_VISUAL);
}


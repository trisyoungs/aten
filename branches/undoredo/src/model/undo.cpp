/*
	*** Undo/redo functions
	*** src/model/undo.cpp
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

#include "model/model.h"
#include "base/master.h"
#include "base/elements.h"

// Start recording a new undo state
void model::begin_undostate(const char *text)
{
	dbg_begin(DM_CALLS,"model::begin_undostate");
	// First, check that we're not already recording a state
	if (recordingstate != NULL)
	{
		printf("model::begin_undostate <<<< Last state has not been stored >>>>\n");
		dbg_end(DM_CALLS,"model::begin_undostate");
		return;
	}
	// Create a new state for us to add to
	recordingstate = new undostate;
	recordingstate->set_text(text);
	dbg_end(DM_CALLS,"model::begin_undostate");
}

// Finish recording the new undo state
void model::end_undostate()
{
	dbg_begin(DM_CALLS,"model::end_undostate");
	// Make sure that we have a valid state to store
	if (recordingstate == NULL)
	{
		printf("model::end_undostate <<<< No state to store >>>>\n");
		dbg_end(DM_CALLS,"model::end_undostate");
		return;
	}
	// Add the new state to the end of the undo level list
	undolevels.own(recordingstate);
	// Set the current undo level to the new state and nullify the pointer
	currentundostate = recordingstate;
	printf("Undo list now has %i states\n",undolevels.size());
	// Nullify the redostate pointer, since we must now be at the top of the undo stack
	currentredostate = NULL;
	recordingstate = NULL;
	dbg_end(DM_CALLS,"model::end_undostate");
}

// Perform actions in current undostate
void model::undo()
{
	dbg_begin(DM_CALLS,"model::undo");
	if (currentundostate == NULL) msg(DM_NONE,"Nothing to undo.\n");
	else
	{
		// Undo the changes
		currentundostate->revert();
		// Set new undo/redo pointers
		currentredostate = currentundostate;
		currentundostate = currentundostate->prev;
	}
	dbg_end(DM_CALLS,"model::undo");
}

// Perform actions in current undostate
void model::redo()
{
	dbg_begin(DM_CALLS,"model::redo");
	dbg_end(DM_CALLS,"model::redo");
}

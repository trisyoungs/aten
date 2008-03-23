/*
	*** Undo/redo functions
	*** src/model/undo.cpp
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
#include "base/master.h"
#include "base/elements.h"

// Return the current undo level pointer
Undostate *Model::currentUndostate()
{
	return currentUndostate_;
}

// Return the current redo level pointer
Undostate *Model::currentRedoState()
{
	return currentRedoState_;
}

// Start recording a new undo state
void Model::beginUndostate(const char *text)
{
	dbgBegin(DM_CALLS,"Model::beginUndostate");
	// First, check that we're not already recording a state
	if (recordingState_ != NULL)
	{
		printf("Model::beginUndostate <<<< Last state has not been stored >>>>\n");
		dbgEnd(DM_CALLS,"Model::beginUndostate");
		return;
	}
	// Create a new state for us to add to
	recordingState_ = new Undostate;
	recordingState_->setDescription(text);
	recordingState_->setStartLog(LOG_STRUCTURE, logs_[LOG_STRUCTURE]);
	recordingState_->setStartLog(LOG_COORDS, logs_[LOG_COORDS]);
	recordingState_->setStartLog(LOG_SELECTION, logs_[LOG_SELECTION]);
	msg(DM_VERBOSE,"Undo list prepped for new state.\n");
	msg(DM_VERBOSE,"   --- Logs at start of state are: structure = %i, coords = %i, selection = %i\n", logs_[LOG_STRUCTURE], logs_[LOG_COORDS], logs_[LOG_SELECTION]);
	dbgEnd(DM_CALLS,"Model::beginUndostate");
}

// Finish recording the new undo state
void Model::endUndostate()
{
	dbgBegin(DM_CALLS,"Model::endUndostate");
	// Make sure that we have a valid state to store...
	if (recordingState_ == NULL)
	{
		printf("Model::endUndostate <<<< No state to store >>>>\n");
		dbgEnd(DM_CALLS,"Model::endUndostate");
		return;
	}
	// ...and that it contains something
	if (recordingState_->nChanges() == 0)
	{
		recordingState_ = NULL;
		dbgEnd(DM_CALLS,"Model::endUndostate");
		return;
	}
	recordingState_->setEndLog(LOG_STRUCTURE, logs_[LOG_STRUCTURE]);
	recordingState_->setEndLog(LOG_COORDS, logs_[LOG_COORDS]);
	recordingState_->setEndLog(LOG_SELECTION, logs_[LOG_SELECTION]);
	// Delete all redo (i.e. future) states from the undo list
	if (currentUndostate_ == NULL) undoStates_.clear();
	else for (Undostate *u = currentUndostate_->next; u != NULL; u = undoStates_.removeAndGetNext(u)); 
	// Add the new state to the end of the undo level list
	undoStates_.own(recordingState_);
	// Set the current undo level to the new state and nullify the pointer
	currentUndostate_ = recordingState_;
	msg(DM_VERBOSE,"Undo list now has %i states (%i events caught in last state).\n",undoStates_.nItems(),currentUndostate_->nChanges());
	msg(DM_VERBOSE,"   --- Logs at end of state are: structure = %i, coords = %i, selection = %i\n", logs_[LOG_STRUCTURE], logs_[LOG_COORDS], logs_[LOG_SELECTION]);
	// Nullify the redostate pointer, since we must now be at the top of the undo stack
	currentRedoState_ = NULL;
	recordingState_ = NULL;
	// Check the size of the undoStates_ list - if greater than prefs.maxundo, must remove the first item in the list
	if (undoStates_.nItems() == (prefs.maxUndoLevels()+1)) undoStates_.remove(undoStates_.first());
	dbgEnd(DM_CALLS,"Model::endUndostate");
}

// Perform actions in current Undostate
void Model::undo()
{
	dbgBegin(DM_CALLS,"Model::undo");
	if (currentUndostate_ == NULL) msg(DM_NONE,"Nothing to undo.\n");
	else
	{
		// Undo the changes
		currentUndostate_->reverse(this);
		logs_[LOG_STRUCTURE] = currentUndostate_->startLog(LOG_STRUCTURE);
		logs_[LOG_COORDS] = currentUndostate_->startLog(LOG_COORDS);
		// Log a visual change if necessary
		if (currentUndostate_->doLogsDiffer()) logChange(LOG_VISUAL);
		// Set new undo/redo pointers
		currentRedoState_ = currentUndostate_;
		currentUndostate_ = currentUndostate_->prev;
	}
	dbgEnd(DM_CALLS,"Model::undo");
}

// Perform actions in current Undostate
void Model::redo()
{
	dbgBegin(DM_CALLS,"Model::redo");
	if (currentRedoState_ == NULL) msg(DM_NONE,"Nothing to redo.\n");
	else
	{
		// Undo the changes
		currentRedoState_->perform(this);
		logs_[LOG_STRUCTURE] = currentRedoState_->endLog(LOG_STRUCTURE);
		logs_[LOG_COORDS] = currentRedoState_->endLog(LOG_COORDS);
		// Log a visual change if necessary
		if (currentRedoState_->doLogsDiffer()) logChange(LOG_VISUAL);
		// Set new undo/redo pointers
		currentUndostate_ = currentRedoState_;
		currentRedoState_ = currentRedoState_->next;
	}
	dbgEnd(DM_CALLS,"Model::redo");
}

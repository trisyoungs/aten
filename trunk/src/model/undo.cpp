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
#include "base/aten.h"
#include "base/elements.h"

// Flag that undo/redo should be enabled
void Model::enableUndoRedo()
{
	msg.print(Messenger::Verbose, "Undo/redo has been enabled for this model.\n");
	undoRedoEnabled_ = TRUE;
}

// Return the current undo level pointer
UndoState *Model::currentUndoState()
{
	return currentUndoState_;
}

// Return the current redo level pointer
UndoState *Model::currentRedoState()
{
	return currentRedoState_;
}

// Start recording a new undo state
void Model::beginUndoState(const char *text)
{
	if (!undoRedoEnabled_) return;
	msg.enter("Model::beginUndoState");
	// First, check that we're not already recording a state
	if (recordingState_ != NULL)
	{
		printf("Model::beginUndoState <<<< Last state has not been stored >>>>\n");
		msg.exit("Model::beginUndoState");
		return;
	}
	// Create a new state for us to add to
	recordingState_ = new UndoState;
	recordingState_->setDescription(text);
	recordingState_->setStartLogs(changeLog);
	msg.print(Messenger::Verbose,"Undo list prepped for new state.\n");
	msg.print(Messenger::Verbose,"   --- Logs at start of state are: structure = %i, coords = %i, selection = %i\n", changeLog.log(Log::Structure), changeLog.log(Log::Coordinates), changeLog.log(Log::Selection));
	msg.exit("Model::beginUndoState");
}

// Finish recording the new undo state
void Model::endUndoState()
{
	if (!undoRedoEnabled_) return;
	msg.enter("Model::endUndoState");
	// Make sure that we have a valid state to store...
	if (recordingState_ == NULL)
	{
		printf("Model::endUndoState <<<< No state to store >>>>\n");
		msg.exit("Model::endUndoState");
		return;
	}
	// ...and that it contains something
	if (recordingState_->nChanges() == 0)
	{
		recordingState_ = NULL;
		msg.exit("Model::endUndoState");
		return;
	}
	recordingState_->setEndLogs(changeLog);
	// Delete all redo (i.e. future) states from the undo list
	if (currentUndoState_ == NULL) undoStates_.clear();
	else for (UndoState *u = currentUndoState_->next; u != NULL; u = undoStates_.removeAndGetNext(u)); 
	// Add the new state to the end of the undo level list
	undoStates_.own(recordingState_);
	// Set the current undo level to the new state and nullify the pointer
	currentUndoState_ = recordingState_;
	msg.print(Messenger::Verbose,"Undo list now has %i states (%i events caught in last state).\n",undoStates_.nItems(),currentUndoState_->nChanges());
	msg.print(Messenger::Verbose,"   --- Logs at end of state are: structure = %i, coords = %i, selection = %i\n", changeLog.log(Log::Structure), changeLog.log(Log::Coordinates), changeLog.log(Log::Selection));
	// Nullify the redostate pointer, since we must now be at the top of the undo stack
	currentRedoState_ = NULL;
	recordingState_ = NULL;
	// Check the size of the undoStates_ list - if greater than prefs.maxundo, must remove the first item in the list
	if (undoStates_.nItems() == (prefs.maxUndoLevels()+1)) undoStates_.remove(undoStates_.first());
	//listUndoStates();
	msg.exit("Model::endUndoState");
}

// Perform actions in current UndoState
void Model::undo()
{
	msg.enter("Model::undo");
	if (currentUndoState_ == NULL) msg.print("Nothing to undo.\n");
	else
	{
		// Undo the changes
		currentUndoState_->undo(this);
		changeLog.setLog(Log::Structure, currentUndoState_->startLog(Log::Structure));
		changeLog.setLog(Log::Coordinates, currentUndoState_->startLog(Log::Coordinates));
		// Log a visual change 
		changeLog.add(Log::Visual);
		// Set new undo/redo pointers
		currentRedoState_ = currentUndoState_;
		currentUndoState_ = currentUndoState_->prev;
	}
	//listUndoStates();
	msg.exit("Model::undo");
}

// Perform actions in current UndoState
void Model::redo()
{
	msg.enter("Model::redo");
	if (currentRedoState_ == NULL) msg.print("Nothing to redo.\n");
	else
	{
		// Undo the changes
		currentRedoState_->redo(this);
		changeLog.setLog(Log::Structure, currentRedoState_->endLog(Log::Structure));
		changeLog.setLog(Log::Coordinates, currentRedoState_->endLog(Log::Coordinates));
		// Log a visual change if necessary
		if (currentRedoState_->doLogsDiffer()) changeLog.add(Log::Visual);
		// Set new undo/redo pointers
		currentUndoState_ = currentRedoState_;
		currentRedoState_ = currentRedoState_->next;
	}
	//listUndoStates();
	msg.exit("Model::redo");
}

// List undo states and the changes within
void Model::listUndoStates()
{
	char suffix[32];
	int count = 0;
	printf("Current UndoStates in Model '%s' are:\n", name_.get());
	for (UndoState *u = undoStates_.first(); u != NULL; u = u->next)
	{
		count ++;
		if (currentUndoState_ == u) strcpy(suffix,"(Current Undo State)");
		else if (currentRedoState_ == u) strcpy(suffix,"(Current Redo State)");
		else suffix[0] = '\0';
		printf(" %3i : '%s'\n", count, u->description());
		printf("       Ptr=%li, %4i changes %s\n", u, u->nChanges(), suffix);
		u->print();
	}	
}

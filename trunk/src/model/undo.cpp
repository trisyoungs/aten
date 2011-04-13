/*
	*** Undo/redo functions
	*** src/model/undo.cpp
	Copyright T. Youngs 2007-2011

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
#include "classes/prefs.h"

// Flag that undo/redo should be enabled
void Model::enableUndoRedo()
{
	msg.print(Messenger::Verbose, "Undo/redo has been enabled for model '%s'.\n", name_.get());
	undoRedoEnabled_ = TRUE;
}

// Flag that undo/redo should be disabled
void Model::disableUndoRedo()
{
	msg.print(Messenger::Verbose, "Undo/redo has been disabled for model '%s'.\n", name_.get());
	undoRedoEnabled_ = FALSE;
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
void Model::beginUndoState(const char *fmt, ...)
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
	// Generate description text
	va_list arguments;
	static char msgs[8096];
	msgs[0] = '\0';
	// Parse the argument list (...) and internally write the output string into msgs[]
	va_start(arguments,fmt);
	vsprintf(msgs,fmt,arguments);
	va_end(arguments);
	recordingState_->setDescription(msgs);
	recordingState_->setStartLogs(changeLog);
	msg.print(Messenger::Verbose,"Undo list prepped for new state.\n");
	msg.print(Messenger::Verbose,"   --- Logs at start of state are: structure = %i, coords = %i, selection = %i, camera = %i\n", changeLog.log(Log::Structure), changeLog.log(Log::Coordinates), changeLog.log(Log::Selection), changeLog.log(Log::Camera));
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
		changeLog.setLog(Log::Camera, currentUndoState_->startLog(Log::Camera));
		changeLog.setLog(Log::Style, currentUndoState_->startLog(Log::Style));
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
		changeLog.setLog(Log::Camera, currentUndoState_->endLog(Log::Camera));
		changeLog.setLog(Log::Style, currentUndoState_->endLog(Log::Style));
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
	Dnchar suffix;
	int count = 0;
	printf("Current UndoStates in Model '%s' are:\n", name_.get());
	for (UndoState *u = undoStates_.first(); u != NULL; u = u->next)
	{
		count ++;
		if (currentUndoState_ == u) suffix = "(Current Undo State)";
		else if (currentRedoState_ == u) suffix = "(Current Redo State)";
		printf(" %3i : '%s'\n", count, u->description());
		printf("       Ptr=%p, %4i changes %s\n", u, u->nChanges(), suffix.get());
		u->print();
	}	
}

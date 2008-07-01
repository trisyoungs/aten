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
	msg.enter("Model::beginUndostate");
	// First, check that we're not already recording a state
	if (recordingState_ != NULL)
	{
		printf("Model::beginUndostate <<<< Last state has not been stored >>>>\n");
		msg.exit("Model::beginUndostate");
		return;
	}
	// Create a new state for us to add to
	recordingState_ = new Undostate;
	recordingState_->setDescription(text);
	recordingState_->setStartLog(Change::StructureLog, logs_[Change::StructureLog]);
	recordingState_->setStartLog(Change::CoordinateLog, logs_[Change::CoordinateLog]);
	recordingState_->setStartLog(Change::SelectionLog, logs_[Change::SelectionLog]);
	msg.print(Messenger::Verbose,"Undo list prepped for new state.\n");
	msg.print(Messenger::Verbose,"   --- Logs at start of state are: structure = %i, coords = %i, selection = %i\n", logs_[Change::StructureLog], logs_[Change::CoordinateLog], logs_[Change::SelectionLog]);
	msg.exit("Model::beginUndostate");
}

// Finish recording the new undo state
void Model::endUndostate()
{
	msg.enter("Model::endUndostate");
	// Make sure that we have a valid state to store...
	if (recordingState_ == NULL)
	{
		printf("Model::endUndostate <<<< No state to store >>>>\n");
		msg.exit("Model::endUndostate");
		return;
	}
	// ...and that it contains something
	if (recordingState_->nChanges() == 0)
	{
		recordingState_ = NULL;
		msg.exit("Model::endUndostate");
		return;
	}
	recordingState_->setEndLog(Change::StructureLog, logs_[Change::StructureLog]);
	recordingState_->setEndLog(Change::CoordinateLog, logs_[Change::CoordinateLog]);
	recordingState_->setEndLog(Change::SelectionLog, logs_[Change::SelectionLog]);
	// Delete all redo (i.e. future) states from the undo list
	if (currentUndostate_ == NULL) undoStates_.clear();
	else for (Undostate *u = currentUndostate_->next; u != NULL; u = undoStates_.removeAndGetNext(u)); 
	// Add the new state to the end of the undo level list
	undoStates_.own(recordingState_);
	// Set the current undo level to the new state and nullify the pointer
	currentUndostate_ = recordingState_;
	msg.print(Messenger::Verbose,"Undo list now has %i states (%i events caught in last state).\n",undoStates_.nItems(),currentUndostate_->nChanges());
	msg.print(Messenger::Verbose,"   --- Logs at end of state are: structure = %i, coords = %i, selection = %i\n", logs_[Change::StructureLog], logs_[Change::CoordinateLog], logs_[Change::SelectionLog]);
	// Nullify the redostate pointer, since we must now be at the top of the undo stack
	currentRedoState_ = NULL;
	recordingState_ = NULL;
	// Check the size of the undoStates_ list - if greater than prefs.maxundo, must remove the first item in the list
	if (undoStates_.nItems() == (prefs.maxUndoLevels()+1)) undoStates_.remove(undoStates_.first());
	msg.exit("Model::endUndostate");
}

// Perform actions in current Undostate
void Model::undo()
{
	msg.enter("Model::undo");
	if (currentUndostate_ == NULL) msg.print("Nothing to undo.\n");
	else
	{
		// Undo the changes
		currentUndostate_->reverse(this);
		logs_[Change::StructureLog] = currentUndostate_->startLog(Change::StructureLog);
		logs_[Change::CoordinateLog] = currentUndostate_->startLog(Change::CoordinateLog);
		// Log a visual change if necessary
		if (currentUndostate_->doLogsDiffer()) logChange(Change::VisualLog);
		// Set new undo/redo pointers
		currentRedoState_ = currentUndostate_;
		currentUndostate_ = currentUndostate_->prev;
	}
	msg.exit("Model::undo");
}

// Perform actions in current Undostate
void Model::redo()
{
	msg.enter("Model::redo");
	if (currentRedoState_ == NULL) msg.print("Nothing to redo.\n");
	else
	{
		// Undo the changes
		currentRedoState_->perform(this);
		logs_[Change::StructureLog] = currentRedoState_->endLog(Change::StructureLog);
		logs_[Change::CoordinateLog] = currentRedoState_->endLog(Change::CoordinateLog);
		// Log a visual change if necessary
		if (currentRedoState_->doLogsDiffer()) logChange(Change::VisualLog);
		// Set new undo/redo pointers
		currentUndostate_ = currentRedoState_;
		currentRedoState_ = currentRedoState_->next;
	}
	msg.exit("Model::redo");
}

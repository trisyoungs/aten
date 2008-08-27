/*
	*** Undo state storage
	*** src/classes/undostate.cpp
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

#include "classes/undostate.h"
#include "model/model.h"

// Constructor
UndoState::UndoState()
{
	// Public variables
	prev = NULL;
	next = NULL;
}

// Set the text associated with the current undo state
void UndoState::setDescription(const char *s)
{
	description_ = s;
}

// Return the current text associated with the state
const char *UndoState::description()
{
	return description_.get();
}

// Add event to undostate
void UndoState::addEvent(UndoEvent *ue)
{
	events_.own(ue);
}

// Return number of changes in list
int UndoState::nChanges()
{
	return events_.nItems();
}

// Set logs at start of state
void UndoState::setStartLogs(Log source)
{
	startLogs_ = source;
}

// Get structure log point at start of state
int UndoState::startLog(Log::LogType log)
{
	return startLogs_.log(log);
}

// Set logs at end of state
void UndoState::setEndLogs(Log source)
{
	endLogs_ = source;
}

// Get structure log point at end of state
int UndoState::endLog(Log::LogType log)
{
	return endLogs_.log(log);
}

// Undo changes detailed in state
void UndoState::undo(Model *m)
{
	msg.enter("UndoState::Undo");
	// Undo the changes stored in the change list
	for (UndoEvent* u = events_.last(); u != NULL; u = u->prev) u->undo(m);
	// Set model logs to the old values
	m->changeLog = startLogs_;
	msg.exit("UndoState::Undo");
}

// Redo changes detailed in state
void UndoState::redo(Model *m)
{
	msg.enter("UndoState::redo");
	for (UndoEvent* u = events_.first(); u != NULL; u = u->next) u->redo(m);
	// Set model logs to the new values
	m->changeLog = endLogs_;
	msg.exit("UndoState::redo");
}

// Check differences between logs for start/end points
bool UndoState::doLogsDiffer()
{
	if (startLogs_.log(Log::Structure) != endLogs_.log(Log::Structure)) return TRUE;
	if (startLogs_.log(Log::Coordinates) != endLogs_.log(Log::Coordinates)) return TRUE;
	if (startLogs_.log(Log::Selection) != endLogs_.log(Log::Selection)) return TRUE;
	return FALSE;
}

// Print changes in state
void UndoState::print()
{
	for (UndoEvent* u = events_.first(); u != NULL; u = u->prev) u->print();
}

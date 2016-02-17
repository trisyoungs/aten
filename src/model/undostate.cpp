/*
	*** Undo state storage
	*** src/model/undostate.cpp
	Copyright T. Youngs 2007-2016

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

#include "model/undostate.h"
#include "model/undoevent.h"
#include "model/model.h"
#include "base/messenger.h"

ATEN_USING_NAMESPACE

// Constructor
UndoState::UndoState() : ListItem<UndoState>()
{
}

// Set the text associated with the current undo state
void UndoState::setDescription(QString description)
{
	description_ = description;
}

// Return the current text associated with the state
QString UndoState::description() const
{
	return description_;
}

// Add event to undostate
void UndoState::addEvent(UndoEvent* ue)
{
	events_.own(ue);
}

// Return number of changes in list
int UndoState::nChanges() const
{
	return events_.nItems();
}

// Set logs at start of state
void UndoState::setStartLogs(Log source)
{
	startLogs_ = source;
}

// Get structure log point at start of state
int UndoState::startLog(Log::LogType log) const
{
	return startLogs_.log(log);
}

// Set logs at end of state
void UndoState::setEndLogs(Log source)
{
	endLogs_ = source;
}

// Get structure log point at end of state
int UndoState::endLog(Log::LogType log) const
{
	return endLogs_.log(log);
}

// Undo changes detailed in state
void UndoState::undo(Model* m)
{
	Messenger::enter("UndoState::Undo");
	// Undo the changes stored in the change list
	for (UndoEvent* u = events_.last(); u != NULL; u = u->prev) u->undo(m);
	// Set model logs to the old values
	m->setChangeLog(startLogs_);
	Messenger::exit("UndoState::Undo");
}

// Redo changes detailed in state
void UndoState::redo(Model* m)
{
	Messenger::enter("UndoState::redo");
	for (UndoEvent* u = events_.first(); u != NULL; u = u->next) u->redo(m);
	// Set model logs to the new values
	m->setChangeLog(endLogs_);
	Messenger::exit("UndoState::redo");
}

// Check differences between logs for start/end points
bool UndoState::doLogsDiffer() const
{
	if (startLogs_.log(Log::Structure) != endLogs_.log(Log::Structure)) return true;
	if (startLogs_.log(Log::Coordinates) != endLogs_.log(Log::Coordinates)) return true;
	if (startLogs_.log(Log::Selection) != endLogs_.log(Log::Selection)) return true;
	if (startLogs_.log(Log::Style) != endLogs_.log(Log::Style)) return true;
	if (startLogs_.log(Log::Cell) != endLogs_.log(Log::Cell)) return true;
	if (startLogs_.log(Log::Labels) != endLogs_.log(Log::Labels)) return true;
	if (startLogs_.log(Log::Glyphs) != endLogs_.log(Log::Glyphs)) return true;
	if (startLogs_.log(Log::Grids) != endLogs_.log(Log::Grids)) return true;
	if (startLogs_.log(Log::Misc) != endLogs_.log(Log::Misc)) return true;
	return false;
}

// Print changes in state
void UndoState::print() const
{
	for (UndoEvent* u = events_.first(); u != NULL; u = u->prev) u->print();
}

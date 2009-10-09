/*
	*** Log Class
	*** src/base/log.cpp
	Copyright T. Youngs 2007-2009

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUE ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "base/constants.h"
#include "base/messenger.h"
#include "base/log.h"

// Constructor
Log::Log()
{
	reset();
}

// Operator == (test equivalence of all log values except Total)
bool Log::operator==(Log &l) const
{
	for (int n=0; n<Log::Total; ++n) if (logs_[n] != l.logs_[n]) return FALSE;
	return TRUE;
}

// Reset all logs to zero
void Log::reset()
{
	for (int n=0; n<Log::nLogTypes; ++n) logs_[n] = 0;
	savePoint_ = 0;
}

// Set a log to a specific value
void Log::setLog(Log::LogType lt, int value)
{
	logs_[Log::Total] -= logs_[lt];
	logs_[lt] = value;
	logs_[Log::Total] += value;
	
}

// Log change
void Log::add(Log::LogType cl)
{
	logs_[cl] ++;
	// For all logs except Log::CameraLog we also update the total log
	if (cl != Log::Camera) logs_[Log::Total] ++;
}

// Return the log quantity specified
int Log::log(Log::LogType cl)
{
	return logs_[cl];
}

// Set the save point log for the model
void Log::updateSavePoint()
{
	savePoint_ = logs_[Log::Structure] + logs_[Log::Coordinates];
}

// Return if the log has been modified since last being saved
bool Log::isModified()
{
	return (savePoint_ == (logs_[Log::Structure] + logs_[Log::Coordinates]) ? FALSE : TRUE);
}

// Print logs
void Log::print()
{
	msg.print("Structure [%i], Coordinates [%i], Visual [%i], Selection [%i], Camera [%i], Glyph [%i], Total [%i]\n", logs_[Log::Structure], logs_[Log::Coordinates], logs_[Log::Visual], logs_[Log::Selection], logs_[Log::Camera], logs_[Log::Glyphs], logs_[Log::Total]); 
}

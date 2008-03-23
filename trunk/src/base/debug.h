/*
	*** Debug / messaging routines
	*** src/base/debug.h
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

#ifndef ATEN_DEBUG_H
#define ATEN_DEBUG_H

#include "../config.h"

/*
// Messaging
*/

// Debug messaging modes
enum DebugMode { DM_NONE=0, DM_CALLS=1, DM_MORECALLS=2, DM_TYPING=4, DM_PARSE=8, DM_VERBOSE=16, DM_FILTERS=32 };
// Add a debug level to the debug output bitvector
void addDebugLevel(DebugMode);
// Remove a debug level from the debug output bitvector
void removeDebugLevel(DebugMode);
// Returns whether the specified debug level is set
bool isDebugLevelActive(DebugMode);

// Messaging Functions
void msg(DebugMode, const char* ...);
void dbgBegin(DebugMode, const char* ...);
void dbgEnd(DebugMode, const char* ...);

#endif

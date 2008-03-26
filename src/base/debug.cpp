/*
	*** Debug / messaging routines
	*** src/base/debug.cpp
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

#include "gui/gui.h"
#include <stdarg.h>

/*
// Messaging
*/

// Bitvector of debug levels
int Debug::debugOutput = 0;
// Formatting indent for call debugging output
int funclevel = 0;
// Add a debug level to the debug output bitvector
void Debug::addDebug(Debug::DebugMode dm)
{
	if (!(Debug::debugOutput&dm)) Debug::debugOutput += dm;
}
// Remove a debug level from the debug output bitvector
void Debug::removeDebug(Debug::DebugMode dm)
{
	if (Debug::debugOutput&dm) Debug::debugOutput -= dm;
}
// Returns whether the specified debug level is set
bool Debug::isDebugActive(Debug::DebugMode dm)
{
	return ((Debug::debugOutput&dm) ? TRUE : FALSE);
}

// Standard message
void msg(Debug::DebugMode dm, const char *fmt ...)
{
	// Print to the text view in the main window if it has been initialised.
	// Otherwise, print to stdout. Also print to stdout if debuglevel >= msglevel.
	va_list arguments;
	static char msgs[8096];
	msgs[0] = '\0';
	// Parse the argument list (...) and internally write the output string into msgs[]
	va_start(arguments,fmt);
	vsprintf(msgs,fmt,arguments);
	// We always print messages with mode Debug::None to stdout *or* the GUI (if it has been initialised)
	// For other message levels, only print if it's debug level is active
	if (dm == Debug::None)
	{
		if (gui.exists()) gui.printMessage(msgs);
		else printf("%s",msgs);
	}
	else if (Debug::isDebugActive(dm)) printf("%s",msgs);
	va_end(arguments);
}

// Function enter
void dbgBegin(Debug::DebugMode dm, const char *fmt ...)
{
	// Debug Messaging - Enter Function
	static char msgs[8096];
	if (!Debug::isDebugActive(dm)) return;
	va_list arguments;
	msgs[0] = '\0';
	// Parse the argument list (...) and internally write the output string into msgs[]
	va_start(arguments,fmt);
	vsprintf(msgs,fmt,arguments);
	printf("%2i ",funclevel);
	for (int n=0; n<funclevel; n++) printf("--");
	printf("Begin : %s...\n",msgs);
	funclevel ++;
	va_end(arguments);
}

// Function leave
void dbgEnd(Debug::DebugMode dm, const char *fmt ...)
{
	// Debug Messaging - Leave Function
	static char msgs[8096];
	if (!Debug::isDebugActive(dm)) return;
	va_list arguments;
	msgs[0] = '\0';
	// Parse the argument list (...) and internally write the output string into msgs[]
	va_start(arguments,fmt);
	vsprintf(msgs,fmt,arguments);
	funclevel --;
	printf("%2i ",funclevel);
	for (int n=0; n<funclevel; n++) printf("--");
	printf("End   : %s.\n",msgs);
	va_end(arguments);
}

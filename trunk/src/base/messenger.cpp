/*
	*** Messaging routines
	*** src/base/messenger.cpp
	Copyright T. Youngs 2007-2015

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

#include "base/messenger.h"
#include "base/sysfunc.h"
#include <QtCore/QString>
#include <QtCore/QRegExp>
#include <stdarg.h>
#include <stdio.h>

ATEN_USING_NAMESPACE

// Static Members
int Messenger::outputTypes_ = 0;
int Messenger::callLevel_ = 0;
bool Messenger::quiet_ = false;

// Message output types
const char* OutputTypeKeywords[] = { "all", "calls", "commands", "parse", "typing", "verbose" };
Messenger::OutputType Messenger::outputType(const char* s, bool reportError)
{
	Messenger::OutputType ot = (Messenger::OutputType) enumSearch("output type",Messenger::nOutputTypes,OutputTypeKeywords,s);
	if ((ot == Messenger::nOutputTypes) && reportError) enumPrintValid(Messenger::nOutputTypes,OutputTypeKeywords);
	return ot;
}
const char* Messenger::outputType(Messenger::OutputType ot)
{
	return OutputTypeKeywords[ot];
}

// Constructor
Messenger::Messenger()
{
}

// Add a debug level to the debug output bitvector
void Messenger::addOutputType(Messenger::OutputType outputType)
{
	// Convert output type into bit if necessary
	if (outputType == Messenger::All) outputTypes_ = (1 << nOutputTypes) - 1;
	else if (!(outputTypes_&(1 << outputType))) outputTypes_ += (1 << outputType);
}

// Remove a debug level from the debug output bitvector
void Messenger::removeOutputType(Messenger::OutputType outputType)
{
	// Convert output type into bit if necessary
	if (outputType == Messenger::All) outputTypes_ = 0;
	else if (outputTypes_&(1 << outputType)) outputTypes_ -= (1 << outputType);
}

// Returns whether the specified debug level is set
bool Messenger::isOutputActive(Messenger::OutputType outputType)
{
	return outputTypes_&(1 << outputType);
}

// Set status of quiet mode
void Messenger::setQuiet(bool quiet)
{
	quiet_ = quiet;
}

// Return status of quiet mode
bool Messenger::isQuiet()
{
	return quiet_;
}

// Standard message
void Messenger::print(const char* fmtString, ...)
{
	// If program is in quiet mode, don't print anything to stdout
	if (quiet_) return;

	// Otherwise, print to stdout. Also print to stdout if debuglevel >= msglevel.
	va_list arguments;
	static char msgs[8096];
	msgs[0] = '\0';
	// Parse the argument list (...) and internally write the output string into msgs[]
	va_start(arguments, fmtString);
	vsprintf(msgs, fmtString, arguments);
	printf("%s", msgs);
	va_end(arguments);
}

// Standard message in specific output level
void Messenger::print(Messenger::OutputType outputType, const char* fmtString, ...)
{
	// If program is in quiet mode, or the output type isn't active, return now
	if (quiet_ || (! isOutputActive(outputType))) return;

	// Process arguments list
	va_list arguments;
	static char msgs[8096];
	msgs[0] = '\0';
	// Parse the argument list (...) and internally write the output string into msgs[]
	va_start(arguments, fmtString);
	vsprintf(msgs, fmtString ,arguments);
	printf("%s", msgs);
	va_end(arguments);
}

// Function enter
void Messenger::enter(const char* callname)
{
	if (!isOutputActive(Messenger::Calls)) return;

	printf("%2i ",callLevel_);
	for (int n=0; n<callLevel_; n++) printf("--");
	printf("Begin : %s...\n", callname);
	++callLevel_;
}

// Function leave
void Messenger::exit(const char* callName)
{
	if (!isOutputActive(Messenger::Calls)) return;

	--callLevel_;
	printf("%2i ", callLevel_);
	for (int n=0; n<callLevel_; n++) printf("--");
	printf("End   : %s.\n", callName);
}

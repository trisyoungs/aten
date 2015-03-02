/*
	*** Messaging routines
	*** src/base/messenger.h
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

#ifndef ATEN_MESSENGER_H
#define ATEN_MESSENGER_H

#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
/* none */

// Global messaging and program output levels
class Messenger
{
	public:
	// Constructor
	Messenger();
	// Messaging output types
	enum OutputType { All, Calls, Commands, Parse, Typing, Verbose, nOutputTypes };
	// Convert text string to debug mode
	static OutputType outputType(const char* name, bool reportError = 0);
	// Convert debug mode to text string
	static const char* outputType(OutputType ot);


	/*
	// Output Modes
	*/
	private:
	// Bitvector of active output levels
	static int outputTypes_;
	// Call level, used to provide indent for enter/exit info
	static int callLevel_;
	// Quiet mode
	static bool quiet_;

	public:
	// Add an output type to the output bitvector
	static void addOutputType(Messenger::OutputType outputType);
	// Remove an output type from the output bitvector
	static void removeOutputType(Messenger::OutputType outputType);
	// Returns whether the specified output type is active
	static bool isOutputActive(Messenger::OutputType outputType);
	// Set status of quiet mode
	static void setQuiet(bool quiet);
	// Return status of quiet mode
	static bool isQuiet();


	/*
	// Messaging functions
	*/
	public:
	// Print normal message
	static void print(const char* fmtString, ...);
	// Print message in specific output level
	static void print(Messenger::OutputType outputType, const char* fmtString, ...);
	// Entrance to subroutine
	static void enter(const char* callName);
	// Exit from subroutine
	static void exit(const char* callName);
};

ATEN_END_NAMESPACE

#endif

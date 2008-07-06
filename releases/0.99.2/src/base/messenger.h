/*
	*** Messaging routines
	*** src/base/messenger.h
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

#ifndef ATEN_MESSENGER_H
#define ATEN_MESSENGER_H

// Global messaging and program output levels
class Messenger
{
	public:
	// Constructor
	Messenger();
	// Messaging output types
	enum OutputType { None=0, Calls=1, Typing=2, Parse=4, Verbose=8, Commands=16, Error=32, nOutputTypes=7 };
	// Convert text string to debug mode
	static OutputType outputType(const char *s);

	/*
	// Output Modes
	*/
	private:
	// Bitvector of active output levels
	int outputTypes_;
	// Call level, used to provide indent for enter/exit info
	int callLevel_;
	// Quiet mode
	bool quiet_;

	public:
	// Add an output type to the output bitvector
	void addOutputType(Messenger::OutputType);
	// Remove an output type from the output bitvector
	void removeOutputType(Messenger::OutputType);	
	// Returns whether the specified output type is active
	bool isOutputActive(Messenger::OutputType);
	// Set status of quiet mode
	void setQuiet(bool b);

	/*
	// Messaging functions
	*/
	public:
	// Print normal message
	void print(const char* ...);
	// Print message in specific output level
	void print(Messenger::OutputType, const char* ...);
	// Entrances / exits to / from subroutines
	void enter(const char *callname);
	void exit(const char *callname);
};

extern Messenger msg;

#endif

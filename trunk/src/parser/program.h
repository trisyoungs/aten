/*
	*** Program
	*** src/parser/program.h
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

#ifndef ATEN_PROGRAM_H
#define ATEN_PROGRAM_H

#include "base/dnchar.h"
#include "parser/tree.h"
#include "parser/scopenode.h"

// Program
class Program : public ListItem<Program>
{
	public:
	// Constructors / Destructor
	Program();
	~Program();


	/*
	// Program data
	*/
	private:
	// Name, if any
	Dnchar name_;
	// Original source filename, if any
	Dnchar filename_;
	// Main program
	Tree mainProgram_;
	// List of filters belonging to this program
	List<Tree> filters_;
	// Whether this program is being populated from a filter file
	bool fromFilterFile_;
	// Whether or not a tree was pushed initially
	bool initialPushTree_;
	
	public:
	// Clear contents of program, including filters and functions
	void clear();
	// Set name of program
	void setName(const char *s);
	// Return name of program
	const char *name();
	// Return associated filename (if any)
	const char *filename();
	// Generate program from string 
	bool generateFromString(const char *s, const char *name, const char *sourceInfo, bool dontPushTree = FALSE, bool clearExisting = TRUE);
	// Generate program from string list
	bool generateFromStringList(Dnchar *stringListHead, const char *name, const char *sourceInfo, bool dontPushTree = FALSE, bool clearExisting = TRUE);
	// Generate program from input file
	bool generateFromFile(const char *filename, const char *name = NULL, bool dontPushTree = FALSE, bool clearExisting = TRUE, bool isFilterFile = FALSE);
	// Reload program (provided it was from a file...)
	bool reload();
	// Finalise program
	bool finalise();
	// Return main program
	Tree* mainProgram();
	// Add a filter tree
	Tree* addFilter();
	// Delete specified tree
	void deleteTree(Tree* t);
	// Return whether the Program is being generated from a filter file
	bool isFromFilterFile();
	// Execute main program, including GUI options if specified
	bool execute(ReturnValue &rv);
	// Print program information
	void print();


	/*
	// Program-Wide Functions
	*/
	private:
	// User-defined functions (local to this program)
	List<Tree> functions_;

	public:
	// Add a new Program-global function tree
	Tree* addFunction(const char *name);
	// Search for existing global function
	Tree* findFunction(const char *s);
	// Return first defined global function
	Tree* functions();
	// Execute specified global function
	bool executeFunction(const char *name, ReturnValue &rv, const char* arglist, ...);
};

#endif

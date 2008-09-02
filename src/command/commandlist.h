/*
	*** Command list
	*** src/command/commandlist.h
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

#ifndef ATEN_COMMANDLIST_H
#define ATEN_COMMANDLIST_H

#include "templates/reflist.h"
#include "templates/vector3.h"
#include "command/commands.h"
#include "variables/variablelist.h"
#include "base/parser.h"

// Forward declarations
class Command;
class Format;
class Filter;
class Model;
class ForcefieldAtom;

// Command List Structure
class CommandList
{
	public:
	// Constructor
	CommandList();
	// List pointers
	CommandList *prev, *next;

	/*
	// Command List
	*/
	private:
	// Original filename (if script)
	Dnchar scriptFilename_;
	// Name associated with command list
	Dnchar name_;
	// List of commands
	List<Command> commands_;
	// List of pointers to stacked branches
	Reflist<List<Command>,int> branchStack_;
	// Basic command types of stacked branches
	List<Command> branchCommandStack_;
	// Add specified branch to stack
	void pushBranch(List<Command>*, CommandAction, Command*);
	// Pop topmost branch from stack
	void popBranch();
	// Add command to topmost branch
	Command* addTopBranchCommand(CommandAction, Command*);
	// Return basenode of topmost branch of specified type in current stack (if any)
	Command *topmostBranch(CommandAction);

	public:
	// Set name of CommandList
	void setName(const char *s);
	// Return name of CommandList
	const char *name();
	// Return filename of CommandList
	const char *scriptFilename();
	// Return size of branch stack
	int nBranches();
	// Return type of topmost branch on stack
	CommandAction topBranchType();
	// Return basenode pointer of topmost branch on stack
	Command* topBranchBaseNode();
	// Add action to lst node
	bool addCommand(CommandAction);
	// Clear and reinitialise command list
	void clear();
	// Read semicolon-separated commands from string
	bool cacheLine(const char *s);
	// Cache command arguments in main parser
	bool cacheCommand();
	// Load commands from file
	bool load(const char *filename);
	// Execute command list
	bool execute(ifstream *altfile = NULL);
	// Check structure of command list
	bool validate();

	/*
	// Associated Filter
	*/
	private:
	// Parent filter (if CommandList is in a filter)
	Filter *parentFilter_;

	public:
	// Set parent filter
	void setFilter(Filter *f);
	// Return parent filter
	Filter *filter();
	
	/*
	// Variables
	*/
	public:
	// Associative variable list
	VariableList variables;
// 	// Create model variables with specified prefix
// 	bool createModelVariables(const char *s);
// 	// Set model variables with specified prefix
// 	void setModelVariables(const char *s, Model *m);
// 	// Create atom variables
// 	bool createAtomVariables(const char *s);
// 	// Set atom variables
// 	void setAtomVariables(const char*, Atom*);
// 	void setAtomVariables(const char*, int);
// 	// Create pattern variables
// 	bool createPatternVariables(const char *s);
// 	// Set pattern variables
// 	void setPatternVariables(const char*, Pattern*);
// 	// Create grid variables
// 	bool createGridVariables(const char *s);
// 	// Set grid variables
// 	void setGridVariables(const char*, Grid*);
// 	// Create pattern bound term variables
// 	bool createPatternBoundVariables(const char*);
// 	// Set pattern bound term variables
// 	void setPatternBoundVariables(const char*, PatternBound*);
// 	// Set pattern bound term variables (from simple Bond)
// 	void setPatternBoundVariables(const char*, Bond*);
// 	// Create atomtype atomtype variables
// 	bool createAtomtypeVariables(const char*);
// 	// Set atomtype variables
// 	void setAtomtypeVariables(const char*, ForcefieldAtom*);
// 	// Create subvariables for the specified variable (if necessary)
// 	bool createSubvariables(Variable *v);
// 	// Set subvariables for the specified variable (if necessary)
// 	void setSubvariables(Variable *v);

	/*
	// Files
	*/
	private:
	// Input file stream
	ifstream *inputFile_;
	// Output file stream
	ofstream *outputFile_;
	// Filename
	Dnchar filename_;
	// Parser read options for this CommandList
	int readOptions_;

	public:
	// Set input stream
	bool setInputFile(const char *inFile_);
	// Get input stream
	ifstream *inputFile();
	// Set output stream
	bool setOutputFile(const char *outFile_);
	// Get output stream
	ofstream *outputFile();
	// Return filename associated to infile/outfile
	const char *filename();
	// Close files
	void closeFiles();
	// Add read option
	void addReadOption(Parser::ParseOption po);
	// Remove read option
	void removeReadOption(Parser::ParseOption po);
	// Return read options
	int readOptions();
};

#endif

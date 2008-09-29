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
#include "command/command.h"
#include "variables/variablelist.h"
#include "base/parser.h"

// Forward declarations
class CommandNode;
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
	List<CommandNode> commands_;
	// List of pointers to stacked branches
	Reflist<List<CommandNode>,int> branchStack_;
	// Basic command types of stacked branches
	List<CommandNode> branchCommandStack_;
	// Add specified branch to stack
	void pushBranch(List<CommandNode>*, Command::Function, CommandNode*);
	// Pop topmost branch from stack
	void popBranch();
	// Add command to topmost branch
	CommandNode* addTopBranchCommand(Command::Function, CommandNode*);
	// Return basenode of topmost branch of specified type in current stack (if any)
	CommandNode *topmostBranch(Command::Function);

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
	Command::Function topBranchType();
	// Return basenode pointer of topmost branch on stack
	CommandNode* topBranchBaseNode();
	// Add action to lst node
	bool addCommand(Command::Function);
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
	private:
	// Associative variable list
	VariableList variables_;

	public:
	// Set header/frame variables in variable list
	void setHeaderVars(bool readingheader);

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

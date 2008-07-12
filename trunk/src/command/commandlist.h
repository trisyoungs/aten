/*
	*** Command list
	*** src/command/CommandList.h
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

#ifndef ATEN_COMMAND_H
#define ATEN_COMMAND_H

#include "templates/list.h"
#include "templates/reflist.h"
#include "templates/vector3.h"
#include "command/commands.h"
#include "parse/variablelist.h"
#include "base/constants.h"
#include "parse/parser.h"

// If Conditions
namespace IfTests
{
	enum IfTest { EqualTo=1, LessThan=2, LessThanEqualTo=3, GreaterThan=4, GreaterThanEqualTo=5, NotEqualTo=6 };
	const char *ifTest(IfTest);
}

// Variable Assignment Operators
namespace AssignOps
{
	enum AssignOp { Equals, PlusEquals, MinusEquals, DivideEquals, MultiplyEquals, nAssignOps };
	AssignOp assignOp(const char *s);
	const char *assignOp(AssignOp);
}

// Forward declarations
class CommandList;
class Format;
class Filter;
class Model;
class ForcefieldAtom;

// Command node
class Command
{
	public:
	// Constructor / Destructor
	Command();
	~Command();
	// List pointers
	Command *prev, *next;

	/*
	// Command
	*/
	private:
	// Command that this node performs
	CommandAction action_;
	// Pointer to action function
	CommandFunction function_;
	// Parent list
	CommandList *parent_;

	public:
	// Set parent CommandList
	void setParent(CommandList *cl);
	// Get parent CommandList
	CommandList *parent();
	// Set command
	void setCommand(CommandAction ca);
	// Get command
	CommandAction command();
	// Execute command
	int execute(Command *&c);

	/*
	// Format
	*/
	private:
	// Associated format (if any)
	Format *format_;

	public:
	// Create format structure
	bool createFormat(const char *s, VariableList &vlist, bool delimited);
	// Returns the formatter
	Format *format();
	// Delete the associated format
	void deleteFormat();

	/*
	// Loop Data
	*/
	private:
	// Whether the loop is currently executing
	bool loopActive_;
	// NUmber of iterations performed by loop
	int loopIterations_;

	public:
	// Set status of loop
	void setLoopActive(bool b);
	// Get status of loop
	bool isLoopActive();
	// Set iteration count
	void setLoopIterations(int n);
	// Get iteration count
	int loopIterations();
	// Increase interation count
	void increaseIterations();

	/*
	// Command Branch
	*/
	private:
	// Lists for branched commands (if any)
	List<Command> *branch_;
	// Pointer for use by flow control nodes
	Command *ptr_;

	public:
	// Create branch for the node
	List<Command> *createBranch();
	// Returns branch list structure
	List<Command> *branch();
	// Returns first item in branch 
	Command *branchCommands();
	// Set FormatNode pointer variable
	void setPointer(Command *f);
	// Return FormatNode pointer variable
	Command *pointer();

	/*
	// If Test Data
	*/
	private:
	// If condition structure
	IfTests::IfTest ifTest_;

	public:
	// Set if test type
	bool setIfTest(const char*);
	// Evaluate the if expression
	bool ifEvaluate();

	/*
	// Data Variables
	*/
	private:
	// Data variables
	Reflist<Variable,int> args_;

	public:
	// Set variables from parser arguments
	bool addVariables(const char*, const char*, VariableList&);
	// Return number of arguments given to command
	int nArgs();
	// Return variable argument
	Variable *arg(int argno);
	// Return argument as character
	const char *argc(int argno);
	// Return argument as integer
	int argi(int argno);
	// Return argument as double
	double argd(int argno);
	// Return argument as float
	float argf(int argno);
	// Return argument as bool
	bool argb(int argno);
	// Return arguments as Vec3<double>
	Vec3<double> arg3d(int);
	// Return arguments as Vec3<float>
	Vec3<float> arg3f(int);
	// Return arguments as Vec3<int>
	Vec3<int> arg3i(int);
	// Return argument as atom pointer
	Atom *arga(int argno);
	// Return argument as pattern pointer
	Pattern *argp(int argno);
	// Return argument as model pointer
	Model *argm(int argno);
	// Return argument as PatternBound pointer
	PatternBound *argpb(int argno);
	// Return argument as ForcefieldAtom pointer
	ForcefieldAtom *argffa(int argno);
	// Returns whether argument 'n' was provided
	bool hasArg(int argno);
	// Return variable type of argument
	Variable::VariableType argt(int argno);
	// Print data variables
	void printArgs();
};

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
	// Create model variables with specified prefix
	bool createModelVariables(const char *s);
	// Set model variables with specified prefix
	void setModelVariables(const char *s, Model *m);
	// Create atom variables
	bool createAtomVariables(const char *s);
	// Set atom variables
	void setAtomVariables(const char*, Atom*);
	void setAtomVariables(const char*, int);
	// Create atom variables
	bool createPatternVariables(const char *s);
	// Set pattern variables
	void setPatternVariables(const char*, Pattern*);
	// Create pattern bound term variables
	bool createPatternBoundVariables(const char*);
	// Set pattern bound term variables
	void setPatternBoundVariables(const char*, PatternBound*);
	// Create atomtype atomtype variables
	bool createAtomtypeVariables(const char*);
	// Set atomtype variables
	void setAtomtypeVariables(const char*, ForcefieldAtom*);
	// Create subvariables for the specified variable (if necessary)
	bool createSubvariables(Variable *v);

	/*
	// Local Variables
	*/
	public:
	// Pen orientation matrix
	Mat3<double> penOrientation;
	// Pen position
	Vec3<double> penPosition;

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

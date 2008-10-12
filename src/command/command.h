/*
	*** Command
	*** src/command/command.h
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

#include "templates/reflist.h"
#include "templates/list.h"
#include "templates/vector3.h"
#include "command/commands.h"
#include "base/parser.h"
#include "base/vtypes.h"

// Forward declarations
class CommandList;
class Format;
class Filter;
class Model;
class ForcefieldAtom;
class PatternBound;
class Variable;
class AccessPath;
class VariableList;

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

// Command node
class CommandNode
{
	public:
	// Constructor / Destructor
	CommandNode();
	~CommandNode();
	// List pointers
	CommandNode *prev, *next;

	/*
	// Command Data
	*/
	private:
	// Command that this node performs
	Command::Function function_;
	// Parent list
	CommandList *parent_;
	
	public:
	// Set parent CommandList
	void setParent(CommandList *cl);
	// Get parent CommandList
	CommandList *parent();
	// Set command function
	void setFunction(Command::Function ca);
	// Get command function
	Command::Function function();
	// Execute command
	int execute(CommandNode *&c);

	/*
	// Format
	*/
	private:
	// Associated format (if any)
	Format *format_;

	public:
	// Create format structure
	bool createFormat(const char *s, bool delimited);
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
	// Number of iterations performed by loop
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
	List<CommandNode> *branch_;
	// Pointer for use by flow control nodes
	CommandNode *ptr_;

	public:
	// Create branch for the node
	List<CommandNode> *createBranch();
	// Returns branch list structure
	List<CommandNode> *branch();
	// Returns first item in branch 
	CommandNode *branchCommands();
	// Set command pointer variable (used in flow control)
	void setPointer(CommandNode *f);
	// Return pointer variable
	CommandNode *pointer();

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
	// Arguments
	*/
	private:
	// Argument list
	Reflist<Variable,int> args_;
	// Variable list from which the command arguments were set
	VariableList *variableList_;
	// Add variable argument to reference list, given the name
	bool addArgument(int argid, Parser::ArgumentForm af = Parser::UnknownForm);

	public:
	// Set arguments from parser arguments
	bool setArguments(const char *cmdname, const char *specifiers, VariableList *sourcevars);
	// Add constant to reference list
	void addConstant(const char *s, bool forcechar = FALSE);
	// Add integer constant to reference list
	void addConstant(int i);
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
	// Return argument as pointer
	void *argp(int argno, VTypes::DataType );
	// Return argument as model pointer
	// Returns whether argument 'n' was provided
	bool hasArg(int argno);
	// Return variable type of argument
	VTypes::DataType argt(int argno);
	// Print data variables
	void printArgs();
};

#endif

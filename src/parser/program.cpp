/*
	*** Program
	*** src/parser/forest.cpp
	Copyright T. Youngs 2007-2011

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

#include "parser/parser.h"
#include "parser/program.h"
#include "parser/usercommandnode.h"
#include "parser/integer.h"
#include "parser/double.h"
#include "parser/character.h"
#include "parser/forcefieldatom.h"
#include "parser/forcefieldbound.h"
#include "parser/atom.h"
#include "main/aten.h"
#include "base/sysfunc.h"

// Constructors
Program::Program()
{
	// Private variables
	name_ = "NewProgram";
	fromFilterFile_ = FALSE;
	initialPushTree_ = FALSE;
	mainProgram_.setParent(this);
	
	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
Program::~Program()
{
}

// Clear forest
void Program::clear()
{
	functions_.clear();
	filters_.clear();
	mainProgram_.reset();
}

// Set name of forest
void Program::setName(const char *s)
{
	name_ = s;
}

// Return name of forest
const char *Program::name()
{
	return name_.get();
}

// Return filename of source file
const char *Program::filename()
{
	return filename_.get();
}

// Finalise forest
void Program::finalise()
{
	msg.enter("Program::finalise");
// 	// Create GUI controls for main program    TGAY XXX
// 	if (mainProgram_.widgets() != NULL)
// 	{
// 		if (!mainProgram_.isFilter()) mainProgram_.createCustomDialog(name_.get());
// 		else
// 		{
// 			Dnchar title;
// 			if (mainProgram_.filter.isExportFilter()) title.sprintf("Export Options (%s)", mainProgram_.name());
// 			else title.sprintf("Import Options (%s)", mainProgram_.name());
// 			mainProgram_.createCustomDialog(title.get());
// 		}
// 		// Grab default values
// 		mainProgram_.executeCustomDialog(TRUE);
// 	}
	// Cycle over generated filters
	for (Tree *filter = filters_.first(); filter != NULL; filter = filter->next)
	{
		// Register file filters with the master
		if (filter->isFilter())
		{
			aten.registerFilter(filter, filter->filter.type());
			// For trajectory import filters, we expect to find the two functions readheader and readframe, both returning integers
			if (filter->filter.type() == FilterData::TrajectoryImport)
			{
				// Search for 'int readheader()' function
				Tree *func = filter->findLocalFunction("readheader");
				if (func != NULL)
				{
					// Does the function have the correct return type?
					if (func->returnType() != VTypes::IntegerData) msg.print("Warning: 'readheader' function returns %s when it should return an int (importtrajectory filter '%s').\n", VTypes::aDataType(func->returnType()), filter->filter.name());
				}
				else msg.print("Warning: 'readheader' function has not been defined in the importtrajectory filter '%s'.\n", filter->filter.name());
				filter->filter.setTrajectoryHeaderFunction(func);

				// Search for 'int readframe()' function
				func = filter->findLocalFunction("readframe");
				if (func != NULL)
				{
					// Does the function have the correct return type?
					if (func->returnType() != VTypes::IntegerData) msg.print("Warning: 'readframe' function returns %s when it should return an int (importtrajectory filter '%s').\n", VTypes::aDataType(func->returnType()), filter->filter.name());
				}
				else msg.print("Warning: 'readframe' function has not been defined in the importtrajectory filter '%s'.\n", filter->filter.name());
				filter->filter.setTrajectoryFrameFunction(func);
			}
		}
// 		// Generate widgets (if Tree has any)  TGAY XXX
// 		if (filter->widgets() != NULL)
// 		{
// 			if (!filter->isFilter()) filter->createCustomDialog(name_.get());
// 			else
// 			{
// 				Dnchar title;
// 				if (filter->filter.isExportFilter()) title.sprintf("Export Options (%s)", filter->filter.name());
// 				else title.sprintf("Import Options (%s)", filter->filter.name());
// 				filter->createCustomDialog(title.get());
// 			}
// 			// Grab default values
// 			filter->executeCustomDialog(TRUE);
// 		}
	}
	// Generate widgets in global functions
	for (Tree *t = functions_.first(); t != NULL; t = t->next)
	{
	}
	msg.exit("Program::finalise");
}

// Return main program
Tree *Program::mainProgram()
{
	return &mainProgram_;
}

// Create a new filter
Tree *Program::addFilter()
{
	Tree *tree = filters_.add();
	tree->setParent(this);
	tree->setType(Tree::FilterTree);
	return tree;
}

// Add a Program-global function
Tree *Program::addGlobalFunction(const char *name)
{
	Tree *tree = functions_.add();
	tree->setName(name);
	tree->setType(Tree::FunctionTree);
	tree->setParent(this);
	return tree;
}

// Search for existing global function
Tree *Program::findGlobalFunction(const char *name)
{
	Tree *result;
	for (result = functions_.first(); result != NULL; result = result->next) if (strcmp(result->name(),name) == 0) break;
	return result;
}

// Return first defined global function...
Tree *Program::globalFunctions()
{
	return functions_.first();
}

// Execute specified global function
bool Program::executeGlobalFunction(const char *funcname, ReturnValue &rv, const char *arglist, ...)
{
	msg.enter("Program::executeGlobalFunction");
	// First, locate funciton with the name supplied
	Tree *func = findGlobalFunction(funcname);
	if (func == NULL)
	{
		printf("Error: No global function named '%s' exists in '%s'.\n", funcname, name_.get());
		msg.exit("Program::executeGlobalFunction");
		return FALSE;
	}

	// Construct list of arguments to pass to function
	va_list vars;
	va_start(vars, arglist);
	List<TreeNode> args;
	TreeNode *var;
	for (const char *c = &arglist[0]; *c != '\0'; c++)
	{
		switch (*c)
		{
			case ('i'):
				var = new IntegerVariable(va_arg(vars, int), TRUE);
				break;
			case ('d'):
				var = new DoubleVariable(va_arg(vars, double), TRUE);
				break;
			case ('c'):
			case ('s'):
				var = new StringVariable(va_arg(vars, const char *), TRUE);
				break;
			case ('a'):
				var = new AtomVariable(va_arg(vars, Atom*));
				break;
			case ('y'):
				var = new ForcefieldAtomVariable(va_arg(vars, ForcefieldAtom*));
				break;
			case ('z'):
				var = new ForcefieldBoundVariable(va_arg(vars, ForcefieldBound*));
				break;
			default:
				printf("Invalid argument specifier '%c' in Program::executeGlobalFunction.\n", *c);
				var = NULL;
				break;
		}
		args.own(var);
	}
	va_end(vars);

	// Now, pass all the info on to the static 'run' command in UserCommandNode
	bool success = UserCommandNode::run(func,rv,args.first());

	msg.exit("Program::executeGlobalFunction");
	return success;
}

// Generate forest from string 
bool Program::generateFromString(const char *s, const char *name, bool dontpushtree, bool clearExisting)
{
	msg.enter("Program::generateFromString");
	name_ = name;
	fromFilterFile_ = FALSE;
	initialPushTree_ = dontpushtree;
	bool result = cmdparser.generateFromString(this, s, initialPushTree_, clearExisting);
	finalise();
	msg.exit("Program::generateFromString");
	return result;
}

// Generate forest from string list
bool Program::generateFromStringList(Dnchar *stringListHead, const char *name, bool dontpushtree, bool clearExisting)
{
	msg.enter("Program::generateFromStringList");
	name_ = name;
	fromFilterFile_ = FALSE;
	initialPushTree_ = dontpushtree;
	bool result = cmdparser.generateFromStringList(this, stringListHead, initialPushTree_, clearExisting);
	finalise();
	msg.exit("Program::generateFromStringList");
	return result;
}

// Generate forest from input file
bool Program::generateFromFile(const char *filename, const char *name, bool dontpushtree, bool clearExisting, bool isFilterFile)
{
	msg.enter("Program::generateFromFile");
	filename_ = absoluteFilePath(filename);
	if (name != NULL) name_ = name;
	else name_ = filename;
	fromFilterFile_ = isFilterFile;
	initialPushTree_ = dontpushtree;
	bool result = cmdparser.generateFromFile(this, filename, initialPushTree_, clearExisting);
	finalise();
	msg.exit("Program::generateFromFile");
	return result;
}

// Reload forest (provided it was from a file...)
bool Program::reload()
{
	msg.enter("Program::reload");
	if (filename_.isEmpty())
	{
		msg.print("No filename present in '%s' - can't reload commands.\n", name_.get());
		msg.exit("Program::reload");
		return FALSE;
	}
	// Clear old data...
	clear();
	bool result = cmdparser.generateFromFile(this, filename_, initialPushTree_);
	finalise();
	msg.exit("Program::reload");
	return result;
}

// Delete specified tree
void Program::deleteTree(Tree *t)
{
	if (t == NULL) return;
	// Search for the specified tree...
	if (filters_.contains(t)) filters_.remove(t);
	else if (functions_.contains(t)) functions_.remove(t);
	else printf("Internal Error: Tree to be deleted is not owned by the current parent structure.\n");
}

// Return whether the Program is being generated from a filterfile
bool Program::isFromFilterFile()
{
	return fromFilterFile_;
}

// Execute all trees in forest
bool Program::execute(ReturnValue &rv, bool runOptions)
{
	msg.enter("Program::execute");
	bool result = TRUE;
	if (runOptions) result = mainProgram_.defaultDialog().execute();
	if (result) result = mainProgram_.execute(rv);
	msg.exit("Program::execute");
	return result;
}

// Print forest information
void Program::print()
{
	printf("Program '%s':\nContains: %i filters and %i functions.\n", name_.get(), filters_.nItems(), functions_.nItems());
	if (filters_.nItems() > 0) printf("  Trees:\n");
	for (int n=0; n<filters_.nItems(); ++n) printf("     %-3i  %s\n", n+1, filters_[n]->name());
	if (functions_.nItems() > 0) printf("  Functions:\n");
	for (int n=0; n<functions_.nItems(); ++n) printf("     %-3i  %s\n", n+1, functions_[n]->name());
}


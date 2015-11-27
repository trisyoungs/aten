/*
	*** Program
	*** src/parser/program.cpp
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

#include "parser/program.h"
#include "parser/parser.h"
#include "parser/character.h"
#include "parser/forcefieldatom.h"
#include "parser/forcefieldbound.h"
#include "parser/atom.h"
#include "main/aten.h"
#include "base/sysfunc.h"
#include <QFileInfo>

ATEN_USING_NAMESPACE

// Constructors
Program::Program() : ListItem<Program>()
{
	// Private variables
	name_ = "NewProgram";
	fromFilterFile_ = false;
	initialPushTree_ = false;
	mainProgram_.setParent(this);
	generatedSuccessfully_ = false;
}

// Destructor
Program::~Program()
{
}

// Clear Program
void Program::clear()
{
	functions_.clear();
	filters_.clear();
	mainProgram_.reset();

	generatedSuccessfully_ = false;
}

// Set name of Program
void Program::setName(QString name)
{
	name_ = name;
}

// Return name of Program
QString Program::name()
{
	return name_;
}

// Return filename of source file
QString Program::filename()
{
	return filename_;
}

// Finalise Program
bool Program::finalise(Aten* aten)
{
	Messenger::enter("Program::finalise");

	// Finalise main program
	if (!mainProgram_.finalise())
	{
		Messenger::print("Failed to finalise main content of Program.\n");
		Messenger::exit("Program::finalise");
		return false;
	}

	// Cycle over generated filters
	for (Tree* filter = filters_.first(); filter != NULL; filter = filter->next)
	{
		// Register file filters with the master
		if (filter->isFilter())
		{
			aten->registerFilter(filter, filter->filter.type());
			// For trajectory import filters, we expect to find the two functions readHeader and readFrame, both returning integers
			if (filter->filter.type() == FilterData::TrajectoryImport)
			{
				// Search for 'int readHeader()' function
				Tree* func = filter->findLocalFunction("readHeader");
				if (func != NULL)
				{
					// Does the function have the correct return type?
					if (func->returnType() != VTypes::IntegerData)
					{
						Messenger::print("Error: 'readHeader' function returns %s when it should return an int (importtrajectory filter '%s').", VTypes::aDataType(func->returnType()), qPrintable(filter->filter.name()));
						Messenger::exit("Program::finalise");
						return false;
					}
				}
				else Messenger::print("Warning: 'readHeader' function has not been defined in the importtrajectory filter '%s'.", qPrintable(filter->filter.name()));
				filter->filter.setTrajectoryHeaderFunction(func);

				// Search for 'int readFrame()' function
				func = filter->findLocalFunction("readFrame");
				if (func != NULL)
				{
					// Does the function have the correct return type?
					if (func->returnType() != VTypes::IntegerData)
					{
						Messenger::print("Error: 'readFrame' function returns %s when it should return an int (importtrajectory filter '%s').", VTypes::aDataType(func->returnType()), qPrintable(filter->filter.name()));
						Messenger::exit("Program::finalise");
						return false;
					}
				}
				else Messenger::print("Warning: 'readFrame' function has not been defined in the importtrajectory filter '%s'.", qPrintable(filter->filter.name()));
				filter->filter.setTrajectoryFrameFunction(func);
			}
			
			// Finalise the tree
			if (!filter->finalise())
			{
				Messenger::print("Error finalising filter '%s'.", qPrintable(filter->filter.name()));
				Messenger::exit("Program::finalise");
				return false;
			}
		}
	}

	// Cycle over defined local functions and finalise
	for (Tree* func = functions_.first(); func != NULL; func = func->next)
	{
		if (!func->finalise())
		{
			Messenger::print("Error finalising global function '%s'.", qPrintable(func->name()));
			Messenger::exit("Program::finalise");
			return false;
		}
	}
	
	Messenger::exit("Program::finalise");
	return true;
}

// Return main program
Tree* Program::mainProgram()
{
	return &mainProgram_;
}

// Create a new filter
Tree* Program::addFilter()
{
	Tree* tree = filters_.add();
	tree->setParent(this);
	tree->setType(Tree::FilterTree);
	return tree;
}

// Generate Program from string 
bool Program::generateFromString(QString line, QString name, QString sourceInfo, bool pushTree, bool clearExisting, bool quiet)
{
	Messenger::enter("Program::generateFromString");

	name_ = name;
	fromFilterFile_ = false;
	initialPushTree_ = pushTree;
	quiet_ = quiet;
	generatedSuccessfully_ = cmdparser.generateFromString(this, line, sourceInfo, initialPushTree_, clearExisting);
	if (generatedSuccessfully_) generatedSuccessfully_ = finalise(cmdparser.aten());

	Messenger::exit("Program::generateFromString");
	return generatedSuccessfully_;
}

// Generate Program from string list
bool Program::generateFromStringList(QStringList stringList, QString name, QString sourceInfo, bool pushTree, bool clearExisting, bool quiet)
{
	Messenger::enter("Program::generateFromStringList");

	name_ = name;
	fromFilterFile_ = false;
	initialPushTree_ = pushTree;
	quiet_ = quiet;
	generatedSuccessfully_ = cmdparser.generateFromStringList(this, stringList, sourceInfo, initialPushTree_, clearExisting);
	if (generatedSuccessfully_) generatedSuccessfully_ = finalise(cmdparser.aten());

	Messenger::exit("Program::generateFromStringList");
	return generatedSuccessfully_;
}

// Generate Program from input file
bool Program::generateFromFile(QString filename, QString name, bool pushTree, bool clearExisting, bool quiet, bool isFilterFile)
{
	Messenger::enter("Program::generateFromFile");
	
	QFileInfo fileInfo(filename);
	filename_ = fileInfo.absoluteFilePath();
	if (name != NULL) name_ = name;
	else name_ = filename;
	fromFilterFile_ = isFilterFile;
	initialPushTree_ = pushTree;
	quiet_ = quiet;
	generatedSuccessfully_ = cmdparser.generateFromFile(this, filename, initialPushTree_, clearExisting);
	if (generatedSuccessfully_) generatedSuccessfully_ = finalise(cmdparser.aten());

	Messenger::exit("Program::generateFromFile");
	return generatedSuccessfully_;
}

// Reload Program (provided it was from a file...)
bool Program::reload()
{
	Messenger::enter("Program::reload");
	if (filename_.isEmpty())
	{
		Messenger::print("No filename present in '%s' - can't reload commands.", qPrintable(name_));
		Messenger::exit("Program::reload");
		return false;
	}
	// Clear old data...
	clear();
	bool result = cmdparser.generateFromFile(this, filename_, initialPushTree_);
	if (result) result = finalise(cmdparser.aten());
	Messenger::exit("Program::reload");
	return result;
}

// Delete specified tree
void Program::deleteTree(Tree* t)
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

// Execute all trees in Program
bool Program::execute(ReturnValue& rv)
{
	Messenger::enter("Program::execute");
	bool result = mainProgram_.execute(rv);
	Messenger::exit("Program::execute");
	return result;
}

// Print Program information
void Program::print()
{
	printf("Program '%s':\nContains: %i filters and %i functions.\n", qPrintable(name_), filters_.nItems(), functions_.nItems());
	if (filters_.nItems() > 0) printf("  Trees:\n");
	for (int n=0; n<filters_.nItems(); ++n) printf("     %-3i  %s\n", n+1, qPrintable(filters_[n]->name()));
	if (functions_.nItems() > 0) printf("  Functions:\n");
	for (int n=0; n<functions_.nItems(); ++n) printf("     %-3i  %s\n", n+1, qPrintable(functions_[n]->name()));
}

// Return whether the program was successfully created by the last generate*() call
bool Program::generatedSuccessfully()
{
	return generatedSuccessfully_;
}

// Return whether to generate program quietly (i.e. don't print any error messages)
bool Program::quiet()
{
	return quiet_;
}

/*
 * Global Functions
 */

// Add a Program-global function
Tree* Program::addFunction(QString name)
{
	Tree* tree = functions_.add();
	tree->setName(name);
	tree->setType(Tree::FunctionTree);
	tree->setParent(this);
	return tree;
}

// Search for existing global function
Tree* Program::findFunction(QString functionName)
{
	Tree* function;
	for (function = functions_.first(); function != NULL; function = function->next) if (functionName == function->name()) break;
	return function;
}

// Return first defined global function...
Tree* Program::functions()
{
	return functions_.first();
}

// Execute specified global function
bool Program::executeFunction(QString functionName, ReturnValue& rv, const char* argList, ...)
{
	Messenger::enter("Program::executeGlobalFunction");
	// First, locate function with the name supplied
	Tree* func = findFunction(functionName);
	if (func == NULL)
	{
		printf("Error: No global function named '%s' exists in '%s'.\n", qPrintable(functionName), qPrintable(name_));
		Messenger::exit("Program::executeGlobalFunction");
		return false;
	}

	// Construct list of arguments to pass to function
	va_list vars;
	va_start(vars, argList);
	List<TreeNode> args;
	TreeNode* var;
	for (const char* c = &argList[0]; *c != '\0'; c++)
	{
		switch (*c)
		{
			case ('i'):
				var = new IntegerVariable(va_arg(vars, int), true);
				break;
			case ('d'):
				var = new DoubleVariable(va_arg(vars, double), true);
				break;
			case ('c'):
			case ('s'):
				var = new StringVariable(va_arg(vars, const char*), true);
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
		if (var) args.own(var);
	}
	va_end(vars);

	// Now, pass all the info on to the static 'run' command in UserCommandNode
	bool success = UserCommandNode::run(func,rv,args.first());

	Messenger::exit("Program::executeGlobalFunction");
	return success;
}

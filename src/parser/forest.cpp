/*
	*** Forest
	*** src/parser/forest.cpp
	Copyright T. Youngs 2007-2010

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
#include "parser/forest.h"
#include "parser/usercommandnode.h"
#include "parser/integer.h"
#include "parser/double.h"
#include "parser/character.h"
#include "main/aten.h"

// Constructor
Forest::Forest()
{
	// Private variables
	name_ = "NewForest";
	fromFilterFile_ = FALSE;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
Forest::~Forest()
{
}

// Clear forest
void Forest::clear()
{
	functions_.clear();
	trees_.clear();
}

// Set name of forest
void Forest::setName(const char *s)
{
	name_ = s;
}

// Return name of forest
const char *Forest::name()
{
	return name_.get();
}

// Return filename of source file
const char *Forest::filename()
{
	return filename_.get();
}

// Finalise forest
void Forest::finalise()
{
	// Register any filters with the master
	for (Tree *t = trees_.first(); t != NULL; t = t->next)
	{
		if (t->isFilter())
		{
			aten.registerFilter(t, t->filter.type());
			// For trajectory import filters, we expect to find the two functions readheader and readframe, both returning integers
			if (t->filter.type() == FilterData::TrajectoryImport)
			{
				// Search for 'int readheader()' function
				Tree *func = t->findLocalFunction("readheader");
				if (func != NULL)
				{
					// Does the function have the correct return type?
					if (func->returnType() != VTypes::IntegerData) msg.print("Warning: 'readheader' function returns %s when it should return an int (importtrajectory filter '%s').\n", VTypes::aDataType(func->returnType()), t->filter.name());
				}
				else msg.print("Warning: 'readheader' function has not been defined in the importtrajectory filter '%s'.\n", t->filter.name());
				t->filter.setTrajectoryHeaderFunction(func);

				// Search for 'int readframe()' function
				func = t->findLocalFunction("readframe");
				if (func != NULL)
				{
					// Does the function have the correct return type?
					if (func->returnType() != VTypes::IntegerData) msg.print("Warning: 'readframe' function returns %s when it should return an int (importtrajectory filter '%s').\n", VTypes::aDataType(func->returnType()), t->filter.name());
				}
				else msg.print("Warning: 'readframe' function has not been defined in the importtrajectory filter '%s'.\n", t->filter.name());
				t->filter.setTrajectoryFrameFunction(func);
			}
		}
	}
}

// Return number of trees in forest
int Forest::nTrees()
{
	return trees_.nItems();
}

// Create a new tree
Tree *Forest::addTree(Tree::TreeType type)
{
	Tree *tree = trees_.add();
	tree->setType(type);
	tree->setParent(this);
	return tree;
}

// Add a Forest-global function
Tree *Forest::addGlobalFunction(const char *name)
{
	Tree *tree = functions_.add();
	tree->setName(name);
	tree->setType(Tree::FunctionTree);
	tree->setParent(this);
	return tree;
}

// Search for existing global function
Tree *Forest::findGlobalFunction(const char *name)
{
	Tree *result;
	for (result = functions_.first(); result != NULL; result = result ->next) if (strcmp(result->name(),name) == 0) break;
	return result;
}

// Execute specified global function
bool Forest::executeGlobalFunction(const char *funcname, ReturnValue &rv, const char *arglist ...)
{
	msg.enter("Forest::executeGlobalFunction");
	// First, locate funciton with the name supplied
	Tree *func = findGlobalFunction(funcname);
	if (func == NULL)
	{
		printf("Error: No global function named '%s' exists in '%s'.\n", funcname, name_.get());
		msg.exit("Forest::executeGlobalFunction");
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
			default:
				printf("Invalid argument specifier '%c' in Forest::executeGlobalFunctin.\n", *c);
				var = NULL;
				break;
		}
		args.own(var);
	}
	va_end(vars);

	// Now, pass all the info on to the static 'run' command in UserCommandNode
	bool success = UserCommandNode::run(func,rv,args.first());

	msg.exit("Forest::executeGlobalFunction");
	return success;
}

// Generate forest from string 
bool Forest::generateFromString(const char *s, const char *name, bool dontpushtree)
{
	msg.enter("Forest::generateFromString");
	name_ = name;
	fromFilterFile_ = FALSE;
	bool result = cmdparser.generateFromString(this, s, dontpushtree);
	finalise();
	msg.exit("Forest::generateFromString");
	return result;
}

// Generate forest from string list
bool Forest::generateFromStringList(Dnchar *stringListHead, const char *name, bool dontpushtree)
{
	msg.enter("Forest::generateFromStringList");
	name_ = name;
	fromFilterFile_ = FALSE;
	bool result = cmdparser.generateFromStringList(this, stringListHead, dontpushtree);
	print();
	finalise();
	msg.exit("Forest::generateFromStringList");
	return result;
}

// Generate forest from input file
bool Forest::generateFromFile(const char *filename, const char *name, bool isFilterFile)
{
	msg.enter("Forest::generateFromFile");
	filename_ = filename;
	if (name != NULL) name_ = name;
	else name_ = filename;
	fromFilterFile_ = isFilterFile;
	bool result = cmdparser.generateFromFile(this, filename);
// 	print();
	finalise();
	msg.exit("Forest::generateFromFile");
	return result;
}

// Delete specified tree
void Forest::deleteTree(Tree *t)
{
	if (t == NULL) return;
	// Search for the specified tree...
	if (trees_.ownsItem(t)) trees_.remove(t);
	else if (functions_.ownsItem(t)) functions_.remove(t);
	else printf("Internal Error: Tree to be deleted is not owned by the current parent structure.\n");
}

// Return whether the Forest is being generated from a filterfile
bool Forest::isFromFilterFile()
{
	return fromFilterFile_;
}

// Execute all trees in forest
bool Forest::executeAll(ReturnValue &rv)
{
	msg.enter("Forest::executeAll");
	int count = 0;
	bool result = TRUE;
	for (Tree *t = trees_.first(); t != NULL; t = t->next)
	{
		count ++;
		if (t->isFilter()) msg.print(Messenger::Parse, "Skipping tree %i of %i since it's a filter....\n", count, trees_.nItems());
		else
		{
			msg.print(Messenger::Parse, "Executing tree %i of %i in set '%s')....\n", count, trees_.nItems(), name_.get());
			result = t->execute(rv);
			if (!result) break;
		}
	}
	msg.exit("Forest::executeAll");
	return result;
}

// Print forest information
void Forest::print()
{
	printf("Forest '%s':\nContains:  %i trees and %i functions.\n", name_.get(), trees_.nItems(), functions_.nItems());
	if (trees_.nItems() > 0) printf("  Trees:\n");
	for (int n=0; n<trees_.nItems(); ++n) printf("     %-3i  %s\n", n+1, trees_[n]->name());
	if (trees_.nItems() > 0) printf("  Functions:\n");
	for (int n=0; n<functions_.nItems(); ++n) printf("     %-3i  %s\n", n+1, functions_[n]->name());
}


/*
	*** Forest
	*** src/parser/forest.cpp
	Copyright T. Youngs 2007-2009

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
#include "main/aten.h"

// Constructor
Forest::Forest()
{
	// Private variables
	name_ = "NewForest";

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
	for (Tree *t = trees_.first(); t != NULL; t = t->next) if (t->isFilter())
		aten.registerFilter(t, t->filter.type());
}

// Return number of trees in forest
int Forest::nTrees()
{
	return trees_.nItems();
}

// Create a new tree
Tree *Forest::pushTree(bool isfilter)
{
	msg.enter("Forest::pushTree");
	Tree *tree = trees_.add();
	tree->setParent(this);
	stack_.add(tree,isfilter);
	msg.exit("Forest::pushTree");
	return tree;
}

// Finish the last tree
void Forest::popTree()
{
	msg.enter("Forest::popTree");
	// If the tree to be popped is a Filter, check that a filter type has been defined
	Refitem<Tree,bool> *ri = stack_.last();
	if (ri->data)
	{
		// Can use the 'isFilter' member function to check for the lack of a proper type
		if (!ri->item->isFilter()) msg.print("WARNING - Filter '%s' has not been provided a filter type.\n", ri->item->filter.name());
	}
	stack_.remove( stack_.last() );
	msg.exit("Forest::popTree");
}

// Generate forest from string 
bool Forest::generate(const char *s, const char *name)
{
	msg.enter("Forest::generate[string]");
	name_ = name;
	bool result = cmdparser.generate(this, s);
	finalise();
	msg.exit("Forest::generate[string]");
	return result;
}

// Generate forest from input file
bool Forest::generateFromFile(const char *filename, const char *name)
{
	msg.enter("Forest::generateFromFile");
	filename_ = filename;
	name_ = name;
	bool result = cmdparser.generateFromFile(this, filename);
// 	print();
	finalise();
	msg.exit("Forest::generateFromFile");
	return result;
}

// Delete specified tree
void Forest::deleteTree(Tree *t)
{
	// Search for the specified tree...
	if (trees_.ownsItem(t)) trees_.remove(t);
	else if (functions_.ownsItem(t)) functions_.remove(t);
	else printf("Internal Error: Tree to be deleted is not owned by the current parent structure.\n");
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
}


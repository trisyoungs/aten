/*
	*** Forest
	*** src/parser/forest.h
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

#ifndef ATEN_FOREST_H
#define ATEN_FOREST_H

#include "base/dnchar.h"
#include "parser/tree.h"

// Forest
class Forest
{
	public:
	// Constructor / Destructor
	Forest();
	~Forest();
	// List pointers
	Forest *prev, *next;

	/*
	// Tree data
	*/
	private:
	// Name, if any
	Dnchar name_;
	// Original source filename, if any
	Dnchar filename_;
	// User-defined functions (local to this structure)
	List<Tree> functions_;
	// List of trees belonging to this forest
	List<Tree> trees_;
	// Stack of created trees
	Reflist<Tree,int> stack_;

	public:
	// Clear contents of forest
	void clear();
	// Set name of forest
	void setName(const char *s);
	// Return name of forest
	const char *name();
	// Return associated filename (if any)
	const char *filename();
	// Generate forest from string 
	bool generate(const char *, const char *name = NULL);
	// Generate forest from input file
	bool generateFromFile(const char *filename, const char *name = NULL);
	// Finalise forest
	void finalise();
	// Return number of trees in forest
	int nTrees();
	// Create a new, generic (script or command) tree
	Tree *pushTree();
	// Finish the last created tree
	void popTree();
	// Delete specified tree
	void deleteTree(Tree *t);
	// Execute all trees in forest
	bool executeAll(NuReturnValue &rv);
	// Print forest information
	void print();
};

#endif

/*
	*** Forest
	*** src/parser/forest.h
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

#ifndef ATEN_FOREST_H
#define ATEN_FOREST_H

#include "base/dnchar.h"
#include "parser/tree.h"

// Forest
class Forest
{
	public:
	// Constructors / Destructor
	Forest();
	Forest(const char *name, const char *commands);
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
	// User-defined global functions (but local to this forest)
	List<Tree> functions_;
	// List of trees belonging to this forest
	List<Tree> trees_;
	// Whether this forest is being generated from a filter file...
	bool fromFilterFile_;

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
	bool generateFromString(const char *s, const char *name = NULL, bool dontpushtree = FALSE);
	// Generate forest from string list
	bool generateFromStringList(Dnchar *stringListHead, const char *name = NULL, bool dontpushtree = FALSE);
	// Generate forest from input file
	bool generateFromFile(const char *filename, const char *name = NULL, bool dontpushtree = FALSE, bool isFilterFile = FALSE);
	// Finalise forest
	void finalise();
	// Return number of trees in forest
	int nTrees();
	// Return first tree of forest
	Tree *trees();
	// Add a new, generic (filter, script or command) tree
	Tree *addTree(Tree::TreeType type);
	// Add a new Forest-global function tree
	Tree *addGlobalFunction(const char *name);
	// Search for existing global function
	Tree *findGlobalFunction(const char *s);
	// Execute specified global function
	bool executeGlobalFunction(const char *name, ReturnValue &rv, const char *arglist ...);
	// Delete specified tree
	void deleteTree(Tree *t);
	// Return whether the Forest is being generated from a filterfile
	bool isFromFilterFile();
	// Execute all trees in forest
	bool executeAll(ReturnValue &rv);
	// Print forest information
	void print();
};

#endif

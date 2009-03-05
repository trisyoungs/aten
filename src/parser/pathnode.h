/*
	*** Path Node
	*** src/parser/pathnode.h
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

#ifndef ATEN_PATHNODE_H
#define ATEN_PATHNODE_H

#include "parser/treenode.h"

// Forward declarations
class NuVariable;

// Path Node
class PathNode : public TreeNode
{
	public:
	// Constructor / Destructor
	PathNode(TreeNode *basevar);
	~PathNode();

	private:
	// Base variable where the path starts
	TreeNode *baseVariable_;

	/*
	// Path Functions
	*/
	public:
	// Finalise path, setting return value and redOnly property from last step node
	void finalise();

	/*
	// Inherited Virtuals
	*/
	public:
	// Execute node
	bool execute(NuReturnValue &rv);
	// Print node contents
	void nodePrint(int offset, const char *prefix = "");
	// Set from returnvalue node
	bool set(NuReturnValue &rv);
	// Reset node
	void reset();
	// Search accessors (if any) available for node
	StepNode *findAccessor(const char *s);
};

#endif

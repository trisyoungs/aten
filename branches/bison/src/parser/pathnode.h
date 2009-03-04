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
// #include "parser/accessstep.h"
// #include "base/vtypes.h"
// #include "base/parser.h"
// #include "base/dnchar.h"
// #include "templates/list.h"

// Forward declarations
class NuVariable;
// class ReturnValue;

// Path Node
class PathNode : public TreeNode
{
	public:
	// Constructor / Destructor
	PathNode(TreeNode *base, TreeNode *path);
	~PathNode();

	private:
	// Base variable where the path starts
	TreeNode *baseVariable_;
	// Path leading to final value
	Reflist<TreeNode,int> path_;

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
};

#endif

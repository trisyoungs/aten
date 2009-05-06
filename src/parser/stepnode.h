/*
	*** Step Node
	*** src/parser/stepnode.h
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

#ifndef ATEN_STEPNODE_H
#define ATEN_STEPNODE_H

#include "parser/treenode.h"
#include "parser/vtypes.h"

// Path Step Node
class StepNode : public TreeNode
{
	public:
	// Constructor / Destructor
	StepNode(int id, VTypes::DataType prevtype, TreeNode *arrayindex, VTypes::DataType returntype, bool readonly, bool needsindex);
	~StepNode();

	/*
	// Accessor Data
	*/
	private:
	// Expected type of preceding return value
	VTypes::DataType previousType_;
	// Accessor that this node attempts to access
	int accessor_;
	// Array index (if present)
	TreeNode *arrayIndex_;
	// Whether the accessor should have an array index
	bool requiresArrayIndex_;

	public:
	// Return associated array index
	TreeNode *arrayIndex();
	// Return accessor ID
	int accessor();
	// Return whether the accessor should have an associated array index
	bool requiresArrayIndex();
	// Set from returnvalue nodes
	bool set(ReturnValue &executerv, ReturnValue &setrv);

	/*
	// Inherited Virtuals
	*/
	public:
	// Execute node
	bool execute(ReturnValue &rv);
	// Print node contents
	void nodePrint(int offset, const char *prefix = "");
	// Set from returnvalue node
	bool set(ReturnValue &rv);
	// Reset node
	bool initialise();
	// Search accessors for the type represented by this node
	StepNode *findAccessor(const char *s, TreeNode *arrayindex);
};

#endif

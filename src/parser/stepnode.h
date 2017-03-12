/*
	*** Step Node
	*** src/parser/stepnode.h
	Copyright T. Youngs 2007-2017

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

ATEN_BEGIN_NAMESPACE

// Path Step Node
class StepNode : public TreeNode
{
	public:
	// Constructor / Destructor
	StepNode(int id, VTypes::DataType prevtype, TreeNode* arrayIndex, VTypes::DataType returntype, bool readonly, int arraySize);
	StepNode(int id, VTypes::DataType prevtype, VTypes::DataType returntype);
	~StepNode();

	/*
	 * Accessor Data
	 */
	private:
	// Expected type of preceding return value
	VTypes::DataType previousType_;
	// Accessor that this node attempts to access
	int accessor_;
	// Array index (if present)
	TreeNode* arrayIndex_;
	// Array size of accessor (-1 = list, 0 = none, 1+ = array size)
	int arraySize_;
	// Whether the id refers to a data member or a function
	bool functionAccessor_;

	public:
	// Return associated array index
	TreeNode* arrayIndex();
	// Return accessor ID
	int accessor();
	// Return array size of the associated accessor
	int arraySize();
	// Set from returnvalue nodes
	bool set(ReturnValue& executerv, ReturnValue& setrv);

	/*
	 * Inherited Virtuals
	 */
	public:
	// Execute node
	bool execute(ReturnValue& rv);
	// Print node contents
	void nodePrint(int offset, const char* prefix = "");
	// Set from returnvalue node
	bool set(ReturnValue& rv);
	// Reset node
	bool initialise();
	// Search accessors for the type represented by this node
	StepNode* findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList = NULL);
};

ATEN_END_NAMESPACE

#endif

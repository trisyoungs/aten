/*
	*** Tree Node
	*** src/parser/treenode.h
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

#ifndef ATEN_TREENODE_H
#define ATEN_TREENODE_H

#include "parser/returnvalue.h"
#include "templates/reflist.h"
#include "templates/vector3.h"
#include "base/vtypes.h"

// Forward declarations
//class CommandList;
class VariableList;
class Variable;
// class Tree;

// Tree Node
class TreeNode
{
	public:
	// Constructor / Destructor
	TreeNode();
	~TreeNode();

	/*
	// Argument Data
	*/
	private:
	// Arguments (if any) to leaf node operation
	Reflist<TreeNode,int> args_;

	public:
	// Return number of arguments currently assigned to node
	int nArgs();
	// Add new argument to node
	void addArgument(TreeNode *leaf);
	// Return argument specified
	TreeNode *arg(int i);

	/*
	// Node Character
	*/
	protected:
	// Node return value datatype
	VTypes::DataType returnType_;
	// Whether node is read-only
	bool readOnly_;

	public:
	// Sets the content type of the variable
	void setReturnType(VTypes::DataType dt);
	// Returns content type of the variable
	VTypes::DataType returnType();
	// Set parent variablelist
// 	void setParent(VariableList *vlist);
	// Set the readonly status of the node to TRUE
	void setReadOnly();
	// Return the readonly status of the node
	bool readOnly();

	/*
	// Node Data Set / Get / Execute
	*/
	public:
	// Set value of node (int)
	virtual bool set(int i);
	// Get value of node as integer
	virtual int asInteger();
	// Step node
	virtual bool step(int delta);
	// Reset node contents
	virtual bool reset();
	// Get reduced value of node
	virtual int execute(NuReturnValue &rv);
};

#endif

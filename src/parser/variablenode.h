/*
	*** Variable Node
	*** src/parser/variablenode.h
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

#ifndef ATEN_VARIABLENODE_H
#define ATEN_VARIABLENODE_H

#include "parser/treenode.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Variable;

// Variable Node
class VariableNode : public TreeNode
{
	public:
	// Constructor / Destructor
	VariableNode(Variable* v = NULL);
	~VariableNode();

	/*
	 * Variable Data
	 */
	private:
	// Variable that this node links to
	Variable* variable_;
	// Array index (if any)
	TreeNode* arrayIndex_;
	
	public:
	// Set variable target
	void setVariable(Variable* v);
	// Get variable target
	Variable* variable();
	// Set array index
	void setArrayIndex(TreeNode* index);
	// Return array index
	TreeNode* arrayIndex();	
	// Return name of variable target
	QString name();
	// Finalise variable path (if there is one)
	void finalisePath();

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
	// Initialise node
	bool initialise();
	// Search accessors (if any) available for linked variable
	StepNode* findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList = NULL);
};

ATEN_END_NAMESPACE

#endif

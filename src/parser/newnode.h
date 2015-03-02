/*
	*** 'New' Command Node
	*** src/parser/newnode.h
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

#ifndef ATEN_NEWNODE_H
#define ATEN_NEWNODE_H

#include "parser/treenode.h"
//#include "parser/variablelist.h"
#include "parser/treenode.h"
// #include "parser/vtypes.h"
// #include "parser/returnvalue.h"

ATEN_BEGIN_NAMESPACE

// 'New' Command Node
class NewNode : public TreeNode
{
	public:
	// Constructor / Destructor
	NewNode(VTypes::DataType type);
	~NewNode();


	/*
	// Variables
	*/
	private:
	// Type of object to create when executed
	VTypes::DataType type_;
	// Pointer to object created
	void *object_;


	/*
	// Set / Get / Execute
	*/
	public:
	// Execute command
	bool execute(ReturnValue& rv);
	// Set from returnvalue node
	bool set(ReturnValue& rv);
	// Initialise node
	bool initialise();
	// Print layout of current node
	void nodePrint(int offset, const char* prefix = "");

};

ATEN_END_NAMESPACE

#endif

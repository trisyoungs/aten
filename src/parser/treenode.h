/*
	*** Tree Node
	*** src/parser/treenode.h
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

#ifndef ATENCALC_TREENODE_H
#define ATENCALC_TREENODE_H

#include "parser/returnvalue.h"
#include "templates/reflist.h"
#include "templates/vector3.h"
#include "parser/vtypes.h"
#include <QtOpenGL/QtOpenGL>

// Forward declarations
class ScopeNode;
class StepNode;
class Tree;

// Tree Node
class TreeNode
{
	public:
	// Constructor / Destructor
	TreeNode();
	virtual ~TreeNode();
	// List pointers
	TreeNode *prev, *next;
	// List pointers (for argument list)
	TreeNode *nextArgument, *prevArgument;
	// Node Types
	enum NodeType { BasicNode, CmdNode, ScopedNode, VarNode, VarWrapperNode, SteppedNode, ArrayVarNode, ArrayConstantNode, UserCmdNode, GuiFilterOptNode, nNodeTypes };
	// Copy data
	void copy(TreeNode *source);


	/*
	// Node Type
	*/
	protected:
	// Type of node
	NodeType nodeType_;
	// Pointer to parent tree
	Tree *parent_;

	public:
	// Retrieve node type
	NodeType nodeType() const;
	// Set parent 
	void setParent(Tree *parent);
	// Retrieve parent
	Tree *parent() const;


	/*
	// Argument Data
	*/
	protected:
	// Arguments (if any) to leaf node operation
	Reflist<TreeNode,int> args_;

	public:
	// Return number of arguments currently assigned to node
	int nArgs() const;
	// Return datatype of nth argument
	VTypes::DataType argType(int i);
	// Add list of arguments formas as a plain List<TreeNode>, beginning from supplied list head
	void addListArguments(TreeNode *leaf);
	// Add list of arguments joined by parser, probably with list tail supplied
	void addJoinedArguments(TreeNode *args);
	// Add multiple arguments to node
	void addArguments(int nargs, ...);
	// Add multiple arguments to node
	void addArgument(TreeNode *arg);
	// Check arguments stored in argument list
	bool checkArguments(const char *arglist, const char *funcname);
	// Return (execute) argument specified
	bool arg(int i, ReturnValue &rv);
	// Return (execute) argument specified as a bool
	bool argb(int i);
	// Return (execute) argument specified as an integer
	int argi(int i);
	// Return (execute) argument specified as a double
	double argd(int i);
	// Return (execute) argument specified as a character
	const char *argc(int i);
	// Return (execute) argument specified as a vector
	Vec3<double> argv(int i);
	// Return (execute) argument specified as a pointer
	void *argp(int i, VTypes::DataType type);
	// Return (execute) triplet of 'double' arguments, starting from argument specified
	Vec3<double> arg3d(int i);
	// Return (execute) triplet of 'int' arguments, starting from argument specified
	Vec3<int> arg3i(int i);
	// Return the TreeNode corresponding to the argument, rather than executing it
	TreeNode *argNode(int i);
	// Set argument specified from ReturnValue
	bool setArg(int i, ReturnValue &rv);
	// Return whether argument i was given
	bool hasArg(int i);


	/*
	// Node Character
	*/
	protected:
	// Node return value datatype
	VTypes::DataType returnType_;
	// Whether node is read-only
	bool readOnly_;
	// Whether the node returns an array of values
	bool returnsArray_;

	public:
	// Sets the content type of the variable
	void setReturnType(VTypes::DataType dt);
	// Returns content type of the variable
	VTypes::DataType returnType() const;
	// Set the readonly status of the node to TRUE
	void setReadOnly();
	// Return the readonly status of the node
	bool readOnly() const;
	// Set whether an array of values is returned
	void setReturnsArray(bool b);
	// Return whether an array of values is returned
	bool returnsArray() const;


	/*
	// Node Data Set / Get / Execute
	*/
	public:
	// Set from returnvalue node
	virtual bool set(ReturnValue &rv) = 0;
	// Get reduced value of node
	virtual bool execute(ReturnValue &rv) = 0;
	// Print layout of current node
	virtual void nodePrint(int offset, const char *prefix = "") = 0;
	// Reset node
	virtual bool initialise() = 0;
	// Search accessors (if any) available for node
	virtual StepNode *findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist = NULL);
};

#endif

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
	~TreeNode();
	// List pointers
	TreeNode *prev, *next;
	// List pointers (for argument list)
	TreeNode *nextArgument, *prevArgument;
	// Node Types
	enum NodeType { BasicNode, CmdNode, ScopedNode, VarNode, VarWrapperNode, SteppedNode, ArrayVarNode, nNodeTypes };


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
	NodeType nodeType();
	// Set parent 
	void setParent(Tree *parent);
	// Retrieve parent
	Tree *parent();


	/*
	// Argument Data
	*/
	protected:
	// Arguments (if any) to leaf node operation
	Reflist<TreeNode,int> args_;

	public:
	// Return number of arguments currently assigned to node
	int nArgs();
	// Return datatype of nth argument
	NuVTypes::DataType argType(int i);
	// Add reverse-sorted list of arguments
	void addArgumentList(TreeNode *args);
	// Add multiple arguments to node
	void addArguments(int nargs, ...);
	// Add multiple arguments to node
	void addArgument(TreeNode *arg);
	// Return (execute) argument specified
	bool arg(int i, NuReturnValue &rv);
	// Return (execute) argument specified as a bool
	bool argb(int i);
	// Return (execute) argument specified as an integer
	int argi(int i);
	// Return (execute) argument specified as a double
	double argd(int i);
	// Return (execute) argument specified as a GLFloat
	GLfloat argGLf(int i);
	// Return (execute) argument specified as a character
	const char *argc(int i);
	// Return (execute) argument specified as a pointer
	void *argp(int i, NuVTypes::DataType type);
	// Return (execute) triplet of 'double' arguments, starting from argument specified
	Vec3<double> arg3d(int i);
	// Return (execute) triplet of 'int' arguments, starting from argument specified
	Vec3<int> arg3i(int i);
	// Return (execute) triplet of 'GLfloat' arguments, starting from argument specified
	Vec3<GLfloat> arg3GLf(int i);
	// Return the TreeNode corresponding to the argument, rather than executing it
	TreeNode *argNode(int i);
	// Set argument specified
	bool setArg(int i, NuReturnValue &rv);
	// Return whether argument i was given
	bool hasArg(int i);


	/*
	// Node Character
	*/
	protected:
	// Node return value datatype
	NuVTypes::DataType returnType_;
	// Whether node is read-only
	bool readOnly_;	

	public:
	// Sets the content type of the variable
	void setReturnType(NuVTypes::DataType dt);
	// Returns content type of the variable
	NuVTypes::DataType returnType();
	// Set the readonly status of the node to TRUE
	void setReadOnly();
	// Return the readonly status of the node
	bool readOnly();

	/*
	// Node Data Set / Get / Execute
	*/
	public:
	// Set from returnvalue node
	virtual bool set(NuReturnValue &rv) = 0;
	// Get reduced value of node
	virtual bool execute(NuReturnValue &rv) = 0;
	// Print layout of current node
	virtual void nodePrint(int offset, const char *prefix = "") = 0;
	// Reset node
	virtual bool initialise() = 0;
	// Search accessors (if any) available for node
	virtual StepNode *findAccessor(const char *s, bool array);
};

#endif

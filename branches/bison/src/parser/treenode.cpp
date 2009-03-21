/*
	*** Tree Node
	*** src/parser/treenode.cpp
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

#include "parser/treenode.h"
#include "parser/tree.h"
#include "base/sysfunc.h"
#include "templates/reflist.h"
#include <stdarg.h>

// Constructors
TreeNode::TreeNode()
{
	// Private variables
	returnType_ = NuVTypes::NoData;
	readOnly_ = TRUE;
	parent_ = NULL;
	nextArgument = NULL;
	prevArgument = NULL;
	nodeType_ = TreeNode::BasicNode;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
TreeNode::~TreeNode()
{
}

// Retrieve node type
TreeNode::NodeType TreeNode::nodeType()
{
	return nodeType_;
}

// Set parent 
void TreeNode::setParent(Tree *parent)
{
	parent_ = parent;
}

// Retrieve parent
Tree *TreeNode::parent()
{
	return parent_;
}

// Sets the content type of the variable
void TreeNode::setReturnType(NuVTypes::DataType dt)
{
	returnType_ = dt;
}

// Returns content type of the variable
NuVTypes::DataType TreeNode::returnType()
{
	return returnType_;
}

// Return readonly status
bool TreeNode::readOnly()
{
	return readOnly_;
}

// Set the readonly status of the variable to TRUE
void TreeNode::setReadOnly()
{
	readOnly_ = TRUE;
}

// Return number of arguments currently assigned to node
int TreeNode::nArgs()
{
	return args_.nItems();
}

// Return datatype of nth argument
NuVTypes::DataType TreeNode::argType(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("Argument index %i is out of range.\n", i);
		return NuVTypes::NoData;
	}
	return args_[i]->item->returnType();
}


// Set argument specified
bool TreeNode::setArg(int i, NuReturnValue &rv)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("Argument index %i is out of range.\n", i);
		return FALSE;
	}
	// Check readonly attribute of argument
	if (args_[i]->item->readOnly())
	{
		args_[i]->item->nodePrint(0);
		msg.print("Argument %i is read-only and can't be set.\n", i);
		return FALSE;
	}
	return args_[i]->item->set(rv);
}

// Return whether argument i was given
bool TreeNode::hasArg(int i)
{
	return (i < args_.nItems());
}

// Add argument(s) to node
void TreeNode::addArgumentList(TreeNode *leaf)
{
	/*
	The supplied leaf may be a single node, or it may be a list of nodes.
	In the case of a list, we must walk backwards through the list to get to the beginning since the parser will provide only the last node of the list
	*/
	TreeNode *first;
	for (first = leaf; first != NULL; first = first->prevArgument) if (first->prevArgument == NULL) break;
	for (TreeNode *node = first; node != NULL; node = node->nextArgument) args_.add(node);
}

// Add multiple arguments to node
void TreeNode::addArguments(int nargs, ...)
{
	// Create variable argument parser
	va_list vars;
	va_start(vars,nargs);
	// Add arguments in the order they were provided
	for (int n=0; n<nargs; n++) addArgument(va_arg(vars, TreeNode*));
	va_end(vars);
	msg.print(Messenger::Parse,"Node %li now has %i arguments.\n", this, args_.nItems());
}

// Add multiple arguments to node
void TreeNode::addArgument(TreeNode *arg)
{
	args_.add(arg);
}

// Return (execute) argument specified
bool TreeNode::arg(int i, NuReturnValue &rv)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("Argument index %i is out of range.\n", i);
		return FALSE;
	}
	return args_[i]->item->execute(rv);
}

// Return (execute) argument specified as a bool
bool TreeNode::argb(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("Argument index %i is out of range.\n", i);
		return FALSE;
	}
	static NuReturnValue rv;
	bool success;
	bool result;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	result = (rv.asInteger(success) > 0);
	if (!success) msg.print("Couldn't cast argument %i into an integer.\n", i+1);
	return result;
}

// Return (execute) argument specified as an integer
int TreeNode::argi(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("Argument index %i is out of range.\n", i);
		return FALSE;
	}
	static NuReturnValue rv;
	bool success;
	int result = 0;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	result = rv.asInteger(success);
	if (!success) msg.print("Couldn't cast argument %i into an integer.\n", i+1);
	return result;
}

// Return (execute) argument specified as a double
double TreeNode::argd(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("Argument index %i is out of range.\n", i);
		return FALSE;
	}
	static NuReturnValue rv;
	bool success;
	double result = 0.0;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	result = rv.asReal(success);
	if (!success) msg.print("Couldn't cast argument %i into a real.\n", i+1);
	return result;
}

// Return (execute) argument specified as a GLFloat
GLfloat TreeNode::argGLf(int i)
{
	return (GLfloat) argd(i);
}

// Return (execute) argument specified as a character
const char *TreeNode::argc(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("Argument index %i is out of range.\n", i);
		return FALSE;
	}
	static NuReturnValue rv;
	bool success;
	const char *result = NULL;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	result = rv.asString(success);
	if (!success) msg.print("Couldn't cast argument %i into a character.\n", i+1);
	return result;
}

// Return (execute) argument specified as a pointer
void *TreeNode::argp(int i, NuVTypes::DataType type)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("Argument index %i is out of range.\n", i);
		return FALSE;
	}
	static NuReturnValue rv;
	bool success;
	void *result = NULL;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	result = rv.asPointer(type, success);
	if (!success) msg.print("Couldn't cast argument %i into a pointer of type '%s'.\n", i+1, NuVTypes::dataType(type));
	return result;
}

// Return (execute) argument triplet specified
Vec3<double> TreeNode::arg3d(int i)
{
	if ((i < 0) || (i > (args_.nItems()-3)))
	{
		printf("Argument index %i is out of range for a triplet.\n", i);
		return FALSE;
	}
	Vec3<double> result;
	result.set(argd(i), argd(i+1), argd(i+2));
	return result;
}

// Return (execute) argument triplet specified
Vec3<int> TreeNode::arg3i(int i)
{
	if ((i < 0) || (i > (args_.nItems()-3)))
	{
		printf("Argument index %i is out of range for a triplet.\n", i);
		return FALSE;
	}
	Vec3<int> result;
	result.set(argi(i), argi(i+1), argi(i+2));
	return result;
}

// Return (execute) argument triplet specified
Vec3<GLfloat> TreeNode::arg3GLf(int i)
{
	if ((i < 0) || (i > (args_.nItems()-3)))
	{
		printf("Argument index %i is out of range for a triplet.\n", i);
		return FALSE;
	}
	Vec3<GLfloat> result;
	result.set(argGLf(i), argGLf(i+1), argGLf(i+2));
	return result;
}

// Return the TreeNode corresponding to the argument, rather than executing it
TreeNode *TreeNode::argNode(int i)
{
	if ((i < 0) || (i > args_.nItems()))
	{
		printf("Argument index %i is out of range for returning the argument node.\n", i);
		return FALSE;
	}
	return args_[i]->item;
}

/*
// Virtuals
*/

// Search accessors (if any) available for node
StepNode *TreeNode::findAccessor(const char *s, bool array)
{
	// Default is to retun NULL since there are no accessors available
	printf("Internal Error: This node type does not have any accessors.\n");
	return NULL;
}

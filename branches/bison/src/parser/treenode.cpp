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
#include "main/aten.h"
#include "base/atom.h"
#include <stdarg.h>

// Constructors
TreeNode::TreeNode()
{
	// Private variables
	returnType_ = VTypes::NoData;
	returnsArray_ = FALSE;
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
void TreeNode::setReturnType(VTypes::DataType dt)
{
	returnType_ = dt;
}

// Returns content type of the variable
VTypes::DataType TreeNode::returnType()
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

// Set whether an array of values is returned
void TreeNode::setReturnsArray(bool b)
{
	returnsArray_ = b;
}

// Return whether an array of values is returned
bool TreeNode::returnsArray()
{
	return returnsArray_;
}

// Return number of arguments currently assigned to node
int TreeNode::nArgs()
{
	return args_.nItems();
}

// Return datatype of nth argument
VTypes::DataType TreeNode::argType(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argType : Argument index %i is out of range (node = %li).\n", i, this);
		return VTypes::NoData;
	}
	return args_[i]->item->returnType();
}


// Set argument specified
bool TreeNode::setArg(int i, ReturnValue &rv)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::setArg : Argument index %i is out of range (node = %li).\n", i, this);
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
bool TreeNode::arg(int i, ReturnValue &rv)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::arg : Argument index %i is out of range (node = %li).\n", i, this);
		return FALSE;
	}
	return args_[i]->item->execute(rv);
}

// Return (execute) argument specified as a bool
bool TreeNode::argb(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argb : Argument index %i is out of range (node = %li).\n", i, this);
		return FALSE;
	}
	static ReturnValue rv;
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
		printf("TreeNode::argi : Argument index %i is out of range (node = %li).\n", i, this);
		return FALSE;
	}
	static ReturnValue rv;
	bool success;
	int result = 0;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	result = rv.asInteger(success);
	if (!success) msg.print("Couldn't cast argument %i into an integer.\n", i+1);
	return result;
}

// Return (execute) argument specified as an atomic number
short int TreeNode::argz(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argz : Argument index %i is out of range (node = %li).\n", i, this);
		return FALSE;
	}
	static ReturnValue rv;
	bool success;
	Namemap<int> *nm;
	Atom *atm;
	short int result = 0;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	switch (rv.type())
	{
		case (VTypes::IntegerData):
			result = rv.asInteger();
			break;
		case (VTypes::DoubleData):
			result = (short int) floor(rv.asDouble() + 0.15);
			break;
		case (VTypes::StringData):
			// Attempt conversion of the string first from the users type list
			for (nm = aten.typeMap.first(); nm != NULL; nm = nm->next)
				if (strcmp(nm->name(),rv.asString()) == 0) break;
			if (nm == NULL) result = elements().find(rv.asString());
			else result = nm->data();
			break;
		case (VTypes::AtomData):
			atm = (Atom*) rv.asPointer(VTypes::AtomData);
			atm == NULL ? result = 0 : atm->element();
			break;
		default:
			msg.print("Couldn't cast argument %i into an atomic number.\n", i+1);
			result = 0;
			break;
	}
	return result;
}

// Return (execute) argument specified as a double
double TreeNode::argd(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argd : Argument index %i is out of range (node = %li).\n", i, this);
		return FALSE;
	}
	static ReturnValue rv;
	bool success;
	double result = 0.0;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	result = rv.asDouble(success);
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
		printf("TreeNode::argc : Argument index %i is out of range (node = %li).\n", i, this);
		return FALSE;
	}
	static ReturnValue rv;
	bool success;
	const char *result = NULL;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	result = rv.asString(success);
	if (!success) msg.print("Couldn't cast argument %i into a character.\n", i+1);
	return result;
}

// Return (execute) argument specified as a vector
Vec3<double> TreeNode::argv(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argv : Argument index %i is out of range (node = %li).\n", i, this);
		return FALSE;
	}
	static ReturnValue rv;
	bool success;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	return rv.asVector(success);
}

// Return (execute) argument specified as a pointer
void *TreeNode::argp(int i, VTypes::DataType type)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argp : Argument index %i is out of range (node = %li).\n", i, this);
		return FALSE;
	}
	static ReturnValue rv;
	bool success;
	void *result = NULL;
	if (!args_[i]->item->execute(rv)) msg.print("Couldn't retrieve argument %i.\n", i+1);
	result = rv.asPointer(type, success);
	if (!success) msg.print("Couldn't cast argument %i into a pointer of type '%s'.\n", i+1, VTypes::dataType(type));
	return result;
}

// Return (execute) argument triplet specified
Vec3<double> TreeNode::arg3d(int i)
{
	if ((i < 0) || (i > (args_.nItems()-3)))
	{
		printf("TreeNode::arg3d : Argument index %i is out of range for a triplet (node = %li).\n", i, this);
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
		printf("TreeNode::arg3i : Argument index %i is out of range for a triplet (node = %li).\n", i, this);
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
		printf("TreeNode::arg3GLf : Argument index %i is out of range for a triplet (node = %li).\n", i, this);
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
		printf("TreeNode::argNode : Argument index %i is out of range for returning the argument node (node = %li).\n", i, this);
		return FALSE;
	}
	return args_[i]->item;
}

/*
// Virtuals
*/

// Search accessors (if any) available for node
StepNode *TreeNode::findAccessor(const char *s, TreeNode *arrayindex)
{
	// Default is to retun NULL since there are no accessors available
	printf("Internal Error: This node type does not have any accessors.\n");
	return NULL;
}

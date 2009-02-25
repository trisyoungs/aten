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

#include "parser/tree.h"
#include "parser/treenode.h"
#include "variables/accesspath.h"
#include "base/sysfunc.h"
#include "templates/reflist.h"

// Constructors
TreeNode::TreeNode()
{
	// Private variables


	// Public variables
	next = NULL;
	prev = NULL;
}

// Destructor
TreeNode::~TreeNode()
{
}

/*
// Execute
*/


/*
// Argument Creation
*/


/*
// Argument Access
*/

// Return variable argument
// Variable *TreeNode::arg(int argno)
// {
// 	Refitem<Variable,int> *ri = args_[argno];
// 	return ri->item;
// }

// Return argument as character
// const char *TreeNode::argc(int argno)
// {
// 	Tree *t = args_[argno];
// 	return (t == NULL ?  "NULL" : ri->item->asCharacter());
// }

// Return argument as integer
int TreeNode::argi(int argno)
{
	Tree *t = args_[argno];
// 	return (t == NULL ?  0 : ri->item->asInteger());
}

// Return argument as double
// double TreeNode::argd(int argno)
// {
// 	Refitem<Variable,int> *ri = args_[argno];
// 	return (ri == NULL ? 0.0 : ri->item->asDouble());
// }

// Return argument as float
// float TreeNode::argf(int argno)
// {
// 	return (float) argd(argno);
// }

// Return argument as bool
// bool TreeNode::argb(int argno)
// {
// 	Refitem<Variable,int> *ri = args_[argno];
// 	return (ri == NULL ? -1 : ri->item->asBool());
// }

// Return argument as pointer
// void *TreeNode::argp(int argno, VTypes::DataType dt)
// {
// 	Refitem<Variable,int> *ri = args_[argno];
// 	return (ri == NULL ? NULL : ri->item->asPointer(dt));
// }

// Returns whether argument 'n' was provided
// bool TreeNode::hasArg(int argno)
// {
// 	return ((argno+1) > args_.nItems() ? FALSE : TRUE);
// }

// Return variable type of argument
VTypes::DataType TreeNode::argt(int argno)
{
	Tree *t = args_[argno];
// 	return (ri == NULL ? VTypes::NoData : ri->item->type());
}

// Return arguments as Vec3<double>
// Vec3<double> TreeNode::arg3d(int i)
// {
// 	msg.enter("TreeNode::arg3d");
//         static Vec3<double> result;
//         if (i > (args_.nItems()-3)) printf("TreeNode::arg3d - Starting point too close to end of argument list.\n");
//         result.set(args_[i]->item->asDouble(), args_[i+1]->item->asDouble(), args_[i+2]->item->asDouble());
// 	msg.exit("TreeNode::arg3d");
//         return result;
// }

// Return arguments as Vec3<float>
// Vec3<float> TreeNode::arg3f(int i)
// {
// 	msg.enter("TreeNode::arg3f");
//         static Vec3<float> result;
//         if (i > (args_.nItems()-3)) printf("TreeNode::arg3f - Starting point too close to end of argument list.\n");
//         result.set(args_[i]->item->asFloat(), args_[i+1]->item->asFloat(), args_[i+2]->item->asFloat());
// 	msg.exit("TreeNode::arg3f");
//         return result;
// }

// Return arguments as Vec3<int>
// Vec3<int> TreeNode::arg3i(int i)
// {
// 	msg.enter("TreeNode::arg3i");
// 	static Vec3<int> result;
// 	if (i > (args_.nItems()-3)) printf("TreeNode::arg3i - Starting point too close to end of argument list.\n");
//         result.set(args_[i]->item->asInteger(), args_[i+1]->item->asInteger(), args_[i+2]->item->asInteger());
// 	msg.exit("TreeNode::arg3i");
// 	return result;
// }

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

// Return number of arguments given to command
int TreeNode::nArgs()
{
	return args_.nItems();
}

/*
// Set/get (virtuals)
*/

// Set value of variable (int)
bool TreeNode::set(int i)
{
	printf("This node cannot be 'set'.\n");
	return FALSE;
}

// Get value of variable as integer
int TreeNode::asInteger()
{
	printf("This node cannot be returned as an integer.\n");
	return 0;
}

// Step variable
bool TreeNode::step(int delta)
{
	printf("This node cannot be 'stepped'.\n");
	return FALSE;
}

// Reset variable contents
bool TreeNode::reset()
{
	printf("This node cannot be 'reset'.\n");
	return FALSE;
}

// Get reduced value of node
int TreeNode::execute(NuReturnValue &rv)
{
	printf("This node cannot be executed since it is of base-class type.\n");
	return -1;
}


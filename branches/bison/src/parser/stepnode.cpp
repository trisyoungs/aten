/*
	*** Step Node
	*** src/parser/stepnode.cpp
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

#include "parser/stepnode.h"
#include "parser/aten.h"
#include "parser/atom.h"
#include "parser/model.h"
#include "parser/vector.h"
#include <string.h>

// Constructor
StepNode::StepNode(int id, NuVTypes::DataType prevtype, NuVTypes::DataType rtntype) : accessor_(id), previousType_(prevtype)
{
	// Private variables
	readOnly_ = FALSE;
	returnType_ = rtntype;
	printf("Return type of StepNode is %s\n", NuVTypes::dataType(returnType_));
}

// Destructor
StepNode::~StepNode()
{
}

// Execute command
bool StepNode::execute(NuReturnValue &rv)
{
	msg.enter("StepNode::execute");
	// Check that the ReturnValue contains the type that we are expecting
	if (rv.type() != previousType_)
	{
		printf("Internal Error: StepNode was expecting a type of '%s' but was given type '%s'\n", NuVTypes::dataType(previousType_), NuVTypes::dataType(rv.type()));
		msg.exit("StepNode::execute");
		return FALSE;
	}
	// Retrieve a value from the relevant class
	bool result = FALSE;
	switch (previousType_)
	{
		case (NuVTypes::NoData):
			printf("Internal Error: StepNode was expecting NoData.\n");
			break;
		case (NuVTypes::AtomData):
			result = AtomVariable::retrieveAccessor(accessor_, rv, FALSE);
			break;
		case (NuVTypes::ModelData):
			result = ModelVariable::retrieveAccessor(accessor_, rv, FALSE);
			break;
		case (NuVTypes::VectorData):
			result = NuVectorVariable::retrieveAccessor(accessor_, rv, FALSE);
			break;
		default:
			printf("Internal Error: StepNode doesn't recognise this type (%s)\n", NuVTypes::dataType(previousType_));
			break;
	}
	msg.exit("StepNode::execute");
	return result;
}

// Print node contents
void StepNode::nodePrint(int offset, const char *prefix)
{
	printf("Cannot print the contents of an StepNode (yet).\n");
}

// Set from returnvalue node
bool StepNode::set(NuReturnValue &rv)
{
	printf("Cannot SET an StepNode (yet).\n");
	return FALSE;
}

// Reset node
void StepNode::reset()
{
	return;
}

// Static function to search accessors of type represented by this path step
StepNode *StepNode::findAccessor(const char *s)
{
	msg.enter("StepNode::findAccessor");
	// From the return type of the node, determine which (static) function to call
	StepNode *result = NULL;
	switch (returnType_)
	{
		case (NuVTypes::NoData):
			printf("Internal Error: StepNode was expecting NoData.\n");
			break;
		case (NuVTypes::AtomData):
			result = AtomVariable::accessorSearch(s);
			break;
		case (NuVTypes::ModelData):
			result = ModelVariable::accessorSearch(s);
			break;
		case (NuVTypes::VectorData):
			result = NuVectorVariable::accessorSearch(s);
			break;
		default:
			printf("Internal Error: StepNode doesn't recognise this type (%s)\n", NuVTypes::dataType(returnType_));
			break;
	}
	msg.exit("StepNode::findAccessor");
	return result;
}
